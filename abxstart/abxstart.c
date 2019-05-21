/*
 * abxStart.c --
 *
 * Main entry point for abxStart applicaiton.
 *
 * Copyright (c) 1998 Abstrax Inc.
 * Permission to copy this software, in whole or in part, to use this
 * software for any lawful purpose, and to redistribute this software
 * is granted subject to the restriction that all copies made of this
 * software must include this copyright notice in full.  This software
 * is provided AS IS, with NO WARRANTY, EITHER EXPRESS OR IMPLIED,
 * INCLUDING BUT NOT LIMITED TO IMPLIED WARRANTIES OF MERCHANTABILITY
 * OR FITNESS FOR ANY PARTICULAR PURPOSE.  IN NO EVENT SHALL THE
 * AUTHORS BE LIABLE FOR CONSEQUENTIAL OR INCIDENTAL DAMAGES OF ANY
 * NATURE WHATSOEVER.
 */

/*
 * The -Oy- setting is for VC++ 5.0.  Ignore errors with VC++ 4.0.
 *
 * Build with DEBUG defined to get debugging message window output.
 *
 * Usage abxStart <options> <command to execute>
 *
 * Options:
 *
 * -0<file>	File to use for standard input.  Read end of a write close
 *              anonymous pipe if not specified.
 * -1<file>	File to use for standard output.  NUL: if not specified.
 * -2<file>	File to use for standard error.  NUL: if not specified.
 * -c	        Allocate new console and hide first window. 
 *              Use when using command shells.
 * -r	        Do not expand command line environment variables, execute raw.
 * -v	        Version number of AppBase to look for in the registry,
 *              1.0 by default.
 * --	        End of options
 * 
 * The programs augments the environment of the child process with an
 * envirnment variable.
 * 
 * AbxAB	Set the to the value of HKEY_LOCAL_MACHINE
 *              Software\Abstrax\AppBase\<version>\Location.
 * 
 * An environment file "abstrax.env" is also processed if found.  It is
 * searched for from the current directory, and then the method used
 * bye SearchPath.
 *
 * Remember to place cmd.exe /c or command.com /c before any bat files
 * and use the -c option.  The first command  token must have a file
 * extension unless it is .exe.
 *
 * --------
 *
 * The basic idea of winMain.c was to create a hidden console window
 * for scheme to perform its input and output to while swl GUI windows
 * appear on the display.  Win32 API determines if an application is a
 * GUI or a console application from the entry point:  winMain or main
 * respectively.  To build winMain.c execute cl -Ox winMain.c
 * user32.lib.  As a side-effect of this hiding of the first window,
 * GUI applications such as calc.exe execute, but are not seen.  This
 * is not a problem because this program is only needed for console
 * bound applications.
 *
 * You specify an executable command line to winMain.exe and a new
 * unmapped console is always created for the new process.  The first
 * whitespace delimited token of the command line is the file to
 * execute using normal search path rules for the OS.  However, if no
 * file extension is present, .exe is assumed.  WindowsNT will
 * automatically execute a cmd.exe for a .bat file, but Windows95/98
 * requires you to do it, i.e. winMain command.com /c scheme.bat.
 * NOTE: the .com extension is required, otherwise .exe would be
 * assumed.
 *
 * The unmapped console window I/O worked fine under WindowsNT, but not
 * so Windows95/98.  Under Windows95/98 and console window only
 * processes I/O when it has focus.  Therefore, in this version of
 * winMain.c we redirect stdout and stderr to the NUL: device.  Since
 * some 32-bit applications cannot read from a NUL: device, we use the
 * read end of an anonymous pipe with the write end closed for stdin.
 *
 * We are working on other versions that allow stdin, stdout, and
 * stderr to be redirected from/to files or non-console handles.  This
 * is almost done.
 */

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN
#include <winbase.h>
#include <winuser.h>
#include <winreg.h>
#include <locale.h>
#include <ctype.h>
#include <io.h>
#include <fcntl.h>
#include <abxStart.h>

static TCHAR *get_token(TCHAR *, TCHAR **);
static void show_error(TCHAR *);
static void augment_environment (TCHAR *);
static void read_env_file(HANDLE);

#if defined(DEBUG)
static TCHAR debug_buf[4096];

static void show_debug(TCHAR *);
#endif  /* if defined(DEBUG) */


/*
 *----------------------------------------------------------------------
 *
 * WinMain --
 *
 * Main entry point from Windows.
 *
 * Results:
 *      Returns 0 if initialization fails, otherwise it never
 *      returns.
 *
 * Side effects:
 *      None
 *
 *----------------------------------------------------------------------
 */

int APIENTRY
WinMain(hInstance, hPrevInstance, lpCmdLine, nCmdShow)
   HINSTANCE hInstance;
   HINSTANCE hPrevInstance;
   LPSTR lpCmdLine;
   int nCmdShow;
{
   STARTUPINFO sinfo;
   PROCESS_INFORMATION pinfo;
   BOOL rc;
   BOOL expand_cmd = TRUE;
   SECURITY_ATTRIBUTES satts;
   HANDLE wh;
   TCHAR *p;
   TCHAR *end_p;
   TCHAR tmp_char;
   DWORD mode;
   DWORD setup_err = NO_ERROR;
   TCHAR msg_buf[4096];
   TCHAR *msg_ptr = msg_buf;
   TCHAR *cmd;
   TCHAR *exp_buf = NULL;
   DWORD exp_len = 0;
   DWORD creation_flags;
   TCHAR *app_base_ver = DEFAULT_APP_BASE_VERSION;

   sinfo.cb = sizeof(sinfo);
   sinfo.lpReserved = NULL;
   sinfo.lpDesktop = NULL;
   sinfo.lpTitle = _T("abxStart");
   sinfo.dwFlags = STARTF_USESTDHANDLES | STARTF_FORCEONFEEDBACK;
   sinfo.wShowWindow = SW_HIDE;
   sinfo.cbReserved2 = 0;
   sinfo.lpReserved2 = NULL;
   sinfo.hStdInput = INVALID_HANDLE_VALUE;
   sinfo.hStdOutput = INVALID_HANDLE_VALUE;
   sinfo.hStdError = INVALID_HANDLE_VALUE;

   satts.nLength = sizeof(SECURITY_ATTRIBUTES);
   satts.lpSecurityDescriptor = NULL;
   satts.bInheritHandle = TRUE;

   creation_flags = DETACHED_PROCESS;

   /*
    * Set up the default locale to be standard "C" locale so parsing
    * is performed correctly.
    */

   setlocale(LC_CTYPE, "C");

#if defined(DEBUG)
   wsprintf(debug_buf, _T("Raw command line:\n\n|%s|"), lpCmdLine);
   show_debug(debug_buf);
#endif  /* if defined(DEBUG) */

   /*
   * Process the command line for arguments
   */
   p = lpCmdLine;

   while (isspace(*p)) {
      p++;
   }

   rc = *p == '-';
   while (rc) {
      p++;

      switch (*p) {
         case '0':
            p++;
            p = get_token(p, &end_p);
            tmp_char = *end_p;
            *end_p = '\0';

            if (sinfo.hStdInput == INVALID_HANDLE_VALUE) {
               sinfo.hStdInput = CreateFile(p, GENERIC_READ,
                                            FILE_SHARE_READ,
                                            &satts, OPEN_EXISTING,
                                            FILE_ATTRIBUTE_NORMAL, NULL);

               if (sinfo.hStdInput == INVALID_HANDLE_VALUE) {
                  wsprintf(msg_ptr,
                           _T("Error opening stdin file \"%s\", error = %d\n"),
                           p, GetLastError());
                  msg_ptr += lstrlen(msg_ptr);
#if defined(DEBUG)
               } else {
                  wsprintf(debug_buf, _T("stin = \"%s\", handle %d\n"), p,
                           sinfo.hStdInput);
                  show_debug(debug_buf);
#endif  /* if defined(DEBUG) */
               }
            }

            *end_p = tmp_char;
            p = end_p++;
         break;

         case '1':
            p++;
            p = get_token(p, &end_p);
            tmp_char = *end_p;
            *end_p = '\0';

            if (sinfo.hStdOutput == INVALID_HANDLE_VALUE) {
               sinfo.hStdOutput = CreateFile(p, GENERIC_WRITE,
                                             FILE_SHARE_READ,
                                             &satts, CREATE_ALWAYS,
                                             FILE_ATTRIBUTE_NORMAL, NULL);

               if (sinfo.hStdOutput == INVALID_HANDLE_VALUE) {
                  wsprintf(msg_ptr,
                           _T("Error opening stdout file \"%s\", error = %d\n"),
                           p, GetLastError());
                  msg_ptr += lstrlen(msg_ptr);
#if defined(DEBUG)
               } else {
                  wsprintf(debug_buf, _T("stout = \"%s\", handle %d\n"), p,
                           sinfo.hStdOutput);
                  show_debug(debug_buf);
#endif  /* if defined(DEBUG) */
               }
            }

            *end_p = tmp_char;
            p = end_p++;
         break;

         case '2':
            p++;
            p = get_token(p, &end_p);
            tmp_char = *end_p;
            *end_p = '\0';

            if (sinfo.hStdError == INVALID_HANDLE_VALUE) {
               sinfo.hStdError = CreateFile(p, GENERIC_WRITE,
                                            FILE_SHARE_READ,
                                            &satts, CREATE_ALWAYS,
                                            FILE_ATTRIBUTE_NORMAL, NULL);

               if (sinfo.hStdError == INVALID_HANDLE_VALUE) {
                  wsprintf(msg_ptr,
                           _T("Error opening stderr file \"%s\", error = %d\n"),
                           p, GetLastError());
                  msg_ptr += lstrlen(msg_ptr);
#if defined(DEBUG)
               } else {
                  wsprintf(debug_buf, _T("sterr = \"%s\", handle %d\n"), p,
                           sinfo.hStdError);
                  show_debug(debug_buf);
#endif  /* if defined(DEBUG) */
               }
            }

            *end_p = tmp_char;
            p = end_p++;
         break;

         case 'c':
            p++;
            sinfo.dwFlags |= STARTF_USESHOWWINDOW;
            creation_flags &= ~DETACHED_PROCESS;
            creation_flags |= CREATE_NEW_CONSOLE;
         break;

         case 'r':
            p++;
            expand_cmd = FALSE;
         break;

         case 'v':
            p++;
            app_base_ver = get_token(p, &end_p);
            *end_p = '\0';
            p = end_p++;
         break;

         case '-':
            p++;
            while (isspace(*p)) {
               p++;
            }
            rc = FALSE;
         break;

         default:
            p--;
            rc = FALSE;
         break;
      }

      while (isspace(*p)) {
         p++;
      }

      if (rc) {
         rc = *p == '-';
      }
   }


#if defined(DEBUG)
   wsprintf(debug_buf, _T("Done with options, command line:\n\n|%s|"), p);
   show_debug(debug_buf);
#endif  /* if defined(DEBUG) */

   if (msg_ptr == msg_buf) {
      if (sinfo.hStdInput == INVALID_HANDLE_VALUE) {
         int fd;

        /*
         * Get current CRT input handle
         */
         sinfo.hStdInput = GetStdHandle(STD_INPUT_HANDLE);

        /*
         * Get the associated C stream file descriptor.  If this is not
         * a valid file descriptor, we do not have valid CRT I/O.
         * NOTE:  does not open the C stream, just returns the reference.
         */
         fd = _open_osfhandle((long)(sinfo.hStdInput), _O_RDONLY | _O_TEXT);

        /*
         * Redirect input if it is currently coming from a console
         */
         if ((sinfo.hStdInput == INVALID_HANDLE_VALUE) ||
             (fd == -1) ||
             GetConsoleMode(sinfo.hStdInput, &mode)) {

            if (sinfo.hStdInput != INVALID_HANDLE_VALUE) {
               CloseHandle(sinfo.hStdInput);
               sinfo.hStdInput = INVALID_HANDLE_VALUE;
            }

           /*
            * Safer than a NUL: device for input
            */
            rc = CreatePipe(&(sinfo.hStdInput), &wh, &satts, 0);
            if (rc) {
               CloseHandle(wh);
            }
         }
      }

      if (sinfo.hStdOutput == INVALID_HANDLE_VALUE) {
         int fd;

        /*
         * Get current CRT output handle
         */
         sinfo.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);

        /*
         * Get the associated C stream file descriptor.  If this is not
         * a valid file descriptor, we do not have valid CRT I/O.
         * NOTE:  does not open the C stream, just returns the reference.
         */
         fd = _open_osfhandle((long)(sinfo.hStdOutput), _O_WRONLY | _O_TEXT);

        /*
         * Redirect output if it is currently going to a console
         */
         if ((sinfo.hStdOutput == INVALID_HANDLE_VALUE) ||
             (fd == -1) ||
             GetConsoleMode(sinfo.hStdOutput, &mode)) {

            if (sinfo.hStdOutput != INVALID_HANDLE_VALUE) {
               CloseHandle(sinfo.hStdOutput);
               sinfo.hStdOutput = INVALID_HANDLE_VALUE;
            }

            sinfo.hStdOutput = CreateFile(_T("NUL:"), GENERIC_WRITE,
                                          FILE_SHARE_READ | FILE_SHARE_WRITE,
                                          &satts, OPEN_EXISTING,
                                          FILE_ATTRIBUTE_NORMAL, NULL);
         }
      }

      if (sinfo.hStdError == INVALID_HANDLE_VALUE) {
         int fd;

        /*
         * Get current CRT error output handle
         */
         sinfo.hStdError = GetStdHandle(STD_OUTPUT_HANDLE);

        /*
         * Get the associated C stream file descriptor.  If this is not
         * a valid file descriptor, we do not have valid CRT I/O.
         * NOTE:  does not open the C stream, just returns the reference.
         */
         fd = _open_osfhandle((long)(sinfo.hStdError), _O_WRONLY | _O_TEXT);

        /*
         * Redirect error output if it is currently going to a console
         */
         if ((sinfo.hStdError == INVALID_HANDLE_VALUE) ||
             (fd == -1) ||
             GetConsoleMode(sinfo.hStdError, &mode)) {
            if (sinfo.hStdError != INVALID_HANDLE_VALUE) {
               CloseHandle(sinfo.hStdError);
               sinfo.hStdError = INVALID_HANDLE_VALUE;
            }

            sinfo.hStdError = CreateFile(_T("NUL:"), GENERIC_WRITE,
                                         FILE_SHARE_READ | FILE_SHARE_WRITE,
                                         &satts, OPEN_EXISTING,
                                         FILE_ATTRIBUTE_NORMAL, NULL);
         }
      }

      augment_environment(app_base_ver);

      if (expand_cmd) {
         TCHAR dummy;

        /*
         * Under WindowsNT ExpandEnvironmentStrings appears
         * to return lengths for UNICODE strings.
         * Under Windows 9x the buffer pointer cannot be NULL.
         */
         exp_len = ExpandEnvironmentStrings(p, &dummy, 0);
         if (exp_len != 0) {
            exp_buf = GlobalAlloc(GMEM_FIXED, exp_len);

            if (exp_buf != NULL) {
               exp_len = ExpandEnvironmentStrings(p, exp_buf, exp_len);
            }
         }

         if (exp_buf != NULL) {
            cmd = exp_buf;
         } else {
            wsprintf(msg_ptr,
                     _T("Error %d expanding command enironment variables\n"),
                     p, GetLastError());
            msg_ptr += lstrlen(msg_ptr);
            cmd = p;
         }
      } else {
         cmd = p;
      }
      if (cmd != NULL) {
#if defined(DEBUG)
         wsprintf(debug_buf, _T("Command executed:\n\n|%s|"), cmd);
         show_debug(debug_buf);
#endif  /* if defined(DEBUG) */

        /*
         * Create the actual process
         */
         rc = CreateProcess(NULL,
                            cmd,
                            NULL,
                            NULL,
                            TRUE,
                            creation_flags,
                            NULL,
                            NULL,
                            &sinfo,
                            &pinfo);

         if (expand_cmd) {
            GlobalFree(exp_buf);
         }
      } else {
         rc = FALSE;
      }

      if (rc) {
#if defined(DEBUG)
         wsprintf(debug_buf, _T("pid = %d tid = %d"), pinfo.dwProcessId,
                  pinfo.dwThreadId);
         show_debug(debug_buf);
#endif  /* if defined(DEBUG) */

         WaitForSingleObject(pinfo.hProcess, 50);

        /*
         * "When an application spawns a process repeatedly, a new thread
         * instance will be created for each process but the previous
         * instances may not be cleaned up.  This results in a significant
         * virtual memory loss each time the process is spawned.  If there
         * is a WaitForInputIdle() call between CreateProcess() and
         * CloseHandle(), the problem does not occur." PSS ID Number: Q124121
         */
         WaitForInputIdle(pinfo.hProcess, 5000);

         CloseHandle(pinfo.hProcess);
         CloseHandle(pinfo.hThread);
      } else {
         wsprintf(msg_ptr,
                  _T("Error creating process command:\n\n%s\n\nError = %d\n"),
                  p, GetLastError());
         msg_ptr += lstrlen(msg_ptr);
      }
   }

   if (sinfo.hStdInput != INVALID_HANDLE_VALUE) {
      CloseHandle(sinfo.hStdInput);
   }
   if (sinfo.hStdOutput != INVALID_HANDLE_VALUE) {
      CloseHandle(sinfo.hStdOutput);
   }
   if (sinfo.hStdError != INVALID_HANDLE_VALUE) {
      CloseHandle(sinfo.hStdError);
   }

   if (msg_ptr != msg_buf) {
      rc = FALSE;
      show_error(msg_buf);
   }

   return(rc ? 1 : 0);
}


/*
 *----------------------------------------------------------------------
 *
 * get_token --
 *
 *      Gets a command line token
 *
 * Results:
 *      Pointer to start of command line token.
 *
 * Side effects:
 *      Sets nptr to the position in st_ptr where the null character
 *      should be placed to process the token.
 *
 *----------------------------------------------------------------------
 */
static
TCHAR *get_token(st_ptr, nptr)
   TCHAR *st_ptr;
   TCHAR **nptr;
{
   TCHAR  *rc_p = st_ptr;
   TCHAR  *p;

   while (isspace(*rc_p)) {
      rc_p++;
   }
   p = rc_p;

   if (*p == '"') {
      p++;
      while ((*p != '\0') && (*p != '"')) {
         p++;
      }
   } else {
      while (*p != '\0' && !isspace(*p)) {
        p++;
      }
   }

   *nptr = p;

   return(rc_p);
}


/*
 *----------------------------------------------------------------------
 *
 * show_error --
 *
 *      Display an error message.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      Exits the program.
 *
 *----------------------------------------------------------------------
 */

static
void show_error (msg)
   TCHAR  *msg;
{
   MessageBeep(MB_ICONEXCLAMATION);
   MessageBox(NULL, msg, _T("Fatal Error in abxStart"),
              MB_ICONSTOP | MB_OK | MB_TASKMODAL | MB_SETFOREGROUND);

   return;
}

#if defined(DEBUG)

/*
 *----------------------------------------------------------------------
 *
 * show_debug --
 *
 *      Display an debug message.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      Exits the program.
 *
 *----------------------------------------------------------------------
 */

static
void show_debug (msg)
   TCHAR  *msg;
{
   MessageBox(NULL, msg, _T("Debug from abxStart"),
              MB_ICONINFORMATION | MB_OK | MB_TASKMODAL | MB_SETFOREGROUND);

   return;
}
#endif  /* if defined(DEBUG) */


/*
 *----------------------------------------------------------------------
 *
 * augment_environment --
 *
 *      Builds a new enviroment
 *
 * Results:
 *      None
 *
 * Side effects:
 *      None
 *
 *----------------------------------------------------------------------
 */

static
void augment_environment (app_base_ver)
TCHAR   *app_base_ver;
{
   HKEY key;
   DWORD loc_size;
   TCHAR *loc = NULL;
   TCHAR *p;
   TCHAR *buf;
   HANDLE abx_env = INVALID_HANDLE_VALUE;
   DWORD rc;
   DWORD len;
   SECURITY_ATTRIBUTES satts;
#if defined(ABX_SET_PATH)
   TCHAR dummy;
#endif  /* if defined(ABX_SET_PATH) */

   satts.nLength = sizeof(SECURITY_ATTRIBUTES);
   satts.lpSecurityDescriptor = NULL;
   satts.bInheritHandle = FALSE;

   len = lstrlen(ABX_APP_BASE_KEY);
   buf = GlobalAlloc(GMEM_FIXED, len + lstrlen(app_base_ver) + 1);
   if (buf != NULL) {
      lstrcpy(buf, ABX_APP_BASE_KEY);
      lstrcpy((buf + len), app_base_ver);

      if (RegOpenKeyEx(HKEY_LOCAL_MACHINE, buf, 0, KEY_READ, &key) ==
          ERROR_SUCCESS) {

         if (RegQueryValueEx(key, ABX_APP_BASE_LOC, NULL, NULL, NULL,
                             &loc_size) == ERROR_SUCCESS) {
            loc = GlobalAlloc(GMEM_FIXED, loc_size);

            if (loc != NULL) {
               RegQueryValueEx(key, ABX_APP_BASE_LOC, NULL, NULL, loc,
                               &loc_size);
               loc_size;
            }
         }

         RegCloseKey(key);
      }

#if defined(DEBUG)
      if (loc != NULL) {
         wsprintf(debug_buf, _T("Location = %s"), loc);
      } else {
         wsprintf(debug_buf, _T("Registery key not found.\n\n%s"), buf);
      }
      show_debug(debug_buf);
#endif  /* if defined(DEBUG) */

      GlobalFree(buf);
   }

   abx_env = CreateFile(ABX_ENV_FILE, GENERIC_READ, FILE_SHARE_READ, &satts,
                        OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

   if (abx_env == INVALID_HANDLE_VALUE) {
      buf = GlobalAlloc(GMEM_FIXED, MAX_PATH + 1);

      if (buf != NULL) {
         rc = SearchPath(NULL, ABX_ENV_FILE, NULL, MAX_PATH + 1, buf, &p);

         if (rc != 0) {
            abx_env = CreateFile(buf, GENERIC_READ, FILE_SHARE_READ, &satts,
                                 OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

#if defined(DEBUG)
            wsprintf(debug_buf, _T("Opened %s environment file"), buf);
            show_debug(debug_buf);
#endif  /* if defined(DEBUG) */
         }

         GlobalFree(buf);
      }
#if defined(DEBUG)
   } else {
      wsprintf(debug_buf, _T("Opened %s environment file"), ABX_ENV_FILE);
      show_debug(debug_buf);
#endif  /* if defined(DEBUG) */
   }

   if (loc != NULL) {
      rc = SetEnvironmentVariable(ABX_APP_BASE_VAR, loc);

#if defined(DEBUG)
      if (rc == 0) {
         wsprintf(debug_buf, _T("SetEnv %s failed, error = %d\n"),
                  ABX_APP_BASE_VAR, GetLastError());
         show_debug(debug_buf);
      }
#endif  /* if defined(DEBUG) */

#if defined(ABX_SET_PATH)
#define ABX_BIN_DIR _T("bin")
     /*
      * Under WindowsNT ExpandEnvironmentStrings appears
      * to return lengths for UNICODE strings.
      * Under Windows 9x the buffer pointer cannot be NULL.
      */
      rc = ExpandEnvironmentStrings(_T("%PATH%;"), &dummy, 0);

      if (rc != 0) {
         DWORD len;

        /*
         * Both rc and loc_size include null terminating character.
         * The second is absorbed by the file separator character.
         *
         * Under WindowsNT ExpandEnvironmentStrings appears to return
         * lengths for UNICODE strings.
         * Under Windows 9x the buffer pointer cannot be NULL.
         */
         len = rc + loc_size + lstrlen(ABX_BIN_DIR);
         buf = GlobalAlloc(GMEM_FIXED, len);

         if (buf != NULL) {
            rc = ExpandEnvironmentStrings(_T("%PATH%;"), buf, len);

            if (rc != 0) {
               p = buf;

              /*
               * Under WindowsNT ExpandEnvironmentStrings appears to return
               * lengths for UNICODE strings.
               */
               p += lstrlen(buf);
               lstrcpy(p, loc);
               p += loc_size - 1;
               *p++ = '\\';
               lstrcpy(p, ABX_BIN_DIR);

               rc = SetEnvironmentVariable(_T("PATH"), buf);

#if defined(DEBUG)
               if (rc == 0) {
                  wsprintf(debug_buf, _T("SetEnv PATH failed, error = %d\n"),
                           GetLastError());
                  show_debug(debug_buf);
               }
#endif  /* if defined(DEBUG) */
            }

            GlobalFree(buf);
         }
      }
#endif  /* if defined(ABX_SET_PATH) */

      GlobalFree(loc);
   }

   if (abx_env != INVALID_HANDLE_VALUE) {
      read_env_file(abx_env);

      CloseHandle(abx_env);
   }

#if defined(DEBUG)
   {
      LPVOID envp;
      TCHAR *sp;
      TCHAR *op;

      envp = GetEnvironmentStrings();

      sp = envp;
      op = debug_buf;
      p = sp;

      while (*sp != '\0') {
         if (*p == '\0') {
            lstrcpy(op, sp);
            op += lstrlen(sp);
            *op++ = '\n';
            p++;
            sp = p;
         } else {
            p++;
         }
      }

      show_debug(debug_buf);

      FreeEnvironmentStrings(envp);
   }
#endif

   return;
}


/*
 *----------------------------------------------------------------------
 *
 * read_env_file --
 *
 *      Reads an enviroment file into the current environment
 *
 * Results:
 *      None
 *
 * Side effects:
 *      None
 *
 *----------------------------------------------------------------------
 */

static
void read_env_file(env_h)
   HANDLE   env_h;
{
/*
 * 1024 plus 2 for carriage return new line
 */
#define BUF_SIZE 1026

   TCHAR line_buf[BUF_SIZE + 1];
   TCHAR *read_p = line_buf;
   TCHAR *buf_end;
   TCHAR *p;
   TCHAR *line_end;
   TCHAR *var_p;
   TCHAR *exp_buf = NULL;
   DWORD rbytes = BUF_SIZE;
   DWORD nbytes;
   DWORD exp_len = 0;
   BOOL ok;
   BOOL too_long = FALSE;

   do {
      ok = ReadFile(env_h, read_p, rbytes, &nbytes, NULL);

      if ((nbytes > 0) || (read_p > line_buf)) {
         buf_end = read_p + nbytes;
         p = read_p;
         read_p = line_buf;

         while ((p < buf_end) && (*p != '\r') && (*p != '\n')) {
            p++;
         }

         if (too_long) {
            if (p < buf_end) {
               too_long = FALSE;

               while ((p < buf_end) && ((*p == '\r') || (*p == '\n'))) {
                  p++;
               }
               read_p = p;

               while ((p < buf_end) && (*p != '\r') && (*p != '\n')) {
                  p++;
               }
            } else {
               rbytes = BUF_SIZE;
            }
         }

         if (!too_long) {
            while (p < buf_end) {
               line_end = p;
               p = read_p;

               while (isspace(*p)) {
                 p++;
               }
               var_p = p;

               while ((p < line_end) && (*p != '=')) {
                 p++;
               }

               if (p < line_end) {
                  *p++ = '\0';
                  *line_end = '\0';

                  if (p == line_end) {
#if defined(DEBUG)
                     wsprintf(debug_buf,
                              _T("SetEnv |%s| =\n\n|%s|"),
                              var_p, _T(""));
                     show_debug(debug_buf);
#endif  /* if defined(DEBUG) */
                     nbytes = SetEnvironmentVariable(var_p, NULL);
#if defined(DEBUG)
                     if (nbytes == 0) {
                        wsprintf(debug_buf,
                                 _T("SetEnv %s failed, error = %d\n"),
                                 var_p, GetLastError());
                        show_debug(debug_buf);
                     }
#endif  /* if defined(DEBUG) */
                  } else {
                    /*
                     * Under WindowsNT ExpandEnvironmentStrings appears to
                     * return lengths for UNICODE strings.
                     * Under Windows 9x the buffer pointer cannot be NULL.
                     */
                     if (exp_buf != NULL) {
                        nbytes = ExpandEnvironmentStrings(p, exp_buf,
exp_len);
                     } else {
                        TCHAR dummy;

                        nbytes = ExpandEnvironmentStrings(p, &dummy, 0);
                     }
                     if (nbytes > exp_len) {
                        TCHAR  *new;

                        if (exp_buf == NULL) {
                           new = GlobalAlloc(GMEM_FIXED, nbytes);
                        } else {
                           new = GlobalReAlloc(exp_buf, nbytes, GMEM_FIXED);
                        }

                        if (new != NULL) {
                           exp_buf = new;
                           exp_len = nbytes;
                        }
                     }

                     if (exp_buf != NULL) {
                       /*
                        * Under WindowsNT ExpandEnvironmentStrings appears
                        * to return lengths for UNICODE strings.
                        * Under Windows 9x the buffer pointer cannot be NULL.
                        */
                        nbytes = ExpandEnvironmentStrings(p, exp_buf,
exp_len);

#if defined(DEBUG)
                        wsprintf(debug_buf,
                                 _T("SetEnv |%s| =\n\n|%s|"),
                                 var_p, exp_buf);
                        show_debug(debug_buf);
#endif  /* if defined(DEBUG) */
                        nbytes = SetEnvironmentVariable(var_p, exp_buf);
#if defined(DEBUG)
                        if (nbytes == 0) {
                           wsprintf(debug_buf,
                                    _T("SetEnv %s failed, error = %d\n"),
                                    var_p, GetLastError());
                           show_debug(debug_buf);
                        }
#endif  /* if defined(DEBUG) */
                     }
                  }
               }

               p = ++line_end;
               while ((p < buf_end) && ((*p == '\r') || (*p == '\n'))) {
                  p++;
               }
               read_p = p;

               while ((p < buf_end) && (*p != '\r') && (*p != '\n')) {
                  p++;
               }
            }

            rbytes = BUF_SIZE;

            if (read_p == line_buf) {
               too_long = TRUE;
            } else if (read_p < buf_end) {
               rbytes = read_p - line_buf;
               nbytes = BUF_SIZE - rbytes;

               MoveMemory(line_buf, read_p, nbytes);
               read_p = line_buf + nbytes;
            } else {
               read_p = line_buf;
            }
         }
      } else {
         ok = FALSE;
      }

   } while (ok);

   if (exp_buf != NULL) {
      GlobalFree(exp_buf);
   }

   return;
}


