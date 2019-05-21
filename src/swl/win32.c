#define Export extern __declspec (dllexport)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN
#include <shellapi.h>
#include <stdlib.h>
#include <stdio.h>

/* SWL_open_dde_file --
 * Opens the file using the WIN OS file type associations
 * Returns <= 32 if startup fails, > 32 on success;
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

Export HINSTANCE SWL_open_dde_file(TCHAR *file, TCHAR *def_dir, TCHAR *params) {
  /* def_dir should be "" if not used */
  /* params must  be NULL if file specifies a document, may be
     non-NULL if file specifies an executable file */
   HINSTANCE retval;

   retval = ShellExecute(NULL, "open", file, params, def_dir, SW_SHOWNORMAL);

   return(retval);
}

void win32_init() {
#ifdef CUSTOM_SCHEME
    foreign_symbol("SWL_open_dde_file", SWL_open_dde_file);
#endif /* CUSTOM_SCHEME */
}
