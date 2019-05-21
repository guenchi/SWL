/*
 * Copyright (c) 1996 Oscar Waddell
 *
 * See the file "Notice" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include <stdio.h>
#include <stdlib.h>
#include <tcl.h>
#include <tk.h>
#include <errno.h>

#include <string.h>

#if defined(WIN32)
#define Export extern __declspec (dllexport)
#include <stdlib.h>
#else
#define Export
#include <signal.h>
#endif

#if TK_MAJOR_VERSION > 4 || (TK_MAJOR_VERSION == 4 && TK_MINOR_VERSION >= 1)
#define MyDoOneEvent Tcl_DoOneEvent
#define DONT_BLOCK TCL_DONT_WAIT
#define WINDOW_EVENTS TCL_WINDOW_EVENTS
#define NumMainWindows() Tk_GetNumMainWindows()
#else
#define OLD_TK
#define MyDoOneEvent Tk_DoOneEvent
#define DONT_BLOCK TK_DONT_WAIT
#define WINDOW_EVENTS TK_X_EVENTS
#define NumMainWindows() (tk_NumMainWindows)
#endif

#define NoClientData (ClientData)NULL
#define NoDeleteProc (Tcl_CmdDeleteProc *)NULL
#define Screen    (char *)NULL /* use default screen */
#define AppName   ""           /* registers name for use with send command */
#define ClassName "SWL"        /* for matching patterns in option database */

#ifdef DEBUG
#define print_debug_message(x) fprintf(stderr, "%s\n", x);
#else
#define print_debug_message(x)
#endif /* DEBUG */

Export int SWL_TclInit(int qsize, int bufsize);
Export char *SWL_TclEval(char *str, char *flags);
Export int SWL_DoOneEvent(int block, char *buf, int size);
Export int SWL_PeekEvent(void);
Export Tcl_Channel SWL_GetChannel(char* channame);
Export const char *SWL_GetError(void);
Export int SWL_channel_read(Tcl_Channel channel, char *buf, int index, int count);
Export int SWL_channel_write(Tcl_Channel channel, char *buf, int index, int count);
Export void SWL_mem_dbg(void);
Export Tk_Window SWL_name_to_window(char *name);

/* fallbacks */
typedef struct {
   int id;
   int arglen;
   char *args;
} Fallback;
typedef struct {
  Fallback *queue;
  int size;
  int head;
  int tail;
} FallbackQueue; 

int s_eval(ClientData ignored, Tcl_Interp *interp, int argc, const char *argv[]);
void enqueue(FallbackQueue *q, int fbid, int len, char *args);
Fallback *dequeue(FallbackQueue *q);
int queue_init(FallbackQueue *qp, int qsize);
int buf_init(int bufsize);
int enbuf(const char *s);
void debuf(char *s, int length);

static Tcl_Interp *SWL_interp;
static Tk_Window SWL_main_window;

static FallbackQueue fbq; /* the fallback queue */

/*  Signals such as SIGINT (^C) appear to be ignored while the process is
 *  blocked in Tcl_DoOneEvent.  We register a signal handler that inserts
 *  a dummy event in the Tcl event queue so that Tcl_DoOneEvent is unblocked
 *  when a signal is received.
 *
 *   default_act     holds the default signal handler installed by Scheme.
 *   handle_signal   queues a Tcl event when signal is received.
 *   ignore_event    returns 1 to inform the Tcl event loop that the event
 *                   has been handled.  Tcl_Alloc is required because Tcl's
 *                   event loop does the free.
 */
#ifndef WIN32
static struct sigaction default_act;  /* initialized in SWL_TclInit */
static int ignore_event(Tcl_Event *event, int flags) {
  return 1;
}
static void handle_signal(int sig) {
  Tcl_Event *event;
printf ("SWL:SIGINT\n");
  event = (Tcl_Event *)Tcl_Alloc(sizeof(Tcl_Event));
  event->proc = ignore_event;
  Tcl_QueueEvent(event, TCL_QUEUE_TAIL);
  default_act.sa_handler(sig);
}
#endif /* WIN32 */

int SWL_TclInit(int qsize, int bufsize) {
  SWL_interp = Tcl_CreateInterp();

  /* this may solve our initialization problems on other machines */
  Tcl_SetVar(SWL_interp, "argv", "", TCL_GLOBAL_ONLY);
  Tcl_SetVar(SWL_interp, "argc", "0", TCL_GLOBAL_ONLY);
  Tcl_SetVar(SWL_interp, "argv0", "scheme-tk", TCL_GLOBAL_ONLY);
  Tcl_SetVar(SWL_interp, "tcl_interactive", "1", TCL_GLOBAL_ONLY); 

#ifdef OLD_TK
  SWL_main_window = Tk_CreateMainWindow(SWL_interp, Screen, AppName, ClassName);
#endif /* OLD_TK */
  if (Tcl_Init(SWL_interp) == TCL_ERROR) {
    fprintf(stderr, "%s", SWL_interp->result);
    return 0;
  }
  if (Tk_Init(SWL_interp) == TCL_ERROR) {
    fprintf(stderr, "%s", SWL_interp->result);
    return 0;
  }
#ifndef OLD_TK      /* For use w/ new Tk */
  SWL_main_window = Tk_MainWindow(SWL_interp);
  if (SWL_main_window == NULL) {
    fprintf(stderr, "%s", SWL_interp->result);
    return 0;
  }
#endif /* OLD_TK */

  Tcl_CreateCommand(SWL_interp, "s_eval", s_eval, NoClientData, NoDeleteProc);
#ifndef WIN32
  { struct sigaction act;
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    act.sa_handler = handle_signal;
    sigaction(SIGINT, &act, &default_act);
  }
#endif /* WIN32 */
  return (queue_init(&fbq, qsize) & buf_init(bufsize));
}


char *SWL_TclEval(char *str, char *success) {
  int result;
  char *s;

  result = Tcl_Eval(SWL_interp, str);
  if (result == TCL_ERROR)
     *success = 0;
  else
     *success = 1;
  return SWL_interp->result;
}


/* items in the queue consist of id of the callback to call,
   the length of the argument string, and the argument string.
   the latter points into a circular buffer 
   the queue is circular too */

static char *buf, *bufpos, *bufguard, *bufend;
static int bufsize;

int queue_init(FallbackQueue *qp, int qs) {
  qp->size = qs;
  qp->head = 0;
  qp->tail = 0;
  if ((qp->queue = (Fallback *)malloc(qs * sizeof(Fallback))) == NULL) {
     fprintf(stderr,"malloc failed for fallback queue");
     return 0;
  }
  return 1;
}

int buf_init(int bs) {
  bufsize = bs;
  buf = (char *)malloc(bufsize * sizeof(char));
  if (buf == NULL) {
     fprintf(stderr,"malloc failed for fallback queue");
     return 0;
  }
  bufpos = buf;
  bufguard = buf;
  bufend = buf + (bufsize * sizeof(char));
  return 1;
}

#define queue_empty(q) (q.head == q.tail)

/* only called when queue non-empty */
Fallback *queue_head(FallbackQueue *q) {
  return &(q->queue[q->head]);
}

/* only called immediately after dequeue so item is known to be intact
   buf space not freed until we really dequeue */
void dequeue_bang(FallbackQueue *q) {
  q->head = (q->head + 1) % q->size;
}

/* only called when queue non-empty */
Fallback *dequeue(FallbackQueue *q) {
  Fallback *retval = queue_head(q);
  dequeue_bang(q);
  return retval;
}

void enqueue(FallbackQueue *q, int fbid, int len, char *args) {
  int i = q->tail;
  Fallback *item = &(q->queue[i]);
  item->id = fbid;
  item->args = args;
  item->arglen = len;
  q->tail = (i + 1) % q->size;
  if (q->head == q->tail) {
     FallbackQueue new;
     Fallback *fb;

     if (!queue_init(&new, 2 * q->size)) {
        print_debug_message("Malloc of new queue failed, dropped oldest event");
        fb = dequeue(&fbq);
        debuf(fb->args, fb->arglen);
     } else {
       do {
          fb = dequeue(&fbq);
          enqueue(&new, fb->id, fb->arglen, fb->args);
       } while (!queue_empty(fbq));
       free(fbq.queue);
       fbq.queue = new.queue;
       fbq.size = new.size;
       fbq.head = new.head;
       fbq.tail = new.tail;
     }
  }
}


circ_bufcpy(void *dest, void *src, int len) {
  int  overflow = (((int)src + len) - (int)bufend);

  if (overflow > 0) {
     int chunk1 = len - overflow;

     memcpy((void *)dest, (void *)src, chunk1);
     memcpy((void *)((int)dest + chunk1), (void *)buf, overflow);
  } else {
    memcpy((void *)dest, (void *)src, len);
  }
}

int SWL_PeekEvent() {
  if (queue_empty(fbq)) MyDoOneEvent(DONT_BLOCK);
  return (!queue_empty(fbq));
} 

/*  returns
 *   0  no fallback to process, *args not touched
 *  -n  do fallback n, no args
 *   n  do fallback n, args have been hammered into the args array
 *      if the args didn't fit, then the first char of the args array
 *      will be #\nul and n is actually the size of string needed for
 *      the args
 *
 *   If the event queue overflows, we drop the oldest guy in the queue.
 *   If the fallback argument buffer overflows we drop that particular
 *   fallback. (may be out of date comment)
 */

int SWL_DoOneEvent(int block, char *args, int size) {
  static int len;
  static Fallback *fb;

  if (queue_empty(fbq)) {
     MyDoOneEvent(block ? 0 : DONT_BLOCK);
  }
  if (queue_empty(fbq)) {
     return 0;
  } else {
     fb = queue_head(&fbq);
     len = fb->arglen;
     if (len == 0) {
        dequeue_bang(&fbq);
        return (- fb->id);
     } else {
        if (len > size) {
           *args = (char)0;
           return len;
        } else {
           circ_bufcpy((void *)args, (void *)fb->args, len);
           debuf(fb->args, len);
           dequeue_bang(&fbq);
           return fb->id;
        }
     }
  }
}


Tcl_Channel SWL_GetChannel(char* channame) {
  return Tcl_GetChannel(SWL_interp, channame, NULL);
}

int SWL_channel_read(Tcl_Channel channel, char *buf, int index, int count) {
  return Tcl_Read(channel, (char *)(buf + index), count);
}

int SWL_channel_write(Tcl_Channel channel, char *buf, int index, int count) {
  return Tcl_Write(channel, (char *)(buf + index), count);
}

const char *SWL_GetError(void) {
#if defined(AIX) || defined(SPS2) || defined(HPUX) || defined(I3S2)
  return strerror( Tcl_GetErrno() );   /* don't use in multi-threaded env */
#else
  return sys_errlist[ Tcl_GetErrno() ];
#endif
}


void debuf(char *args, int arglen) {
  if (arglen) {
     bufguard = (char *)((int)args + arglen);
     if (bufguard >= bufend) bufguard = (char *)((int)bufguard - bufsize);
  }
}


int enbuf_failed = 0;
void relocate_args() {
  int len, i = fbq.head;
  char *d, *new, *old;

  bufsize *= 2;
  new = (char *)malloc(bufsize);
  if (new == NULL) {
     print_debug_message("malloc failed for fallback queue");
     enbuf_failed = 1;
  } else {
    enbuf_failed = 0;
  }
  d = new;
  /* copy over args for older events */
  while (i != fbq.tail) {
    len = fbq.queue[i].arglen;
    if (len > 0) {
       circ_bufcpy((void *)d, (void *)fbq.queue[i].args, len);
       fbq.queue[i].args = d;
       d = (char *)((int)d + len);
    }
    i = (i + 1) % fbq.size;
  }
  old = buf;
  bufpos = d;
  buf = bufguard = new;
  bufend = (char *)((int)buf + bufsize);
  free(old);
}


int enbuf(const char *s) {
  int len = 0;

  while (!enbuf_failed && *s) {
    *bufpos++ = *s++;
    len++;
    if (bufpos == bufend) bufpos = buf;
    if (bufpos == bufguard) enbuf_failed = 1;
  }
  return len;
}


void dump_queue() {
  int i = fbq.head;
  int j = 0;
  fprintf(stderr, " dump_queue fbq.size = %d %s {\n", fbq.size, queue_empty(fbq) ? "EMPTY" : "not empty");
  while (i != fbq.tail) {
    Fallback fb = fbq.queue[i];
    char *reconstituted = (char *)malloc(fb.arglen * sizeof(char *) + 1);
    if (reconstituted == NULL)
      fprintf(stderr, "  %d: id=%d, len=%d, (malloc for args failed)\n", j++, fb.id, fb.arglen);
    else
    {
      circ_bufcpy(reconstituted, fb.args, fb.arglen);
      *(reconstituted + fb.arglen) = (char)0;
      fprintf(stderr, "  %d: id=%d, len=%d, args=%s\n", j++, fb.id, fb.arglen, reconstituted);
    }
    free(reconstituted);
    i = (i + 1) % fbq.size;
  }
  fprintf(stderr, " }\n");
}


/* return position of the args in the arg buffer and length of the args */
char *copy_args(int argc, const char *argv[], int *length) {
  int len, i;
  char* where = bufpos;

  len = enbuf("(");
  for (i=2; i < argc; i++) {
    len += enbuf(argv[i]);
    if (i < argc - 1) len += enbuf(" ");
  }
  len += enbuf(")");
  while (enbuf_failed) {
     relocate_args();
     where = copy_args(argc, argv, &len);
  }
  *length = len;
  return where;
}


int s_eval(ClientData ignored, Tcl_Interp *interp, int argc, const char *argv[]) {
  switch (argc) {
    case 0:
    case 1:
       interp->result = "Expected at least one argument";
       return TCL_ERROR;
    case 2:
       enqueue(&fbq, atoi(argv[1]), 0, NULL);
       interp->result = "";
       return TCL_OK;
    default:
      { int len;
        char *args;
       /* egregious hack: . is the root of the widget tree and is a useful
          target for events that aren't targeted at a specific window, such
          as the <<NewEdit>> event we arrange to send on Mac OS X systems.
          But . will cause the read in make-event-handler to bomb, and .
          is not associated with a fallback queue anyway, so we fat-finger
          it to swl:system-queue. */
        if (strcmp(argv[2],".") == 0) argv[2] = "swl:system-queue";
        args = copy_args(argc, argv, &len);
        enqueue(&fbq, atoi(argv[1]), len , args);
        interp->result = "";
        return TCL_OK;
      }
  }
}


void SWL_mem_dbg() {
   fprintf(stderr, " qsize = %d\n", fbq.size);
   fprintf(stderr, " qhead = %d\n", fbq.head);
   fprintf(stderr, " qtail = %d\n", fbq.tail);
   fprintf(stderr, " bufsize = %d\n", bufsize);
   fprintf(stderr, "     buf = %p\n", buf);
   fprintf(stderr, "  bufend = %p\n", bufend);
   fprintf(stderr, "  bufpos = %p\n", bufpos);
   fprintf(stderr, "bufguard = %p\n", bufguard);
}

Tk_Window SWL_name_to_window(char *name) {
  Tk_Window x = Tk_NameToWindow(SWL_interp, name, SWL_main_window);
  if (x == NULL) {
    fprintf(stderr,
	    "ERROR:  Tk_NameToWindow(SWL_interp, %s, SWL_main_window) == NULL\n%s\n",
	    name, SWL_interp->result);
  }
  return x;
}

custom_init() {
#ifdef CUSTOM_SCHEME
   foreign_symbol("SWL_TclInit", SWL_TclInit);
   foreign_symbol("SWL_TclEval", SWL_TclEval);
   foreign_symbol("SWL_DoOneEvent", SWL_DoOneEvent);
   foreign_symbol("SWL_GetChannel", SWL_GetChannel);
   foreign_symbol("SWL_GetError", SWL_GetError);
   foreign_symbol("SWL_channel_read", SWL_channel_read);
   foreign_symbol("SWL_channel_write", SWL_channel_write);

   foreign_symbol("Tcl_Eof", Tcl_Eof);
   foreign_symbol("Tcl_Flush", Tcl_Flush);
   foreign_symbol("Tcl_InputBuffered", Tcl_InputBuffered);

  /* for prototyping geometry manager */
   foreign_symbol("Tk_MapWindow", Tk_MapWindow);
   foreign_symbol("Tk_UnmapWindow", Tk_UnmapWindow);
   foreign_symbol("Tk_MoveWindow", Tk_MoveWindow);
   foreign_symbol("Tk_ResizeWindow", Tk_ResizeWindow);
   foreign_symbol("SWL_name_to_window", SWL_name_to_window);

  /* for debugging */
   foreign_symbol("SWL_mem_dbg", SWL_mem_dbg);
#endif /* CUSTOM_SCHEME */

#if defined(WIN32)
  {extern void win32_init(void);
   win32_init();
  }
#endif /* defined(WIN32) */
}
