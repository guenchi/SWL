;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

(case-sensitive #t)
(include "config.scheme")

;;; set tcl/tk versions
;;; change in bintar also
(include "tclversion.ss")
(define tk_version tcl_version)
(define tcl_version_no_dot (list->string (remv #\. (string->list tcl_version))))
(define tk_version_no_dot (list->string (remv #\. (string->list tk_version))))

;;; these defaults may be overridden via set! on a per-machine-type basis
;;; e.g., see i3nt section of configure below for Windows file locations
(define TCL_LIBRARY (format "/usr/local/lib/tcl~a" tcl_version))
(define TK_LIBRARY (format "/usr/local/lib/tk~a" tk_version))

(define (configure)
  (case (machine-type)
    ((i3le)
     (set! TCL_LIBRARY (format "/usr/lib/tcl~a" tcl_version))
     (set! TK_LIBRARY (format "/usr/lib/tk~a" tk_version))
     (write-configuration
       `((NOHEAP 1)
         (CC gcc)
         (CFLAGS ,(format "-m32 -O2 -I/usr/include/tcl~a -fPIC" tcl_version))
         (LD gcc)
         (LDFLAGS "-m32 -melf_i386 -O2 -fPIC -shared")
         (SHLIBS ,(format "-ltk~a -ltcl~a -L/usr/X11R6/lib -lX11 -lc -lm"
                          tk_version tcl_version))
         (FOREIGN_OBJ "swl.so")
         (InstallPrefix "/usr")
         (InstallOwner "root")
         (InstallGroup "root")))
     (write-build-script 'oop (oop-build))
     (write-build-script 'swl (swl-build 'shared)))
    ((i3fb)
     (set! TCL_LIBRARY (format "/usr/local/lib/tcl~a" tcl_version))
     (set! TK_LIBRARY (format "/usr/local/lib/tcl~a" tcl_version))
     (write-configuration
       `((NOHEAP 1)
         (CC gcc)
         (CFLAGS ,(format "-O2 -I/usr/local/include -I/usr/local/include/tcl~a -I/usr/local/include/tk~a -fPIC" tcl_version tk_version))
         (LD gcc)
         (LDFLAGS "-O2 -fPIC -shared")
         (SHLIBS ,(format "-L/usr/local/lib -ltk~a -ltcl~a -L/usr/X11R6/lib -lX11 -lc -lm"
                          tk_version_no_dot tcl_version_no_dot))
         (FOREIGN_OBJ "swl.so")
         (InstallPrefix "/usr/local")
         (InstallOwner "root")
         (InstallGroup "wheel")))
     (write-build-script 'oop (oop-build))
     (write-build-script 'swl
       (swl-build
         (lambda ()
           `(begin
              (with-path load-shared-object "swl.so")
              (load-shared-object ,(format "/usr/local/lib/libtcl~a.so" tcl_version_no_dot))
              (load-shared-object ,(format "/usr/local/lib/libtk~a.so" tcl_version_no_dot)))))))
    ((i3nb)
     (set! TCL_LIBRARY (format "/usr/pkg/lib/tcl~a" tcl_version))
     (set! TK_LIBRARY (format "/usr/pkg/lib/tcl~a" tcl_version))
     (write-configuration
       `((NOHEAP 1)
         (CC gcc)
         (CFLAGS ,(format "-O2 -I/usr/X11R7/include -I/usr/pkg/include -I/usr/pkg/include/tcl~a -I/usr/pkg/include/tk~a -fPIC" tcl_version tk_version))
         (LD gcc)
         (LDFLAGS "-O2 -fPIC -shared")
         (SHLIBS ,(format "-L/usr/pkg/lib /usr/pkg/lib/libtk~a.so /usr/pkg/lib/libtcl~a.so -L/usr/X11R7/lib -lX11 -lc -lm"
                          tk_version_no_dot tcl_version_no_dot))
         (FOREIGN_OBJ "swl.so")
         (InstallPrefix "/usr/local")
         (InstallOwner "root")
         (InstallGroup "wheel")))
     (write-build-script 'oop (oop-build))
     (write-build-script 'swl
       (swl-build
         (lambda ()
           `(begin
              (with-path load-shared-object "swl.so")
              (load-shared-object ,(format "/usr/pkg/lib/libtcl~a.so" tcl_version_no_dot))
              (load-shared-object ,(format "/usr/pkg/lib/libtk~a.so" tcl_version_no_dot)))))))
    ((i3ob)
     (let ([smash (lambda (s) (list->string (remv #\. (string->list s))))])
       (define smashed_tcl_version (smash tcl_version))
       (define smashed_tk_version (smash tk_version))
       (set! TCL_LIBRARY (format "/usr/local/lib/tcl~a" tcl_version))
       (set! TK_LIBRARY (format "/usr/local/lib/tcl~a" tcl_version))
       (write-configuration
         `((NOHEAP 1)
           (CC gcc)
           (CFLAGS ,(format "-O2 -I/usr/X11R6/include -I/usr/local/include/tcl~a -I/usr/local/include/tk~a -fPIC"
                            tcl_version tk_version))
           (LD gcc)
           (LDFLAGS "-O2 -fPIC -shared")
           (SHLIBS ,(format "-L/usr/local/lib -ltk~a -ltcl~a -L/usr/X11R6/lib -lX11 -lc -lm"
                            smashed_tk_version smashed_tcl_version))
           (FOREIGN_OBJ "swl.so")
           (InstallPrefix "/usr/local")
           (InstallOwner "root")
           (InstallGroup "bin")))
       (write-build-script 'oop (oop-build))
       (write-build-script 'swl
         (swl-build
           (lambda ()
             `(begin
                (with-path load-shared-object "swl.so")
                (load-shared-object ,(format "/usr/local/lib/libtcl~a.so" smashed_tcl_version))
                (load-shared-object ,(format "/usr/local/lib/libtk~a.so" smashed_tcl_version))))))))
    ((i3s2)
     (set! TCL_LIBRARY (format "/usr/lib/tcl~a" tcl_version))
     (set! TK_LIBRARY (format "/usr/lib/tk~a" tk_version))
     (write-configuration
       `((NOHEAP 1)
         (CC gcc)
         (CFLAGS ,(format "-m32 -O2 -DI3S2 -I/usr/include/tcl~a -fPIC" tcl_version))
         (LD gcc)
         (LDFLAGS "-m32 -melf_i386 -O2 -fPIC -shared")
         (SHLIBS ,(format "-ltk~a -ltcl~a -L/usr/X11R6/lib -lX11 -lc -lm"
                          tk_version tcl_version))
         (FOREIGN_OBJ "swl.so")
         (InstallPrefix "/usr")
         (InstallOwner "root")
         (InstallGroup "bin")))
     (write-build-script 'oop (oop-build))
     (write-build-script 'swl (swl-build 'shared)))
    ((ppcosx-using-xdarwin)
     (set! TCL_LIBRARY (format "/usr/local/lib/tcl~a" tcl_version))
     (set! TK_LIBRARY (format "/usr/local/lib/tk~a" tk_version))
     (write-configuration
       `((NOHEAP 1)
         (CC cc)
         (CFLAGS "-O2 -I/usr/local/include -I/usr/X11R6/include")
         (LD cc)
         (LDFLAGS "-O2 -dynamiclib")
         (SHLIBS ,(format "-ltk~a -ltcl~a -L/usr/X11R6/lib -lX11 -lc -lm"
                          tk_version tcl_version))
         (FOREIGN_OBJ "swl.so")
         (InstallPrefix "/usr/local")
         (InstallOwner "root")
         (InstallGroup "wheel")))
     (write-build-script 'oop (oop-build))
     (write-build-script 'swl
       (swl-build 
         (lambda ()
           `(begin
              (with-path load-shared-object "swl.so")
              (load-shared-object ,(format "/usr/local/lib/libtcl~a.dylib" tcl_version))
              (load-shared-object ,(format "/usr/local/lib/libtk~a.dylib" tk_version)))))))
    ((ppcosx) ; native Aqua
     (set! TCL_LIBRARY (format "/Library/Frameworks/Tcl.framework/Resources/Scripts/"))
     (set! TK_LIBRARY (format "/Library/Frameworks/Tk.framework/Resources/Scripts/"))
     (let ([libtcl "/Library/Frameworks/Tcl.framework/Tcl"]
           [libtk "/Library/Frameworks/Tk.framework/Tk"]
           [foreign-obj "swl.so"])
       (write-configuration
         `((NOHEAP 1)
           (CC cc)
           (CFLAGS "-O2 -I/usr/X11R6/include -mmacosx-version-min=10.4")
           (LD cc)
           (LDFLAGS "-O2 -dynamiclib")
           (SHLIBS ,(string-append libtcl " " libtk))
           (FOREIGN_OBJ ,foreign-obj)
           (InstallPrefix "/usr")
           (InstallOwner "root")
           (InstallGroup "wheel")))
       (write-build-script 'oop (oop-build))
       (write-build-script 'swl
         (swl-build
           (lambda ()
             `(begin
                (with-path load-shared-object ,foreign-obj)
                (load-shared-object ,libtcl)
                (load-shared-object ,libtk)))))))
    ((i3osx) ; native Aqua
     (set! TCL_LIBRARY (format "/Library/Frameworks/Tcl.framework/Resources/Scripts/"))
     (set! TK_LIBRARY (format "/Library/Frameworks/Tk.framework/Resources/Scripts/"))
     (let ([libtcl "/System/Library/Frameworks/Tcl.framework/Tcl"]
           [libtk "/System/Library/Frameworks/Tk.framework/Tk"]
           [foreign-obj "swl.so"])
       (write-configuration
         `((NOHEAP 1)
           (CC cc)
           (CFLAGS "-m32 -O2 -I/usr/X11R6/include -mmacosx-version-min=10.4")
           (LD cc)
           (LDFLAGS "-m32 -O2 -dynamiclib -mmacosx-version-min=10.4")
           (SHLIBS ,(string-append libtcl " " libtk))
           (FOREIGN_OBJ ,foreign-obj)
           (InstallPrefix "/usr")
           (InstallOwner "root")
           (InstallGroup "wheel")))
       (write-build-script 'oop (oop-build))
       (write-build-script 'swl
         (swl-build
           (lambda ()
             `(begin
                (with-path load-shared-object ,foreign-obj)
                (load-shared-object ,libtcl)
                (load-shared-object ,libtk)))))))
    ((sps2)
     (write-configuration
       `((NOHEAP 1)
         (CC gcc)
         (CFLAGS "-fPIC -G -O2 -DSPS2 -I/usr/local/include")
         (LD gcc)
         (LDFLAGS "-fPIC -shared -G -O2")
         (SHLIBS ,(format "-L/usr/local/lib -R/usr/local/lib -ltk~a -ltcl~a -lc -lm" tk_version tcl_version))
         (FOREIGN_OBJ "swl.so")
         (InstallPrefix "/usr/local")
         (InstallOwner "root")
         (InstallGroup "bin")))
     (write-build-script 'oop (oop-build))
     (write-build-script 'swl (swl-build 'shared)))
    ((i3nt)
     (write-configuration
       `((NOHEAP 1)
         (CC cl)
         (CFLAGS "/MD /nologo")      ; expect INCLUDE to be set up
         (SHLIBS                 ; expect LIB to be set up
           ,(format "tcl~a.lib tk~a.lib"
              (quash-dot tcl_version)
              (quash-dot tk_version)))
         #;(TCL_BIN "")          ; expect PATH to be set up
         (FOREIGN_OBJ "swl.dll")))
     (write-build-script 'oop (oop-build))
     (write-build-script 'swl (swl-build 'shared-dll)))
    (else (printf "Don't know how to build for ~s, edit configure.ss.~%"
                  (machine-type)))))

(define quash-dot ; "x.y" => "xy", used in producing dll names
  (lambda (v)
    (list->string (remv #\. (string->list v)))))

(define absolute-path?
  (lambda (path)
    (let ([len (string-length path)])
      (or (and (> len 0) (char=? #\/ (string-ref path 0)))
          (and (> len 1)
               (char-alphabetic? (string-ref path 0))
               (char=? #\: (string-ref path 1))
               (memv (machine-type) '(ppcnt i3nt)))))))

(define hackslash
  (case-lambda 
	[(s) (hackslash s #\/ #\\)]
	[(s from to)
	 (if (not (memv (machine-type) '(ppcnt i3nt)))
	     s
	     (let ([s (string-copy s)])
	       (let f ([i (fx- (string-length s) 1)])
		 (unless (fx< i 0)
		   (when (char=? (string-ref s i) from)
			 (string-set! s i to))
		   (f (fx- i 1))))
	       s))]))

(define path-cons
  (let ([sep (if (memv (machine-type) '(ppcnt i3nt)) "\\" "/")])
    (lambda x
      (let f ([x x])
        (cond
          [(null? x) ""]
          [(null? (cdr x)) (hackslash (car x))]
          [else (string-append (hackslash (car x)) sep (f (cdr x)))])))))

(define write-configuration
  (lambda (ls)
    (with-output-to-file "config.make"
      (lambda ()
        (unless (eq? (machine-type) 'i3nt)
          (printf "TCL_LIBRARY = ~a~%" (hackslash TCL_LIBRARY))
          (printf "TK_LIBRARY = ~a~%" (hackslash TK_LIBRARY)))
        ;; This is a Scheme path, join with "/" (ie. do not use path-cons)
        (printf "SWL_LIBRARY = ~a\n"
          (if (absolute-path? prefix)
              (string-append prefix "/" (symbol->string (machine-type)))
              (string-append
                (hackslash (current-directory) #\\ #\/) "/" prefix "/"
                (symbol->string (machine-type)))))
        ;; This is a shell path, join with "/" or "\" as needed
        ;; could just hackslash the result of string append...
        (printf "prefix = ~a\n"
          (if (absolute-path? prefix)
              (path-cons prefix (symbol->string (machine-type)))
              (path-cons
                (current-directory) prefix (symbol->string (machine-type)))))
        (printf "SWL_ROOT = ~a\n"
          (if (absolute-path? prefix)
              prefix
              (path-cons (current-directory) prefix)))
        (printf "Scheme = ~a\n" swl:Scheme)
        (printf "m = ~a\n" (machine-type))
        (printf "SWL_VERSION = ~a\n" swl:version)
        (for-each (lambda (x) (printf "~a = ~a\n" (car x) (cadr x))) ls))
      'replace)))

(define write-build-script
  (lambda (who script)
    (let ([path (format "src/~a/build-script" who)])
      (with-output-to-file path
        (lambda () (pretty-print script))
        'replace))))

(define swl-build
  (lambda (foreign)
    (let ([foreign
           (case foreign
             [(shared)
              `(begin
                 ,(if (top-level-bound? 'getenv)
                      '(void)
                      '(begin
                         (load-shared-object "libc.so")
                         (set! getenv
                           (foreign-procedure "getenv" (string) string))))
                 (with-path load-shared-object "swl.so"
                   ,(format "~a/~a" prefix (machine-type))))]
             [(shared-pax)
              `(begin
                 (with-path load-shared-object "swl.sl"
                   ,(format "~a/~a" prefix (machine-type)))
                 (with-path load-shared-object
                   ,(format "libtcl~a.sl" tcl_version)
                   "/usr/local/lib"
                   ,(format "~a/~a" prefix (machine-type)))
                 (with-path load-shared-object
                   ,(format "libtk~a.sl" tk_version)
                   "/usr/local/lib"
                   ,(format "~a/~a" prefix (machine-type))))]
             [(shared-dll)
              ;; under Windows we seem to need to load all the DLLs separately
              ;; We currently require them to be in the user's path or
              ;; in SWL_LIBRARY.
              `(begin
                 ,(if (top-level-bound? 'getenv)
                      '(void)
                      '(begin
                         (load-shared-object "crtdll.dll")
                         (set! getenv
                           (foreign-procedure "getenv" (string) string))))
                 (load-shared-object
                   ,(format "tcl~a.dll" (quash-dot tcl_version)))
                 (load-shared-object
                   ,(format "tk~a.dll" (quash-dot tk_version)))
                 (with-path load-shared-object "swl.dll"))]
             [(custom) '(void)]
             [else
              (unless (procedure? foreign)
                (errorf 'swl-build "what's up with ~s" foreign))
              (foreign)])])
      `(let ()
         (include "../../config.scheme")
         (set! swl:load-foreign
           (lambda ()
             (define with-path
               (lambda (load name . paths)
                 (let ([swl-lib (getenv "SWL_LIBRARY")])
                   (let f ([paths (if swl-lib (cons swl-lib paths) paths)])
                     (if (null? paths)
                         (load name)
                         (let ([try
                                (if (car paths)
                                    (string-append (car paths) "/" name)
                                    name)])
                           (if (file-exists? try)
                               (load try)
                               (f (cdr paths)))))))))
             ,foreign))
         (case-sensitive #f)
         (generate-inspector-information #t)
         (optimize-level 0)))))

(define oop-build
  (lambda ()
    '(begin
       (include "../../config.scheme")
       (compile-file "class.ss")
       (generate-inspector-information #t)
       (optimize-level 0)
       (exit))))

(configure)
(exit)
