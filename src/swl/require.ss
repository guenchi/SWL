;; Copyright (c) 1996 Erik Hilsdale
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;; semantics of require:

;; If a required file has been touched recently, it is loaded with
;; the normal load.

;; If it has not been touched, it is read in but only the require
;; expressions in the file are evaluated.

;; As soon as one file is loaded with the normal load, all expressions
;; will be evaluated normally from then on, until the ``top level''
;; load is done (since that normally loaded file could change syntax
;; definitions, etc, so everything after it could depend on its new
;; definitons/effects).  The one exception to this is if the file is
;; required with the optional 'once flag.  If this is so, it will only
;; be reloaded if it changes.

;; all files are loaded in the context of the directory of their
;; loader.  The directory parameter it uses is local to require, so if
;; current-directory is called during the load that value is ignored
;; for the purposes of the requiring.

;;
;; 01/12/97 (johnz) eliminated load-time capture of load primitive
;;   as threads.ss redefines it at run-time.
;;

  (set! require
    (let* ([times '()]			; [ ((dev inode) mtime) ... ]
	   [verbose? #t]
	   [top? #t]
	   [dirty? #f]
	   [current-directory
	     (let ([old-cd current-directory]
		   [cd-string #f])
	       (case-lambda
		 [() (critical-section
		       (unless cd-string
			 (set! cd-string
			   (old-cd)))
		       cd-string)]
		 [(new) (critical-section
			  (set! cd-string new)
			  (old-cd new))]
		 [(hack0 hack1) (set! cd-string #f)]))])
      (define get-path
	(letrec ([help
		   (lambda (str pos)
		     (if (fxnegative? pos)
			 '""
			 (if (char=? (string-ref str pos) #\/)
			     (substring str 0 (fx+ pos 1))
			     (help str (fx- pos 1)))))])
	  (lambda (str)
	    (help str (fx- (string-length str) 1)))))
      (define directoryify		; returns full pathname of file and
	(lambda (fn)			; directory for load
	  (let ([pathname (get-path fn)])
	    (if (char=? (string-ref fn 0) #\/)
		(values fn pathname)
		(let ([cd (current-directory)])
		  (values (string-append cd "/" fn)
		    (string-append cd "/" pathname)))))))
      (define (stat fn)
        (let ([str
               (begin
                 (swl:tcl-eval 'file 'stat fn 'ligeti)
                 (swl:raw-tcl-eval
                   "list $ligeti(mtime) $ligeti(dev) $ligeti(ino)"))])
          (let ([ip (open-input-string str)])
            (let* ([mtime (read ip)]
                   [device (read ip)]
                   [inode (read ip)])
              (values (cons device inode) mtime)))))
      (define (eval-require e)
	(when (or dirty? (and (pair? e) (eq? (car e) 'require)))
	  (eval e)))
      (define verb
	(lambda args
	  (when verbose? (apply printf args) (flush-output-port))))
      (lambda (fn . rest)
	(let ([once? (memq 'once rest)]
	      [clean? (memq 'clean rest)])
	  (cond
	    [(string? fn)
	     (unless (file-exists? fn)
	       (assertion-violationf 'require "File ~s does not exist" fn))
	     (call-with-values (lambda () (directoryify fn))
	       (lambda (realfn base-dir)
		 (call-with-values (lambda () (stat fn))
		   (lambda (id mtime)
		     (fluid-let ([top? #f])
		       (parameterize ([current-directory base-dir])
			 (let ([p (assoc id times)])
			   (cond
			     [(not p)
			      (unless clean? (set! dirty? #t))
			      (set! times (cons (list id mtime) times))
			      (verb "loading   ~s~n" fn)
			      (load realfn)]
			     [(or (> mtime (cadr p))
				  (and dirty? (not once?)))
			      (unless clean? (set! dirty? #t))
			      (set-car! (cdr p) mtime)
			      (verb "reloading ~s~n" fn)
			      (load realfn)]
			     [else
			       (verb "bypassing ~s~n" fn)
			       (load realfn eval-require)]))))))))
	     (when top?
	       (set! dirty? #f)
	       (current-directory 'ignored 'ignored))]
	    [(eq? fn 'clear) (set! times '())]
	    [(eq? fn 'state)
	     (printf "verbose: ~s  dirty: ~s  top: ~s  times:~n"
	       verbose? dirty? top?)
	     (pretty-print times)]
	    [(eq? fn 'verbose) (set! verbose? #t)]
	    [(eq? fn 'quiet) (set! verbose? #f)]
	    [else
	      (assertion-violationf 'require "~s is not a filename" fn)])))))

(when (memq (machine-type) '(ppcnt i3nt))
  (set! require (lambda (x . y) (printf "loading ~s~n" x) (load x))))
