;; Copyright (c) 1996 Oscar Waddell and John Zuckerman
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;; (require "../common/scrollframe.ss")
;; (require "../common/selectbox.ss")
;; (require "../common/warning-dialog.ss")

;;
;; provides: select-file, is-directory?, is-file?
;;

;;
;; select-file: the optional fourth argument, if present, restricts
;;   the selection as follows:
;;
;;     existing     - file must already exist

;; rename to swl:select-file ?

(define select-file
  (case-lambda
    [(dir title parent)
     (select-file dir title parent 'existing)]
    [(dir title parent file-restriction)
     (let ([who
            (if (eq? file-restriction 'existing)
                '|tk_getOpenFile|
                '|tk_getSaveFile|)])
       (let ([result
              (if parent
                  (swl:tcl-eval who
                    '-initialdir dir
                    '-title title
                    '-parent parent)
                  (swl:tcl-eval who '-initialdir dir '-title title))])
         (if (string=? result "") #f result)))]))

(define is-directory?
  (lambda (path)
    (fx= 1 (string->number (swl:tcl-eval 'file 'isdirectory path)))))

(define is-file?
  (lambda (path)
    (fx= 1 (string->number (swl:tcl-eval 'file 'isfile path)))))





#!eof

     (define read-from-process
       (lambda (string)
         (let ([proc (process string)])
           (let ([ip (car proc)])
             (let loop ([line (read-line ip)])
               (if (not line)
		 (begin (close-input-port (car proc))
		   (close-output-port (cadr proc))
		   '())
		 (cons line (loop (read-line ip)))))))))

     (define read-line
       (let ([op (open-output-string)])
         (lambda (ip)
           (let loop ([x (read-char ip)])
             (cond
               [(eof-object? x) #f]
               [(char=? x #\newline) (get-output-string op)]
               [else (write-char x op) (loop (read-char ip))])))))

     (define ls
       (case-lambda
         [() (ls ".")]
         [(dir) (read-from-process (string-append "exec ls " dir))]))

     (define join-path
       (lambda (path name)
         (cond
           [(string=? name ".") path]
	   [(and (positive? (string-length name))
	      (let ((ch0 (string-ref name 0)))
		(or (char=? ch0 #\/) (char=? ch0 #\\))))
	    name]
           [(string=? name "..")
            (let loop ([i (fx1- (string-length path))])
              (if (fxnegative? i)
		"/"
		(let ([ch (string-ref path i)])
		  (cond
		    [(fx= i 0)
		     (if (char=? ch #\~)
		       (let ([val
			       (read-from-process
				 (string-append
				   "(cd "
				   path
				   "/..; pwd)"))])
			 (if (list? val) (car val) "/"))
		       "/")]
		    [(or (char=? ch #\\) (char=? ch #\/))
		     (substring path 0 i)]
		    [else (loop (fx1- i))]))))]
           [else
	     (if (string=? path "/")
	       (string-append "/" name)
	       (string-append path "/" name))])))

     (define trim-slash
       (lambda (s)
         (let ([l (fx- (string-length s) 1)])
           (cond
             [(fx< l 0) s]
             [(and (char=? (string-ref s l) #\/)
		(fx> l 0)
		(not (char=? (string-ref s (fx- l 1)) #\\)))
              (substring s 0 l)]
             [else s]))))

     (let ([selectq (thread-make-msg-queue 'select-file)])
       (swl:application-modal
         (if parent
	   (send parent get-application-context)
	   (swl:fallback-queue))
         (lambda ()
           (let ([top (create <toplevel> with (title: title))])
             (when parent
               (set-transient! top parent)
               (send top set-geometry!
                 (format "+~a+~a" ;;; allow auto sizing
                   (+ (get-root-x parent) (quotient (get-width parent) 4))
                   (+ (get-root-y parent) (quotient (get-height parent) 4))))
	       )
             (let* ([return-filename
		      (lambda (name)
			(destroy top)
			(thread-send-msg selectq name))]
                    [dirlab
		      (create <label> top with
			(title: (string-append "Current Directory: " dir))
			)]
                    [entry (create <entry> top with
			     (background-color: 'white)
			     )]
                    [f0 (create <frame> top)]
                    [f1 (create <frame> f0)]
                    [f2 (create <frame> f0)]
                    [sf1
		      (create <scrollframe> f1
			with (sticky-vscroll: #t) (sticky-hscroll: #t))]
                    [sf2
		      (create <scrollframe> f2
			with (sticky-vscroll: #t) (sticky-hscroll: #t))]
                    [dirls (create <selectbox> sf1)]
                    [filels (create <selectbox> sf2)]
                    [dlab (create <label> f1 with (title: "Directories"))]
                    [flab (create <label> f2 with (title: "Files"))]
                    [ok (create <button> f1 with (title: " ok "))]
                    [cancel (create <button> f2 with (title: "Cancel"))])
               (pack entry (side: 'top) (fill: 'x))
               (pack dirlab (side: 'top) (anchor: 'w))
               (pack f0 (side: 'top) (fill: 'both) (expand: #t))
               (pack dlab (side: 'top) (anchor: 'w))
               (pack flab (side: 'top) (anchor: 'w))
               (pack ok (side: 'bottom))
               (pack cancel (side: 'bottom))
               (pack dirls (side: 'left) (expand: #t) (fill: 'both))
               (pack filels (side: 'left) (expand: #t) (fill: 'both))
               (pack sf1 (side: 'left) (expand: #t) (fill: 'both))
               (pack sf2 (side: 'left) (expand: #t) (fill: 'both))
               (pack f1 (side: 'left) (expand: #t) (fill: 'both))
               (pack f2 (side: 'left) (expand: #t) (fill: 'both))
               (let ([seldir (trim-slash dir)] [selname #f] [curdir #f])
                 (letrec
		   ([update-display
		      (lambda ignore
			(set! curdir seldir)
			(set! seldir #f)
			(set! selname #f)
			(delete-all entry)
			(let ([names (cons ".." (ls curdir))])
			  (delete-all dirls)
			  (delete-all filels)
			  (set-title! dirlab
			    (string-append "Current Directory: " curdir))
			  (let loop ([names names])
			    (unless (null? names)
			      (let* ([name (car names)]
				     [path (join-path curdir name)])
				(if (is-directory? path)
				  (send dirls add-choice name
				    (lambda ()
				      (set! seldir path)
				      (set! selname #f)
				      (send filels clear-selection)
				      (delete-all entry)))
				  (send filels add-choice name
				    (lambda ()
				      (set! selname name)
				      (set! seldir #f)
				      (send dirls clear-selection)
				      (delete-all entry)
				      (insert entry name)))))
			      (loop (cdr names))))))]
		    [entry-action
		      (lambda ignore
			(let* ([str (get-string entry)]
			       [path (join-path curdir str)])
			  (cond
			    [(string=? str "") (return-filename #f)]
			    [(is-file? path)
			     (if (eq? file-restriction 'non-existing)
			       (let ((ans (warning-dialog top
					    (string-append
					      "File already exists: " path " "
					      "
Do you want to overwrite it?")
					    '(yes no))))
				 (when (eq? ans 'yes)
				   (delete-file path)
				   (return-filename path)))
			       (return-filename path))]
			    [(is-directory? path)
			     (set! seldir path)
			     (update-display)]
			    [(eq? file-restriction 'existing)
			     (warning-dialog top
			       (string-append "File not found: "
				 path " "
				 "
Please try again."
				 ))]
			    [else   ;;; no restrictions -- let it go
			      (return-filename path)])))]
		    [ok-action
		      (lambda ignore
			(if seldir (update-display) (entry-action)))])
                   (set-action! dirls update-display)
                   (set-action! entry entry-action)
                   (set-action! filels entry-action)
                   (set-action! ok ok-action)
                   (update-display)
                   (set-action! cancel
                     (lambda (self) (return-filename #f)))
                   (thread-receive-msg selectq))))))))]))

    
