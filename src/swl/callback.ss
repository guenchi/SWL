;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;(define broken-weak-pair
;  (let ((x (weak-cons (cons 1 2) 3)))
;    (collect 4)
;    (car x)))

;;  N O T E S
;;
;;   * swl:callback-lambda defn moved to syntax.ss for separate compilation
;;   * callback id's are now non-zero so that Tk event code can return
;;     zero to indicate no callback (yeah, we could rig it so that callback
;;     zero was the continuation of the loop, but that would be slower)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Callbacks    totally out of date
;;
;l   Callbacks are expressed as procedures.  The number of arguments
;l   with which a callback is invoked depends on the widget function
;l   it serves.  For example, the procedure supplied as the \scheme{command:}
;l   option for a button is invoked without arguments.
;l   Thus we might specify a button's callback as:
;l   \scheme{(command: (lambda () (printf "hello~n")))}.
;l   Other callbacks are invoked with arguments so we have:
;l
;l   \scheme{(scroll-command: (lambda (total window first last)
;l    (set-scrollbar mvscroll-bar total window first last)))}
;l
;l   In addition to thunks, the \scheme{bind} form accepts procedures created by
;l   the \scheme{swl:callback-lambda} form which provides event-record destructuring.
;l   For example, a user interested in the canvas-relative x-coordinate and
;l   screen-relative y-coordinate at which mouse-button 1 is pressed on a
;l   canvas \scheme{c1} could write:
;l
;l   \scheme{(bind c1 (event: "<ButtonPress-1>")
;l        (swl:callback-lambda (x screen-y)
;l          (printf "x = ~a screen-y = ~a~n" x screen-y)))}
;;
;;    swl:callback-lambda expands into an instance of <callback-proc>
;;
;;    <callback-proc> creates token and registers token in a database
;;    so we can convert between token <--> callback-proc.  So for callbacks
;;    we arrange for Tcl to return the token to the Scheme event loop where
;;    we lookup the token in the database and apply the corresponding callback
;;    proc to any arguments provided when Tcl returned to Scheme.

;; introduced swl:fbq so that menu items could construct suitable
;; s_eval handle when consing up their callback-procs.
(define-record swl:fbq ((mutable queue) (immutable handle)))

(define-swl-class (<callback-proc> proc flag-or-args skip-1st?) (<tk-object>)
  ;; First arg is the procedure to wrap for use as Tk callback.
  ;; Second arg describes the extra args that Tk needs to provide.
  ;;   (either by destructuring the event record, or passing along
  ;;    a token for the benefit of swl:event-dispatch)
  ;; See note at end of swl:callback-lambda for explanation of skip-1st?
  (ivars (proc proc)
         (xtra-args (or flag-or-args ""))
         (token #f)
         (skip-1st? skip-1st?))
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public
    [swl:apply-callback-proc (args)
     (when (swl:bug) (fprintf (swl:bug-port) "fallback-eval:  ~s ~s~n" token args))
     (if args (apply proc (if skip-1st? (cdr args) args)) (proc))]
    [init ignore-args
     (set! token (swl:insert-callback self))
     self]
    ;; THIS is to support make-destroy-notify in init.ss
    ;;      which is used so that toplevels aren't destroyed by Tk
    ;;      when a window-manager close event is received on the Tk side
    ;;      instead we use a script that tells Scheme about the destroy
    ;;      message.  to do this we build a script that looks like
    ;;        {s_eval 0 .t134}
    ;;      where 0 is the index of the destroy callback proc, and
    ;;      .t134 is the handle of the toplevel (see init method of <toplevel>)
    [index-of () token]
    [callback->procedure () proc]
    [scheme->tcl (op)
     ;; NOTE:
     ;;  before we moved the behavior of widgets into Scheme
     ;;  it had been essential that we put a break at the end of the s_eval
     ;;  to prevent Tk from looking further down the list of bindtags for
     ;;  another callback to invoke.  consider what would happen w/ destroy
     ;;  however, break only works when we're in a bind (ie. not a command:)
     ;;
     ;;  Currently checking to see what happens when I eliminate the break
     ;;  (since I probably don't want an implicit break for the tag bindings).
     ;;
     ;; Keep the name 's_eval synchronized with foreign.c and the "binding"
     ;; method.
     (swl:display* op "{" 's_eval " " token " " xtra-args "}")]))

(define-generic swl:apply-callback-proc)

(define swl:procedure->callback
 ;; fbq-token is the handle of the fallback queue on which we want to evaluate
 ;; proc
  (lambda (proc fbq-token)
    (create <callback-proc> proc fbq-token #t)))

;; needs callbacks to support table-index method to return the index they were
;; assigned in the table

(define swl:lookup-callback)  ;; given handle returns callback
(define swl:insert-callback)  ;; takes callback returns handle for swl:lookup-callback

;; lookup operations are much more common than inserts
;; still, we might speed up inserts by preallocating the weak-pairs
;; and just hammering in the car field
;;
;; table contains weak pairs of (callback . index-in-table)
;; we scan the table in next-index
;; looking for pairs w/ #!bwp car  (we'd want to collect first)
;;
;; next is either the integer index of the next available slot in the table
;; or a pair whose car is such an integer and whose cdr is a next
;;
;; shrink-vector! grow-vector! and next-index only called from critical-section

(let ()

  (define (out->in x) (fx- x 1))
  (define (in->out x) (fx+ x 1))
  (define (make-item) (weak-cons #!bwp #f))
;  (define (make-item) (weak-cons broken-weak-pair #f))
  (define (set-item! item what) (set-car! item what))
  (define (item-value item)
     (let ((t (car item))) (and (not (eq? t #!bwp)) t)))
;  (define (item-value item)
;     (let ((t (car item))) (and (not (eq? t broken-weak-pair)) t)))

  ;;  (define (shrink-worthy? count len) (< count (quotient len 2)))
  ;; disable shrinking for now
  (define (shrink-worthy? count len) #f)

(let* ([len 200]
       [count 0]
       [free 0]
       [init-vector
        (lambda (vec start end)
          (let loop ((i start))
            (unless (fx= i end)
              (vector-set! vec i (make-item))
              (loop (fx+ i 1))))
          vec)]
       [vec (init-vector (make-vector len) 0 len)])

  ;; could shrink the vector if we set up a mechanism for the callback
  ;; bindings to notify their binders that they need to update the
  ;; index they were using
  ;;
  ;;  this means callback lambda would have to keep a list of all the
  ;;  widgets it's associated with and each widget has to keep a list of
  ;;  all the callbacks associated with it
  ;;  is that memory cost greater than the amount of space wasted in the table?
  (define shrink-vector! "shrink-vector! disabled")

  ;; grow-vector is called when free list is empty, ie. free == len
  ;; upon return, vec is bigger and free points to the first
  ;; empty slot in vec

  (define grow-vector!
    (lambda ()
      (let* ([new-len (fx+ 1 (fx* 7 (fxquotient len 4)))]
             [new-vec (make-vector new-len)])
        (vector-copy! vec new-vec len)
        (init-vector new-vec len new-len)
        (set! vec new-vec)
        (set! len new-len))))

  (define vector-copy!
    (lambda (vec new-vec len)
      (let loop ([i (fx- len 1)])
        (when (fx>= i 0)
          (vector-set! new-vec i (vector-ref vec i))
          (loop (fx- i 1))))))

  ;; table is currently full,
  ;; sweep table and rebuild the freelist
  ;; thus at the end, free is either the same integer it was
  ;; when we started, or a pair (indicating that we freed up something)

  (define update-freelist!
    (lambda ()
      (let loop ([i 0] [fls free] [n 0])
        (if (fx= i len)
            (unless (fxzero? n)
              (set! free fls)
              (when (swl:bug) (fprintf (swl:bug-port) "collected ~s callbacks~n" n))
              (set! count (fx- count n)))
            (if (item-value (vector-ref vec i))
                (loop (fx+ i 1) fls n)
                (loop (fx+ i 1) (cons i fls) (fx+ n 1)))))))

  (define next-index
    (lambda ()
      (cond
        [(pair? free)
         (let ([i (car free)]) (set! free (cdr free)) i)]
        [(fx< free len) (let ([i free]) (set! free (fx+ free 1)) i)]
        [else
         (update-freelist!)
         (if (integer? free) (grow-vector!))
         (next-index)])))
 
  (set! swl:insert-callback
    (lambda (cb)
      (critical-section
        (let ([i (next-index)])
          (set-item! (vector-ref vec i) cb)
          (set! count (fx+ count 1))
          (in->out i)))))

;  (set! swl:lookup-callback (lambda (i) (item-value (vector-ref vec (out->in i)))))
  (set! swl:lookup-callback
    (lambda (i)
      (let ([x (item-value (vector-ref vec (out->in i)))])
        (unless (vector? x)
          (assertion-violationf 'swl:lookup-callback
            "attempt to lookup dead callback ~s"
            (out->in i)))
        x)))))


(define-syntax swl:callback-lambda
  ;* \scheme{(swl:callback-lambda (keyword ...) body ...)}
  ;*
  ;* \scheme{swl:callback-lambda} creates callback procedures that bind parts of
  ;* the event record as arguements.  The formals list of a \scheme{swl:callback-lambda}
  ;* is a set of keywords chosen from \scheme{keycode}, \scheme{keysym-text},
  ;* \scheme{ascii}, \scheme{widget}, \scheme{x}, \scheme{y}, \scheme{screen-x}, \scheme{screen-y}.
  ;* When invoked as a callback the formals will be bound to the
  ;* relevant value extracted from the event record.  For example,
  ;* \scheme{widget} is bound to the widget that received the event,
  ;* \scheme{x} is bound to the widget-relative x-coordinate,
  ;* \scheme{screen-x} is bound to the screen-relative x-coordinate, etc.
  (let* ((database
           '((above "%a")
             (button-number "%b")
             (count "%c")
             (detail "%d")
             (focus "%f")
             (height "%h")
             (keycode "%k")
             (mode "%m")
             (override-redirect "%o")
             (place "%p")
             (state "%s")
             (time "%t")
             (value-mask "%v")
             (width "%w")
             (x "%x")
             (y "%y")
;; current bug w/ ascii is that using read we're hosed because
;; Tcl is quoting things like the left-arrow key #\033 with { }
;; so it comes over here like:  #\{ }
;; need our own tcl-reader to be used in foreign.ss.
             (ascii "#\\\\%A");; testing (seems to work)
             (border-width "%B")
             (display "%D")
             (send-event "%E")
             (keysym-text "|%K|")
             (keysym-decimal "%N")
             (root-window "%R")
             (sub-window "%S")
             (type "%T")
             (widget "%W" . swl:lookup-widget)
             (screen-x "%X")
             (screen-y "%Y")))
         (make-trans
           (lambda (symkeys sokeys)
             (let loop ((ls symkeys) (sos sokeys))
               (if (null? ls)
                   '()
                   (let ((x (assq (car ls) database)))
                     (if (not x)
                         (assertion-violationf 'swl:callback-lambda "invalid key ~s" (car ls))
                         (let ((x (cddr x)))
                           (if (null? x)
                               (loop (cdr ls) (cdr sos))
                               (with-syntax
                                  ((var (car sos))
                                   (exp (datum->syntax-object (syntax _) x)))
                                 (cons (syntax (var (exp var)))
                                       (loop (cdr ls) (cdr sos))))))))))))
         (lookup-code
           (lambda (sym)
             (let ((x (assq sym database)))
               (if x (cadr x) (assertion-violationf 'swl:callback-lambda "invalid key ~s" sym))))))
    (define concat
      (lambda (args)
        (let f ((ls args))
          (cond
            ((null? ls) "")
            ((null? (cdr ls)) (car ls))
            (else (string-append (car ls) " " (f (cdr ls))))))))
    (lambda (x)
      (syntax-case x ()
        ((_ (key ...) e1 e2 ...)
         (let ((keys (syntax-object->datum (syntax (key ...)))))
            (with-syntax
              ((xtras (concat (map lookup-code keys)))
               (trans (make-trans keys (syntax (key ...)))))
              (if (and (not (null? keys)) (eq? (car keys) 'widget))
                  (syntax
                    (send (make <callback-proc>
                            (lambda (key ...)
                              (let trans e1 e2 ...))
                            xtras
                            #f)
                          init))
; egregious hack
; teventloop.ss event-dispatch requires widget name as first argument to
; callback functions so that we can use the widget name to lookup the appropriate
; fallback queue.  If widget wasn't first, then we hack in in here and arrange
; to ignore our first argument.
                  (syntax
                    (send (make <callback-proc>
                            (lambda (key ...)
                              (let trans e1 e2 ...))
                            (string-append "%W " xtras)
                            #t)
                          init))
))))))))

