;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; graph classes
;;
(printf "
  This is a simple graph editor that permits interactive and
  algorithmic manipulation of undirected graphs.  For example,
  try the following:

    (define g (new-graph 300 300))
    (define x (new-vertex g 10 10))
    (define y (new-vertex g 290 290))
    (connect x y)
    (move x 40 100)

    ...
    (show-dfs x)

  The mouse can be used as follows:

    left-button    create a new vertex
                   if the mouse is already over a vertex, then the
                   new one is connected to that vertex and may be
                   dragged to its new location
    middle-button  move a vertex
    right-button   delete a vertex
")

(eval-when (compile load eval)
  (putprop '#{letrec* |tm.ss|} 'value (internal-defines-as-letrec*))
  (internal-defines-as-letrec* #t))

(module (connect neighbors-of mark unmark marked? move other-vertex select-vertex
         edges-of vertices-of with-selected
         new-graph new-vertex show-dfs)

(import swl:oop)
(import swl:threads)
(import swl:macros)
(import swl:option)
(import swl:generics)
(import swl:module-setup) ; to get swl:begin-application

;; Todo
;;   - add directed edges
;;   - add weighted edges, or at least let someone subclass that way
;;   - fix stupid bugs in with-selected method

(define-generic connect)
(define-generic neighbors-of)
(define-generic mark)
(define-generic marked?)
(define-generic unmark)
(define-generic other-vertex)
(define-generic select-vertex)
(define-generic edges-of)
(define-generic vertices-of)
(define-generic with-selected)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; help stuff
;;

(define first-vertex
  (case-lambda
    ((x) (first-vertex x #f))
    ((x other-than)
     (cond
       ((null? x) #f)
       ((pair? x) (or (first-vertex (car x) other-than)
                      (first-vertex (cdr x) other-than)))
       ((eq? x other-than) #f)
       ((and (instance? x) (isa? x <vertex>)) x)
       (else #f)))))

(define connected?
  (lambda (x y)
    (or (eq? x y)
        (and (instance? x) (isa? x <vertex>)
             (instance? y) (isa? y <vertex>)
             (let ((xn (neighbors-of x)))
               (and xn (memq y xn)))))))

(define same-parent?
  (lambda (x y)
    (and (instance? x) (instance? y)
         (isa? x <vertex>)
         (isa? y <vertex>)
         (eq? (get-parent x) (get-parent y)))))

(define mapt
  (lambda (f ls)
    (if (null? ls)
        '()
        (let ((x (f (car ls))) (rest (mapt f (cdr ls))))
          (if x (cons x rest) rest)))))

(define get-selected (lambda (g) (with-selected g (lambda (x) x))))
 

(define-swl-class (<edge> from to) (<line> (get-parent from) 0 0 0 0)
  (ivars (from from) (to to))
  (inherited)
  (inheritable)
  (private)
  (protected [class-name () '<edge>])
  (public
    [init (from to)
     (call-with-values
       (lambda () (send from get-center))
       (lambda (x1 y1)
         (call-with-values
           (lambda () (send to get-center))
           (lambda (x2 y2)
             (send-base self init (get-parent from) x1 y1 x2 y2)))))
     (lower self)]
    [other-vertex (opposite)
     (cond
       ((eq? opposite from) to)
       ((eq? opposite to) from)
       (else (assertion-violationf 'other-vertex "unknown vertex ~s" opposite)))]
    [move-end (xamt yamt end)
     (apply (lambda (x1 y1 x2 y2)
               (cond
                 ((eq? end from)
                  (set-coords! self (+ xamt x1) (+ yamt y1) x2 y2))
                 ((eq? end to)
                  (set-coords! self x1 y1 (+ xamt x2) (+ yamt y2)))
                 (else (assertion-violationf 'move-end "invalid end ~s" end))))
            (get-coords self))]))

;; NOTE:  almost every operation in this class should be
;;        covered w/ critical section

(define-swl-class (<vertex> graph x y) (<oval> graph 0 0 0 0)
  (ivars (x x) (y y) (ox #f) (oy #f) (edges '()) (mrk #f) (oldcolor #f))
  (inherited)
  (inheritable)
  (private)
  (protected [class-name () '<vertex>])
  (public
    [init (graph x y)
      ;; The create macro passes the same arguments to the init method
      ;; that were passed to create the instance.  Since <oval> expects
      ;; different # of args, we make our init method call <oval>'s with
      ;; the right number of args.
     (send-base self init graph (- x 5) (- y 5) (+ x 5) (+ y 5))
      ;; the various mouse events seem to register only when the mouse is
      ;; on a visible part of the object (eg. the outline if visible, or
      ;; the filled region, if not transparent)
     (set-fill-color! self 'red)]
    [mouse-motion (x y mods)
     (event-case ((modifier= mods))
       (([middle-button])
        (when ox
          (move self (- x ox) (- y oy))
          (set! ox x)
          (set! oy y)))
       (else (send-base self mouse-motion x y mods)))]
    [mouse-press (x y mods)
     (event-case ((modifier= mods))
       (([middle-button])
        (set! ox x)
        (set! oy y)
        (select-vertex (get-parent self) self))
       (([right-button])
        (destroy self))
       (else (send-base self mouse-press x y mods)))]
    [mouse-release (x y mods)
     (event-case ((modifier= mods))
       (([middle-button]) (set! ox #f))
       (else (send-base self mouse-release x y mods)))]
    [get-center () (values x y)]
    [destroy ()
     (for-each
       (lambda (edge)
         (send (other-vertex edge self) del-edge edge)
         (destroy edge))
       edges)
     (set! edges '())  ;; This is unfortunate, but must drop for GC sake.
     (send-base self destroy)]
    [move (xamt yamt)
     (set! x (+ x xamt))
     (set! y (+ y yamt))
     (send-base self move xamt yamt)
     (for-each (lambda (edge) (send edge move-end xamt yamt self)) edges)]
    [del-edge (edge) (set! edges (remq edge edges))]
    [add-edge (edge) (set! edges (cons edge edges))]
    [connect (to-vertex)
     (unless (connected? self to-vertex)
       (let ((edge (create <edge> self to-vertex)))
         (send self add-edge edge)
         (send to-vertex add-edge edge)))]
    [neighbors-of ()
     (let ((x (map (lambda (e) (other-vertex e self)) edges)))
       (and (not (null? x)) x))]
    [mark ()
     ;; could use the color to determine whether we're marked or not
     (unless mrk
       (set! oldcolor (get-fill-color self))
       (set-fill-color! self 'black)
       (set! mrk #t))]
    [marked? () mrk]
    [unmark () (when mrk (set-fill-color! self oldcolor) (set! mrk #f))]))

(define-swl-class (<graph> parent) (<canvas> parent)
  (ivars (selected #f) (msg-q (thread-make-msg-queue 'selected))
         (destroyed? #f) (new-vertex #f) (from-x #f) (from-y #f))
  (inherited)
  (inheritable)
  (private)
  (protected [class-name () '<graph>])
  (public
    [mouse-press (x y mods)
     (event-case ((modifier= mods))
       (([left-button])
        ;; when left button is pressed, find the vertex hit, if any or create
        (let ([hit (first-vertex (find-overlapping self x y x y))])
          (select-vertex self
            (or hit (create <vertex> self x y with (fill-color: 'red))))))
       (else (send-base self mouse-press x y mods)))]
    [mouse-motion (x y mods)
     (event-case ((modifier= mods))
       (([left-button])
        (critical-section
          (cond
            [new-vertex
             (move new-vertex (- x from-x) (- y from-y))
             (set! from-x x)
             (set! from-y y)]
            [selected
             (set! new-vertex
               (create <vertex> self x y with (fill-color: 'blue)))
             (connect selected new-vertex)
             (set! from-x x)
             (set! from-y y)])))
       (else (send-base self mouse-motion x y mods)))]
    [mouse-release (x y mods)
     (event-case ((modifier= mods))
       (([left-button])
        ;; when left button is released, find out whether we're dropping the
        ;; new vertex on an old one.  if so, connect these and nuke the temp
        ;; else the temp is promoted to a "real" vertex.
        (when new-vertex
          (let ([hit (first-vertex (find-overlapping self x y x y) new-vertex)])
            (if hit
                (begin (destroy new-vertex)
(unless selected (printf "nothing selected~n"))
                       (connect selected hit))
                (set-fill-color! new-vertex 'red)))
            (set! new-vertex #f)))
       (else (send-base self mouse-release x y mods)))]
    [destroy ()
     (unless destroyed?
       (set! destroyed? #t)
       (send (get-parent self) destroy)
       (send-base self destroy))]
    [edges-of () (mapt (lambda (x) (and (isa? x <edge>) x)) (get-items self))]
    [vertices-of ()
     (mapt (lambda (x) (and (isa? x <vertex>) x)) (get-items self))]
    [with-selected (f) (f (thread-receive-msg msg-q))]
    [select-vertex (v)
     (set! selected v)
     (thread-send-msg msg-q v)]))

(define new-graph
  (lambda (cw ch)
    (let ([q (thread-make-msg-queue 'new-graph)])
      (swl:begin-application
        (lambda (token)
          (let ([c 0])
            (set! c (+ c 1))
            (let* ([top
                    (create <toplevel> with
                      (title: (format "Graph ~a" c))
                      (destroy-request-handler:
                        (lambda (self)
                          (swl:end-application token)
                          #t)))]
                   [close (create <button> top with (title: "Close")
                            (action: (lambda (b) (send top destroy))))]
                   [graph
                    (create <graph> top
                      with
                        (width: cw)
                        (height: ch)
                        (border-width: 3)
                        (relief: 'ridge))])
              (pack close (side: 'top) (fill: 'x))
              (show graph)
              (thread-send-msg q graph)
              (lambda () (send top destroy))))))
      (thread-receive-msg q))))

(define new-vertex
  (lambda (g x y)
    (create <vertex> g x y)))

(define dfs
  (lambda (fn vertex)
    (unless (marked? vertex)
      (mark vertex)
      (fn vertex)
      (for-each (lambda (x) (dfs fn x)) (neighbors-of vertex)))))

(define run
  (lambda (traversal vertex)
    (let* ([continue (thread-make-msg-queue 'continue)]
           [tl (create <toplevel> with (title: ""))]
           [btn
            (create <button> tl with
              (title: "Continue")
              (action: (lambda (self) (thread-send-msg continue #t))))])
      (define pause
        (lambda (vertex)
          (thread-receive-msg continue)))
      (thread-fork-group
        (lambda ()
          (send btn show)
          (traversal pause vertex)
          (set-title! btn "done")
          (set-action! btn (lambda x (send tl destroy)))))
      (void))))

(define show-dfs (lambda (vertex) (run dfs vertex)))

)

(eval-when (compile load eval)
  (internal-defines-as-letrec* (getprop '#{letrec* |tm.ss|} 'value))
  (remprop '#{letrec* |tm.ss|} 'value))

(define g (new-graph 300 300))
(define x (new-vertex g 150 10))
(define y (new-vertex g 290 290))
(connect x y)
