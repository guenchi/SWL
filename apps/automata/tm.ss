;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;; TODO
;;   - rewrite.
;;   - thread running machine should be killed when application is killed
;;     to avoid error messages
;;   - need to be able to cancel an arc draw since that confuses people when
;;     they can't abort it
;;   - need to constrain minimum h/v size of toplevel or the <machine>
;;   - moving transitions sucks right now if they're curved because they
;;     add in the xamt yamt for each state they're connected to that moves
;;       - also placement of labels leaves a bit to be desired (esp. when moved)
;;   - fix the incredibly naive tape drawing mechanism
;;       - it's totally stupid to redraw the entire tape if it hasn't even been
;;         modified (eg. only tape head moved), or if a symbol has been written
;;         (just redraw that cell).  draw-tape should only be needed when we
;;         actually have to shift the tape to keep the tape head on screen.
;;       - drawing of tape is still a bit flaky (tape head also not updated
;;         quite right sometimes)  try the ceiling machine...
;;   - clean up code
;;        - abstract do-transition (etc.) so we can do other
;;          kinds of automata (pda, nfa, dfa)
;;        - perhaps add some comments so people can tell how it works
;;        - now that we have a module system, we should use it
;;   - need some way to enter/edit/save comments about the machines
;;     (what they do, what the input looks like, what the start/end states are)
;;   - need stop/resume buttons
;;   - need some scroll bars on the canvas
;;   - allow breakpoints to be set on states
;;   - provide user-interface control over execution speed
;;   - optimize transition lookup (currently bogus list walk)
;;     would be easy to "compile" the code 
;;   - load/save should be improved:  prompt for unsaved work, allow
;;     importing machines (need alpha conversion for state names), etc.
;;   - would be nice if we could collapse an entire TM down into a black
;;     box (by indicating it's start and end states so that any transitions
;;     in go to start state, and transitions out exit the final state)
;;     then we can have libraries of subroutines.  (Could eventually build
;;     a universal TM)
;;
;;   - For non-deterministic TMs, we need to spawn additional tapes
;;     at choice points.  We could put a colored box beside them that
;;     corresponds with the color used to highlight states and transitions.
;;     Keeping one visual instance of the finite control might be easier
;;     on the eyes than simply cloning the whole machine.  It might be
;;     tricky to show multiple machines in the same state/transition
;;     when that happens.  Another way to render it is to draw multiple
;;     copies of the machine overlapping one another at an offset (and the
;;     same for their tapes) and provide a means to bring any given machine
;;     to the top and a means to lift that machine into a separate toplevel.
;;        - start method on machines, and do-transition method on states
;;          need to take a tape as another argument (passing the store)

(eval-when (compile load eval)
  (putprop '#{letrec* |tm.ss|} 'value (internal-defines-as-letrec*))
  (internal-defines-as-letrec* #t))

(module turing-machine (<state> <transition>)
  (import swl:oop)
  (import swl:macros)
  (import swl:generics)
  (import swl:option)
  (import swl:threads)
  (import swl:eventloop)
  (import swl:module-setup)

(define-syntax mv-let*
  (lambda (x)
    (syntax-case x ()
      [(_ () . body) (syntax (begin . body))]
      [(_ (((x ...) v) more ...) . body)
       (syntax
         (call-with-values
           (lambda () v)
           (lambda (x ...) (mv-let* (more ...) . body))))])))

(define blank-symbol 'blank)
(define state-radius 16)
(define tape-cell-size 25)

(define find-item-isa
  (lambda (x <class>)
    (let find-item ([x x])
      (cond
        [(null? x) #f]
        [(pair? x) (or (find-item (car x)) (find-item (cdr x)))]
        [(and (instance? x) (isa? x <class>)) x]
        [else #f]))))

(define file-load
  (lambda (tm)
    (lambda (self)
      (let ([filename
             (swl:file-dialog "File to load:" 'open
               (parent: (send tm get-toplevel))
               (file-types: '(("Turing Machine" ("*.tm")))))])
        (when filename (send tm load filename))))))

(define file-save
  (lambda (tm)
    (lambda (self)
      (let ([filename
             (swl:file-dialog "Save as:" 'save
               (parent: (send tm get-toplevel))
               (file-types: '(("Turing Machine" ("*.tm")))))])
        (when filename
          (send tm save (open-output-file filename 'truncate)))))))


; (unless (top-level-bound? 'warning-dialog) (load "../common/warning-dialog.ss"))

;; <state>
;;     m     - machine it belongs to (derived from canvas)
;;     name  - name of the state
;;     x, y  - position on the display
;;

(define-swl-class (<state> m name x y) (<oval> m 0 0 0 0)
  (ivars (label #f) (name name) (m m) (x x) (y y)
         (transitions '()) (edges '())
         (set-modified! (send m get-modified-notifier)))
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public
    [init (m name x y)
     (send-base self init m
       (- x state-radius) (- y state-radius)
       (+ x state-radius) (+ y state-radius))
     (set-modified!)
     (set-fill-color! self 'white)
     (when name
       (set! label
         (create <canvas-text> m x y with (title: (symbol->string name)))))]
    [add-edge (trans)
     (set-modified!)
     (unless (memq trans edges) (set! edges (cons trans edges)))]
    [remove-edge (trans)
     (set-modified!)
     (set! edges (remq trans edges))]
    [add-transition (trans)
     (set-modified!)
     (unless (memq trans edges) (set! edges (cons trans edges)))
     (set! transitions (cons trans transitions))]
    [remove-transition (trans)
     (set-modified!)
     (set! edges (remq trans edges))
     (set! transitions (remq trans transitions))]
    [state-name () name]
    [hilight ()
     (set-outline-color! self 'red)
     (set-line-thickness! self 3)]
    [un-hilight ()
     (set-outline-color! self 'black)
     (set-line-thickness! self 1)]
    [state-transitions () transitions]
    [move (xamt yamt)
     (set-modified!)
     (set! x (+ x xamt))
     (set! y (+ y yamt))
     (when label (send label move xamt yamt))
     (send-base self move xamt yamt)
     (for-each (lambda (trans) (send trans move xamt yamt)) edges)]
    [get-center () (values x y)]
    [do-transition (symbol)
     (let loop ((ts transitions))
       (unless (null? ts)
         (let ((trans (car ts)))
           (if (eqv? symbol (send trans transition-reading))
               (let ((action (send trans transition-action)))
                 (send m pause 2)
                 (send self un-hilight)
                 (send trans hilight)
                 (send m pause 1)
                 ;; could abstract this out so we can do DFAs too
                 (case action
                   ((left) (send m move-left))
                   ((right) (send m move-right))
                   (else (send m write-symbol action)))
                 (send m pause 1)
                 (send trans un-hilight)
                 (send m start (send trans transition-to)))
               (loop (cdr ts))))))]
    [save (op)
     (pretty-print `(define ,name (create <state> machine ',name ,x ,y)) op)]
    [non-deterministic? (trans symbol)
     ;; will this state be non-deterministic if we add the given transition
     ;; for the given symbol
     (ormap (lambda (t)
              (and (not (eq? t trans))
                   (eqv? symbol (send t transition-reading))))
            transitions)]
    [destroy ()
     (for-each (lambda (x) (send x destroy)) edges)
     (set-modified!)
     (when label (send label destroy))
     (set! transitions '())
     (set! edges '())
     (set! label #f)
     (send-base self destroy)]))

;; <transition>
;;   m     - machine (canvas)
;;   from  - from state
;;   reading - tape symbol that must be read
;;   action  - left, right, or a tape symbol to be written
;;   to    - to state

(define-swl-class (<transition> m from reading action to) (<line> m 0 0 0 0)
  (ivars (from from) (m m) (reading reading) (action action) (to to) (label #f)
         (labx #f) (laby #f) (points #f)
         (set-modified! (send m get-modified-notifier)))
  (inherited)
  (inheritable)
  (private
    [update-label ()
     (when (and reading action)
       (set-modified!)
       (if label
           (begin
             (set-title! label (format "~s:~s" reading action))
             (set-coords! label labx laby))
           (set! label
             (create <canvas-text> m labx laby
               with (title: (format "~s:~s" reading action))))))]
    [mid (a b) (exact->inexact (+ (min a b) (/ (abs (- a b)) 2)))]
    [compute-adjust (x1 y1 x2 y2)
     (let* ([adj (- x1 x2)]
            [opp (- y1 y2)]
            [hyp (sqrt (+ (* adj adj) (* opp opp)))]
            [ratio (if (zero? hyp) 0 (/ state-radius hyp))])
       (values (exact->inexact (* ratio adj)) (exact->inexact (* ratio opp))))]
    [update-coords-from-points (pts)
     (let ((len (vector-length pts)))
       (mv-let* (((cx1 cy1) (send from get-center))
                 ((cx2 cy2) (send to get-center)))
         (let ((x1 (vector-ref pts 2))
               (y1 (vector-ref pts 3))
               (x2 (vector-ref pts (fx- len 4)))
               (y2 (vector-ref pts (fx- len 3))))
           (mv-let* (((a1 b1) (compute-adjust cx1 cy1 x1 y1))
                     ((a2 b2) (compute-adjust x2 y2 cx2 cy2)))
             (vector-set! pts 0 (- cx1 a1))
             (vector-set! pts 1 (- cy1 b1))
             (vector-set! pts (fx- len 2) (+ cx2 a2))
             (vector-set! pts (fx- len 1) (+ cy2 b2))
             (set! points pts)
             (send-apply self set-coords! (vector->list pts))))))]
    [update-coords (k)
     (mv-let* (((x1 y1) (send from get-center))
               ((x2 y2) (send to get-center))
               ((a b) (compute-adjust x1 y1 x2 y2)))
       (k (- x1 a) (- y1 b) (+ x2 a) (+ y2 b)))])
  (protected)
  (public 
    [init (m from reading action to)
     (update-coords
       (lambda (x1 y1 x2 y2)
         (set! labx (mid x1 x2))
         (set! laby (mid y1 y2))
         (update-label)
         (send-base self init m x1 y1 x2 y2)))
     (set-arrow-style! self 'last)
     (send from add-transition self)
     (send to add-edge self)]
    [hilight ()
     (set-fill-color! self 'red)
     (set-line-thickness! self 3)]
    [un-hilight ()
     (set-fill-color! self 'black)
     (set-line-thickness! self 1)]
    [transition-reading () reading]
    [transition-action () action]
    [transition-from () from]
    [transition-to () to]
    [hide-label () (when label (set-coords! label -1000 -1000))]
    [show-label (x y)
     (set! labx x)
     (set! laby y)
     (when label (set-coords! label labx laby))]
    [set-transition-info! (r a)
     (set! reading r)
     (set! action a)
     (update-label)]
    [destroy ()
     (send from remove-transition self)
     (send to remove-edge self)
     (when label (send label destroy))
     (send-base self destroy)]
    [move (xamt yamt)
     (set! labx (+ xamt labx))
     (set! laby (+ yamt laby))
     (update-label)
     (cond
       ((eq? from to) (send-base self move xamt yamt))
       (points
         (let loop ((i (fx- (vector-length points) 3)))
           (unless (fx= i 1)
             (vector-set! points i (+ yamt (vector-ref points i)))
             (let ((j (fx- i 1)))
               (vector-set! points j (+ xamt (vector-ref points j))))
             (loop (fx- i 2))))
         (update-coords-from-points points))
       (else
         (update-coords
            (lambda (x1 y1 x2 y2)
              (set-coords! self x1 y1 x2 y2)))))]
    [suggest-points (pts)
     ;; Ignore the start and end points because we'll use the centers
     ;; of the from and to states to compute better start/end points
     (let* ((pts (list->vector pts)))
       (when (> (vector-length pts) 4)
         (set-draw-spline! self #t)
         (update-coords-from-points pts)))]
    [save (op)
     (pretty-print 
       `(let ((x
          (create <transition> machine
            ,(send from state-name) ',reading ',action ,(send to state-name))))
          ,(and points `(send x suggest-points ',(vector->list points)))
          (send x show-label ,labx ,laby))
       op)]))

(define char-index
  (lambda (ch s)
    (let ([len (string-length s)])
      (let loop ([i 0])
        (cond
          [(fx= i len) #f]
          [(eq? ch (string-ref s i)) i]
          [else (loop (fx+ i 1))])))))


;; could do error checking of various kinds here.
(define enter-transition-info
  (lambda (trans subwin entry clean-up)
    (lambda (self)
      (let ((s (get-string entry)))
        (delete-all entry)
        (hide subwin)
        (let ((i (char-index #\: s)))
          (when i
            (let ((reading (read (open-input-string (substring s 0 i))))
                  (action (read (open-input-string (substring s (+ i 1) (string-length s))))))
              (if (send (send trans transition-from) non-deterministic? trans reading)
                  (error-non-deterministic)
                  (send trans set-transition-info! reading action))))
          (clean-up))))))

(define error-non-deterministic
  (lambda ()
    (let* ((top (create <toplevel> with (title: "Error")))
           (labl (create <label> top with (title: "
    Simulating non-deterministic Turing Machines
    is left as an exercise for the reader.
    See comments in tm.ss
")))
           (butn (create <button> top with
                    (title: "Ok") (action: (lambda (self) (destroy top))))))
      (pack labl)
      (pack butn))))
 
;; <machine>
;;   parent    - window parent

(define-swl-class (<machine> parent) (<canvas> parent)
  (ivars (tape-left '()) (tape-right '()) (tape-head-pos 0)
         (tape-cells '()) (tape-head #f) (tape-lines '())
         (width 0) (ox 0) (oy 0) (mode #f) (selected #f) (trans #f)
         (entry #f) (subwin #f) (thread #f) (saved? #t)
         (pause-time default-pause-time) (top #f)
         (count 0) (top-margin (* 2 (+ state-radius tape-cell-size))))
  (inherited)
  (inheritable)
  (private
    [deselect-all ()
     (for-each
       (lambda (item)
         (when (or (isa? item <transition>)
                   (isa? item <state>))
           (send item un-hilight)))
       (get-items self))]
    [new-state-name ()
     (let ((name (string->symbol (format "q~a" count))))
       (set! count (+ count 1))
       name)]
    [make-drag-arrow (state)
     (mv-let* (((sx sy) (send state get-center)))
       (let ([tr (create <line> self sx sy sx sy)])
         (lower tr)
         (let ((pts (list sx sy)))
           (lambda (msg . args)
             (case msg
               [(move-end)
                (send-apply tr set-coords! (append pts args))]
               [(add-point)
                (append! pts args)
                (send-apply tr set-coords! pts)]
               [(get-points!) (set! trans #f) (destroy tr) pts])))))]
    [reset-tape-head! (x)
     (set! tape-head-pos x)
     (if tape-head
         (set-coords! tape-head
           tape-head-pos tape-cell-size
           tape-head-pos (* 2 tape-cell-size))
         (set! tape-head
           (create <line> self
              tape-head-pos tape-cell-size
              tape-head-pos (* 2 tape-cell-size)
              with (line-thickness: 5) (arrow-style: 'first))))]
    [draw-tape ()
     (for-each (lambda (x) (send x destroy)) tape-cells)
     (let ((w width) (ht (/ tape-cell-size 2)))
       (let loop ((ls tape-right) (i tape-head-pos))
         (unless (or (null? ls) (> i w))
           (draw-tape-cell i ht (car ls))
           (loop (cdr ls) (+ i tape-cell-size))))
       (let loop ((ls tape-left) (i (- tape-head-pos tape-cell-size)))
         (unless (or (null? ls) (< i 0))
           (draw-tape-cell i ht (car ls))
           (loop (cdr ls) (- i tape-cell-size)))))]
    [draw-line (x1 y1 x2 y2)
     (set! tape-lines
       (cons (create <line> self (exact->inexact x1) (exact->inexact y1) (exact->inexact x2) (exact->inexact y2))
             tape-lines))]
    [draw-tape-cell (x y content)
     (unless (eqv? content blank-symbol)
       (let ([txt (create <canvas-text> self (exact->inexact x) (exact->inexact y)
                    with (title: (format "~a" content)))])
         (set! tape-cells (cons txt tape-cells))))])
  (protected)
  (public
    [mouse-press (x y mods)
     (let ((hits (find-overlapping self (- x 5) (- y 5) (+ x 5) (+ y 5))))
       (case mode
         ((choose-start)
          (let ([state (find-item-isa hits <state>)])
            (when state
              (set! mode #f)
              (set! thread (thread-fork (lambda () (send self start state)))))))
         ((edit)
          ((get-action entry) entry) ;; for now, we'll complete the edit if clicked
          (delete-all entry)
          (hide subwin)
          (set! mode #f))
         ((connect)
          (let ([state (find-item-isa hits <state>)])
            (trans 'add-point x y)
            (when state
              (let ([new (create <transition> self selected #f #f state)])
                (send new suggest-points (trans 'get-points!))
                (set! mode #f)))))
         (else
           (event-case ((modifier= mods))
             (([right-button])
              (let ([trans (find-item-isa hits <transition>)])
                (if trans
                    (destroy trans)
                    (let ([state (find-item-isa hits <state>)])
                      (if state (destroy state))))))
             (([middle-button])
              (let ([state (find-item-isa hits <state>)])
                (when state
                  (set! mode 'drag)
                  (set! selected state)
                  (set! ox x)
                  (set! oy y))))
             (([left-button])
              (let ((tr (find-item-isa hits <transition>)))
                (if tr
                    (begin
                      (send tr hide-label)
                      (set! mode 'edit)
                      (let ((action (send tr transition-action))
                            (reading (send tr transition-reading)))
                        (when (and action reading)
                          (insert entry (format "~s:~s" reading action)))
                        (set-focus entry)
                        (set-action! entry
                          (enter-transition-info tr subwin entry
                            (lambda ()
                              (send tr show-label x y)
                              (set! mode #f))))
                        (set-coords! subwin x y)))
                    (let ((state (find-item-isa hits <state>)))
                      (set! selected
                        (or state (create <state> self (new-state-name) x y)))
                      (when state
                        (set! trans (make-drag-arrow selected))
                        (set! mode 'connect))))))))))]
    [mouse-motion (x y mods)
     (let ((y (max y top-margin)))
       (event-case ((modifier= mods))
         (([middle-button])
          (when (eq? mode 'drag) (move selected (- x ox) (- y oy))))
         (else (when trans (trans 'move-end x y))))
       (set! ox x)
       (set! oy y))]
    [mouse-release (x y mods)
     (case mode
       ((choose-state edit) (void))
       ((connect)
         (let ((state
                 (find-item-isa
                   (find-overlapping self (- x 3) (- y 3) (+ x 3) (+ y 3))
                   <state>)))
           (event-case ((modifier= mods))
             (([shift left-button])
              (let ([new
                     (create <transition> self selected #f #f
                       (or state (create <state> self (new-state-name) x y)))])
                (send new suggest-points (trans 'get-points!))
                (set! mode #f)))
             (([left-button])
              (when (and state (not (eq? state selected)))
                (let ([new (create <transition> self selected #f #f state)])
                  (send new suggest-points (trans 'get-points!))
                  (set! mode #f)))))))
       (else (set! mode #f)))]
    [init-tape (ls)
     (deselect-all)
     (set! tape-left '())
     (set! tape-right ls)
     (reset-tape-head! (exact->inexact (/ (get-width self) 2)))
     (draw-tape)]
    [move-left ()
     (let ((new (- tape-head-pos tape-cell-size)))
       (unless (< new (/ tape-cell-size 2))
         (set! tape-head-pos new)
         (move tape-head (- tape-cell-size) 0)))
     (if (null? tape-left)
         (set! tape-right (cons blank-symbol tape-right))
         (begin
           (set! tape-right (cons (car tape-left) tape-right))
           (set! tape-left (cdr tape-left))))
     (draw-tape)]
    [move-right ()
     (let ((new (+ tape-head-pos tape-cell-size)))
       (unless (> new (- width (/ tape-cell-size 2)))
         (set! tape-head-pos new)
         (move tape-head tape-cell-size 0)))
     (if (null? tape-right)
         (set! tape-left (cons blank-symbol tape-left))
         (begin
           (set! tape-left (cons (car tape-right) tape-left))
           (set! tape-right (cdr tape-right))))
     (draw-tape)]
    [write-symbol (what)
     (set! tape-right (cons what (if (null? tape-right) '() (cdr tape-right))))
     (draw-tape)]
    [configure (w h)
     (unless (zero? w)
       (create <rectangle> self -1 -1 w tape-cell-size with (fill-color: 'white))
       (for-each (lambda (x) (send x destroy)) tape-lines)
       (draw-line 0 tape-cell-size w tape-cell-size)
       (let ([new-head-pos
              (exact->inexact
                (if (> width 0) (* (/ w width) tape-head-pos) (/ w 2)))])
         (reset-tape-head! new-head-pos)
         (let ([half (/ tape-cell-size 2)])
           (let loop ([x1 (- new-head-pos half)])
             (unless (< x1 0)
               (draw-line x1 0 x1 tape-cell-size)
               (loop (- x1 tape-cell-size))))
           (let loop ([x2 (+ new-head-pos half)])
             (unless (> x2 w)
               (draw-line x2 0 x2 tape-cell-size)
               (loop (+ x2 tape-cell-size))))))
       (set! width w)
       (draw-tape))]
    [pause (factor) (thread-sleep (* factor pause-time))]
    [start (state)
     (send state hilight)
     (let ((symbol (if (null? tape-right) blank-symbol (car tape-right))))
       (send state do-transition symbol))]
    [stop () (when thread (thread-kill thread))]
    [set-pause-time! (ms) (set! pause-time ms)]
    [choose-start ()
     (deselect-all)
     (set! mode 'choose-start)]
    [get-toplevel () top]
    [print ()
     (if (not (ormap
                (lambda (item)
                  (or (isa? item <transition>) (isa? item <state>)))
                (get-items self)))
         (warning-dialog top "Nothing to print but the tape." '(cancel))
         (let ([psout
                (swl:file-dialog "Print PostScript to:" 'save
                  (parent: top))])
           (when psout
             (with-output-to-file psout
               (lambda () (display (send self postscript)))
               'truncate))))]
    [destroy ()
     (when thread (thread-kill thread))
     (send-base self destroy)]
    [get-modified-notifier ()
     ; this is a useful trick -- if this were a method, we could get
     ; in trouble if the destroy method of a <state> instance (eg.)
     ; ran after the <machine> instance were destroyed.
     (lambda () (set! saved? #f))]
    [check-file-saved ()
     (unless saved?
       (when (eq? 'yes
                  (warning-dialog top
"The current machine is not saved.
Would you like to save it first?"
                    '(yes no)))
         ((file-save self) self)))]
    [load (filename)
(pretty-print (interaction-environment))
     (and (file-exists? filename)
          (let ((f #f))
            ;; oops.  we really should do this before we get the load message...
            (send self check-file-saved)
            (load filename (lambda (x) (set! f (eval x))))
            ;; don't blitz the actual tape
            (for-each (lambda (item)
                         (when (or (isa? item <transition>) (isa? item <state>))
                           (destroy item)))
              (get-items self))
            (f self)
            ; abstract later
            (set-title! top (format "Turing Machine Simulation: ~s" filename))
            (set! saved? #t)) ; by definition, if it came from a file
            ; return #t so file-save dialog exits
            #t)]
    [save (op)
     (let ((kids (get-items self)))
       (fprintf op "(lambda (machine)\n(import swl:oop)\n(import swl:macros)\n(import swl:generics)\n(import swl:option)\n(import turing-machine)\n")
       (for-each (lambda (x) (when (isa? x <state>) (send x save op))) kids)
       (for-each (lambda (x) (when (isa? x <transition>) (send x save op))) kids)
       (fprintf op ")~n"))
     (set! saved? #t)
     ; return #t so file-save dialog exits
     #t]
    [init (parent)
     (send-base self init parent)
     (set-traversal-thickness! self 0)
     (set! entry
       (create <entry> self with
         (width/char: 0) (relief: 'flat) (traversal-thickness: 0)))
     (set-font! entry (make-font (get-font entry) (add: '(bold))))
     (set! subwin
       (create <canvas-sub-window> self -1000 -1000 with (window: entry)))
     (set! top
       (let f ([w parent])
         (if (isa? w <toplevel>) w (f (send w get-parent)))))]))

(define tm
  (lambda ()
    (let ([msg-queue (thread-make-msg-queue 'tm)])
      (swl:begin-application
        (lambda (token)
          (let* ([top (create <toplevel> with (title: "Turing Machine Simulation"))]
                 [help (create <label> top)]
                 [tm (create <machine> top with (width: 500) (height: 400))])
            (send top set-destroy-request-handler!
              (lambda (top)
                (send tm check-file-saved)
                (swl:end-application token)
                #t))
            (set-menu! top
              (make-menu
                ("_File"
                  (make-menu
                    ("_Load..." (file-load tm))
                    ("_Save..." (file-save tm))
                    ("_Print..." (lambda (i) (send tm print)))
                    ("_Quit" (lambda (i) (destroy top)))))
                ("_Tape"
                  (make-menu
                    ("_Clear" (lambda (i) (send tm init-tape '())))
                    ("_Enter" (lambda (i)
                               (let* ([top (create <toplevel> with
                                             (title: "Tape input")
                                             (geometry: 
                                               (format "+~a+~a"
                                                 (+ 50 (get-root-x top))
                                                 (+ 50 (get-root-y top)))))]
                                      [update-tape
                                       (lambda (e)
                                         (let ([result
                                                (guard (c [#t #f])
                                                  (read (open-input-string (send e get-string))))])
                                           (when (and result (list? result))
                                             (destroy top)
                                             (send tm init-tape result))))]
                                      [entry
                                       (create <entry> top with
                                         (background-color: 'white)
                                         (action:
                                           (lambda (e) (update-tape e))))])
                                 (pack (create <label> top with
                                         (justify: 'left)
                                         (title:
"Enter a list to be used as the input tape.

Indicate a blank cell with the symbol `blank'.
Other type symbols should be Scheme values
comparable with eqv?.

Because tape cells are drawn at a fixed size,
it's a good idea to keep any symbols short.
"))
                                       (side: 'top)
                                       (fill: 'both))
                                 (pack entry (side: 'top) (fill: 'x))
                                 (set-focus entry)
                                 (let ([frm (create <frame> top)])
                                   (pack (create <button> frm with
                                           (title: "Ok")
                                           (action: (lambda (b) (update-tape entry))))
                                         (side: 'left))
                                   (pack (create <button> frm with
                                           (title: "Cancel")
                                           (action: (lambda (b) (destroy top))))
                                         (side: 'left))
                                   (pack frm (side: 'top) (fill: 'x))))))))
                ("_Run"
                  (make-menu
                    ("_Start" (lambda (i)
                               (set-title! help "Click on the start state")
                               (send tm choose-start)
                               (thread-fork (lambda () (thread-sleep 2000) (set-title! help "")))))
                    ("Sto_p" (lambda (i) (send tm stop)))
                    ("_Fast" (lambda (i) (send tm set-pause-time! 1)))
                    ("S_low" (lambda (i) (send tm set-pause-time! default-pause-time)))))))
            (pack tm (side: 'top) (expand: #t) (fill: 'both))
            (pack help (side: 'top) (expand: #t) (fill: 'x))
            (thread-send-msg msg-queue tm)
            (lambda () (send top destroy)))))
      (thread-receive-msg msg-queue))))

(define default-pause-time 250)

(let ()
(define run
  (lambda (tape)
    (let ([m (tm)])
      (send m init-tape tape))))

(run '(1 1 1 1 1 1 blank 1 1 1 1 1))
)
)

(eval-when (compile load eval)
  (internal-defines-as-letrec* (getprop '#{letrec* |tm.ss|} 'value))
  (remprop '#{letrec* |tm.ss|} 'value))

