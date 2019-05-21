; Todo
;  - add annotation to cp0
;      x propagated propagated-test folded integrated-call integrated-mvcall
;        integrated-apply
;        integrated marks source of the function that was integrated.
;            - should wire all this together so we can link them up...
;         - history of exp is now recorded in source field:
;            <item> = #(descriptor from-source <item>) | <source>
;      - give us a way to find places where we integrated.
;      - could also indicate why integrations failed:
;          score-limit  /  effort-limit exceeded.
;  - address situation where multiple points in object code correspond to
;    same source code (should cycle through list of the many when we click
;    on the one).
;  - recognize
;      or
;      named let
;      extend and / or recognition to variable # of args.
;      let*
;  x add searching capability
;  x eliminated the markup raise hackery
;  x moving the mouse should highlight the largest corresponding expression
;    containing the current mouse coordinates.
;  x pressing mouse should scroll the other window so that the corresponding
;    line is the same number of lines from the top of the window as the point
;    where we clicked.

(printf "===> need cmacros and my patched pretty-printer~n")
(load "cmacros.so")
(load "pretty.so")
(printf "===> need xpatch containing {cprep,cp0,cp1,cp1in,primvars}.patch~n")
(require "../common/scrollframe.ss")

; old markup-intensive code took 12.610 sec (34 collections) to do cp0.ss

(define cf
  (lambda x
    (let ([view (make-view)])
      (parameterize ([run-cp0
                      (lambda (f x) (let ([z (f x)]) (view z) z))]
                     [subset-mode 'system]
                     [generate-inspector-information #t])
        (apply compile-file x)))))


(define make-view
  (let ()

(define correlate
  (lambda (ast op mark-correlation)
    (define tag
      (lambda (source form)
        (if source
            (make-pretty-escape form (mark-correlation source))
            form)))
     (define unseq
       (lambda (x rest)
         (c-record-case x
           [(seq) (e1 e2) (unseq e1 (cons (correlate e2) rest))]
           [else (cons (correlate x) rest)])))
     (define imp-begin
       (lambda (body)
         (c-record-case body
           [(seq) (e1 e2) (unseq body '())]
           [else (list (correlate body))])))
    (define not?
      (lambda (x)
        (c-record-case x
          [(if) (src e1 e2 e3)
           (and (c-record-case e2
                  [(quote) (s datum) (eq? datum #f)]
                  [else #f])
                (c-record-case e3
                  [(quote) (s datum) (eq? datum #t)]
                  [else #f]))]
          [else #f])))
    (define correlate
      (lambda (ast)
        (c-record-case ast
          [(quote) (source x)
           (tag source
             (if (or (boolean? x) (number? x) (char? x) (string? x) (null? x))
                 x
                 `',x))]
          [(ref) (source id) (tag source (prelex-name id))]
          [(seq) (e1 e2) (cons 'begin (unseq ast '()))]
          [(symref) (source type sym) (tag source sym)]
          [(symset) (source type sym e)
           (tag source `(set! ,sym ,(correlate e)))]
          [(set) (source id e)
           (tag source
             `(set! ,(prelex-name id) ,(correlate e)))]
          [(if) (source e1 e2 e3)
           (tag source
             (or (and (not? ast) `(not ,(correlate e1)))
                 (c-record-case e3
                   [(quote) (src datum)
                    (cond
                      [(eq? datum (void))
                       `(,(if (not? e1) 'unless 'when)
                            ,(correlate e1)
                            ,@(imp-begin e2))]
                      [(eq? datum #f)
                       `(and ,(correlate e1) ,(correlate e2))]
                      [else #f])]
                   [else #f])
                 `(if ,(correlate e1) ,(correlate e2) ,(correlate e3))))]
          [(foreign) (source name arg-types result-type)
           (tag source
             `(foreign-procedure ,(correlate name)
                ,arg-types
                ,result-type))]
          [(call) (source f args)
           (or (c-record-case f
                 [(case-lambda) (src libspec clauses)
                  (and (null? (cdr clauses))
                       (apply
                         (lambda (ids interface body)
                           (and (fx= interface (length args))
                                (tag
                                  src
                                  `(let ,(map (lambda (id arg)
                                                (list (prelex-name id)
                                                      (correlate arg)))
                                              ids
                                              args)
                                     ,@(imp-begin body)))))
                         (car clauses)))]
                 [else #f])
               (tag source
                 `(,(correlate f) . ,(map correlate args))))]
          [(case-lambda) (source libspec clauses)
           (tag source
             (let ([clauses
                    (map (lambda (x)
                           (apply
                             (lambda (ids interface body)
                               `(,(if (fx< interface 0)
                                      (let f ([ids ids])
                                        (if (pair? (cdr ids))
                                            (cons (prelex-name (car ids))
                                                  (f (cdr ids)))
                                            (prelex-name (car ids))))
                                      (map (lambda (x) (prelex-name x)) ids))
                                  ,@(imp-begin body)))
                             x))
                         clauses)])
               (if libspec
                   `(library-case-lambda ,libspec ,@clauses)
                   (if (null? (cdr clauses))
                       `(lambda ,@(car clauses))
                       `(case-lambda ,@clauses)))))]
          [(rec-binding) (source ids vals body)
           (tag source
             `(letrec ,(map (lambda (id val)
                              (list (prelex-name id) (correlate val)))
                            ids
                            vals)
                ,@(imp-begin body)))]
          [(primref) (source level sym pflags) (tag source sym)]
          [(mvcall) (cwv-source source consumer producer args)
           (tag (or cwv-source source)
             `(call-with-values
                (lambda () (,(correlate producer) . ,(map correlate args)))
                ,(correlate consumer)))]
          [(values) (source args)
           (tag source
             `(values . ,(map correlate args)))])))
    (correlate ast)))

(define cp0-note?
  (lambda (x)
    (and (vector? x)
         (fx= (vector-length x) 3)
         (memq (vector-ref x 0)
               '(integrated-call
                 integrated
                 folded
                 propagated
                 propagated-test
                 integrated-mvcall
                 integrated-apply)))))

(define cp0-note-type (lambda (x) (vector-ref x 0)))
(define cp0-note-from (lambda (x) (vector-ref x 1)))
(define cp0-note-source (lambda (x) (vector-ref x 2)))

; <text> pos structures
(define row-of cdr)
(define col-of car)

(define pos<
  (lambda (x y)
    (or (fx< (row-of x) (row-of y))
        (and (fx= (row-of x) (row-of y))
             (fx< (col-of x) (col-of y))))))

(define pos=
  (lambda (x y)
    (and (fx= (row-of x) (row-of y))
         (fx= (col-of x) (col-of y)))))

(define-structure (range start end val))
(define-structure (tree key val left right))
 ; range tree has range as val and either tree or range as left / right
 ; range-val will by an item or an (improper) list of items

(define insert-range
  (lambda (r node)
    (insert-end-point r (insert-start-point r node))))

(define find-range
  (lambda (pos node)
    (if (tree? node)
        (let ((key (tree-key node)))
          (cond
            [(pos< pos key) (find-range pos (tree-left node))]
            [(pos= pos key) (tree-val node)]
            [else (find-range pos (tree-right node))]))
        node)))

; new start point for range r looks like:
;     *
;     |\
;     r \         where x is r if this node did not displace anything
;        x        else #f

; new end point for range r looks like:
;     *
;    /|\
;   / r \         where x is the (old) right child of our parent
;  r     x        and x.end >= r.end

; new end point for range r looks like:
;     *
;    /| 
;   / r           if x is the (old) right child of our parent
;  r              and x is #f or x.end < r.end

(define insert-start-point
  (lambda (r node)
    (define insert
      (lambda (start r node parent install-node)
        (if (tree? node)
            (let ([key (tree-key node)])
              (cond
                [(pos< start key)
                 (insert start r (tree-left node) node set-tree-left!)]
                [(pos= start key)
                 (let ([end (range-end (tree-val node))]
                       [new (range-end r)])
                   (if (pos= new end)
                       (let ([r2 (tree-val node)])
                         (set-range-val! r2
                           (cons (range-val r) (range-val r2))))
                       (begin
                         (when (pos< new end) (set-tree-val! node r))
                         ; hack?
                         (insert-end-point r node))))]
                [else (insert start r (tree-right node) node set-tree-right!)]))
            (install-node
              parent
              (make-tree start r node (if (not node) r node))))))
    (let ((new (make-tree #f #f #f #f)))
      (insert (range-start r) r node new set-tree-left!)
      (or (tree-left new) node))))

(define insert-end-point
  (lambda (r node)
    (define insert
      (lambda (end r node parent install-node)
        (if (tree? node)
            (let ([key (tree-key node)])
              (cond
                [(pos< end key)
                 (insert end r (tree-left node) node set-tree-left!)]
                [(pos= end key)
                 (let ([start (range-start (tree-val node))]
                       [new (range-start r)])
                   (unless (pos< new start) (set-tree-val! node r))
                   ; hack?
                   (insert-start-point r node))]
                [else (insert end r (tree-right node) node set-tree-right!)]))
            (install-node parent
              (if (or (not node) (pos< end (range-end node)))
                  (make-tree end r r node)
                  (make-tree end r r #f))))))
    (let ((new (make-tree #f #f #f #f)))
      (insert (range-end r) r node new set-tree-left!)
      (or (tree-left new) node))))

(define show-tree
  (lambda (tree wd ht height-inc)
    (let* ((top (create <toplevel> with (title: "Tree")))
           (can (create <canvas> top with (width: wd) (height: ht))))
      (define compute-mid
        (lambda (lo hi) (fx+ lo (fxsra (fx- hi lo) 1))))
      (define draw
        (lambda (node lo hi lvl)
          (when node
            (let ((mid (compute-mid lo hi)))
              (if (tree? node)
                  (let ((nxt (fx+ lvl height-inc)))
                    (draw (tree-left node) lo mid nxt)
                    (draw (tree-right node) mid hi nxt)
                    (when (tree-left node)
                      (create <line> can mid lvl (compute-mid lo mid) nxt))
                    (when (tree-right node)
                      (create <line> can mid lvl (compute-mid mid hi) nxt))
                    (create <canvas-text> can mid lvl with
                      (anchor: 'center)
                      (title: (format "~s" (tree-key node))))
                    (create <canvas-text> can mid (fx+ lvl 15) with
                      (anchor: 'center)
                      (title: (format "~s" (range-val (tree-val node))))))
                  (create <canvas-text> can mid lvl with
                    (anchor: 'center)
                    (title: (format "~s" (range-val node)))))))))
      (draw tree 0 wd 10)
      (show can))))

(define-structure (corr view start end))

(define-swl-class (<view> p viewer) (<text> p)
  (ivars
    (range-tree #f) (bold #f) (cur-range #f) (other-views '()) (viewer viewer)
    (registry #f))
  (inherited)
  (inheritable)
  (private
    [ins-range (r)
     (set! range-tree (insert-range r range-tree))]
    [mark-correlated-pos (pos)
     (let ((r (find-range pos range-tree)))
       (unless (eq? r cur-range)
         (when cur-range
           (remove-markup bold self
             (range-start cur-range)
             (range-end cur-range))
           (for-each
             (lambda (x) (send (corr-view x) clear-hilite))
             other-views)
           (set! other-views '()))
         (when r
           (apply-markup bold self (range-start r) (range-end r))
           (let loop ((val (range-val r)))
             (cond
               ((corr? val)
                (set! other-views (cons val other-views))
                (send (corr-view val) hilite (corr-start val) (corr-end val)))
               ((pair? val) (loop (car val)) (loop (cdr val)))
               (else (void)))))
         (flush-output-port))
       (set! cur-range r))]
    [show-correlated-pos ()
     (when cur-range
       (let* ([s1 (range-start cur-range)]
              [dy (- (row-of s1) (row-of (xy->index self 0 0)))])
         (for-each
           (lambda (other)
             (let ([view (corr-view other)] [s2 (corr-start other)])
               (send view make-visible s2)
               (send view move-to-top
                 (cons (col-of s2) (max 0 (- (row-of s2) dy))))
               "; this is a scrollview method:"
               (send (get-parent view) make-visible view)))
           other-views)))])
  (protected)
  (public
    [init (p viewer)
     (send-base self init p)
     (set! bold
       (create <markup> with
         (font: (make-font (get-font self) (weight: 'bold)))))]
    [destroy ()
     (when registry (destroy registry))
     (send-base self destroy)]
    [mouse-motion (x y mods)
     ; don't need mutex around this to protect ivars we whack on
     ; because events are serialized.  (Of course, someone could
     ; send us a forged mouse-motion event...)
     (mark-correlated-pos (xy->index self x y))
     (send-base self mouse-motion x y mods)]
    [mouse-press (x y mods)
     (show-correlated-pos)
     (send-base self mouse-press x y mods)]
    [key-press (key mods)
     (event-case ((key= key) (modifier= mods))
       (([#\/]) (send viewer search self 'forward))
       (([#\?]) (send viewer search self 'backward))
       (([#\n]) (send viewer research self))
       (([#\N])
        (send viewer search self 'backward)
        (send viewer research self))
       (([control #\f]) (vscroll self 1 'pages))  ; should set cursor pos, too
       (([control #\b]) (vscroll self -1 'pages))
       (([control #\y]) (vscroll self -1 'units))
       (([control #\e]) (vscroll self 1 'units))
       (([#\h]) (move-char self -1))
       (([#\l]) (move-char self 1))
       (([#\j]) (move-line self 1))
       (([#\k]) (move-line self -1))
       (([#\b])
       ; this is broken
        (set-cursor-pos! self (word-start self (get-cursor-pos self))))
       (([#\e])
       ; this is broken
        (set-cursor-pos! self (word-end self (get-cursor-pos self))))
       (([#\w])
       ; this is broken (should skip white space)
        (set-cursor-pos! self
          (word-start self (word-end self (get-cursor-pos self)))))
       (([#\newline] [#\return]) (show-correlated-pos))
       (else (send-base self key-press key mods)))]
    [set-cursor-pos! (new)
     (send-base self set-cursor-pos! new)
     (mark-correlated-pos new)]
    [set-font! (fnt)
     (send-base self set-font! fnt)
     (set! bold
       (create <markup> with
         (font: (make-font fnt (weight: 'bold)))))]
    [correlate (s1 e1 view s2 e2)
     (ins-range (make-range s1 e1 (make-corr view s2 e2)))]
    [mark-note (type filename pos) ; used only for src text
     (unless registry
       (set! registry
         (create <registry> self with (title: "Summary"))))
     (let ((start (add-offset self '(0 . 0) pos))
           (end (add-offset self '(0 . 0) (sexp-end filename pos))))
       (let ((r (make-range start end type)))
         (ins-range r)
         (send registry mark-note type r)))]
    [notes-marked ()
     (when registry (send registry notes-marked))]
    [add-range (r) (ins-range r)]
    [hilite (start end) (apply-markup bold self start end)]
    [clear-hilite () (remove-markup bold self '(0 . 0) 'end)]
 ;; we want to be read only.
    [insert (str) (void)]
    [delete (i) (void)]
    [delete (i j) (void)]
    [delete-char (disp) (void)]
    [delete-all () (void)]
    [delete-eol () (void)]
    [delete-selection-at-cursor () (void)]))

; bogus:  should generalize scale or make buttons mutually exclusive
(define-swl-class (<registry> client) (<toplevel>)
  (ivars
    (notes
      (vector
        (list 0)  ; integrated-call     (count . list)
        (list 0)  ; integrated
        (list 0)  ; folded
        (list 0)  ; propagated
        (list 0)  ; propagated-test
        (list 0)  ; integrated-mvcall
        (list 0))); integrated-apply
    (titles
      '#7("Integrated call ~a"
          "Integrated ~a"
          "Folded ~a"
          "Propagated ~a"
          "Propagated (test) ~a"
          "Integrated mvcall ~a"
          "Integrated apply ~a"))
    (client client)
    (btns #f) (active-btn #f)
    (scale #f))
  (inherited)
  (inheritable)
  (private
    [type->index (type)
     (case type
       [(integrated-call) 0]
       [(integrated) 1]
       [(folded) 2]
       [(propagated) 3]
       [(propagated-test) 4]
       [(integrated-mvcall) 5]
       [(integrated-apply) 6]
       [else (assertion-violationf 'mark-note "unknown type ~s" type)])])
  (protected)
  (public
    [init (client)
     (send-base self init)
     (set! btns 
       (vector
         (create <button> self with (anchor: 'w))
         (create <button> self with (anchor: 'w))
         (create <button> self with (anchor: 'w))
         (create <button> self with (anchor: 'w))
         (create <button> self with (anchor: 'w))
         (create <button> self with (anchor: 'w))
         (create <button> self with (anchor: 'w))))
     (set! scale
       (create <scale> self with
         (min: 1) (orientation: 'horizontal)))]
    [mark-note (type range)
     (let ((x (vector-ref notes (type->index type))))
       (set-car! x (fx+ (car x) 1))
       (set-cdr! x (cons range (cdr x))))]
    [notes-marked ()
     (do ((i 0 (fx+ i 1)))
         ((fx= i (vector-length notes)))
       (let ((count (car (vector-ref notes i))))
         (if (fxzero? count)
             (set-enabled! (vector-ref btns i) #f)
             (let ((btn (vector-ref btns i)))
               (set-title! btn (format (vector-ref titles i) count))
               (set-cdr! (vector-ref notes i)
                 (sort (lambda (r1 r2)
                         (pos< (range-start r1) (range-start r2)))
                       (cdr (vector-ref notes i))))
               (pack btn (side: 'top) (fill: 'x))
               (set-action! btn
                 (let ([flag #f]
                       [mkup (create <markup> with (foreground-color: 'blue))])
                   (lambda (btn)
                     (set! flag (not flag))
                     (set! active-btn
                       (and flag
                            (begin
                              (when (and active-btn (not (eq? active-btn btn)))
                                (send active-btn invoke))
                              btn)))
                     (set-relief! btn (if flag 'sunken 'raised))
                     (let ((first (cadr (vector-ref notes i))))
                       (send client make-visible (range-end first))
                       (send client make-visible (range-start first)))
                     (if (and flag (fx> count 1))
                         (begin
                           (set-max! scale count)
                           (set-focus scale)
                           (set-action! scale
                             (lambda (scale j)
                               ;; could do list->vector after sorting it above
                               (let ((r (list-ref (vector-ref notes i) j)))
                                 (send client make-visible (range-end r))
                                 (send client make-visible (range-start r))
                                 (send client clear-hilite)
                                 (send client hilite
                                   (range-start r) (range-end r)))))
                           (set-value! scale 1)
                           (pack scale (side: 'bottom) (fill: 'x)))
                         (hide scale))
                     (let ([op (if flag apply-markup remove-markup)])
                       (let loop ([rs (cdr (vector-ref notes i))])
                         (unless (null? rs)
                           (let ([r (car rs)])
                             (op mkup client (range-start r) (range-end r)))
                           (loop (cdr rs))))))))))))]))

(define-swl-class (<viewer>) (<toplevel>)
  (ivars (entry #f) (quarry #f) (curview #f) (curdir #f))
  (inherited)
  (inheritable)
  (private
    [dosearch ()
     (let ((pos (get-cursor-pos curview)))
       (let ([hit (send curview search quarry pos (list curdir))])
         (if hit
             (begin
               (set-focus curview)
               (set-cursor-pos! curview hit)
               (send entry clear-selection))
             (send entry select-range 0 'end))
         hit))])
  (protected)
  (public
    [init ()
     (send-base self init)
     (set! entry
       (create <entry> self with
         (relief: 'flat)
         (select-foreground-color: 'white)
         (select-background-color: 'black)
         (select-border-width: 0)
         (action:
           (lambda (ent)
             (when curview (set! quarry (get-string ent)) (dosearch))))))
     (pack entry (side: 'bottom) (fill: 'x))]
    [search (who dir)
     (when (eq? dir curdir) (send entry select-range 0 'end))
     (set! curview who)
     (set! curdir dir)
     (let ([range (send who get-selected-range)])
       (when range
         (let ([str (get-string who (car range) (cdr range))])
           (delete-all entry)
           (insert entry str))))
     (set-focus entry)]
    [research (who)
     (set! curview who)
     (case curdir
       ((forward)
        (let ((pos (get-cursor-pos curview)))
          (send who move-char 1)
          (or (dosearch) (set-cursor-pos! curview pos))))
       ((backward) (dosearch)))]))

(define make-<view>-output-port
  (lambda (text)
    (define write
      (lambda (str cnt)
        (send text insert-at 'end
          (if (fx< cnt (string-length str)) (substring str 0 cnt) str))))
    (define port-handler
      (lambda args
        (record-case args
          [flush-output-port (p)
           (let ([i (port-output-index p)] [b (port-output-buffer p)])
             (write b i)
             (set-port-output-index! p 0))]
          [write-char (c p)
           (let ([i (port-output-index p)]
                 [s (port-output-size p)]
                 [b (port-output-buffer p)])
             (let ([i (if (fx= i s) (begin (flush-output-port p) 0) i)])
               (string-set! b i c)
               (set-port-output-index! p (fx+ i 1))))]
          [close-port (p)
           (flush-output-port p)
           (set-port-output-size! p 0)
           (mark-port-closed! p)]
          [else (assertion-violationf 'port-handler "unhandled message ~s" args)])))
    (make-output-port port-handler (make-string 4096))))

(define source-filename
  (lambda (x)
    (if (vector? x) ; compiler record              (if cp-source is run)
        (source-filename (vector-ref x 1))
        (and (pair? x)
             (let ((x (cdr x)))
               (and (vector? x)      ;; #2(annotation "lsdkfldsjf")
                    (vector-ref x 1)))))))

(define source-filepos
  (lambda (x)
    (if (vector? x) ; compiler-record               (if cp-source is run)
        (source-filepos (vector-ref x 1))
        (car x))))

(define sexp-end
  (lambda (filename pos)
    (let ([ip (open-input-file filename)])
      (file-position ip pos)
      (read ip)
      (let ([end (file-position ip)])
        (close-input-port ip)
        end))))

(define markup-sexp
  (lambda (markup widget filename start-pos)
    (markup-range markup widget start-pos (sexp-end filename start-pos))))

(define markup-range
 ; this may be faster than the more straightforward approach
  (lambda (markup widget start end)
    (let ([begin (send widget add-offset '(0 . 0) start)])
      (apply-markup markup widget begin
        (send widget add-offset begin (- end start))))))

; this is currently exploiting the fact that markups can span multiple texts
; however, it's not making much use of that fact...

(define new-hilite-markup
  (lambda (src src-pos obj obj-pos)
    (create <markup> with
      (foreground-color: (make <rgb> (random 255) (random 255) (random 255)))
      (mouse-press-method:
        (lambda (self text x y mods)
          (call-with-values
            (lambda ()
              (if (eq? text src)
                  (values obj obj-pos src-pos)
                  (values src src-pos obj-pos)))
            (lambda (hit offset other)
              (let ([pos
                     (if (pair? offset)
                         offset
                         (send hit add-offset '(0 . 0) offset))])
                (let ([disp
                       (- (row-of
                            (if (pair? other)
                                other
                                (send text add-offset '(0 . 0) other)))
                          (row-of (xy->index text 0 0)))])
                  (printf "pos = ~s disp = ~s~n" pos disp)
                  (send hit move-to-top
                    (cons (col-of pos) (max 0 (- (row-of pos) disp))))
                  (send (get-parent hit) make-visible hit)))))))
      (mouse-enter-method:
        (lambda (self text x y mods)
          '(send self set-font! (make-font (get-font text) (weight: 'bold)))))
      (mouse-leave-method:
        (lambda (self text x y mods)
          '(send self set-font! #f))))))

(define with-source
  (lambda (s k)
    (let ([filename (source-filename s)])
      (when filename (k filename (source-filepos s))))))

(define make-correlator
 ; correlate ensures that the procedure returned is only called if
 ; we have source information for the pretty-escape-handler
  (lambda (obj post-process lookup)
    (lambda (source) ; mark-correlation
      (lambda (start-row start-col end-row end-col) ; pretty-escape-handler
       ; pretty-print's row-offset re-starts at 1 each time he's called
       ; here we adjust them to reflect the current offset (including previous
       ; calls to pretty-print)
        (let ([start-row (fx+ start-row obj-row-offset -1)]
              [end-row (fx+ end-row obj-row-offset -1)])
          (set-box!
            post-process
            (cons (lambda ()
                    (let loop ((x source))
                      (if (cp0-note? x)
                          (begin
                            (with-source (cp0-note-from x)
                              (lambda (filename pos)
                                (let ([src (lookup filename)]
                                      [type (cp0-note-type x)])
                                  (send src mark-note type filename pos))))
                            (loop (cp0-note-source x)))
                          (with-source source 
                            (lambda (filename src-pos)
                              (let* ([src (lookup filename)]
                                     [obj-pos (cons start-col start-row)]
                                     [markup
                                      (new-hilite-markup
                                        src src-pos obj obj-pos)])
                          (if #f ; was: "use Tk markups"
                              (begin
                                (markup-sexp markup src filename src-pos)
                                (apply-markup
                                  markup obj obj-pos (cons end-col end-row))
                                (send src markup-raise markup)
                                (send obj markup-raise markup))
                              (let ((s1 (add-offset src '(0 . 0) src-pos))
                                    (e1 (add-offset src '(0 . 0)
                                          (sexp-end filename src-pos)))
                                    (s2 (cons start-col start-row))
                                    (e2 (cons end-col end-row)))
                                (send src correlate s1 e1 obj s2 e2)
                                (send obj correlate s2 e2 src s1 e1)))
                                src))))))
                  (unbox post-process))))))))

(define make-file-table
  (lambda (new)
    (let ([alist '()])
      (case-lambda
        [() alist]
        [(filename)
         (let ([key (string->symbol filename)])
           (let ([hit (assq key alist)])
             (if hit
                 (cdr hit)
                 (let ([hit (new)])
                   (set! alist (cons (cons key hit) alist))
                   (read-file! filename hit)
                   hit))))]))))

(define read-file!
  (lambda (filename txt)
    (let ([buf (make-string 2048)])
      (let loop ([ip (open-input-file filename)])
        (let ([x (block-read ip buf 2048)])
          (unless (eof-object? x)
            (insert-at txt 'end (if (< x 2048) (substring buf 0 x) buf))
            (loop ip)))))))

  (define obj-row-offset)

  (lambda () ; make-view
    (let* ([top (create <viewer> with (title: "Viewer"))]
           [tf (create <frame> top)]
           [done (create <button> tf with
                   (title: "Done")
                   (action: (lambda (btn) (send top destroy))))]
           [frm (create <frame> top)]
           [scr1 (create <scrollframe> frm with (sticky-hscroll: #t))]
           [scr2 (create <scrollframe> frm with (sticky-hscroll: #t))]
           [obj (create <view> scr2 top with
                  (wrap: 'none) (background-color: 'white))]
           [views (list obj)]
           [font (create <button> tf with
                   (title: "Font...")
                   (action:
                     (lambda (item)
                       (swl:font-dialog top "Select a font"
                         (swl:font-families 'fixed)
                         '(-8 -10 -12 -14 -16 -18 -20 -22 -24 8 10 12 14 16 18 20 22 24)
                         '(bold normal)
                         (lambda () (send (car views) get-font))
                         (lambda (fnt)
                           (when fnt
                             (for-each (lambda (x) (set-font! x fnt)) views)))))))])
      (set-focus obj)
      (pack tf (side: 'top) (fill: 'x))
      (pack font (side: 'left))
      (pack done (side: 'left) (fill: 'x))
      (pack frm (side: 'top) (expand: #t) (fill: 'both))
      (pack scr1 (side: 'top) (fill: 'both) (expand: #t))
      (pack scr2 (side: 'top) (fill: 'both) (expand: #t))
      (let* ([op (make-<view>-output-port obj)]
             [post-process (box '())]
             [file-table
              (make-file-table
                (lambda ()
                  (let ([src (create <view> scr1 top with
                               (wrap: 'none) (background-color: 'white))])
                    (set-focus src)
                    (set! views (cons src views))
                    src)))]
             [correlator (make-correlator obj post-process file-table)])
        (lambda (ast) ; view
          (fluid-let ([obj-row-offset (cdr (get-cursor-pos obj))])
            (printf "pretty-printing~n")
            (pretty-print (correlate ast op correlator) op)
            (newline op)
            (flush-output-port op)
            (printf "post-processing~n")
            (for-each force (unbox post-process))
            (printf "updating summary~n")
            (for-each (lambda (x) (send (cdr x) notes-marked)) (file-table))
            (set-box! post-process '()))))))))

