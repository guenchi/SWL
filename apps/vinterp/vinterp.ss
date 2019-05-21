;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

; TODO
;  * users should be told that breakpoints will not be settable on code that
;    gets optimized away via cp0
;  * Improve efficiency of instrument method of <viewer> (it's amazingly stupid
;    in this prototype).  The interpreter's parse function should invoke our
;    instrument method with the record type and source position to get a procedure
;    that takes an environment.  The parse function then has code like this:
;        (let ((update-view (send viewer instrument rectype source-pos)))
;          (lambda (e)
;            (update-view e)    ; pauses execution if need be
;            (p e))             ; the usual threaded-code proc
;
;    We should then have something roughly like:
;      [extend-table! (pos setter)
;       ; instead of an entry for each row of the file, make k entries
;       ; for each column of the file (eg. 5 entries for each of 80 cols,
;       ; or compute k as a function of col since not many exps start at
;       ; col > 50 perhaps) and in each entry we have an alist mapping
;       ; the row number to a setter.  (could sort it, of course)
;       ; This gives us fast enough lookup that we can set mouse-cursor
;       ; based on lookup in mouse-motion method (to give visual feedback
;       ; for where we could set breakpoint).
;       (let ([index (compute-index pos)])
;         (vector-set! table index
;           (cons (cons (row-of pos) setter)
;                 (vector-ref table index))))]
;      [instrument (rectype pos)
;       ; for set rectype return proc that also updates env view (or flags it
;       ; as needing update and records the env)
;       (let ([stop? #f])
;         (extend-table! pos
;           (lambda ()        ; setter for free lexical var
;             (set! stop? (or (eq? mode 'step) (eq? mode rectype)))))
;         (lambda (env)       ; this is the update-view invoked by intepreter
;           ; could trim # of free vars...
;           (set! last-pos pos)   ; used for show-last-pos replacement
;           (set! last-env env)   ; (shows where we stopped on error)
;           (when stop?       ; test free lexical var
;             (let ((view (find-view ...)))
;               (update-env-view env)
;               (show the view and wait on the semaphore)))))]
;      [set-break-point! (pos)
;       ((table-lookup table (compute-index pos)))]
;  - should extend t1.ss and t2.ss to make them into a tutorial on using the
;    visual interpreter.  Add a bunch of comments basically.
;  - if env viewer is destroyed, he should notify the main viewer so he can
;    cons up a new one...  (SWL needs better support for this)
;      - perhaps make env viewer a sub-window of the main interpreter window
;        to avoid confusing students with lots of toplevel windows
;      - need better SWL support for managing subwindows in resizable subframes
;        (split views)
;  - provide menu access to the various features of the system.
;  - need better tools for jumping over / through calls
;      - the improved cursor motion & ability to run to cursor helps with this.
;  - disable all insertion into text widget (eg. mouse paste)
;  - rename <viewer>
;  - clear hilight when execution completes
;  - add a File Load... menu
;  - sload should eagerly load the file so folks can set breakpoints before
;    evaluating.
;  - provide wrapper that does the work of creating a <viewer> (rename that),
;    making an interpreter that uses the viewer, firing up a repl with
;    current-eval set to the resulting interpreter, and changing load
;    to be sload (wish we could change it for this repl only...).
;      - might be nice if we had a protocol for applications to create
;        themselves within a frame provided by other applications.  This
;        would let us do things like:
;          (let ((frame (create <frame> top)))
;            (let ((repl (make-repl-in-frame frame)))
;              ...))
;  - need help system  (dust off netporte?)
;      - documentation + tutorial
;  - improve environment browser (perhaps rename)
;      - replace with visual inspector?  (need some hooks, I suspect)
;          - maybe should really just dust off netport and make visual
;            inspector a filter that constructs HTML for netport (or
;            provide API from netport)
;      - print methods
;      - don't show all bindings by default (should be a button to do this)
;          - introduce hrules in the environment display to show scopes
;            and sort the display from closest scope on out.  i.e., free vars
;            of current expression show up below the first hrule ...
;      - have mode that lets you see just the vals on the current frame
;      - allow people to highlight identifiers in the program text and have
;        them automatically added to the watch list (use source info, see below)
;      - when mouse moves over identifier, show its binding
;        (tricky, can't just assume that because it says "foo" it's the same
;         foo that's in the environment.  may be able to use source info to
;         good advantage for this)
;  - need some way to view your stack to see who called you, etc.
;    would be cool if you could click to any particular frame and have it
;    jump to the right place in the source and show the corresponding environ
;    (need better support from interpreter)
;  - do something quasi-intelligent when someone clicks on the wrong place
;    to set a breakpoint (eg. clicks in the middle of an identifier, etc.)
;  - when you reach a breakpoint (or step point) there should be an
;    easy operation to let you get back to that point if you have scrolled
;    away, for example.  eg. an easy way to visually jump to current program
;    point.
;  - when clicking to set break point, it should add to a breakpoint
;    window so you can easily enable / disable breakpoints.
;    for each breakpoint, window might list breakpoint number, short
;    snippet of code, and provide enable / disable radiobutton
;    (as well as an enable/disable all breakpoints button)
;       - perhaps jump you to the appropriate point in the code if you
;         click on the breakpoint number.  jumps you to the highlighted
;         breakpoint (when you release button, it could jump you back to
;         where you were (maybe only certain mouse button))
;  - elide parts of current File: name to ensure relevant part fits in display
;  - add ability to view return results from function calls
;  - env viewer can exploit source information to provide hyper links in the
;    source (or color coding) between vars in the env and in the program src.
;  - should provide ways of stepping into a call or jumping over it
;      ? need support from modified interpret.ss parse function?
;  - worth the effort to make instrumented closures smaller?
;      - pass source intact to cut down on # of free vars for instrumented
;        program points.
;      - trim the free:  p sf self offset filename bps rec-type inst cte
;  - perhaps the mode setting should be on a per-file basis so we can step
;    through one file and run to completion in another (eg. set-mode! get-mode
;    become methods of <view-text>)
;  - what happens if it can't find the file?
;      - should make this more tolerant (could prompt for source file, or
;        fail silently, or give warning, maybe give warning and make it possible
;        to interrupt and supply file info if desired)
;  - fix egregious hack in position->offset
;  - could add circular buffer of source positions and environment snapshots
;    that could be used to backtrace (even for tail calls) the last 10
;    expressions evaluated (whether or not a breakpoint / single-step was set
;    for these positions).  It would be nice to be able to backtrack from the
;    current breakpoint to see how you got there.
;  - figure out the thread syncing stuff so that this evaluator can be
;    used instead on itself, etc.

;; Interpreter
;;    * modify interpreter to work with inspector (right now inspecting a
;;      continuation of interpreter frames is far from enlightening)
;;    - could speed up the env lookups using table:
;;        (case rib
;;          ((0) (lambda (e) (vector-ref e elt)))
;;          ((1) (lambda (e) (vector-ref (vector-ref e 0) elt)))
;;          ((2) (lambda (e) (vector-ref (vector-ref (vector-ref e 0) 0) elt)))
;;          (else ...))
;;    - could go nuts and avoid creating all those (lambda (e) ...)'s
;;    - could strip out the 'e' arg to parse.
;;        - need to modify dolambda so there's some way for an env-viewer
;;          to display env if there's a viewer

;;    - should be able to pull up any of the files it's loaded
;;      (or that it will load) so we can set breakpoints there
;;    - can't rely on the eq? ness of filename anymore if we allow them
;;      to load files by themselves (ie. not by demand of viewer)
;;    - make viewer an object instead of a single-interface procedure
;;        - needs methods for:
;;           - reset view (clear markups after we've finished evaluating)
;;    - should pass the environment to the viewer so that it can be visualized.
;;      Then we'd probably want to hang onto
;;      some of the prelexes so the environment can be displayed w/ names and
;;      source info.
;;    - need to be able to skip over calls, etc.  maybe just make it work
;;      to skip over whatever expression has a paren we're on...
;;    - fix it so that clicking 'step' or holding 'n' down for a long time
;;      after the computation has finished doesn't allow it to step that far
;;      the next time a computation is started...
;;    - need to be a lot smarter about the fallback queues and where we
;;      suspend threads.  currently possible to hose the thing by trying to
;;      run another swl app in the vinterpreter (in step mode) because they're
;;      currently sharing a single fallback queue and the vinterpreter sleeps
;;      on it.  stupid.  it should be sleeping the thread that called the
;;      interpreter...

(printf "Once this loads (Chez Scheme version >= 5.0f), try the following:

(define v (create <viewer>))
(define int (make-interpret v))
(sload \"t1.ss\" int)
(sload \"t2.ss\" int)
(send v set-mode! (make-bset step)) ;; or click on the step button
(int '(my-odd? 4))

")

(require "interpret.ss")
(subset-mode #f)
(require "../common/selectbox.ss")
(require "../common/scrollframe.ss")
(require "../common/semaphore.ss")

;; currently something broken for inspector -- out of heap space just
;; trying to load the code...
'(require "../inspector/inspector.ss")
(define vinspect
  (lambda x
    (printf "Some recent SWL change seems to have broken the visual inspector~n")
    (printf "It may well be the same safe symbol printing change that broke the~n")
    (printf "require.ss code.  For now, I'm replacing it with this stub.~n~n")
    (printf "When you click on an environment entry you're *supposed* to get~n")
    (printf "a visual inspector window...~n")))

(define kwd->bset
  (lambda (x)
    (case x
      [(ref)         #b000000000001]
      [(primref)     #b000000000010]
      [(symref)      #b000000000100]
      [(quote)       #b000000001000]
      [(case-lambda) #b000000010000]
      [(set)         #b000000100000]
      [(symset)      #b000001000000]
      [(if)          #b000010000000]
      [(call)        #b000100000000]
      [(seq)         #b001000000000]
      [(rec-binding) #b010000000000]
      [(foreign)     #b100000000000]
      [(step)        #b111111111111]
      [else (assertion-violationf 'kwd->bset "unrecognized keyword ~s" x)])))

(define-syntax bset-union
  (lambda (x)
    (syntax-case x ()
      [(_) (syntax 0)]
      [(_ s . rest)
       (syntax (fxlogor s (bset-union . rest)))])))

(define-syntax make-bset
  (lambda (x)
    (syntax-case x ()
      [(_ kwd ...)
       (with-syntax ([(val ...)
                      (datum->syntax-object
                        (syntax _)
                        (map kwd->bset
                             (syntax-object->datum (syntax (kwd ...)))))])
         (syntax (bset-union val ...)))])))

(define-syntax bset-empty? (identifier-syntax fxzero?))

(define-syntax bset-intersect
  (lambda (x)
    (syntax-case x ()
      [(_) (most-positive-fixnum)]
      [(_ s . rest)
       (syntax (fxlogand s (bset-intersect . rest)))])))


; thunk should make it easy to provide quick & dirty enable/disable
; should move all the stuff currently done by <viewer> when it hits
; a breakpoint into the thunk.  Right now that looks like a pain.
; Another possibility is to have the thunk return #t or #f depending
; on whether the bpt is enabled.

(define-structure (breakpoint thunk offset pos))


(define-swl-class (<view-text> parent viewer filename bps) (<text> parent)
  (ivars (viewer viewer) (filename filename) (mkup #f)
         (bpt-mkup (create <markup> with (foreground-color: 'red)))
         (bps bps))
  ;; bps is a pair (held also by viewer) whose cdr is an
  ;;     alist of breakpoint structures keyed by source file offset
  (inherited)
  (inheritable)
  (private
    ;; manipulation of breakpoint list isn't terribly thread-safe...
    [bpt-end-point (start) (cons (fx+ 1 (car start)) (cdr start))]
    [lookup-breakpoint (where) (assq where (cdr bps))]
    [toggle-breakpoint! (where thunk)
     (if (lookup-breakpoint where)
         (delete-breakpoint! where)
         (set-breakpoint! where thunk))]
    [delete-breakpoint! (where)
     (let lp ([bps (cdr bps)] [last bps])
       (unless (null? bps)
         (if (eq? where (caar bps))
             (let* ([start (breakpoint-pos (cdar bps))]
                    [end (bpt-end-point start)])
               (set-cdr! last (cdr bps))
               (remove-markup bpt-mkup self start end))
             (lp (cdr bps) bps))))]
    [set-breakpoint! (where thunk)
     (unless (lookup-breakpoint where)
       (let* ([start (add-offset self '(0 . 0) where)]
              [end (bpt-end-point start)]
              [bkpt (make-breakpoint thunk where start)])
         (apply-markup bpt-mkup self start end)
         (set-cdr! bps (cons (cons where bkpt) (cdr bps)))))])
  (protected)
  (public
    [show-source (offset)
     (remove-markup mkup self '(0 . 0) 'end)
     (let ([start (add-offset self '(0 . 0) offset)]
           [end (add-offset self '(0 . 0) (sexp-end-position filename offset))])
       (send self make-visible end)
       (set-cursor-pos! self start)
       (apply-markup mkup self start end))]
    [mouse-press (x y mods)
     (event-case ((modifier= mods))
       (([left-button])
        (toggle-breakpoint! (position->offset self (xy->index self x y)) void))
       (([right-button])
        ;; quick hack, I'm really supposed to be working on cp0 now...
        (let ((here (position->offset self (xy->index self x y)))
              (mode (send viewer get-mode)))
          (unless (lookup-breakpoint here)
            ;; save source position and mark bpt as temporary
            (set-breakpoint! here
              (lambda ()
                (send viewer set-mode! mode)
                (delete-breakpoint! here)))))
        (send viewer set-mode! (make-bset))
        (send viewer step-next)))
     (send-base self mouse-press x y mods)]
    [key-press (key mods)
     (event-case ((key= key) (modifier= mods))
      ; standard vi bindings  (and emacs search keys for John Z)
      ; (we desperately need support for dynamic key binding tables)
      ; (eg. emacs keys don't really work right...)
       (([#\/] [control #\s]) (send viewer search 'forward))
       (([#\?] [control #\r]) (send viewer search 'backward))
       (([#\n]) (send viewer research))
       (([#\N])
        ; should go forward if last search start dir was backward (ie. #\?)
        (send viewer search 'backward)
        (send viewer research))
       (([control #\f]) (vscroll self 1 'pages))
       (([control #\b]) (vscroll self -1 'pages))
       (([control #\e]) (vscroll self 1 'units))
       (([control #\y]) (vscroll self -1 'units))
       (([page_up]) (vscroll self -1 'pages))
       (([page_down]) (vscroll self 1 'pages))
       (([up] [#\k]) (move-line self -1))
       (([down] [#\j]) (move-line self 1))
       (([left] [#\h]) (move-char self -1))
       (([right] [#\l]) (move-char self 1))
      ; stepper bindings
      ; should abstract these things into methods and call from here...
       (([#\space]) (send viewer step-next))
       (([#\c]) (set-cdr! bps '())
                (remove-markup bpt-mkup self '(0 . 0) 'end))
       (([#\S]) (send viewer set-mode! (make-bset call if case-lambda))
                (send viewer step-next))
       (([#\s]) (send viewer set-mode! (make-bset step))
                (send viewer step-next))
       (([#\b])
        (toggle-breakpoint! (position->offset self (get-cursor-pos self)) void))
       (([#\r]) (send viewer set-mode! (make-bset)) (send viewer step-next))
       (([#\g])
        (let ([here (position->offset self (get-cursor-pos self))]
              [mode (send viewer get-mode)])
          (unless (lookup-breakpoint here)
            (set-breakpoint! here
              (lambda ()
                (send viewer set-mode! mode)
                (delete-breakpoint! here))))
          (send viewer set-mode! (make-bset))
          (send viewer step-next))))]
    [unhilite () (remove-markup mkup self '(0 . 0) 'end)]
    [insert (what) (void)]              ; disable mouse paste, etc.
    [insert-at (index what) (void)]
    [init (p v f b)
     (send-base self init p)
     (let ([buf (make-string 2048)])
       (let loop ([ip (open-input-file f)])
         (let ([x (block-read ip buf 2048)])
           (unless (eof-object? x)
             (send-base self insert-at 'end
               (if (fx= x 2048) buf (substring buf 0 x)))
             (loop ip)))))
     (set-focus self)
     (set! mkup
       (create <markup>
         with (font: (make-font (get-font self) (add: '(bold))))))]))

(printf "Key/Mouse bindings:
   b           set breakpoint at cursor pos
               (put cursor on left paren or 1st char of identifier)
   r           run til next breakpoint
   g           run til expression under cursor
   c           clear breakpoints
   S           set coarser step mode (roughly, parenthesized exprs) and step
   s           set single step mode and step
   space       step (in whatever current step mode is)
   up          scroll screen down
   down        scroll screen up
   page up     scroll screen down
   page down   scroll screen up
   /           forward search               [Escape or ^G cancels]
   ?           reverse search
   n           repeat last search
   N           reverse search direction and repeat search (broken)
   left-button   set breakpoint
   right-button  run until expression under mouse cursor

Click on items in environment viewer to bring up inspector...
")


(define-swl-class (<my-entry> p) (<entry> p)
  (ivars (viewer p))
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public
    [key-press (key mods)
     (event-case ((key= key) (modifier= mods))
       (([escape] [control #\g])
        (send viewer abort-search))
       (else (send-base self key-press key mods)))]))

;; search stuff coded at higher level so that we can repeat last search on
;; different buffer.  makes the code just a bit gummy.

(define-swl-class (<viewer>) (<toplevel>)
  (ivars (sem #f)
    (show-last-pos void)  ;; for error handler
    (sf #f) (mbar #f) (mode (make-bset)) (bstep #f) (brun #f) (fmenu #f)
    (top2 #f) (sf2 #f) (env-view #f) (old-e #f) (flist '())
    (bug-out void) ;; how to get the hell out of here
    (curview #f) (quarry #f) (curdir 'forward) (entry #f))
  (inherited)
  (inheritable)
  (private
    [search-start-pos (offset)
     (let ((pos (get-cursor-pos curview)))
       (if (zero? offset)
           pos
           (add-offset curview pos offset)))]
    [dosearch (offset)
     (when (and curview quarry (not (string=? quarry "")))
       (let ([pos (search-start-pos offset)])
         (let ([hit (send curview search quarry pos (list curdir))])
           (if hit
               (begin
                 (set-focus curview)
                 (set-cursor-pos! curview hit)
                 (send entry clear-selection))
               (send entry select-range 0 'end))
           hit)))]
    [add-view (filename bps)
     ; hmm... when do we remove a view?
     (let ([view
            (create <view-text> sf self filename (find-breaklist filename)
              with (wrap: 'none) (background-color: 'white))])
       (set! flist
         (cons (cons filename (lambda () (make-current view filename)))
               flist))
       (send fmenu set-options! flist)
       view)]
    [make-current (view filename)
     (send sf make-visible view)
     (set-focus view)
     (set! curview view)
     (set-title! fmenu filename)]
    [display-env (cte e)
     (set! old-e e)
     (delete-all env-view)
     (parameterize ([print-level 3] [print-length 5])
       (let next-rib ([cte cte] [e e])
         (unless (null? cte)
           (let do-rib ([vars (cdr cte)])
             (unless (null? vars)
               (let ([id (car vars)])
                 (let ((value (vector-ref e (cdr (prelex-operand id)))))
                   (send env-view add-choice
                     (format "~s  =  ~s" (prelex-name id) value)
                     (lambda () (vinspect value))))
                 (do-rib (cdr vars)))))
           (next-rib (car cte) (vector-ref e 0)))))]
    [update-env (cte e)
     (unless (eq? e old-e) (display-env cte e))])
  (protected)
  (public
    [instrument (rec-type source cte p)
     ;; NOTE:  The start method must be called before any code
     ;;        instrumented here is run.
     ;;
     ;; rec-type is the kind of record we're dealing w/ (use for skip-to)
     ;; source is known to be good if were called
     ;; cte is the compile-time environment
     ;; p is the procedure we're instrumenting
     (let* ([offset (car source)]
            [sfd (cdr source)]
            [filename
             (let ()
               (import $system)
               (with-values ($locate-source sfd offset)
                 (case-lambda
                   [() (source-file-descriptor-name sfd)]
                   [(path line char) path])))]
            [bps (find-breaklist filename)]
            [p (if (eq? rec-type 'set)
                   (lambda (e) (p e) (display-env cte e))
                   p)]
            [rec-type (kwd->bset rec-type)])
       (lambda (e)
         (set! show-last-pos  ;; make this less inefficient
           (lambda ()
             (let ([view
                    (find-view
                      filename
                      (lambda (filename) (add-view filename bps)))])
               (send view show-source offset)
               (update-env cte e))))
         (let ([bpt
                (or (not (bset-empty? (bset-intersect mode rec-type)))
                    (let ([bls (cdr bps)])
                      (and (not (null? bls))
                           (let ([hit (assq offset bls)])
                             (and hit (cdr hit))))))])
           (when bpt
             (let ([view
                    (find-view
                      filename
                      (lambda (filename) (add-view filename bps)))])
               (make-current view filename)
               (send view show-source offset)
               (update-env cte e)
               (when (breakpoint? bpt) ((breakpoint-thunk bpt)))
               (send sem wait))))
         (p e)))]
    [step-next () (send sem signal)]
    [show-last-position () (show-last-pos)]
    [start ()
     (set! old-e #f)
     (set! sem (let ((sem (make <sem> 1))) (send sem wait) sem))
     (call/cc (lambda (k) (set! bug-out k)))]
    [finish ()
     (display-env '() '())
     (send sf for-children
       (lambda (c) (when (isa? c <view-text>) (send c unhilite))))]
    [get-mode () mode]
    [set-mode! (v)
     ;; mode is set containing
     ;;   step if we're single-stepping
     ;;   type if we're running until we get to the next record of type
     ;;   or nothing (run)  if we're running flat-out
     (set! mode v)]
    [abort-search ()
     (delete-all entry)
     (when curview (set-focus curview))]
    [search (dir)
     ; dir is 'forward or 'backward
     (when (eq? dir curdir) (send entry select-range 0 'end))
     (set! curdir dir)
     (let ([range (send curview get-selected-range)])
       (when range
         (let ([str (get-string curview (car range) (cdr range))])
           (delete-all entry)
           (insert entry str))))
     (set-focus entry)]
    [research ()
     (case curdir
       ((forward) (dosearch 1))
       ((backward) (dosearch 0)))]
    [init ()
     (send-base self init)
     (set-title! self "Interpreter")
     (set! sf (create <scrollframe> self with
                (default-vscroll: #t) (sticky-hscroll: #t)))
     (set! mbar (create <frame> self))
 ;; Step and Run should really be mutually exclusive (radiobuttons)
     (set! bstep (create <button> mbar with
                   (title: "Step")
                   (action: (lambda (button)
                              (set! mode (make-bset step))
                              (send self step-next)))))
     (set! brun (create <button> mbar with
                   (title: "Run")
                   (action: (lambda (button)
                              (set! mode (make-bset))
                              (send self step-next)))))
     (set! fmenu (create <option-button> mbar "(no file)"))
     (set! entry
       (create <my-entry> self with
         (action:
           (lambda (ent)
             (when curview (set! quarry (get-string ent)) (dosearch 0))))))

    ; lame environment viewer.
     (set! top2 (create <toplevel> with (title: "Environment")))
     (set! sf2 (create <scrollframe> top2))
     (set! env-view (create <selectbox> sf2))

     (pack bstep (side: 'left))
     (pack brun (side: 'left))
     (pack fmenu (side: 'left))
     (pack mbar (side: 'top) (anchor: 'w))
     (pack sf (side: 'top) (expand: #t) (fill: 'both))
     (pack sf2 (expand: #t) (fill: 'both))
     (pack entry (side: 'top) (fill: 'x))]
    [destroy ()
     (destroy top2) ;; torch env viewer
     (let ([exit bug-out]) (send-base self destroy) (exit))]))

(define find-view
  (let ([views '()])
    (lambda (filename fail)
      (define try (lambda (hit) (and hit (cdr hit))))
      (or (try (assq filename views))
          (try (assoc filename views))
          (let ([view (fail filename)])
            (set! views (cons (cons filename view) views))
            view)))))

;; locations shared by all program points in a given file
;; could play cuter games here
(define find-breaklist
  (let ([bps '()])
    (lambda (filename)
      (let ([hit (assq filename bps)])
        (or (and hit (cdr hit))
            (let ([it (cons #f '())])
              (set! bps (cons (cons filename it) bps))
              it))))))


;; quick and *DIRTY* hack.  should at least cache.
;; could now use the offset-cache that used to be in the view-text class..

(define position->offset
  (lambda (txt pos)
    (string-length (get-string txt '(0 . 0) pos))))

(define sexp-end-position
  (lambda (filename start)
    (let ([ip (open-input-file filename)])
      (file-position ip start)
      (read ip)
      (let ([end (file-position ip)])
        (close-input-port ip)
        end))))

(define sload
  (lambda (f e)
    (import |#system|)
    (let* ([ip (open-input-file f)]
           [sfd ($source-file-descriptor f ip)])
      (let loop ()
        (let ([x (read ip sfd)])
          (unless (eof-object? x)
            (if (procedure? x) (x) (e x))
            (loop)))))))
     
