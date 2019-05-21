;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Text
;;

(define-swl-class (<mark> txt-proc) (<tk-object>)
  ;; when the text widget creates a mark, it passes an evaluation
  ;; proc in so that when the text widget is destroyed, it can
  ;; also blitz the txt-proc.  this also avoids the need for
  ;; more public methods on the text widget for manipulating marks.
  ;; I may have to try this with canvas-items if it works ok here.
  ;;
  ;; if someone wants to be able to change from floating <--> fixed
  ;; they can easily create a mark of one type at the pos of the other
  ;;
  ;; need to watch out for marks that don't exist.  return #f...
  ;; actually, it appears that marks are not deleted when text around
  ;; them is deleted.
  ;;
  (ivars (txt-proc txt-proc) (handle (gensym)))
  (inherited)
  (inheritable handle txt-proc)
  (private)
  (protected)
  (public
    [print-rep (str)
     ;% \ret{a string}
      (send-base self print-rep
	(string-append
	  str
	  "x: " (number->string (send self get-x/char)) " y: "
	  (number->string (send self get-y/char))
	  ))]
    [scheme->tcl (op) (display handle op)]
    [destroy ()
     ;% \ret{unspecified}
     (txt-proc 'destroy-mark handle)
     (send-base self destroy)]
    [get-y/char ()
     ;* \ret{integer}
     ;* Returns the distance of this mark (in lines) from the beginning of
     ;* the text widget.
     (txt-proc 'get-y/char handle)]
    [get-x/char ()
     ;* \ret{integer}
     ;* Returns the offset of this mark (in character positions) from the start
     ;* the line.
     (txt-proc 'get-x/char handle)]
    [get-pos ()
     ;* \ret{see below}
     ;* Returns a pair containing the current line and character position
     ;* of the mark.
     (txt-proc 'get-pos handle)]
    [mark-name ()
     ;* \ret{THIS IS INTERNAL USE, YOU SHOULD NOT SEE THIS DOCUMENTATION}
     ;* Internal use.
     handle]
    [init (txt-proc)
     ;* \ret{unspecified}
     ;* Initializes the instance.
     (swl:widget-guardian self)]))

;; Custom generics for these guys so they apply equally well to pairs
;; currently used in a few places among the apps, but could be eliminated
;; with just a little effort

(define get-x/char
  (lambda (x)
    (if (pair? x)
        (car x)
        (send x get-x/char))))

(define get-y/char
  (lambda (x)
    (if (pair? x)
        (cdr x)
        (send x get-y/char))))

(define swl:get-pos
  (lambda (x)
    (if (pair? x)
        (values (car x) (cdr x))
        (send x get-pos))))


(swl:api-class (<fixed-mark> txt-proc) (<mark> txt-proc)
  ;% \swlapi{class}{<fixed-mark>}{(send \var{text-widget} fixed-mark \var{position})}
  ;* \ret{instance}
  ;;
  ;* Instances of the \scheme{<fixed-mark>} class are used to refer to positions
  ;* between two characters in a text widget.
  ;* A \scheme{<fixed-mark>} does not move when text is inserted at the position
  ;* of the mark but may move when text is inserted or deleted elsewhere
  ;* in the widget.
  ;*
  ;* The \scheme{fixed-mark} method of the \scheme{<text>} widget creates
  ;* a new mark at the given position (which is either an index or another
  ;* mark).
  (ivars)
  (inherited handle txt-proc)
  (inheritable)
  (private)
  (protected)
  (public))

(swl:api-class (<floating-mark> txt-proc) (<mark> txt-proc)
  ;% \swlapi{class}{<floating-mark>}{(send \var{text-widget} floating-mark \var{position})}
  ;* \ret{instance}
  ;;
  ;* Instances of the \scheme{<floating-mark>} class are used to refer to positions
  ;* between two characters in a text widget.
  ;* A \scheme{<floating-mark>} moves when text is inserted at the position
  ;* of the mark and may move when text is inserted or deleted elsewhere
  ;* in the widget.
  ;*
  ;* The \scheme{floating-mark} method of the \scheme{<text>} widget creates
  ;* a new mark at the given position (which is either an index or another
  ;* mark).
  ;*
  ;* \scheme{(send text-widget floating-mark position)}
  (ivars)
  (inherited handle txt-proc)
  (inheritable)
  (private)
  (protected)
  (public
    ))


(swl:api-class (<text> parent) (<proto-editable> parent 'text)
  ;; should probably abstract the
  ;;   (format (if (< disp 0) "~a~a" "~a+~a") blah disp)
  ;; junk
  ;;
  ;% \swlapi{class}{<text>}{(create <text> parent)}
  ;* \ret{instance}
  ;;
  ;* The text widget
  ;* displays multiple lines of text which may be edited.  Text within
  ;* the widget can be selected, and the selection can be exported as
  ;* the X selection if desired.  When a text widget has input focus
  ;* it displays an insertion cursor.  Text widgets can be scrolled
  ;* both horizontally and vertically to show contents that do not fit
  ;* within the window.
  ;*
  ;* Positions within a text widget are represented by pairs of
  ;* natural numbers \scheme{(column . line)}, the symbol \scheme{end} which
  ;* refers to the end of the text, 
  ;* by instances of the \scheme{<floating-mark>} and \scheme{<fixed-mark>} classes
  ;* returned by the \scheme{floating-mark} and \scheme{fixed-mark} methods
  ;* of the text widget, and by the symbolic marks \scheme{insert} (the position
  ;* of the insertion cursor), and \scheme{end} (the end of the buffer).
  ;*
  ;* In addition to text, the text widget can display embedded sub-windows
  ;* provided they have the same parent as the text widget, or have the
  ;* text widget as their parent.
  ;* 
  ;* The font, foreground and background color, 3-D relief, stippling
  ;* effects, and event bindings of individual ranges in the text
  ;* widget can be manipulated by a mechanism that has been temporarily
  ;* disabled.
  (ivars (existing-marks '()) (gc-safe-mkups '())
         (children '())
         (txt-proc (lambda (x y) (void)))
         ;; All <markup> event bindings are now implemented using Tcl/Tk's ".text tag bind" command
         ;; for a single tag named 'callback.  We used to do separate "tag bind"'s for each markup
         ;; and this made things like the HTML viewer horribly slow.  Note that this means
         ;; the bindings for distinct SWL <markup>s are in fact bound to the same text tag.
         ;; One consequence of this change is that Tcl/Tk does not produce mouse-enter or leave
         ;; events when the mouse moves from one SWL <markup> to another (because we're still
         ;; moving over the same Tk tag).  Therefore we must fabricate the enter and leave events
         ;; ourselves in some cases:
         ;;   - current-markup records the markup we've most recently entered.
         ;;   - in the mouse-motion callback handler we check to see whether we've moved
         ;;     from one logical SWL <markup> to another and if so:
         ;;       - send current-markup a mouse-leave message
         ;;       - send new markup a mouse-enter message
         ;;       - set current-markup to the new markup and motion event...
         ;;   ? coords in fabricated event may be slightly bogus?
         ;;   - on mouse-motion or mouse-enter we need to save away the list of current-markups
         ;;     so we can process them on mouse-leave (obviously we're not going to find the markup
         ;;     via markups-at if we're processing a leave event!)
         (current-markup #f)
         (current-markups '())
         (markup-mouse-enter-cbproc #f)
         (markup-mouse-leave-cbproc #f)
         (markup-keypress-cbproc #f)
         (markup-keyrelease-cbproc #f)
         (markup-mousepress-cbproc #f)
         (markup-mouserelease-cbproc #f)
         (markup-double-mousepress-cbproc #f)
         (markup-triple-mousepress-cbproc #f)
         (markup-double-mouserelease-cbproc #f)
         (markup-triple-mouserelease-cbproc #f)
         (markup-mousemotion-cbproc #f))
  (inherited handle sel-start-pos sel-end-pos sel-anchor select-mode)
  (inheritable handle)
  (private
    [with-selected-range (handler)
     (thread-critical-section
       (and (send self selection-exists?)
            (handler (make-index-for 'sel.first)
                     (make-index-for 'sel.last))))]
    [bang-dots! (s)
     ; ignore first & last char of s since Tcl text index can't have dot there
     (let bang-dots ([i (fx- (string-length s) 2)])
       (unless (fxzero? i)
         (when (char=? (string-ref s i) #\.)
           (string-set! s i #\space))
         (bang-dots (fx- i 1))))]
    [index->input-port (mark)
     (let ((s (swl:tcl-eval handle 'index mark)))
       (bang-dots! s)
       (open-input-string s))]
    [make-index-for (what)
     (make-index-from-tcl-string (swl:tcl-eval handle 'index what))]
    [make-index-from-tcl-string (string)
     (bang-dots! string)
     (let* ((p (open-input-string string)) [line (read p)] [char (read p)])
       (cons char (fx- line 1)))]
    [clone-index-for (pos disp qual)
     ;; already error checked
     ;; not sure what I want to do about the 'insert and 'end marks yet
     ;; so for now I'll permit them here
     (let ([new-pos
            (swl:tcl-eval handle 'index
              ;; would be nicer if we could just blast chars to the tcl-port
              (if disp (list pos #\+ disp qual) (list pos qual)))])
       (make-index-from-tcl-string new-pos))]
    [real-make-mark (pos type)
     (case type
       [(floating)
        (let ([mk (create <floating-mark> txt-proc)])
          (swl:tcl-eval handle 'mark 'set mk pos)
          mk)]
       [(fixed)
        (let ([mk (create <fixed-mark> txt-proc)])
          (swl:tcl-eval handle 'mark 'set mk pos)
          (swl:tcl-eval handle 'mark 'gravity mk 'left)
          mk)]) ]
    [protect (tag-name markup)
     (set! gc-safe-mkups (cons (cons tag-name markup) gc-safe-mkups))]
    [unprotect (tag-name)
     ;; potential inefficiency
     (set! gc-safe-mkups (remassq tag-name gc-safe-mkups))]
    [remassq (pname ls)
     (let loop ([pname pname] [ls ls])
       (cond
         [(null? ls) '()]
         [(eq? pname (caar ls)) (cdr ls)]
         [else (cons (car ls) (loop pname (cdr ls)))]))]
    [check-text-index (index who)
      (if (or (eq? index 'insert)
	      (eq? index 'end)
	      (and (pair? index)
		(let ((line (cdr index)))
		  (and (fixnum? line)
		    (fxnonnegative? line)
		    (let ((char (car index)))
		      (and (fixnum? char) (fxnonnegative? char))))))
	      (and (instance? index)
		(send index isa? <mark>)
		(mark-exists? index)))
	index
	(assertion-violationf who "invalid index ~s (must be 'insert, 'end, pair, or instance of <mark>)" index))]
    [real-search (pattern index1 index2 options)
     (swl:safety-check
       (unless (list? options)
         (assertion-violationf 'search "~s is not a proper options list" options))
       (check-text-index index1 'search)
       (when index2 (check-text-index index2 'search)))
     (let loop ([in options]
                [rest
                 (if index2
                     `(-- ,pattern ,index1 ,index2)
                     `(-- ,pattern ,index1))])
       (cond
         [(null? in)
          (let ([result (apply swl:tcl-eval handle 'search rest)])
            (and (not (string=? result ""))
                 (make-index-from-tcl-string result)))]
         [(assq (car in)
                '((forward . -forward)
                  (backward . -backward)
                  (regexp . -regexp)
                  (case-insensitive . -nocase)))
          =>
          (lambda (hit) (loop (cdr in) (cons (cdr hit) rest)))]
         [else (assertion-violationf 'search "invalid option ~s" (car in))]))]
    [prim-insert (index what markup)
     (if markup
         (begin
           (swl:safety-check
             (unless (isa-markup? markup)
               (assertion-violationf 'insert "~s not an instance of markup" markup)))
           (send markup ensure-configured-for self)
           (swl:tcl-eval handle 'insert index what markup))
         (swl:tcl-eval handle 'insert index what))]
    [real-insert (index what markup)
     (cond
       ((string? what) (prim-insert index what markup))
       ((char? what) (prim-insert index (string what) markup))
       (else (assertion-violationf 'insert "~s is not a string or character" what)))]
    [real-get-char (index)
     (swl:safety-check (check-text-index index 'get-char))
     (let ((str (swl:tcl-eval handle 'get index)))
       (if (fxzero? (string-length str))
	   #f
	   (string-ref str 0)))]
    [mark-exists? (mark)
     ;* returns true if the given mark exists in this text widget.
     ;* ============ Hey, is this thing actually used?  is this just
     ;* leftover code that needs to be culled.
     ;; if not, we need to check to see that mark truly isa? <mark>
     (not (string=? "" (swl:tcl-eval handle 'index mark)))]
    [compare-index (index1 op index2 who)
     (swl:safety-check
       (check-text-index index1 who)
       (check-text-index index2 who))
     (fxpositive?
       (string->number (swl:tcl-eval handle 'compare index1 op index2)))])
  (protected
    [tcl-name () 'text])
  (public
    [add-offset (pos disp)
     ;* \ret{see below}
     ;* Returns a new pair scheme{(column . line)}  that refers
     ;* to a new position whose character offset relative to \var{pos} is
     ;* given by \var{disp}.  Negative offsets are permitted.  \var{pos} is
     ;* either a pair or an instance of \scheme{<fixed-mark>} or \scheme{<floating-mark>}.
     (swl:safety-check
       (check-text-index pos 'add-offset)
       (unless (fixnum? disp)
         (assertion-violationf 'add-offset "invalid displacement ~s" disp)))
     (clone-index-for pos disp 'c)]
    [word-start (pos)
     ;* \ret{see below}
     ;* Returns a new pair scheme{(column . line)}  that refers
     ;* to the position of the first character of the word at position \var{pos}.
     ;* \var{pos} is
     ;* either a pair or an instance of \scheme{<fixed-mark>} or \scheme{<floating-mark>}.
     (swl:safety-check (check-text-index pos 'word-start))
     (clone-index-for pos #f 'wordstart)]
    [word-end (pos)
     ;* \ret{see below}
     ;* Returns a new pair scheme{(column . line)}  that refers
     ;* to the position of the last character of the word at position \var{pos}.
     ;* \var{pos} is
     ;* either a pair or an instance of \scheme{<fixed-mark>} or \scheme{<floating-mark>}.
     (swl:safety-check (check-text-index pos 'word-end))
     (clone-index-for pos #f 'wordend)]
    [line-start (pos)
     ;* \ret{see below}
     ;* Returns a new pair scheme{(column . line)}  that refers
     ;* to the beginning of the line containing position
     ;* \var{pos}.
     ;* \var{pos} is
     ;* either a pair or an instance of \scheme{<fixed-mark>} or \scheme{<floating-mark>}.
     (swl:safety-check (check-text-index pos 'line-start))
     (clone-index-for pos #f 'linestart)]
    [line-end (pos)
     ;* \ret{see below}
     ;* Returns a new pair scheme{(column . line)}  that refers
     ;* to the end of the line containing position
     ;* \var{pos}.
     ;* \var{pos} is
     ;* either a pair or an instance of \scheme{<fixed-mark>} or \scheme{<floating-mark>}.
     (swl:safety-check (check-text-index pos 'line-end))
     (clone-index-for pos #f 'lineend)]
    [floating-mark (where)
     ;; gack, what a bad description.
     ;* \ret{an instance of \scheme{<floating-mark>}}
     ;* Returns a floating mark for the position described by \var{where}.
     ;* A floating mark moves when text is inserted or deleted before its
     ;* current position and moves when text is inserted at its current
     ;* position.  For example, the insertion cursor behaves as
     ;* a floating mark.
     ;; user's fault if they didn't do this in a critical section and needed to.
     (swl:safety-check (check-text-index where 'floating-mark))
     (real-make-mark where 'floating)]
    [fixed-mark (where)
     ;* \ret{an instance of \scheme{<fixed-mark>}}
     ;* Returns a fixed mark for the position described by \var{where}.
     ;* A fixed mark moves only when text is inserted or deleted somewhere
     ;* before its current position.  It remains fixed when text is inserted
     ;* at its current position.
     ;; user's fault if they didn't do this in a critical section and needed to.
     (swl:safety-check (check-text-index where 'fixed-mark))
     (real-make-mark where 'fixed)]
    [key-press (key modifiers)
     ;% \ret{unspecified}
     (event-case ((key= key) (modifier= modifiers))
       (([up] [control #\p] [kp_up]) (move-line self -1))
       (([down] [control #\n] [kp_down]) (move-line self 1))
       (([control #\o]) (cursor-eol self) (insert self #\newline))
       (([#\newline] [#\return] [control #\j] [control #\m] [kp_enter])
        (insert self #\newline))
       (([prior] [kp_page_up]) (vscroll self -1 'pages))
       (([next] [kp_page_down]) (vscroll self 1 'pages))
       (([shift #\tab]) (send-base self key-press key modifiers))
       (([#\tab] [kp_tab]) (insert self #\tab)))
     ;; Here we always send to base class (unless #\tab) since we're not
     ;; redefining any base class behaviors, only extending them (except #\tab)
     (unless (eq? key #\tab)
       (send-base self key-press key modifiers))]
    [insert (what)
     ;* \ret{unspecified}
     ;* Inserts the given character, string, or widget
     ;* at the current cursor position, and ensures that the
     ;* cursor is visible on-screen.
     (send self insert-at 'insert what)
     (make-visible self 'insert)]
;    [insert (what markup)
;     ;* inserts given character, string, or widget at current cursor position
;     ;* with the given markup.
;     (real-insert 'insert what markup)
;     (make-visible self 'insert)]
    [insert-at (index what)
     ;* \ret{unspecified}
     ;* Inserts given character, string, or widget just before index.
     ;* (no attempt is made to ensure that the newly inserted text is visible)
     (swl:safety-check (check-text-index index 'insert-at))
     (real-insert index what #f)
     (void)]
;    [insert-at (index what markup)
;     ;* Inserts the given character, string, or widget just before index.
;     ;*
;     ;* the optional markup must be an instance of <markup>
;     ;* 
;     (real-insert index what markup)]
    [insert-widget-at (index widget)
     ;* \ret{unspecified}
     ;* Quick and dirty first stab at permitting embedded windows in a
     ;* text widget.  No idea yet how this will affect the event-case stuff.
     (swl:safety-check
       (check-text-index index 'insert-widget-at)
       (unless (isa? widget <tk-widget>)
         (assertion-violationf 'insert-widget-at "~s is not a widget" widget))
;;;johnz       (unless (memq widget children)
;;;         (assertion-violationf 'insert-widget-at
;;;           "~s must be a child of the text widget ~s" widget self))
       )
     (swl:tcl-eval handle 'window 'create index '-window widget)
     (void)]

    [get-selected-range ()
     ;* \ret{see below}
     ;* Returns a pair of the start and end points of the selected
     ;* region or \scheme{#f} if nothing is selected.
     (with-selected-range cons)]

    [set-hpad! (val)
     ;* \ret{unspecified}
     ;* Specifies in pixels the extra horizontal distance to add around
     ;* the widget.
     (set-tk-option '-padx val swl:distance-unit? 'set-hpad!)]
    [get-hpad ()
     ;* \ret{see below}
     ;* Returns in pixels the extra horizontal distance to add around
     ;* the widget.
     (get-tk-option '-padx string->number 'get-hpad)]
    [set-vpad! (val)
     ;* \ret{unspecified}
     ;* Specifies in pixels the extra vertical distance to add around
     ;* the widget.
     (set-tk-option '-pady val swl:distance-unit? 'set-vpad!)]
    [get-vpad ()
     ;* \ret{see below}
     ;* Returns in pixels the extra vertical distance to add around
     ;* the widget.
     (get-tk-option '-pady string->number 'get-vpad)]
    [set-grid! (val)
     ;* \ret{unspecified}
     ;* \var{val} is a boolean determining whether or not the 
     ;* text widget should strive to keep its dimensions in even
     ;* multiples of the dimensions of a character in its current font.
     ;* The default is \scheme{#f}.
     (set-tk-option '-setgrid val boolean? 'set-grid!)]
    [get-grid ()
     ;* \ret{boolean}
     ;* Returns a boolean indicating whether or not the 
     ;* text widget should strive to keep its dimensions in even
     ;* multiples of the dimensions of a character in its current font.
     (get-tk-option '-setgrid tk->boolean 'get-grid)]
    [set-above-paragraph-space! (val)
     ;* \ret{unspecified}
     ;* Sets in pixels the amount of extra space to insert above each
     ;* paragraph in the text.
     (set-tk-option '-spacing1 val swl:distance-unit? 'set-above-paragraph-space!)]
    [get-above-paragraph-space ()
     ;* \ret{see below}
     ;* Returns in pixels the amount of extra space to insert above each
     ;* paragraph in the text.
     (get-tk-option '-spacing1 string->number 'get-above-paragraph-space)]
    [set-below-paragraph-space! (val)
     ;* \ret{unspecified}
     ;* Sets in pixels the amount of extra space to insert below each
     ;* paragraph in the text.
     (set-tk-option '-spacing3 val swl:distance-unit? 'set-below-paragraph-space!)]
    [get-below-paragraph-space ()
     ;* \ret{see below}
     ;* Returns in pixels the amount of extra space to insert below each
     ;* paragraph in the text.
     (get-tk-option '-spacing3 string->number 'get-below-paragraph-space)]
    [set-line-space! (val)
     ;* \ret{unspecified}
     ;* Sets in pixels the amount of extra space to insert between each line
     ;* in the text.
     (set-tk-option '-spacing2 val swl:distance-unit? 'set-line-space!)]
    [get-line-space ()
     ;* \ret{see below}
     ;* Returns in pixels the amount of extra space to insert between each line
     ;* in the text.
     (get-tk-option '-spacing2 string->number 'get-line-space)]
    [set-tabs! (val)
     ;* \ret{unspecified}
     ;* This sets the default tab stops for this text widget to
     ;* those supplied by the list of \scheme{<tab-stop>} instances.
     ;* Four kinds of tabs are supported:  left aligned, right aligned,
     ;* centered, and numeric.
     ;* (See the \scheme{tab-stops} macro.)
     ;* Note that markups may be applied to alter tab stops within
     ;* a particular region.
     (set-tk-option '-tabs val swl:tabs? 'set-tabs!)]
    [get-tabs ()
     ;* \ret{see below}
     ;* Returns a list of the default tab stops in effect for this text widget.
     (get-tk-option '-tabs tk->tabs 'get-tabs)]
    [set-wrap! (val)
     ;* \ret{unspecified}
     ;* Determines how lines that are too long to be displayed will be
     ;* wrapped.  \var{val} can be either \scheme{char} for wrapping lines at
     ;* character boundaries, \scheme{word} for wrapping lines a word boundaries,
     ;* or \scheme{none} which prevents lines from being wrapped.
     (set-tk-option '-wrap val
       (lambda (x) (memq x '(word char none))) 'set-wrap!)]
    [get-wrap ()
     ;* \ret{see below}
     ;* Returns a symbol indicating how long lines are wrapped.
     (get-tk-option '-wrap string->symbol 'get-wrap)]
;; duh, these could be abstracted
    [delete-eol ()
     ;* \ret{unspecified}
     ;* Deletes characters from current cursor position to the end of the line.
     (send self set-saved-text! (swl:tcl-eval handle 'get 'insert "insert lineend"))
     (swl:tcl-eval handle 'delete 'insert "insert lineend")
     (make-visible self 'insert)]
    [delete-all ()
     ;* \ret{unspecified}
     ;* Deletes all characters in the widget.
     (send self delete '(0 . 0) 'end)
     (void)]
    [cursor-eol ()
     ;* \ret{unspecified}
     ;* Moves the insertion cursor to the end of the line.
     (send self set-cursor-pos! (make-index-for "insert lineend"))]
    [cursor-bol ()
     ;* \ret{unspecified}
     ;* Moves the insertion cursor to the beginning of the line.
     (send self set-cursor-pos! (make-index-for "insert linestart"))]
;; duh, these could be abstracted
    [cursor-eol? ()
     ;* \ret{boolean}
     ;* Returns true if the insertion cursor is at the end of a line.
     (tk->boolean (swl:tcl-eval handle 'compare 'insert '== "insert lineend"))]
    [cursor-bol? ()
     ;* \ret{boolean}
     ;* Returns true if the insertion cursor is at the beginning of a line.
     (tk->boolean (swl:tcl-eval handle 'compare 'insert '== "insert linestart"))]
    [cursor-eof? ()
     ;* \ret{boolean}
     ;* Returns true if the insertion cursor is at the end of the text buffer.
;; NOTE: hacked to compensate for Tk 4.0 bug? that insert != end ever
     (tk->boolean (swl:tcl-eval handle 'compare 'insert+1c '== 'end))]
    [cursor-bof? ()
     ;* \ret{boolean}
     ;* Returns true if the insertion cursor is at the beginning
     ;* of the text buffer.
     (tk->boolean (swl:tcl-eval handle 'compare 'insert '== 1.0))]
    [move-char (offset)
     ;* \ret{unspecified}
;; could be abstracted w/ move-line  "inlining" for now
     ;* Advances the cursor \var{offset} characters, going to the next line
     ;* if there are not enough characters to satisfy offset on the
     ;* current line.  Negative offsets are allowed.
     (swl:safety-check
       (unless (fixnum? offset) (assertion-violationf 'move-char "offset must be an integer")))
     (send self set-cursor-pos!
       (make-index-for
         (parameterize ((print-radix 10))
           (format (if (< offset 0) "insert~ac" "insert+~ac") offset))))]
    [move-line (offset)
     ;* \ret{unspecified}
;; could be abstracted w/ move-char
     ;* Advances the cursor \var{offset} lines.  The resulting cursor
     ;* position is
     ;* the minimum of the current character (col) position and the
     ;* length of
     ;* the line we end up on.
     ;* Negative offsets are allowed.
     (swl:safety-check
       (unless (fixnum? offset) (assertion-violationf 'move-line "offset must be an integer")))
     (send self set-cursor-pos!
       (make-index-for
         (parameterize ((print-radix 10))
           (format (if (< offset 0) "insert~al" "insert+~al") offset))))]
    [move-word (offset)
     ;* \ret{unspecified}
     ;* Intended to advance the cursor \var{offset} words.
     ;* A word is currently a sequence of letters, digits or underscores.
;; YO! SHOULD BE ABLE TO IMPROVE THIS A LOT USING REGEXP SEARCH
;; MIGHT BE SLOWER THOUGH DUE TO OVERHEAD OF Tcl regexp compile.
     ;* Negative offsets move to the start of the current word
     ;* (to left of cursor).
     ;* Positive offsets move to the end of the current word (to right
     ;* of cursor).
     (swl:safety-check
       (unless (fixnum? offset) (assertion-violationf 'move-word "offset must be an integer")))
     (let mw ((offset offset))
       (unless (zero? offset)
         (let ((dir (if (< offset 0) -1 1)))
           ;; to scan to the next word if we're already at the start of
           ;; a word...
           (when (< dir 0) (send self move-char dir))
           (let loop ((ch (send self get-char)))
             (unless (or (and (> dir 0) (send self cursor-eof?))
                         (and (< dir 0) (send self cursor-bof?))
                         (and (char? ch)
                              (or (char-alphabetic? ch)
                                  (char-numeric? ch)
                                  (char=? ch #\_))));; Tk-ism
                (send self move-char dir)
                (loop (send self get-char))))
; redo this w/ newly added index stuff
           (send self set-cursor-pos!
             (make-index-for
               (format "insert ~a" (if (< offset 0) 'wordstart 'wordend))))
           (mw (+ offset (- dir))))))]
    [pos<? (index1)
     ;* \ret{boolean}
     ;* Returns \scheme{#t} if position of index1 is less than the position of the
     ;* \var{insertion} cursor,
     ;* else returns \scheme{#f}.
     (compare-index index1 '< 'insert 'pos<?)]
    [pos<? (index1 index2)
     ;* \ret{boolean}
     ;* Returns \scheme{#t} if position of index1 is less than index2,
     ;* else returns \scheme{#f}.
     (compare-index index1 '< index2 'pos<?)]
    [pos<=? (index1)
     ;* \ret{boolean}
     ;* Returns \scheme{#t} if position of index1 is less than or equal to
     ;* the position of the
     ;* \var{insertion} cursor,
     ;* else returns \scheme{#f}.
     (compare-index index1 '<= 'insert 'pos<=?)]
    [pos<=? (index1 index2)
     ;* \ret{boolean}
     ;* Returns \scheme{#t} if position of index1 is less than or equal to
     ;* index2,
     ;* else returns \scheme{#f}.
     (compare-index index1 '<= index2 'pos<=?)]
    [pos=? (index1)
     ;* \ret{boolean}
     ;* Returns \scheme{#t} if position of index1 is equal to the position of the
     ;* \var{insertion} cursor,
     ;* else returns \scheme{#f}.
     (compare-index index1 '== 'insert 'pos=?)]
    [pos=? (index1 index2)
     ;* \ret{boolean}
     ;* Returns \scheme{#t} if position of index1 is equal to index2,
     ;* else returns \scheme{#f}.
     (compare-index index1 '== index2 'pos=?)]
    [pos>? (index1)
     ;* \ret{boolean}
     ;* Returns \scheme{#t} if position of index1 is greater than the position of the
     ;* \var{insertion} cursor,
     ;* else returns \scheme{#f}.
     (compare-index index1 '> 'insert 'pos>?)]
    [pos>? (index1 index2)
     ;* \ret{boolean}
     ;* Returns \scheme{#t} if position of index1 is greater than index2,
     ;* else returns \scheme{#f}.
     (compare-index index1 '> index2 'pos>?)]
    [pos>=? (index1)
     ;* \ret{boolean}
     ;* Returns \scheme{#t} if position of index1 is greater than or equal
     ;* to the position of the
     ;* \var{insertion} cursor,
     ;* else returns \scheme{#f}.
     (compare-index index1 '>= 'insert 'pos>=?)]
    [pos>=? (index1 index2)
     ;* \ret{boolean}
     ;* Returns \scheme{#t} if position of index1 is greater than or equal
     ;* to index2,
     ;* else returns \scheme{#f}.
     (compare-index index1 '>= index2 'pos>=?)]
    [pos<=? (index1 index index2)
     ;* \ret{boolean}
     ;* Returns \scheme{#t} if position of index is between index1
     ;* and index2 (inclusive), else returns \scheme{#f}.
     (swl:safety-check
       (check-text-index index 'pos<=?)
       (check-text-index index1 'pos<=?)
       (check-text-index index2 'pos<=?))
     (and
       (fxpositive?
         (string->number (swl:tcl-eval handle 'compare index1 '<= index)))
       (fxpositive?
         (string->number (swl:tcl-eval handle 'compare index '<= index2))))]
    [set-cursor-pos! (index)
     ;* \ret{unspecified}
     ;* Sets the insertion cursor position within the widget
     ;* Note:  the cursor position can't be set outside the
     ;* bounds of the text in the widget.  Attempts to do so
     ;* place the cursor at the point in the text closest to
     ;* the target index.  For example, setting cursor position to
     ;* line 7 char 4000 when line 7 doesn't have 4000 chars will
     ;* put the cursor at line 8 char 0.
     (swl:tcl-eval handle 'mark 'set 'insert
       (check-text-index index 'set-cursor-pos!))
     (make-visible self 'insert)]
    [get-cursor-pos ()
     ;* \ret{integer}
     ;* Returns the position of insertion cursor within the widget
     ;* as a pair of character position and line number.
     (make-index-for 'insert)]
    ;; There was a "get-current-pos" but that can easily be gotten by xy->index
    [delete (index1)
     ;* \ret{unspecified}
     ;* Deletes the character at index1.
     (swl:tcl-eval handle 'delete (check-text-index index1 'delete))
     (void)]
    [delete (index1 index2)
     ;* \ret{unspecified}
     ;* Deletes characters in the range specified by index1 and
     ;* index2 (inclusive).
     ;; may be a bug w/ index2 < index1
     (swl:tcl-eval handle 'delete
        (check-text-index index1 'delete)
        (check-text-index index2 'delete))
     (void)]
    [delete-char (disp)
     ;* \ret{unspecified}
     ;* Deletes the characters between the cursor position and the given
     ;* displacement.
     (swl:safety-check
       (unless (fixnum? disp)
          (assertion-violationf 'delete-char "invalid displacement ~s" disp)))
     (if (fx= disp 1)
         (swl:tcl-eval handle 'delete 'insert)
         (let* ([old (get-cursor-pos self)]
                [new (add-offset self old disp)])
           (swl:tcl-eval handle 'mark 'set 'insert new)
           (if (fxnegative? disp)
               (delete self new old)
               (delete self old new))))
     (make-visible self 'insert)]
    [get-char ()
     ;* \ret{see below}
     ;* Returns the character at cursor position or \scheme{#f} if none but
     ;* does not advance the cursor.
     (real-get-char 'insert)]
    [get-char (index)
     ;* \ret{see below}
     ;* Returns the character at index, \scheme{#f} if none.
     (real-get-char index)]
    [get-string (index1 index2)
     ;* \ret{see below}
     ;* Returns the string defined by the range \scheme{index1} \scheme{index2}.
     (swl:safety-check
       (check-text-index index1 'get-string)
       (check-text-index index2 'get-string))
     (swl:tcl-eval handle 'get index1 index2)]
    [move-to-left (index)
     ;* \ret{unspecified}
; this needs to be more like move-to-top in that there the line component is
; all that matters, and here the char component should be all that matters
     ;* scrolls the view in the widget horizontally so that the character
     ;* at \var{index}
     ;* is displayed at the left edge of the window.
(assertion-violationf 'move-to-left "unimplemented for text widget at present")
     (swl:safety-check (check-text-index index 'move-to-left))
; basically need to use dlineinfo to find the y coord of this line
; then use xy->index to figure out what the first index visible on
; that line is and scroll the appropriate number of units
; only problem is if that line isn't currently visible (since dlineinfo
; gives back "")
     (swl:tcl-eval handle 'xview index)
     (void)]
    [move-to-top (index)
     ;* \ret{unspecified}
     ;* scrolls the view in the widget vertically so that the line containing
     ;* \var{index} is displayed at the top edge of the window.
     (swl:safety-check (check-text-index index 'move-to-top))
     (swl:tcl-eval handle 'yview index)
     (void)]
; if  "handle yview index" weren't supported we could do it like this:
;   [move-to-top (pos)
;    ;* Scrolls the line indicated by pos to the top of the widget.
;    (swl:safety-check (check-text-index pos 'move-to-top))
;    (let ((lines (send (make-index-for 'end) get-y/char)) (line (get-char-y pos)))
;      (swl:tcl-eval handle 'yview 'moveto
;         (if (zero? lines) 0 (float (/ line lines))))
;      (void))]
    [clear-selection ()
     ;* \ret{unspecified}
     ;* clears the selection if anything in this widget is selected.
     (set! sel-anchor #f)
     (set! sel-start-pos #f)
     (set! sel-end-pos #f)
     (swl:tcl-eval handle 'tag 'remove 'sel '1.0 'end)
     (void)]
    [selection-exists? ()
     ;* \ret{boolean}
     ;* returns true if anything in the entry is selected.
     ;; probably inefficient, could be better given native graphics
     (not (string=? "" (swl:tcl-eval handle 'tag 'ranges 'sel)))]
    [select-range (start end)
     ;* \ret{unspecified}
     ;* selects only those characters in the text between \var{start} and
     ;* \var{end}.
     (swl:safety-check
       (check-text-index start 'select-range)
       (check-text-index end 'select-range))
     (if (send self pos>? start end)
         (clear-selection self)
         (begin
           (swl:tcl-eval handle 'tag 'add 'sel start end)
           (swl:tcl-eval handle 'tag 'remove 'sel '|0.0| start)  ; why symbol? (OW)
           (swl:tcl-eval handle 'tag 'remove 'sel end 'end)
           (unless sel-anchor (set! sel-anchor (get-cursor-pos self)))
           (set! sel-start-pos start)
           (set! sel-end-pos end)))]
    [delete-selection-at-cursor ()
     ;* \ret{unspecified}
     ;* Deletes the selected text, if any, surrounding the cursor.
     (with-selected-range
       (lambda (start end)
         (let ((cursor (send self get-cursor-pos)))
           (let ((cx (car cursor)) (cy (cdr cursor)))
             (when (and
                     (let ([sy (cdr start)])
                       (or (fx< sy cy)
                           (and (fx= sy cy) (fx<= (car start) cx))))
                     (let ([ey (cdr end)])
                       (or (fx> ey cy)
                           (and (fx= ey cy) (fx>= (car end) cx)))))
         (send self delete start end)
         (send self clear-selection))))))
     (void)]
    [search (pattern index options)
     ;* \ret{see below}
     ;* Searches text for the given \var{pattern} starting at \var{index}.
     ;* \var{options} is a possibly empty list of options modifying the
     ;* search:  \scheme{regexp}, \scheme{forward}, \scheme{backward}, \scheme{case-insensitive}.
     ;* Returns the index of the position where \var{pattern} was found,
     ;* otherwise \scheme{#f}.
     (real-search pattern index #f options)]
    [search (pattern index1 index2 options)
     ;* \ret{see below}
     ;* Searches text for the given \var{pattern} between \var{index1} and
     ;* \var{index2}.
     ;* \var{options} is a possibly empty list of options modifying the
     ;* search:  \scheme{regexp}, \scheme{forward}, \scheme{backward}, \scheme{case-insensitive}.
     ;* Returns the index of the position where \var{pattern} was found,
     ;* otherwise \scheme{#f}.
     (real-search pattern index1 index2 options)]
    [xy->index (x y)
     ;* \ret{see below}
     ;* Returns an index corresponding to the given \var{x} and \var{y}
     ;* coordinate.
     (swl:safety-check
       (unless (and (fixnum? x) (fixnum? y))
         (assertion-violationf 'xy->index "x and y coordinates not integers: ~s ~s" x y)))
     (make-index-for
       (string-append
         "@"
         (number->string x 10)  ; avoid need for parameterize of print-radix
         ","
         (number->string y 10)))]
    [bounding-box (pos)
     ;* \ret{see below}
     ;* Returns a list of four values describing the bounding box of the character
     ;* at the position \var{pos}, or \scheme{#f} if the character is not visible on screen.
     ;*  The first two values give the x- and y-
     ;* coordinates of the top left corner of the area occupied by the
     ;* character.  The last two values describe the width and height
     ;* of that area.
     (swl:safety-check (check-text-index pos 'bounding-box))
     (let ((x (swl:tcl->scheme (swl:tcl-eval handle 'bbox pos))))
       (and (not (null? x)) x))]
    [make-visible (index)
     ;* \ret{unspecified}
     ;* Ensures that the text at the given index is visible on screen
     (swl:safety-check (check-text-index index 'make-visible))
     (swl:tcl-eval handle 'see index)
     (void)]
    [markup-raise (mkup)
     ;* \ret{unspecified}
     ;* Raises the given markup to the top of the markup stacking order.
     ;* Stacking order determines which characteristics of markups obscure
     ;* those of other markups covering the same range.
     (swl:safety-check
       (unless (isa-markup? mkup)
         (assertion-violationf 'markup-raise "~s is not a <markup>" mkup)))
; dumb way to verify that the mkup exists for this text
     (send mkup ensure-configured-for self)
     (swl:tcl-eval handle 'tag 'raise mkup)
     (void)]
    [markup-lower (mkup)
     ;* \ret{unspecified}
     ;* Lowers the given markup to the bottom of the markup stacking order.
     ;* Stacking order determines which characteristics of markups obscure
     ;* those of other markups covering the same range.
     (swl:safety-check
       (unless (isa-markup? mkup)
         (assertion-violationf 'markup-lower "~s is not a <markup>" mkup)))
; dumb way to verify that the mkup exists for this text
     (send mkup ensure-configured-for self)
     (swl:tcl-eval handle 'tag 'lower mkup)
     (void)]
    [markups-at (pos)
     ;* \ret{see below}
; should support form where pos isn't supplied...    (see list-all-markups below)
     ;* returns a list of the markups at a given position
     ;; we screen out things that looks like sel (or begin w/ s)
     ;; since markups have handles that look like #n where n is
     ;; a hex number
     (swl:safety-check (check-text-index pos 'markups-at))
     (let ([s (swl:tcl-eval handle 'tag 'names pos)])
       (or (and (not (string=? s ""))
                (not (string=? s "sel"))
                (not (string=? s "callback"))
                (let ([len (string-length s)])
                  (let loop ([i (fx- len 1)] [last len])
                    (if (fx< i 0)
                        '()
                        (case (string-ref s i)
                          [(#\#)
                           (cons (swl:lookup-widget (string->symbol (substring s i last)))
                                 (loop (fx- i 1) i))]
                          [(#\k)
                           (let ([next (fx- i (string-length "callback"))])
                             (loop next next))]
                          [(#\space) (loop (fx- i 1) i)]
                          [else (loop (fx- i 1) last)])))))
           '()))]
    [list-all-markups ()
     ;* \ret{see below}
     ;* returns a list of all markups applied in the text widget
     ;; we screen out things that looks like sel (or begin w/ s)
     ;; since markups have handles that look like #n where n is
     ;; a hex number
     (map cdr gc-safe-mkups)]

;; THIS ISN'T REALLY PUBLIC  (at least not API)

    [tag-ranges (tag-name)
     (and handle
          (let ((result (swl:tcl-eval handle 'tag 'ranges tag-name)))
            (and (not (string=? result ""))
                 (begin
; certainly could write a custom reader for this, but probably not worth it
                   (bang-dots! result)
                   (let ((ip (open-input-string result)))
;; really need to abstract this again with make-index-from-tcl-string above
;; but that guy needs to change to take a port...
                     (let loop ((y (read ip)))
                       (if (eof-object? y)
                           '()
                           (let ((x (read ip)))
                             (cons (cons x (fx- y 1)) (loop (read ip)))))))))))]
    [tag-range (tag-name op index1 index2 callback?)
     (and handle
          (swl:safety-check
            (let ([who (if (eq? op 'add) 'apply-markup 'remove-markup)])
              (check-text-index index1 who)
              (check-text-index index2 who)))
          (begin
            (swl:tcl-eval handle 'tag op tag-name index1 index2)
            (when callback? (swl:tcl-eval handle 'tag op 'callback index1 index2))
            (when (and (eq? op 'remove)
                       (string=? "" (swl:tcl-eval handle 'tag 'ranges tag-name)))
              (unprotect tag-name))
            #t))]
    [tag-bind (tag-name event callback)
     ;; hack of returning callback is for convenience in the markup.ss
     ;; (this is another I've got 5 mins to get this wokring hack)
     (when handle
       (swl:tcl-eval handle 'tag 'bind tag-name event callback)
       callback)]
    [tag-config (tag-name options markup)
     (and handle
          (begin (apply swl:tcl-eval handle 'tag 'config tag-name options)
                 ;; protect markup from collection
                 (protect tag-name markup)
                 #t))]
    [init (parent)
     ;* \ret{unspecified}
     ;* Initializes the instance.
     (send-base self init parent)
     ;; These are just ivars but we have to whack them in here because this
     ;; lame object system doesn't make the method names available in the
     ;; scope in which the ivars are bound.
     (set! markup-mouse-enter-cbproc
       (swl:callback-lambda (x y state)
         (let ([markups (send self markups-at (send self xy->index x y))])
           (set! current-markups markups)
           (set! current-markup
             (ormap
               (lambda (markup)
                 (and (send markup mouse-enter self x y state) markup))
               markups)))))
     (set! markup-mouse-leave-cbproc
       (swl:callback-lambda (x y state)
         (let ([markups current-markups])
           (set! current-markup #f)
           (set! current-markups '())
           (ormap
             (lambda (markup) (send markup mouse-leave self x y state))
             markups))))
     (set! markup-mousemotion-cbproc
       (swl:callback-lambda (x y state)
         (let ([markups (send self markups-at (send self xy->index x y))])
           (set! current-markups markups)
           (if (or (not current-markup) (memq current-markup markups))
               (set! current-markup
                 (or (ormap
                       (lambda (markup)
                         (and (send markup mouse-motion self x y state)
                              markup))
                       markups)
                     current-markup))
               (begin
                 (when current-markup
                   (send current-markup mouse-leave self x y state))
                 (set! current-markup
                   (or (ormap
                         (lambda (markup) (and (send markup mouse-enter self x y state) markup))
                         markups)
                       (ormap
                         (lambda (markup) (and (send markup mouse-motion self x y state) markup))
                         markups))))))))
     (set! markup-keypress-cbproc
       (swl:callback-lambda (keysym-decimal state)
         (ormap
           (lambda (markup)
             (send markup key-press self (decimal->key keysym-decimal) state))
           (send self markups-at (send self get-cursor-pos)))))
     (set! markup-keyrelease-cbproc
       (swl:callback-lambda (keysym-decimal state)
         (ormap
           (lambda (markup)
             (send markup key-release
                self (decimal->key keysym-decimal) state))
           (send self markups-at (send self get-cursor-pos)))))
     (set! markup-mousepress-cbproc
       (swl:callback-lambda (x y button-number state)
         (ormap
           (lambda (markup)
             (send markup mouse-press self x y
               (fxlogor (fxsll 1 (fx+ 7 button-number)) state)))
           (send self markups-at (send self xy->index x y)))))
     (set! markup-mouserelease-cbproc
       (swl:callback-lambda (x y state)
         ;; here button-number should already be implied in the state field, though
         ;; we may be missing exactly the button that was released which would be
         ;; bogus
         (ormap
           (lambda (markup)
             (send markup mouse-release self x y state))
           (send self markups-at (send self xy->index x y)))))
     (let ([make-multipress-cbproc
            (lambda (bit)
              (swl:callback-lambda (x y button-number state)
                (ormap
                  (lambda (markup)
                    (send markup mouse-press self x y
                      (fxlogor bit (fxlogor (fxsll 1 (fx+ 7 button-number)) state))))
                  (send self markups-at (send self xy->index x y)))))]
           [make-multirelease-cbproc
            (lambda (bit)
              (swl:callback-lambda (x y state)
                (ormap
                  (lambda (markup) (send markup mouse-release self x y (fxlogor bit state)))
                  (send self markups-at (send self xy->index x y)))))])
       (set! markup-double-mousepress-cbproc (make-multipress-cbproc 2048))  ; gack, manifest constants
       (set! markup-triple-mousepress-cbproc (make-multipress-cbproc 4096))
       (set! markup-double-mouserelease-cbproc (make-multirelease-cbproc 2048))  ; gack, manifest constants
       (set! markup-triple-mouserelease-cbproc (make-multirelease-cbproc 4096)))

     ;; Not sure I like this because it involves creating an extra closure
     ;; for each text widget (not such a big deal since text widgets are
     ;; heavy-weight already).  See notes elsewhere about other ways I've
     ;; considered dealing with this shared abstraction barrier problem.
     (set! txt-proc
       (let ([live? #t])
         (case-lambda
           [(msg markname)
            (unless (or live? (memq msg '(destroy-mark text-destroyed)))
              (assertion-violationf msg "text widget containing the mark has been destroyed"))
            (case msg
              [(get-pos) (make-index-for markname)]
              [(get-x/char)
               (let ([p (index->input-port markname)]) (read p) (read p))]
              [(get-y/char) (fx- (read (index->input-port markname)) 1)]
              [(destroy-mark)
               (when (and markname handle)
                 (swl:tcl-eval handle 'mark 'unset markname))]
              [(text-destroyed) (set! live? #f)])])))
     (send self tag-bind 'callback '|<Enter>| markup-mouse-enter-cbproc)
     (send self tag-bind 'callback '|<Leave>| markup-mouse-leave-cbproc)
     (send self tag-bind 'callback '|<KeyPress>| markup-keypress-cbproc)
     (send self tag-bind 'callback '|<KeyRelease>| markup-keyrelease-cbproc)
     (send self tag-bind 'callback '|<Motion>| markup-mousemotion-cbproc)
     (send self tag-bind 'callback '|<ButtonPress>| markup-mousepress-cbproc)
     (send self tag-bind 'callback '|<Double-ButtonPress>| markup-mousepress-cbproc)
     (send self tag-bind 'callback '|<Triple-ButtonPress>| markup-mousepress-cbproc)
     (send self tag-bind 'callback '|<ButtonRelease>| markup-mouserelease-cbproc)
     (send self tag-bind 'callback '|<Double-ButtonRelease>| markup-mouserelease-cbproc)
     (send self tag-bind 'callback '|<Triple-ButtonRelease>| markup-mouserelease-cbproc)]
     [destroy ()
      (critical-section
        (txt-proc 'text-destroyed #f)
        (set! existing-marks #f)
        (set! gc-safe-mkups #f)
        (set! children #f)
        (set! txt-proc #f)
        (send-base self destroy))]
     [adopt (child)
      (thread-critical-section
        (when handle (set! children (cons child children))))]
     [disown (child)
      (thread-critical-section
        (when handle (set! children (remq child children))))]))
