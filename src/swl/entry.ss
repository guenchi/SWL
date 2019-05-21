;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Entry
;;

(swl:api-class (<entry> parent) (<proto-editable> parent 'entry)
  ;% \swlapi{class}{<entry>}{(create <entry> parent)}
  ;* \ret{instance}
  ;* An entry displays a single line of text that may be edited using
  ;* the arrow keys and emacs-like key bindings.
  ;* Text is displayed in a single font that may be changed by \scheme{set-font!}.
  ;* Text in an entry can be selected and the selection can exported
  ;* as the X selection if desired.  When an entry has input focus it
  ;* displays an insertion cursor.  An entry can be scrolled
  ;* horizontally when the string is too long to be displayed in the window.
  ;* The contents of an entry can be retrieved as a Scheme string.
  ;* Several entry methods take an argument specifying position in the
  ;* entry.
  ;* Positions in the entry are numbered from zero.  The symbol
  ;* \scheme{end} can be used to refer to the last position in the entry.
  ;;
  ;; Not amazingly thrilled with the sel-start-pos setup
  (ivars (action (lambda args (void))) (content #f))
  (inherited handle thread sel-start-pos sel-end-pos select-mode sel-anchor)
  (inheritable handle)
  (private
    [with-selected-range (handler)
     (thread-critical-section
       (and (send self selection-exists?)
            (handler (index->number 'sel.first)
                     (index->number 'sel.last))))]
    [scan-by-word (content ipos update)
     (let ([len (string-length content)])
       (let loop ([pos (min (index->number ipos) (fx- len 1))])
         (cond
           [(fx< pos 0) 0]
           [(fx>= pos len) len]
           [(let ([ch (string-ref content pos)])
              (or (char-alphabetic? ch)
                  (char-numeric? ch)
                  (char=? ch #\_)))
            (loop (fx+ pos update))]
           [else (if (fxnegative? update)
                     (if (fx= ipos pos)
                         pos
                         (fx+ pos 1))
                     pos)])))]
    [index->number (index)
     (if (symbol? index)
         (string->number (swl:tcl-eval handle 'index index))
         index)]
    [compare-index (index1 fn index2 who)
     (check-entry-index index1 who)
     (check-entry-index index2 who)
     (let ([index1 (index->number index1)]
           [index2 (index->number index2)])
       (fn index1 index2))]
    [check-entry-index (index who)
     (unless (or (memq index '(insert end)) (and (integer? index) (>= index 0)))
       (assertion-violationf who "invalid index ~s (must be natural number or 'end)" index))]
    [real-insert (char/string index)
     (let ((what (cond
                   ((char? char/string) (string char/string))
                   ((string? char/string) char/string)
                   (else
                     (assertion-violationf 'insert
                       "must be character or string: ~s" char/string)))))
        (set! content #f)
        (swl:tcl-eval handle 'insert index what)
        (make-visible self 'insert))])
  (protected
    [sanitized-xy->index (x y)
     (x->index self x)]
    [index->number (index)
     (string->number (swl:tcl-eval handle 'index index))])
  (public
    [key-press (key modifiers)
     ;* \ret{unspecified}
     ;* Entries have the following default behaviors.
     ;* When \mytt{newline} or \mytt{return} is pressed, the action
     ;* specified by \scheme{set-action!} is performed.
     ;* When \mytt{shift-left} or \mytt{shift-right} is pressed,
     ;* the selection is extended in the direction indicated.
     (event-case ((key= key) (modifier= modifiers))
       (([#\return] [#\newline]) (action self))
       (else (send-base self key-press key modifiers)))]
    [get-vertical-view ()
     ;* \ret{list}
     ;* Because entries cannot be scrolled vertically, this always
     ;* returns the list \scheme{(0 1)} to indicate that no part of the
     ;* widget's content is out of view to the top, and all of the
     ;* content is in view above the bottom.
     (list 0 1)]
    [set-justify! (val)
     ;* \ret{unspecified}
     ;* \var{val} must be \scheme{left}, \scheme{right}, or \scheme{center}.
     ;* This determines where the the insertion cursor begins
     ;* and how the text is displayed when it all fits in
     ;* the widget.
     (set-tk-option '-justify val swl:justify? 'set-justify!)]
    [get-justify ()
     ;* \ret{symbol}
     ;* Returns a symbol indicating where where the the insertion cursor begins
     ;* and how the text is displayed when it all fits in
     ;* the widget.
     (get-tk-option '-justify string->symbol 'get-justify)]
    [set-action! (val)
     ;* \ret{unspecified}
     ;* \var{val} is a procedure of one argument (self) evaluated for effect when
     ;* \mytt{return} or \mytt{newline} is pressed in an \scheme{<entry>}.
     ;; eventually allow generalized callback object for Wally's sake.
     (unless (procedure? val)
       (assertion-violationf 'set-action! "~s is not a procedure" val))
     (set! action val)]
    [get-action ()
     ;* \ret{procedure}
     ;* returns a procedure of one argument (self) evaluated for effect when
     ;* \mytt{return} or \mytt{newline} is pressed in an \scheme{<entry>}.
     ;; eventually allow generalized callback object for Wally's sake.
     action]
    [set-blank! (val)
     ;* \ret{unspecified}
     ;* If \var{val} is a character, then the contents of the widget are
     ;* displayed using only that character (e.g. for passwords).  The
     ;* default is \scheme{#f} indicating that the contents should be displayed
     ;* without translation.
     (set-tk-option '-show (or val "")
       (lambda (x) (or (char? x) (eq? x "")))
       'set-blank!)]
    [get-blank ()
     ;* \ret{see below}
     ;; may be buggy if the string gets tcl-quoted by Tk.
     ;* Returns \scheme{#f} if the entry displays its contents without translation
     ;* else returns the character used in the translation. (see \scheme{set-blank!})
     (get-tk-option '-show
       (lambda (x) (if (eq? x "") #f (string-ref x 0))) 'get-blank)]
    [delete (first)
     ;* \ret{unspecified}
     ;* deletes the character if any at index \var{first} in the entry.
     (swl:safety-check
       (check-entry-index first 'delete))
     (set! content #f)
     (swl:tcl-eval handle 'delete first)
     (void)]
    [delete (first last)
     ;* \ret{unspecified}
     ;* deletes the characters in the range of \var{first}
     ;* to \var{last} (inclusive).
     (swl:safety-check
       (check-entry-index first 'delete)
       (check-entry-index last 'delete))
     (set! content #f)
     (swl:tcl-eval handle 'delete first last)
     (void)]
    [get-string ()
     ;* \ret{string}
     ;* returns the contents of the entry as a Scheme string.
     (swl:tcl-eval handle 'get)]
    [set-cursor-pos! (index)
     ;* \ret{unspecified}
     ;* sets the position of the insertion cursor within the widget.
     ;* If \var{index} is outside the
     ;* bounds of the text displayed in the widget, the cursor is
     ;* placed at the point in the text closest to
     ;* the target index.
     (swl:safety-check
       (check-entry-index index 'set-cursor-pos!))
     (swl:tcl-eval handle 'icursor index)
     (make-visible self 'insert)]
    [get-cursor-pos ()
     ;* \ret{integer}
     ;* Returns the position of insertion cursor within the widget.
     (string->number (swl:tcl-eval handle 'index 'insert))]
    [delete-eol ()
     ;* \ret{unspecified}
     ;* Deletes characters from current insert position to end of line.
     (set! content #f)
     (send self set-saved-text!
       (let ([s (send self get-string)]
             [start (index->number 'insert)]
             [end (index->number 'end)])
         (if (<= 0 start end (string-length s))
             (substring s start end)
             "")))
     (swl:tcl-eval handle 'delete 'insert 'end)
     (make-visible self 'insert)]
    [delete-all ()
     ;* \ret{unspecified}
     ;* Deletes all characters in the widget.
     (set! content #f)
     (swl:tcl-eval handle 'delete 0 'end)
     (void)]
    [cursor-eol ()
     ;* \ret{unspecified}
     ;* Moves the insertion cursor to the end of the entry.
     (set-cursor-pos! self 'end)]
    [cursor-bol ()
     ;* \ret{unspecified}
     ;* Moves the insertion cursor to the beginning of the entry.
     (set-cursor-pos! self 0)]
    [cursor-eol? ()
     ;* \ret{boolean}
     ;* Returns true if the insertion cursor is at the end of the string
     ;* displayed in the entry.
     (= (get-cursor-pos self) (string->number (swl:tcl-eval handle 'index 'end)))]
    [cursor-bol? ()
     ;* \ret{boolean}
     ;* Returns true if the insertion cursor is at the beginning of the string displayed
     ;* in the entry.
     (zero? (get-cursor-pos self))]
    [cursor-eof? ()
     ;* \ret{boolean}
     ;* Returns true if the insertion cursor is at the end of the text buffer.
     (send self cursor-eol?)]
    [delete-char (offset)
     ;* \ret{unspecified}
     ;* Deletes the character at the given \var{offset} in the \scheme{<entry>}.
     (swl:safety-check
       (unless (fixnum? offset) (assertion-violationf 'delete-char "offset must be an integer")))
     (set! content #f)
     (if (fx= offset 1)
         (swl:tcl-eval handle 'delete 'insert)
         (let ([old (get-cursor-pos self)])
           (let ([new (max 0 (fx+ offset old))])
             (swl:tcl-eval handle 'icursor new)
             (if (fxnegative? offset)
                 (delete self new old)
                 (delete self old new)))))
     (make-visible self 'insert)]
    [move-char (offset)
     ;* \ret{unspecified}
     ;* Advances the cursor \var{offset} characters.
     ;* If there are not enough characters to satisfy offset then the
     ;* cursor is left at one end of the entry.
     ;* Negative offsets are allowed.
     (swl:safety-check
       (unless (fixnum? offset) (assertion-violationf 'move-char "offset must be an integer")))
     (let ([cur (get-cursor-pos self)])
       (set-cursor-pos! self (fxmax 0 (fx+ cur offset))))]
    [make-visible (index)
     ;* \ret{unspecified}
     ;* Ensures that the text at the given index is visible on screen
     (swl:safety-check
       (check-entry-index index 'make-visible))
     (let ([x (string->number (swl:tcl-eval handle 'index index))]
           [left (x->index self 0)])
       (if (< x left)
           (move-to-left self x)
           (let ([cols (get-width/char self)])
             (when (< (+ left cols) x) (move-to-left self (- x cols))))))
     (void)]
    [x->index (x)
     ;* \ret{integer}
     ;* Returns the character index in the entry closest to the
     ;* x-coordinate given by \var{x}.
     (swl:safety-check
       (unless (fixnum? x)
         (assertion-violationf 'x->index "x coordinate not an integer ~s" x)))
     (swl:tcl->scheme
       (swl:tcl-eval handle 'index
         (string-append "@" (number->string x 10))))]
    [insert (char/string)
     ;* \ret{unspecified}
     ;* inserts the given character or string at the current position of
     ;* the insertion cursor in the entry.
     (real-insert char/string 'insert)]
    [insert-at (index char/string)
     ;* \ret{unspecified}
     ;* inserts the given character or string at the position
     ;* given by \var{index}.
     (swl:safety-check (check-entry-index index 'insert))
     (real-insert char/string index)]
    [clear-selection ()
     ;* \ret{unspecified}
     ;* clears the selection if anything in this widget is selected.
     (set! sel-anchor #f)
     (set! sel-start-pos #f)
     (set! sel-end-pos #f)
     (swl:tcl-eval handle 'selection 'clear)
     (void)]
    [delete-selection-at-cursor ()
     ;* \ret{unspecified}
     ;* Deletes the selected text, if any, surrounding the cursor.
     ;; Note this could be dangerous because delete is now called
     ;; from within a critical-section
     (with-selected-range
       (lambda (first last)
         (let ((i (get-cursor-pos self)))
           (when (and (<= first i) (<= i last))
             (send self delete first last)
             (send self clear-selection)))))
     (void)]
; transliterated directly from Tcl/Tk, maybe we can get by without them
; this makes them more like <text> and <listbox>
;    [select-from (index)
;     ;* sets the selection anchor point to just before the
;     ;* character indicated by \var{index}, but does not change the selection.
;     ;* See also \scheme{select-to}.
;     (swl:safety-check
;       (check-entry-index index 'select-from))
;     (swl:tcl-eval handle 'selection 'from index)
;     (void)]
;    [select-to (index)
;     ;* selects the characters between the most recent selection
;     ;* anchor point and \var{index} (inclusive).  The selection anchor
;     ;* point is set by \scheme{select-from} and \scheme{select-adjust}.
;     (swl:safety-check
;       (check-entry-index index 'select-to))
;     (swl:tcl-eval handle 'selection 'to index)
;     (void)]
;    [select-adjust (index)
;     ;* adjusts the end of the selection nearest to the given
;     ;* \var{index} to extend to that index.  If nothing in the entry
;     ;* is selected, the characters between \var{index} and the most
;     ;* recent selection anchor point (inclusive) are selected.
;     (swl:safety-check (check-entry-index index 'select-adjust))
;     (swl:tcl-eval handle 'selection 'adjust index)
;     (void)]
    [selection-exists? ()
     ;* \ret{boolean}
     ;* returns true if anything in the entry is selected.
     (tk->boolean (swl:tcl-eval handle 'selection 'present))]
    [select-range (start end)
     ;* \ret{unspecified}
     ;* Selects the characters starting at \var{start} up to but not
     ;* including the character before \var{end}.  Characters already
     ;* selected before this method was called remain selected only
     ;* if they are in the new range.
     (swl:safety-check
       (check-entry-index start 'select-range)
       (check-entry-index end 'select-range))
     (if (or (and (fixnum? start) (fixnum? end) (fx> start end))
             (eq? start 'end))
         (clear-selection self)
         (begin
           (swl:tcl-eval handle 'selection 'range start end)
           (unless sel-anchor (set! sel-anchor (get-cursor-pos self)))
           (set! sel-start-pos start)
           (set! sel-end-pos end)))]
    [move-to-left (index)
     ;* \ret{unspecified}
     ;* scrolls the view in the widget horizontally so that the character at \var{index}
     ;* is displayed at the left edge of the window.
     (swl:safety-check (check-entry-index index 'move-to-left))
     (swl:tcl-eval handle 'xview index)
     (void)]
    [vscroll (n qualifier)
     ;* \ret{unspecified}
     ;* Vertical scroll requests are ignored by \scheme{<entry>} widgets.
     ; perhaps this should do error checking?
     (case qualifier
       ((fraction)
        (unless (or (flonum? n) (fixnum? n))
          (scroll-error n qualifier 'vscroll)))
       ((pages units)
        (unless (fixnum? n)
          (scroll-error n qualifier 'vscroll)))
       (else (assertion-violationf 'vscroll "invalid qualifier ~s" qualifier)))
     (void)]
    [set-vscroll-notify! (val)
     ;* \ret{unspecified}
     ;* This method overrides the inherited method.
     ;* The given value is not used for an \scheme{<entry>}, but is
     ;* provided for compatibility with other scrollable widgets.
     (let ([cb
            (if val
                (begin (unless (procedure? val)
                         (assertion-violationf 'set-vscroll-notify!
                           "~s is not a procedure"
                           val))
                       (swl:procedure->callback val handle))
                val)])
       (prop-set! '-yscrollcommand cb))]

;;
;;  just added this stuff in for compatibility w/ text.ss
;;

    [word-start (pos)
     ;* \ret{see below}
     ;* Returns the index of the first character of the word
     ;* at position \var{pos} within the widget.
     (critical-section
       (unless content (set! content (get-string self)))
       (scan-by-word content pos -1))]
    [word-end (pos)
     ;* \ret{see below}
     ;* Returns the index of the last character of the word
     ;* at position \var{pos} within the widget.
     (critical-section
       (unless content (set! content (get-string self)))
       (scan-by-word content pos 1))]
    [line-start (pos)
     ;* \ret{see below}
     ;* Returns zero, the index of the start of the line within the widget.
     (check-entry-index pos 'line-start)
     0]
    [line-end (pos)
     ;* \ret{see below}
     ;* Returns the index of the end of the widget contents.
     (check-entry-index pos 'line-start)
     (if content
         (string-length content)
         (string->number (swl:tcl-eval handle 'index 'end)))]

    [get-selected-string ()
     ;* \ret{see below}
     ;* Returns the string selected in the entry or \scheme{#f} if none.
;* Very tempted to eliminate this
     (with-selected-range
       (lambda (first last)
         (substring (swl:tcl-eval handle 'get) first last)))]
    [get-selected-range ()
     ;* \ret{see below}
     ;* Returns a pair of the start and end points of the selected
     ;* region or \scheme{#f} if nothing is selected.
     (with-selected-range cons)]

    [pos<? (index1)
     ;* \ret{boolean}
     ;* Returns \scheme{#t} if position of index1 is less than the position of the
     ;* \var{insertion} cursor,
     ;* else returns \scheme{#f}.
     (compare-index index1 < 'insert 'pos<?)]
    [pos<? (index1 index2)
     ;* \ret{boolean}
     ;* Returns \scheme{#t} if position of index1 is less than index2,
     ;* else returns \scheme{#f}.
     (compare-index index1 < index2 'pos<?)]
    [pos<=? (index1)
     ;* \ret{boolean}
     ;* Returns \scheme{#t} if position of index1 is less than or equal to
     ;* the position of the
     ;* \var{insertion} cursor,
     ;* else returns \scheme{#f}.
     (compare-index index1 <= 'insert 'pos<=?)]
    [pos<=? (index1 index2)
     ;* \ret{boolean}
     ;* Returns \scheme{#t} if position of index1 is less than or equal to
     ;* index2,
     ;* else returns \scheme{#f}.
     (compare-index index1 <= index2 'pos<=?)]
    [pos=? (index1)
     ;* \ret{boolean}
     ;* Returns \scheme{#t} if position of index1 is equal to the position of the
     ;* \var{insertion} cursor,
     ;* else returns \scheme{#f}.
     (compare-index index1 = 'insert 'pos=?)]
    [pos=? (index1 index2)
     ;* \ret{boolean}
     ;* Returns \scheme{#t} if position of index1 is equal to index2,
     ;* else returns \scheme{#f}.
     (compare-index index1 = index2 'pos=?)]
    [pos>? (index1)
     ;* \ret{boolean}
     ;* Returns \scheme{#t} if position of index1 is greater than the position of the
     ;* \var{insertion} cursor,
     ;* else returns \scheme{#f}.
     (compare-index index1 > 'insert 'pos>?)]
    [pos>? (index1 index2)
     ;* \ret{boolean}
     ;* Returns \scheme{#t} if position of index1 is greater than index2,
     ;* else returns \scheme{#f}.
     (compare-index index1 > index2 'pos>?)]
    [pos>=? (index1)
     ;* \ret{boolean}
     ;* Returns \scheme{#t} if position of index1 is greater than or equal
     ;* to the position of the
     ;* \var{insertion} cursor,
     ;* else returns \scheme{#f}.
     (compare-index index1 >= 'insert 'pos>=?)]
    [pos>=? (index1 index2)
     ;* \ret{boolean}
     ;* Returns \scheme{#t} if position of index1 is greater than or equal
     ;* to index2,
     ;* else returns \scheme{#f}.
     (compare-index index1 >= index2 'pos>=?)]

))

