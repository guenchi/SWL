;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

; if we add an action notion, should it be for double-click or for
; item-was-selected?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Listbox
;;
  
  ;; would be cool to have a listbox where you can display any kind of
  ;; elements you want and you just specify an element->string method
  ;; that can be applied to get a printable representation of stuff.
  ;; (on the other hand, this should be trivial to implement)
  ;; let someone add it to apps/common

; shares setgrid and yscrollcommand with text 
; would need multiple inheritance to make it worthwhile, and we don't want that

;; Tk things missing:
;;    * activate
;;    * bbox

(swl:api-class (<listbox> parent) (<proto-select-scroll-textual> parent 'listbox)
  ;% \swlapi{class}{<listbox>}{(create <listbox> parent)}
  ;* \ret{instance}
  ;;
  ;* A listbox displays a list of strings, one per line, in a uniform font.
  ;* Items may be inserted, deleted, or retrieved from the listbox using
  ;* methods described below.  The position of each item in the listbox 
  ;* is described by a zero-based index.  The special index \scheme{end} refers
  ;* to the last item in the listbox.
  ;* Multiple items in the listbox
  ;* may be selected and the selection may be exported as the X selection.
  ;* Listboxes can be scrolled horizontally and vertically.
  ;* A cursor is displayed as an underline when the listbox
  ;* has input focus.
  (ivars (cpos 0) (lb-select-mode 'browse))
  (inherited handle sel-start-pos sel-end-pos select-mode sel-anchor)
  (inheritable handle)
  (private
    [update-cpos ()
     (set! cpos (string->number (swl:tcl-eval handle 'index 'active)))]
    [select-action (action index index2 name)
     (swl:safety-check (check-index index name))
     (if index2
         (begin
           (swl:safety-check (check-index index2 name))
           (swl:tcl-eval handle 'selection action index index2))
         (swl:tcl-eval handle 'selection action index))]
    [primitive-insert (index elt)
     (swl:safety-check
       (unless (string? elt) (assertion-violationf 'insert "~s is not a string" elt)))
     (swl:tcl-eval handle 'insert index elt)]
    [check-index (i mname)
     (unless (or (fixnum? i) (eq? i 'end))
       (assertion-violationf mname "invalid index ~s" i))])
  (protected
    [sanitized-xy->index (x y)
     (y->index self y)]
    [begin-selection (pos)
     (case lb-select-mode
       ((browse single extended)
        ;; clear-selection bangs on sel-start-pos and sel-end-pos
        (clear-selection self)
        (when (eq? lb-select-mode 'extended) (set! sel-anchor pos))
        (select self pos))
       ((multiple)
        (if (selected? self pos)
            (clear-selection self pos)
            (select self pos))))]
    [paste-at (pos) (void)]
    [update-selection (cur mods select-mode)
     (case lb-select-mode
       ((browse)
        (clear-selection self)
        (select self cur))
       ((extended)
        (event-case ((modifier= mods))
          (([left-button] [shift])
           (unless sel-anchor (set! sel-anchor cpos))
           (if (> sel-anchor cur)
               (select-range self (max 0 cur) sel-anchor)
               (select-range self sel-anchor cur))))))]
    [end-selection (x y)
     (set-cursor-pos! self (y->index self y))])
  (public
    [key-press (key mods)
     ;% \ret{unspecified}
     ;; don't send to base class  unless we decide not to let proto.ss
     ;; clear the selection (maybe that's something the insert method should
     ;; do?)
     (event-case ((key= key) (modifier= mods))
       (([#\tab]) (send-base self key-press key mods))  ;; see above
       (([control prior]) (vscroll self 0 'fraction))
       (([control next]) (vscroll self 1 'fraction))
       (([prior]) (vscroll self -1 'pages))
       (([next]) (vscroll self 1 'pages))
       (([#\space]) (begin-selection cpos))
       (([shift up])
        (move-line self -1)
        (update-selection cpos mods 'line))
       (([shift down])
        (move-line self 1)
        (update-selection cpos mods 'line))
       (([up]) (move-line self -1) (update-selection cpos mods 'line))
       (([down]) (move-line self 1) (update-selection cpos mods 'line))
       (([left]) (hscroll self -1 'units))
       (([right]) (hscroll self 1 'units)))]
    [set-cursor-pos! (pos)
     ;* \ret{unspecified}
     ;* Positions the listbox cursor at the index \var{pos} (zero-based).
     (swl:safety-check
       (unless (fixnum? pos)
         (assertion-violationf 'set-cursor-pos! "invalid position ~s" pos)))
     (let ([pos (if (fxnegative? pos) 0 (fxmin pos (item-count self)))])
       (swl:tcl-eval handle 'activate pos)
       (make-visible self cpos)
       (set! cpos pos))]
    [get-cursor-pos ()
     ;* \ret{integer}
     ;* Returns the index of the item currently under the cursor.
     cpos]
    [move-line (disp)
     ;* \ret{unspecified}
     ;* Moves the cursor by \var{disp} lines.  Negative displacements allowed.
     (swl:safety-check
       (unless (fixnum? disp)
         (assertion-violationf 'move-line "invalid displacement ~s" disp)))
     (set-cursor-pos! self (fx+ cpos disp))]
    [set-grid! (val)
     ;* \ret{unspecified}
     ;* \var{val} is a boolean determining whether or not the 
     ;* listbox should strive to keep its dimensions in even
     ;* multiples of the dimensions of a character in its current font.
     ;* The default is \scheme{#f}.
     (set-tk-option '-setgrid val boolean? 'set-grid!)]
    [get-grid ()
     ;* \ret{boolean}
     ;* Returns a boolean indicating whether or not the 
     ;* listbox should strive to keep its dimensions in even
     ;* multiples of the dimensions of a character in its current font.
     (get-tk-option '-setgrid tk->boolean 'get-grid)]
    [set-select-mode! (val)
     ;* \ret{unspecified}
     ;* \var{val} is either \scheme{single}, \scheme{browse}, \scheme{multiple}, or \scheme{extended}
     ;* determining how items in the listbox can be selected.
     ;* When the select mode is \scheme{single} or \scheme{browse} at most one
     ;* item can be selected at a time.  Multiple items can be selected
     ;* when the select mode is \scheme{multiple} or \scheme{extended}.
     ;* In \scheme{multiple} mode, the selected state of an item is toggled.
     ;* The selection can be dragged in \scheme{browse} and \scheme{extended}
     ;* modes.
     (swl:safety-check
       (unless (memq val '(single browse multiple extended))
         (assertion-violationf 'set-select-mode!
            "~s must be single, browse, multiple, or extended" val)))
     (set! lb-select-mode val)]
    [get-select-mode ()
     ;* \ret{see below}
     ;* Returns a symbol describing the current selection mode.
     ;*  See \scheme{set-select-mode!} for more details.
     lb-select-mode]
    [get-selected-items ()
     ;* \ret{see below}
     ;* Returns the index of the item currently selected,
     ;* a list of such indices if more than one is selected, or
     ;* \scheme{#f} if none is selected.
     (let ((x (swl:tcl-eval handle 'cursel)))
       (if (eq? x "")
           #f
           (swl:tcl->scheme x)))]
    [delete-all ()
     ;* \ret{unspecified}
     ;* deletes all the items in the listbox.
     (swl:tcl-eval handle 'delete 0 'end)
     (update-cpos)]
    [delete (item)
     ;* \ret{unspecified}
     ;* Deletes the item with index \var{item}.
     (swl:safety-check
       (check-index item 'delete))
     (swl:tcl-eval handle 'delete item)
     (update-cpos)]
    [delete (first last)
     ;* \ret{unspecified}
     ;* Deletes the elements in the range from
     ;* \var{first} to \var{last}.
     (swl:safety-check
       (check-index first 'delete)
       (check-index last 'delete))
     (swl:tcl-eval handle 'delete first last)
     (update-cpos)]
    [get-item (index)
     ;* \ret{see below}
     ;* Returns the string in the listbox at the given index.
     (swl:safety-check
       (check-index index 'get-item))
     (swl:tcl-eval handle 'get index)]
    [insert (index s)
     ;* \ret{unspecified}
     ;* Inserts the string \var{s} just before the
     ;* item at the position given by index.
     (swl:safety-check
       (check-index index 'insert))
     (primitive-insert index s)
     (update-cpos)]
    [insert (index s . more)
     ;* \ret{unspecified}
     ;* Inserts the given strings just before the
     ;* item at the position given by index.
     (swl:safety-check
       (check-index index 'insert)
       (unless (andmap string? (cons s more))
         (assertion-violationf 'insert
           (string-append "elements to insert must be strings:"
             (apply string-append
               (map (lambda (x) (format " ~s" x)) (cons s more)))))))
     (apply swl:tcl-eval handle 'insert index s more)
     (update-cpos)]
    [y->index (y)
     ;* \ret{integer}
     ;* Returns the index of the listbox element displayed nearest to
     ;* the given y-coordinate within the listbox window
     (swl:safety-check
       (unless (swl:oknum? y)
         (assertion-violationf 'y->index "invalid y-coordinate ~s" y)))
     (string->number (swl:tcl-eval handle 'nearest y) 10)]
    [select (index)
     ;* \ret{unspecified}
     ;* Selects the item at the given index
     (select-action 'set index #f 'select)
     (void)]
    [select-range (index1 index2)
     ;* \ret{unspecified}
     ;* Selects the only items between \var{index1} and \var{index2} inclusive
     (if (or (and (fixnum? index1) (fixnum? index2) (fx> index1 index2))
             (eq? index1 'end))
         (clear-selection self)
         (begin
           (select-action 'clear 0 index1 'select-range)
           (select-action 'clear index2 'end 'select-range)
           (select-action 'set index1 index2 'select-range)
           (unless sel-anchor (set! sel-anchor (get-cursor-pos self)))
           (set! sel-start-pos index1)
           (set! sel-end-pos index2)))]
    [selected? (index)
     ;* \ret{boolean}
     ;* Returns true if the item at the given index is selected, false
     ;* otherwise.
     (tk->boolean (select-action 'includes index #f 'selected?))]
    [clear-selection ()
     ;* \ret{unspecified}
     ;* De-selects all items in the listbox.
     (set! sel-anchor #f)
     (set! sel-start-pos #f)
     (set! sel-end-pos #f)
     (select-action 'clear 0 'end  'clear-selection)]
    [clear-selection (index)
     ;* \ret{unspecified}
     ;* De-selects the item at the given index
     (select-action 'clear index #f 'clear-selection)
     (void)]
    [clear-selection (index1 index2)
     ;* \ret{unspecified}
     ;* De-selects the items between \var{index1} and \var{index2} inclusive
     (select-action 'clear index1 index2 'clear-selection)
     (void)]
    [item-count ()
     ;* \ret{integer}
     ;* Returns the total number of elements in the listbox.
     (string->number (swl:tcl-eval handle 'size))]
    [make-visible (index)
     ;* \ret{unspecified}
     ;* Scrolls the element given by \var{index} into view.
     (swl:safety-check (check-index index 'make-visible))
     (swl:tcl-eval handle 'see index)
     (void)]
    [move-to-left (index)
     ;* \ret{unspecified}
     ;* scrolls the view in the widget horizontally so that the character at \var{index}
     ;* is displayed at the left edge of the window.
     (swl:safety-check (check-index index 'move-to-left))
     (swl:tcl-eval handle 'xview index)
     (void)]
    [move-to-top (index)
     ;* \ret{unspecified}
     ;* scrolls the view in the widget vertically so that the character at \var{index}
     ;* is displayed at the top edge of the window.
     (swl:safety-check (check-index index 'move-to-top))
     (swl:tcl-eval handle 'yview index)
     (void)]))
