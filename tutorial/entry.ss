(module ()
(import swl:oop)
(import swl:macros)
(import swl:generics)
(import swl:option)
(swl:tutorial <entry>
;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

  (illustrates
    <entry> <scrollbar> <toplevel> action: create destroy expand: fill:
    get-string hscroll hscroll-notify: orientation: pack set-option!
    set-view! title: width/char: xview)
  (follows <canvas>)
  ;* An entry displays a single line of user-editable text.
  ;* If the contents of the entry exceed its width, it can be
  ;* scrolled horizontally.  This example shows how scrollbars
  ;* interact with scrollable widgets.
  ;*
  ;* We create a toplevel window \scheme{top}, an entry 20 characters wide called
  ;* \scheme{entry}, and a horizontal scrollbar called \scheme{scroll}.
  ;* We arrange for the entry to have the focus when \scheme{top} has focus
  ;* via \scheme{set-focus}.
  ;* Next we set \scheme{entry}'s \scheme{hscroll-notify:} procedure to
  ;* a procedure that informs the scrollbar \scheme{scroll} of the
  ;* new left and right extents of the entry.
  ;* This lets the scrollbar represent the position and size
  ;* of the current view in the entry relative to the entry's
  ;* total content.
  ;* We set the action for the scrollbar to be a procedure that
  ;* passes its number and qualifier arguments to the \scheme{hscroll}
  ;* method of the entry.
  ;* This tells the scrollbar who to notify and how to notify them
  ;* when the user manipulates the slider vi keyboard or mouse.
  ;*
  ;* Next we create a button titled ``done'' that retrieves via
  ;* \scheme{get-string} the contents of the entry, prints the resulting
  ;* string, then destroys the application
  ;* by destroying the toplevel window \scheme{top}.
  ;* The button and scrollbar are packed so that they expand and fill
  ;* the horizontal dimension of their parent \scheme{top}.
  ;* The entry is packed without these properties to illustrate the
  ;* difference when the toplevel is resized.  Run the demo, type
  ;* a long string in entry, and try scrolling the contents.
  
(let* ([top (create <toplevel> with (title: "Entry Example"))]
       [entry (create <entry> top with (width/char: 20))]
       [scroll (create <scrollbar> top with (orientation: 'horizontal))])
  (set-focus entry)
  (set-option! entry
    (hscroll-notify:
      (lambda (left right) (set-view! scroll left right))))
  (set-option! scroll
    (action: (lambda (self n q) (hscroll entry n q))))
  (pack (create <button> top with
          (title: "done")
          (action:
            (lambda (self)
              (printf "Entry: ~s~n" (get-string entry))
              (destroy top))))
        (expand: #t)
        (fill: 'x))
  (pack entry)
  (pack scroll (expand: #t) (fill: 'x)))

) ; end tutorial
)
