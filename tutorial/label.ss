(module ()
(import swl:oop)
(import swl:macros)
(import swl:generics)
(import swl:option)
(swl:tutorial <label>
;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

  (illustrates <label> create show <toplevel> title: font: width/char:)
  (follows <toplevel>)
  ;* A label displays text that cannot be edited by the user.
  ;* If the string to be displayed contains newline characters,
  ;* or exceeds a user-specified length limit for the label,
  ;* then multiple lines of text are displayed.
  ;*
  ;* When creating any widget (other than a \scheme{<toplevel>}), the
  ;* widget's parent is passed as a positional argument to
  ;* the \scheme{create} macro before the keyword arguments.
  ;* Below we first create an appropriately titled toplevel window \scheme{top} to
  ;* display the label.  Next we create the label, passing \scheme{top} as a
  ;* positional argument before the keyword arguments \scheme{title:} and \scheme{font:}
  ;* which specify the string to be displayed and the font to display it in.
  ;* We request a 14 point bold italic Times font.
  ;* For labels and many other widgets that display text, the \scheme{width/char:}
  ;* keyword is used to specify the width of the widget in characters.
  ;* If \scheme{width:} is omitted, the label shrinks to fit its text.
  ;* Here we specify a width of 30 characters to show that a label
  ;* centers its text by default.  The placement of the text can be
  ;* adjusted with the \scheme{anchor:} and \scheme{justify:} keywords.
  ;* Finally, to make the label visible we call the \scheme{show} method.
  ;* The toplevel is visible by default.

(let ([top (create <toplevel> with (title: "Label Example"))])
  (let ([label (create <label> top with
                 (title: "This is a label")
                 (font: (create <font> 'times 14 '(bold italic)))
                 (width/char: 30))])
    (show label)))

) ; end tutorial
)

