(module ()
(import swl:oop)
(import swl:macros)
(import swl:generics)
(import swl:option)
(swl:tutorial <photo>
;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

  (illustrates <photo> <label> create show <toplevel> title: <bitmap>)
  (follows <label>)
  ;* Many widgets, including labels and buttons, can display
  ;* images instead of static text.
  ;*
  ;* Below we create an appropriately titled toplevel window \scheme{top} to
  ;* display a label.  We create a \scheme{<photo>}, specifying the name of the
  ;* file that contains the data.  Currently supported file types
  ;* are GIF and PPM.  Next we create the label, with \scheme{top} as its
  ;* parent, and specify the \scheme{<photo>} as the value for the
  ;* keyword argument \scheme{title:}.
  ;* Finally, to make the label visible we call the \scheme{show} method.
  ;* The toplevel is visible by default.
  ;*
  ;* The \scheme{<bitmap>} class is used just as the \scheme{<photo>} class,
  ;* except that the specified file must contain monochrome X11 bitmap
  ;* data.

(let ([top (create <toplevel> with (title: "Label Image Example"))]
      [earth (create <photo> with (filename: "earth.gif"))])
  (let ([label (create <label> top with (title: earth))])
    (show label)))

) ; end tutorial
)

