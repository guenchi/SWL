(module ()
(import swl:oop)
(import swl:macros)
(import swl:generics)
(import swl:option)
(swl:tutorial <toplevel>
;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

  (illustrates <toplevel> create in->pixel width: height: title:)
  (follows define-class)
  ;* A toplevel provides a free-standing window on the display and
  ;* serves as the parent for other widgets that make up an application.
  ;* Below we use the \scheme{create} macro and the keywords \scheme{title:},
  ;* \scheme{width:}, and \scheme{height:}, to create a 3-inch by 2-inch
  ;* toplevel window titled
  ;* ``Toplevel Example''.
  ;* Since a toplevel has no parent widget there are no positional arguments
  ;* to the create macro; all arguments are keyword arguments.
  ;* Here \scheme{in->pixels} is used to convert from inches to screen pixels.
  ;* There are also procedures \scheme{cm->pixels}, and \scheme{pt->pixels}
  ;* which convert from centimeters and printer's points to screen pixels.
  ;* Toplevel windows are visible by default.  Later examples show how
  ;* other widgets that are not visible by default can be displayed on
  ;* the screen.

(create <toplevel> with
  (title: "Toplevel Example")
  (height: (in->pixels 2))
  (width: (in->pixels 3)))

) ; end tutorial
)

