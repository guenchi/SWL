(module ()
(import swl:oop)
(import swl:macros)
(import swl:generics)
(import swl:option)
(swl:tutorial <button>
;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

  (illustrates <button> <label> create show <toplevel>
     action: title: set-title!)
  (follows <label>)
  ;* A button displays text that cannot be edited by the user
  ;* and can be pressed to perform some action.
  ;* As with labels, multiple lines of text are displayed
  ;* when the \scheme{title:} string contains newline characters,
  ;* or exceeds a user-specified length limit for the button.
  ;*
  ;* When creating any widget (other than a \scheme{<toplevel>}), the
  ;* widget's parent is passed as a positional argument to
  ;* the \scheme{create} macro before the keyword arguments.
  ;* Here we create a button with \scheme{top} as its parent.
  ;* Initially the button displays ``Never been clicked''.
  ;* The \scheme{action:} keyword introduces a procedure
  ;* of one argument (the instance) to be called whenever the button is clicked.
  ;* In this example, the callback procedure changes the title of the
  ;* button, using \scheme{set-title!}, each time the button is clicked.
  ;* We could also have written \scheme{(set-option! button (title: ...))}.
  ;* Note that the width of the button is adjusted to fit its title
  ;* whenever the title changes because we have not specified a value
  ;* for the \scheme{width:} keyword.
  ;* To make the button visible we call the \scheme{show} method.

(let ([top (create <toplevel> with (title: "Button Example"))])
  (let ([button
         (create <button> top with
           (title: "Never been clicked")
           (action:
             (let ([count 0])
               (lambda (self)
                 (set! count (+ count 1))
                 (set-title! self (format "~a click(s)" count))))))])
    (show button)))

) ; end tutorial
)

