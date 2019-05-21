(module ()
(import swl:oop)
(import swl:macros)
(import swl:generics)
(import swl:option)
(swl:tutorial <canvas>
;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

  (illustrates
    <canvas> <label> <rectangle> <rgb> <toplevel> background-color:
    mouse-press mouse-release mouse-motion create set-coords! set-title!
    show title: event-case define-swl-class send-base)
  (follows <button>)

  ;* \label{tutorial:canvas}
  ;* A canvas provides screen real-estate on which a variety
  ;* of figures such as lines, rectangles, and ovals can be displayed.
  ;* In this example we create 
  ;* a sub-class of \scheme{<canvas>} with event bindings that allow the user to
  ;* draw rectangles by dragging with mouse button 1.
  ;* We also use a label to display help messages to the user.
  ;* The background of the canvas is given a color
  ;* suitable for an easter egg. 
  ;*
  ;* The \scheme{<example-canvas>} class below extends the
  ;* \scheme{mouse-press}, \scheme{mouse-release}, and \scheme{mouse-motion}
  ;* methods of the \scheme{<canvas>} class.
  ;* The \scheme{mouse-press} method notifies a widget of the x- and
  ;* y-coordinates and the state of the modifier keys when a mouse
  ;* button is pressed while the mouse is over the widget.  Using
  ;* \scheme{event-case}, we inspect
  ;* the set of modifiers to see whether the left mouse
  ;* button is pressed.  If so, we record the starting
  ;* positions, update the help text displayed in the \scheme{<label>},
  ;* and create a new rectangle on the canvas at the place
  ;* where the mouse was clicked.  Otherwise we send
  ;* the \scheme{mouse-press} notification on to the base class using
  ;* the \scheme{send-base} syntax.
  ;*
  ;* The \scheme{mouse-motion} method notifies a widget of the x- and
  ;* y-coordinates and the state of the modifier keys when the mouse
  ;* moves over the widget.  If the left button is pressed, we
  ;* update the coordinates of the rectangle
  ;* to reflect the current position of the mouse.
  ;*
  ;* The \scheme{mouse-release} method notifies a widget of the x- and
  ;* y-coordinates and the state of the modifier keys when a mouse
  ;* button is released while the mouse is over the widget.
  ;* If the left button was released, we reset the state of the
  ;* widget and reset the help text.
  ;*
  ;* Although the example below does not demonstrate this, individual
  ;* figures on the canvas can have event methods of their own.
  ;* For instance, it is relatively easy to make the rectangles
  ;* draggable with mouse button 2, or to make them resizable after they
  ;* have been drawn.

(let* ([top (create <toplevel> with (title: "Canvas Example"))]
       [start-text "Click button 1 in canvas below"]
       [label (create <label> top with (title: start-text))])
  (define-swl-class (<example-canvas> parent) (<canvas> parent)
    (ivars (x1 #f) (y1 #f) (rect #f))
    (inherited)
    (inheritable)
    (private)
    (protected)
    (public
      [mouse-press (x y mods)
       (event-case ((modifier= mods))
         (([left-button])
          (set! x1 x)
          (set! y1 y)
          (set-title! label "Hold down button 1 and drag")
          (set! rect (create <rectangle> self x1 y1 x1 y1)))
         (else (send-base self mouse-press x y mods)))]
      [mouse-motion (x y mods)
       (event-case ((modifier= mods))
         (([left-button])
          (when rect
            (set-coords! rect
              (min x x1)
              (min y y1)
              (max x x1)
              (max y y1))))
         (else (send-base self mouse-motion x y mods)))]
      [mouse-release (x y mods)
       (event-case ((modifier= mods))
         (([left-button])
          (set! rect #f)
          (set-title! label start-text))
         (else (send-base self mouse-release x y mods)))]))
  (let ([canvas (create <example-canvas> top with
                 (background-color: (make <rgb> 215 215 255)))])
    (show label)
    (show canvas)))

) ; end tutorial
)
