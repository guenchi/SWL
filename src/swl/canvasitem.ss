;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Canvas Item
;;

;; NOTE: go back and abstract <markup> and <canvas-item>
;;       event-handling code

;; move all tag functionality into <group> class

;; make sure parent is a canvas
;;  * maybe should have parent eval function for talking to parent
;;    this guy can check that the parent exists...
;;    on the other hand, checking to see that the parent is live
;;    may be more efficient because we can check once for several calls


;; Implementation alternatives:
;;   put the handle and making of it down below
;;   do everything by calling protected method on the
;;   instance that gets redefined to use handle down below
;;   (could simply have protected method for getting handle
;;    and define the protected method in all lower classes)
;; or
;;   require them to use an init method, and blow up otherwise
;;   not so bad since we're going to end up needing to call init
;;   method anyway for most other widgets
;;
;; It's mighty tempting to go with the former just to show it could be
;; done, but the latter will likely be more efficient so that's the way.
;;
;; This means the create macro will have some positional and some
;; optional arguments (parent will prob'ly be positional).  To work
;; with the other classes, we'll have
;;   (create <foo> parent pos-arg1 ... pos-argn (option: opval) ...)
;; expand into:
;;   (let ((inst (make <foo> parent)))
;;     (send inst init pos-arg1 ... pos-argn)
;;     (send inst set-option: opval) ...
;;     inst)
;;
;; Analysis should show that we don't need the vector? checks, but of course
;; when we go to ultra-cool representation OW_1 even that doesn't matter.

(define-swl-class (<canvas-item> canvas) (<tk-prewidget2> #f)
  ;* An abstract class of figures that can be drawn on a canvas.
  ;* Unlike other widgets which are not visible until shown,
  ;* canvas items are visible at the time they are created.
     ;* The various mouse events appear to register only when the mouse is
     ;* on some visible part of the object (for example, they do not register
     ;* when the mouse is over a transparently filled part of the object).
  (ivars (parent canvas))
  (inherited handle)
  (inheritable handle parent)
  (private)
  (protected
    [set-color! (which val who)
     (set-tk-option which
       (if (eq? val 'transparent) "" val)
       (lambda (x) (or (eq? x "") (swl:color? val))) who)]
    [get-color (which who)
     (get-tk-option which
       (lambda (x) (if (string=? x "") 'transparent (tk->color x)))
       who)]
    [coords-error (mname ls)
     (assertion-violationf mname
       (apply string-append "coordinates must be fixnums or flonums "
         (map (lambda (x) (format "~s" x)) ls)))]
    [post-init ()
     (swl:safety-check
       (unless (swl:oknum? handle)
         (assertion-violationf 'create "attempt to create ~s may have failed"
           (class-name self))))
     (send parent adopt handle self)]
    [set-tk-option (tclname value err-check name)
     (swl:safety-check
       (unless (err-check value) (assertion-violationf name "bad value ~s" value)))
     (swl:tcl-eval parent 'itemconfig handle tclname value)
     (#3%void)]
    [get-tk-option (tclname conversion name)
     (conversion (swl:tcl-eval parent 'itemcget handle tclname))]
    [tk-destroy ()
; commented out this garbage
'    (unless (tk-destroyed?)
       ;; already checked to see if parent is live
       ;; we're in a critical section (see below) so we should be ok
       (swl:tcl-eval parent 'delete handle))]
    [tk-destroyed? ()
; commented out this garbage
'    (or (not parent)
         (string=? "0" (swl:tcl-eval 'winfo 'exists parent))
         (string=? "0" (swl:tcl-eval parent 'find 'withtag handle)))])
  (public
    [destroy ()
     ;* \ret{unspecified}
     ;* destroys the canvas figure
     ;; put this here because it's almost sure to be called ?
     (critical-section
       (when handle
         (send parent disown handle self)
         (tk-destroy)
         (set! parent #f)
         (set! handle #f)))]
    [move (xamt yamt)
     ;* \ret{unspecified}
     ;* move the figure by the specified x and y amounts
     ;* (positive or negative).
     (swl:safety-check
       (unless (and (swl:oknum? xamt) (swl:oknum? yamt))
         (assertion-violationf 'move "args must be numeric: ~s ~s~n" xamt yamt)))
     (swl:tcl-eval parent 'move handle xamt yamt)
     (void)]
    [get-coords ()
     ;* \ret{list}
     ;* Returns a list of the coordinates defining the canvas figure.
     (swl:tcl->scheme (swl:tcl-eval parent 'coords handle))]
    [scheme->tcl (op)
     ;% \ret{unspecified}
     (display handle op)]
    [set-parent! (p)
     ;* \ret{unspecified}
     ;* (currently unimplemented)
(assertion-violationf 'implementation "have to do set-parent! still")]
    [get-parent ()
     ;* \ret{see below}
     ;* Returns the canvas on which the figure is drawn.
     parent]
    [show ()
     ;* \ret{unspecified}
     ;* Makes the canvas item visible again after it has been hidden.
     (let ((coords (prop-ref 'coords)))
       (when coords (send-apply self set-coords! coords)))
     (void)]
    [hide ()
     ;* \ret{unspecified}
     ;* Makes the canvas item invisible.
     ;; total hack
     (prop-set! 'coords (send self get-coords))
     (swl:tcl-eval parent 'move handle -999999 -999999)
     (void)]
    [raise ()
     ;* \ret{unspecified}
     ;* Raises this figure in the display list of the canvas.
     (swl:tcl-eval parent 'raise handle)
     (void)]
    [raise (above-this)
     ;* \ret{unspecified}
     ;* Raises this figure above the given figure
     ;* in the display list of the canvas.
     (swl:safety-check
       (unless (isa-canvas-item? above-this)
         (assertion-violationf 'raise "~s is not a canvas figure" above-this)))
     (swl:tcl-eval parent 'raise handle above-this)
     (void)]
    [lower ()
     ;* \ret{unspecified}
     ;* Lowers this figure in the display list of the canvas.
     (swl:tcl-eval parent 'lower handle)
     (void)]
    [lower (below-this)
     ;* \ret{unspecified}
     ;* Lowers this figure above the given figure
     ;* in the display list of the canvas.
     (swl:safety-check
       (unless (isa-canvas-item? below-this)
         (assertion-violationf 'lower "~s is not a canvas figure" below-this)))
     (swl:tcl-eval parent 'lower handle below-this)
     (void)]
;    [scale (xorigin yorigin xscale yscale)
;     ;* Rescales the figure.  xorigin and yorigin identify the
;     ;* origin for the scaling operation and xscale and yscale
;     ;* identify the scale factors for x- and y- coordinates,
;     ;* respectively (a scale factor of 1.0 implies no change to
;     ;* that coordinate).  For each of the points defining the
;     ;* item, the x-coordinate is adjusted to change the distance
;     ;* from xorigin by a factor of xscale.  Similarly, each
;     ;* y-coordinate is adjusted to change the distance from
;     ;* yorigin by a factor of yscale.
;     (swl:safety-check
;       (unless handle (dead-error self 'scale))
;       (unless (and (swl:oknum? xorigin) (swl:oknum? yorigin)
;                    (swl:oknum? xscale) (swl:oknum? yscale))
;         (assertion-violationf 'scale "bad value(s) ~s ~s ~s ~s"
;            xorigin yorigin xscale yscale)))
;     (swl:tcl-eval parent 'scale handle xorigin yorigin xscale yscale)
;     (void)]
    [bounding-box ()
     ;* \ret{list}
     ;* Returns a list of the coordinates giving the bounding box of this
     ;* figure on the canvas.
     (swl:tcl->scheme (swl:tcl-eval parent 'bbox handle))]))

;; Note:  here "width:" is truly in screen-distance units
;;  this is one thing we might be able to improve by moving to a setter/getter
;;  model.

(define-swl-class (<2pt-figure> canvas x1 y1 x2 y2) (<canvas-item> canvas)
  ;* An abstract class for canvas items whose geometry is defined by
  ;* two points.
  (ivars)
  (inherited parent handle)
  (inheritable parent handle)
  (private)
  (protected
    [tcl-name () 'ERROR])
  (public
    [init (p x1 y1 x2 y2)
     ;* \ret{unspecified}
     ;* When creating an instance of this class four numbers
     ;* must be given defining the top-left and bottom-right
     ;* coordinates of a rectangular region enclosing the figure.
     (swl:safety-check
       (unless (and (swl:oknum? x1) (swl:oknum? y1) (swl:oknum? x2) (swl:oknum? y2))
         (coords-error 'create (list x1 y1 x2 y2))))
     (set! handle
       (string->number
         (swl:tcl-eval parent 'create (tcl-name) x1 y1 x2 y2)))
     (post-init)]
    [set-coords! (x1 y1 x2 y2)
     ;* \ret{unspecified}
     ;* Sets the coordinates defining the figure to
     ;* \var{x1}, \var{y1}, \var{x2}, \var{y2}.
     (swl:safety-check
       (unless (and (swl:oknum? x1) (swl:oknum? y1) (swl:oknum? x2) (swl:oknum? y2))
         (coords-error 'set-coords! (list x1 y1 x2 y2))))
     (swl:tcl-eval parent 'coords handle x1 y1 x2 y2)
     (void)]
    [set-fill-color! (val)
     ;* \ret{unspecified}
     ;* Sets the color with which the interior of the figure is filled.
     ;* \var{val} is either the symbol \mytt{transparent}, a symbol naming a
     ;* color in \mytt{rgb.txt}, or
     ;* an instance of \scheme{<rgb>}.
     (set-color! '-fill val 'set-fill-color!)]
    [get-fill-color ()
     ;* \ret{see below}
     ;* Returns the color with which the interior of the figure is filled.
     ;* The value returned is either the symbol \mytt{transparent}, a symbol
     ;* naming a color in \mytt{rgb.txt}, or
     ;* an instance of \scheme{<rgb>}.
     (get-color '-fill 'get-fill-color)]
    [set-outline-color! (val)
     ;* \ret{unspecified}
     ;* Sets the color in which the outline of the figure is drawn.
     ;* \var{val} is either a symbol naming a color in \mytt{rgb.txt} or
     ;* an instance of \scheme{<rgb>}.
     (set-color! '-outline val 'set-outline-color!)]
    [get-outline-color ()
     ;* \ret{see below}
     ;* Returns the color in which the outline of the figure is drawn.
     ;* The value returned is either a symbol naming a color in \mytt{rgb.txt} or
     ;* an instance of \scheme{<rgb>}.
     (get-color '-outline 'get-outline-color)]
    [set-line-thickness! (val)
     ;* \ret{unspecified}
     ;* Sets the thickness in pixels of the outline drawn around the figure.
     (set-tk-option '-width val swl:distance-unit? 'set-line-thickness!)]
    [get-line-thickness ()
     ;* \ret{see below}
     ;* Returns the thickness in pixels of the outline drawn around the figure.
     (get-tk-option '-width string->number 'get-line-thickness)]
    [set-stipple! (val)
     ;* \ret{unspecified}
     ;* (currently unimplemented)
     (set-tk-option '-stipple val swl:bitmap? 'set-stipple!)]
    [get-stipple ()
     ;* \ret{see below}
     ;* (currently unimplemented)
     (get-tk-option '-stipple tk->bitmap 'get-stipple)]))


(swl:api-class (<arc> canvas x1 y1 x2 y2) (<2pt-figure> canvas x1 y1 x2 y2)
  ;% \swlapi{class}{<arc>}{(create <arc> canvas x1 y1 x2 y2)}
  ;* \ret{instance}
  ;* An arc is a section of an
  ;* oval delimited by two angles (see \scheme{set-start!} and \scheme{set-end!}).
  ;* The arc is inscribed within the rectangular region defined by
  ;* \var{x1}, \var{y1}, \var{x2}, \var{y2}.
  ;* An arc can be displayed as a pieslice, chord, or arc
  ;* (see \scheme{set-style!}) and can be filled with a color or have an
  ;* outline or both.  The thickness and color of the outline
  ;* can be specified as well.
  (ivars)
  (inherited parent handle)
  (inheritable)
  (private)
  (protected
    [tcl-name () 'arc])
  (public
    [set-style! (val)
     ;* \ret{unspecified}
     ;* Determines how the arc is displayed.  \var{val} is either
     ;* \scheme{arc}, \scheme{chord}, or \scheme{pieslice}.
     (set-tk-option '-style val
       (lambda (x) (memq x '(arc chord pieslice)))
       'set-style!)]
    [get-style()
     ;* \ret{see below}
     ;* Returns a symbol describing how the arc is displayed.
     ;* The value returns is either \scheme{arc}, \scheme{chord}, or \scheme{pieslice}.
     (get-tk-option '-style string->symbol 'get-style)]
    [set-start! (val)
     ;* \ret{unspecified}
     ;* Sets the position in degrees where the arc begins.
     (set-tk-option '-start val swl:oknum? 'set-start!)]
    [get-start()
     ;* \ret{see below}
     ;* Returns the position in degrees where the arc begins.
     (get-tk-option '-start string->number 'get-start)]
    [set-extent! (val)
     ;* \ret{unspecified}
     ;* Sets the extent in degrees of the arc.
     (set-tk-option '-extent val swl:oknum? 'set-extent!)]
    [get-extent()
     ;* \ret{see below}
     ;* Returns the extent in degrees of the arc.
     (get-tk-option '-extent string->number 'get-extent)]))


(swl:api-class (<oval> canvas x1 y1 x2 y2) (<2pt-figure> canvas x1 y1 x2 y2)
  ;% \swlapi{class}{<oval>}{(create <oval> canvas x1 y1 x2 y2)}
  ;* \ret{instance}
  ;* An oval appears as a circle or oval on the canvas that
  ;* may be filled with a color, or have an outline, or both.
  ;* The oval is inscribed within the rectangular region defined by
  ;* \var{x1}, \var{y1}, \var{x2}, \var{y2}.
  ;* The thickness and color of the oval's outline
  ;* can be specified as well.
  (ivars)
  (inherited)
  (inheritable)
  (private)
  (protected
    [tcl-name () 'oval])
  (public))


(swl:api-class (<rectangle> canvas x1 y1 x2 y2) (<2pt-figure> canvas x1 y1 x2 y2)
  ;% \swlapi{class}{<rectangle>}{(create <rectangle> canvas x1 y1 x2 y2)}
  ;* \ret{instance}
  ;* A rectangle appears as a rectangular region on the canvas and may be
  ;* filled, have an outline or both.
  ;* The region covered by the rectangle is defined by
  ;* \var{x1}, \var{y1}, \var{x2}, \var{y2}.
  ;* The thickness and color of the outline
  ;* can be specified as well.
  (ivars)
  (inherited)
  (inheritable)
  (private)
  (protected
    [tcl-name () 'rectangle])
  (public))


(define-swl-class (<multi-pt-figure> canvas) (<canvas-item> canvas)
  ;* An abstract class for figures that can be defined by several
  ;* points (lines and polygons).
  (ivars)
  (inherited parent handle)
  (inheritable parent handle)
  (private)
  (protected)
  (public
    [set-fill-color! (val)
     ;* \ret{unspecified}
     ;* Sets the color with which the interior of the figure is filled.
     ;* \var{val} is either the symbol \mytt{transparent}, a symbol naming a
     ;* color in \mytt{rgb.txt}, or
     ;* an instance of \scheme{<rgb>}.
     (set-color! '-fill val 'set-fill-color!)]
    [get-fill-color ()
     ;* \ret{see below}
     ;* Returns the color with which the interior of the figure is filled.
     ;* The value returned is either the symbol \mytt{transparent}, a symbol
     ;* naming a color in \mytt{rgb.txt}, or
     ;* an instance of \scheme{<rgb>}.
     (get-color '-fill 'get-fill-color)]
    [set-line-thickness! (val)
     ;* \ret{unspecified}
     ;; could get via inheritance, not worth it?
     ;* Sets the thickness in pixels of the outline drawn around the figure.
     (set-tk-option '-width val swl:distance-unit? 'set-line-thickness!)]
    [get-line-thickness ()
     ;* \ret{see below}
     ;; could get via inheritance, not worth it?
     ;* Returns the thickness in pixels of the outline drawn around the figure.
     (get-tk-option '-width string->number 'get-line-thickness)]
    [set-stipple! (val)
     ;* \ret{unspecified}
     ;* (currently unimplemented)
     (set-tk-option '-stipple val swl:bitmap? 'set-stipple!)]
    [get-stipple ()
     ;* \ret{see below}
     ;* (currently unimplemented)
     (get-tk-option '-stipple tk->bitmap 'get-stipple)]
    [set-draw-spline! (val)
     ;* \ret{unspecified}
     ;* \var{val} is a boolean that determines whether or not the outline
     ;* of the figure is drawn as a spline.
     (set-tk-option '-smooth val boolean? 'set-draw-spline!)]
    [get-draw-spline ()
     ;* \ret{boolean}
     ;* Returns a boolean that indicates whether or not the outline
     ;* of the figure is drawn as a spline.
     (get-tk-option '-smooth tk->boolean 'get-draw-spline)]
    [set-spline-steps! (val)
     ;* \ret{unspecified}
     ;* Sets the number of steps to use when drawing the spline.
     ;* If zero the figure is not drawn as a spline otherwise
     ;* a spline is drawn for every pair of line segments
     ;* using \var{val} segments for every pair
     (set-tk-option '-splinesteps val
       (lambda (x) (and (fixnum? x) (fxpositive? x)))
       'set-spline-steps!)
     (set-tk-option '-smooth (not (fxzero? val)) boolean? #f)]
    [get-spline-steps ()
     ;* \ret{integer}
     ;* Returns the number of steps to be used when drawing the spline.
     ;* If zero the figure is not drawn as a spline.
     (if (get-tk-option '-smooth tk->boolean 'get-spline-steps)
         (get-tk-option '-splinesteps string->number 'get-spline-steps)
         0)]))


(swl:api-class (<line> canvas x1 y1 x2 y2 . more) (<multi-pt-figure> canvas)
  ;% \swlapi{class}{<line>}{(create <line> canvas x1 y1 x2 y2 \dots)}
  ;* \ret{instance}
  ;* Lines are displayed as one or more connected line segments or
  ;* curves.  The \scheme{init} method requires coordinates for a series of
  ;* two or more end-points of a series of connected line segments (or
  ;* curves).  Lines can have arrow heads at either or both ends, and the
  ;* shape of the arrowhead can be adjusted.  The end-points of a
  ;* line can drawn rounded, projecting, etc.  The joints between line
  ;* segments (or curves) can be drawn beveled, mitered, or rounded.
  ;* The line can be smoothed as a spline, and the degree of
  ;* smoothness can be specified.  Width can also be specified.
  (ivars)
  (inherited parent handle)
  (inheritable)
  (private)
  (protected)
  (public
    ;; could use cool object system dispatch to determine if we have rest args
    [init (p x1 y1 x2 y2 . more)
     ;* \ret{unspecified}
     ;* When creating a line, at least four numbers
     ;* must be given defining the endpoints of the line.
     ;* Additional coordinates specify endpoints of connecting
     ;* line segments.
     (swl:safety-check
       (unless (and (swl:oknum? x1) (swl:oknum? y1) (swl:oknum? x2) (swl:oknum? y2)
                    (andmap swl:oknum? more)
                    (fxeven? (length more)))
         (coords-error 'create (list* x1 y1 x2 y2 more))))
     (set! handle
       (string->number
         (if (null? more)
             (swl:tcl-eval parent 'create 'line x1 y1 x2 y2)
             (apply swl:tcl-eval parent 'create 'line x1 y1 x2 y2 more))))
     (post-init)]
    [set-coords! (x1 y1 x2 y2 . more)
     ;* \ret{unspecified}
     ;* Sets the coordinates for the line to 
     ;* \var{x1}, \var{y1}, \var{x2}, \var{y2}, etc.
     (swl:safety-check
       (unless (and (swl:oknum? x1) (swl:oknum? y1) (swl:oknum? x2) (swl:oknum? y2)
                    (andmap swl:oknum? more)
                    (fxeven? (length more)))
         (coords-error 'set-coords! (list* x1 y1 x2 y2 more))))
     (if (null? more)
         (swl:tcl-eval parent 'coords handle x1 y1 x2 y2)
         (apply swl:tcl-eval parent 'coords handle x1 y1 x2 y2 more))
     (void)]
    [set-arrow-shape! (inner-length outer-length flair)
     ;* \ret{unspecified}
     ;* Sets the shape of the arrowheads drawn on the line.
     ;* \var{inner-length} gives the distance along the line
     ;* from the tip of the arrow head to its neck,
     ;* \var{outer-length} gives the distance along the line
     ;* to which the wide part of the arrowhead should extend,
     ;* and \var{flair} describes how far out from the line the
     ;* wide part of the arrowhead should extend.
     (swl:safety-check
       (unless (and (swl:distance-unit? inner-length)
                    (swl:distance-unit? outer-length)
                    (swl:distance-unit? flair))
         (assertion-violationf 'set-arrow-shape! "bad value(s) ~s ~s ~s"
           inner-length outer-length flair)))
     (set-tk-option '-arrowshape (list inner-length outer-length flair)
       (lambda (x) x)
       'set-arrow-shape!)]
    [get-arrow-shape ()
     ;* \ret{see below}
     ;* Returns a list of three numbers describing the shape of the arrowhead
     ;* see \scheme{set-arrow-shape!} for details.
     (get-tk-option '-arrowshape swl:tcl->scheme 'get-arrow-shape)]
    [set-arrow-style! (val)
     ;* \ret{unspecified}
     ;* Determines where the arrowheads are drawn.  \var{val} is either
     ;* \scheme{none}, \scheme{first}, \scheme{last}, or \scheme{both}.
     (set-tk-option '-arrow val
       (lambda (x) (memq x '(none first last both)))
       'set-arrow-style!)]
    [get-arrow-style ()
     ;* \ret{see below}
     ;* Returns a symbol describing where on the line the arrowheads
     ;* should be drawn.
     (get-tk-option '-arrow string->symbol 'get-arrow-style)]
    [set-join-style! (val)
     ;* \ret{unspecified}
     ;* Determines how the articulations between line segments are
     ;* drawn.  \var{val} is either \scheme{bevel}, \scheme{miter}, or \scheme{round}.
     ;* The difference is only appreciable for thick lines.
     (set-tk-option '-joinstyle val
       (lambda (x) (memq x '(bevel miter round)))
       'set-join-style!)]
    [get-join-style ()
     ;* \ret{see below}
     ;* Returns a symbol describing how the joints between line segments are
     ;* drawn.
     (get-tk-option '-joinstyle string->symbol 'get-join-style)]
    [set-cap-style! (val)
     ;* \ret{unspecified}
     ;* Determines how caps are drawn at the endpoints of the line.
     ;* \var{val} is either \scheme{butt}, \scheme{projecting}, or \scheme{round}.
     (set-tk-option '-capstyle val
       (lambda (x) (memq x '(butt projecting round)))
       'set-cap-style!)]
    [get-cap-style ()
     ;* \ret{see below}
     ;* Returns a symbol describing
     ;* how caps are drawn at the endpoints of the line.
     (get-tk-option '-capstyle string->symbol 'get-cap-style)]))


(swl:api-class (<polygon> canvas x1 y1 x2 y2 x3 y3 . more)
           (<multi-pt-figure> canvas)
  ;% \swlapi{class}{<polygon>}{(create <polygon> canvas x1 y1 x2 y2 x3 y3 \dots)}
  ;* \ret{instance}
  ;* A polygon is a closed figure with three or more points
  ;* and may be filled or drawn with an outline.
  (ivars)
  (inherited parent handle)
  (inheritable)
  (private)
  (protected)
  (public
    ;; could use cool object system dispatch to determine if we have rest args
    [init (p x1 y1 x2 y2 x3 y3 . more)
     ;* \ret{unspecified}
     ;* When creating a polygon, at least six numbers
     ;* must be given defining the vertices of the polygon.
     ;* Additional coordinates specify additional vertices.
     (swl:safety-check
       (unless (and (swl:oknum? x1) (swl:oknum? y1) (swl:oknum? x2) (swl:oknum? y2)
                    (swl:oknum? x3) (swl:oknum? y3)
                    (andmap swl:oknum? more)
                    (fxeven? (length more)))
         (coords-error 'create (list* x1 y1 x2 y2 x3 y3 more))))
     (set! handle
       (string->number
         (if (null? more)
             (swl:tcl-eval parent 'create 'polygon x1 y1 x2 y2 x3 y3)
             (apply swl:tcl-eval parent 'create 'polygon x1 y1 x2 y2 x3 y3 more))))
     (post-init)]
    [set-coords! (x1 y1 x2 y2 x3 y3 . more)
     ;* \ret{unspecified}
     ;* Sets the coordinates of the polygon to
     ;* \var{x1}, \var{y1}, \var{x2}, \var{y2},
     ;* \var{x3}, \var{y3}, etc.
     (swl:safety-check
       (unless (and (swl:oknum? x1) (swl:oknum? y1) (swl:oknum? x2) (swl:oknum? y2)
                    (swl:oknum? x3) (swl:oknum? y3)
                    (andmap swl:oknum? more)
                    (fxeven? (length more)))
         (coords-error 'set-coords! (list* x1 y1 x2 y2 x3 y3 more))))
     (if (null? more)
         (swl:tcl-eval parent 'coords handle x1 y1 x2 y2 x3 y3)
         (apply swl:tcl-eval parent 'coords handle x1 y1 x2 y2 x3 y3 more))
     (void)]
    [set-outline-color! (val)
     ;* \ret{unspecified}
     ;* Sets the color in which the outline of the figure is drawn.
     ;* \var{val} is either a symbol naming a color in \mytt{rgb.txt} or
     ;* an instance of \scheme{<rgb>}.
     (set-tk-option '-outline val swl:color? 'set-outline-color!)]
    [get-outline-color ()
     ;* \ret{see below}
     ;* Returns the color in which the outline of the figure is drawn.
     ;* The value returned is either a symbol naming a color in \mytt{rgb.txt} or
     ;* an instance of \scheme{<rgb>}.
     (get-tk-option '-outline tk->color 'get-outline-color)]))


(define-swl-class (<1pt-figure> canvas x1 y1) (<canvas-item> canvas)
  ;* An abstract class for figures whose position on the canvas
  ;* is defined by a single point that anchors the figure.
  (ivars)
  (inherited parent handle)
  (inheritable parent handle)
  (private)
  (protected
    [tcl-name () 'ERROR])
  (public
    [init (p x1 y1)
     ;* \ret{unspecified}
     ;* Two numbers must be given when creating an instance of this class.
     (swl:safety-check
       (unless (and (swl:oknum? x1) (swl:oknum? y1))
         (coords-error 'create (list x1 y1))))
     (set! handle
       (string->number
         (swl:tcl-eval parent 'create (tcl-name) x1 y1)))
     (post-init)]
    [set-coords! (x1 y1)
     ;* \ret{unspecified}
     ;* Sets the coordinates of the anchor for this figure to \var{x1}, \var{y1}.
     (swl:safety-check
       (unless (and (swl:oknum? x1) (swl:oknum? y1))
         (coords-error 'set-coords! (list x1 y1))))
     (swl:tcl-eval parent 'coords handle x1 y1)
     (void)]
    [set-anchor! (val)
     ;* \ret{unspecified}
     ;* Determines where the figure is displayed relative to its
     ;* coordinates.
     ;* Legal values are \scheme{n}, \scheme{s}, \scheme{e}, \scheme{w}, \scheme{ne}, \scheme{se},
     ;* \scheme{sw}, \scheme{nw}, and \scheme{center}.
     (set-tk-option '-anchor val swl:anchor? 'set-anchor!)]
    [get-anchor ()
     ;* \ret{symbol}
     ;* Returns a symbol describing where the figure is displayed
     ;* relative to its coordinates.
     (get-tk-option '-anchor string->symbol 'get-anchor)]))


(swl:api-class (<canvas-image> canvas x1 y1) (<1pt-figure> canvas x1 y1)
  ;% \swlapi{class}{<canvas-image>}{(create <canvas-image> canvas x y)}
  ;* \ret{instance}
  ;* Images can display bitmaps, and images in GIF, or PPM, and PGM format.
  ;* They are not yet finished.
  (ivars (img #f))
  (inherited parent handle)
  (inheritable)
  (private)
  (protected
    [tcl-name () 'image])
  (public
    [set-image! (val)
     ;* \ret{unspecified}
     ;* Sets the image to display in this canvas item to \var{val} which
     ;* should be an instance of \scheme{<bitmap>} or \scheme{<photo>}.
     (set! img val)
     (set-tk-option '-image val isa-image? 'set-image!)]
    [get-image()
     ;* \ret{see below}
     ;* Returns the image to displayed by this canvas item.
     (get-tk-option '-image (lambda (x) (swl:lookup-widget (string->symbol x)))
       'get-image)]))


(swl:api-class (<canvas-text> canvas x1 y1) (<1pt-figure> canvas x1 y1)
  ;% \swlapi{class}{<canvas-text>}{(create <canvas-text> canvas x y)}
  ;* \ret{instance}
  ;* A canvas-text displays a string of characters in a canvas window.
  ;* The main differences between this and a \scheme{<text>} widget are that
  ;* \scheme{<canvas-text>} is a graphics object (so other figures can show
  ;* through or partially occlude the displayed string), and this
  ;* class lacks some of the fancier text-manipulation ability of
  ;* \scheme{<text>}.
  (ivars)
  (inherited parent handle)
  (inheritable)
  (private)
  (protected
    [tcl-name () 'text])
  (public
    [set-title! (val)
     ;* \ret{unspecified}
     ;* Sets the string displayed by the figure.
     (set-tk-option '-text val string? 'set-title!)]
    [get-title ()
     ;* \ret{see below}
     ;* Returns the string displayed by the figure.
     (get-tk-option '-text (lambda (x) x) 'get-title)]
    [set-fill-color! (val)
     ;* \ret{unspecified}
     ;* Sets the color with which the interior of the figure is filled.
     ;* \var{val} is either the symbol \mytt{transparent}, a symbol naming a
     ;* color in \mytt{rgb.txt}, or
     ;* an instance of \scheme{<rgb>}.
     (set-color! '-fill val 'set-fill-color!)]
    [get-fill-color ()
     ;* \ret{see below}
     ;* Returns the color with which the interior of the figure is filled.
     ;* The value returned is either the symbol \mytt{transparent}, a symbol
     ;* naming a color in \mytt{rgb.txt}, or
     ;* an instance of \scheme{<rgb>}.
     (get-color '-fill 'get-fill-color)]
    [set-font! (val)
     ;* \ret{unspecified}
     ;* Sets the font for the text displayed.
     ;* \var{val} is an instance of \scheme{<font>} (see the description
     ;* of the \scheme{make-font} macro).
     (set-tk-option '-font val swl:font? 'set-font!)]
    [get-font ()
     ;* \ret{see below}
     ;* Returns an instance of \scheme{<font>} describing the font
     ;* for text displayed by the figure.
     (get-tk-option '-font tk->font 'get-font)]
    [set-stipple! (val)
     ;* \ret{unspecified}
     ;* (unimplemented)
     (set-tk-option '-stipple val swl:bitmap? 'set-stipple!)]
    [get-stipple ()
     ;* \ret{see below}
     ;* (unimplemented)
     (get-tk-option '-stipple tk->bitmap 'get-stipple)]
    [set-justify! (val)
     ;* \ret{unspecified}
     ;* Determines how lines are to be justified when multiple lines
     ;* are displayed.  Legal values are \scheme{left}, \scheme{right}, and \scheme{center}.
     (set-tk-option '-justify val swl:justify? 'set-justify!)]
    [get-justify ()
     ;* \ret{symbol}
     ;* Returns a symbol describing how lines are to be justified when
     ;* multiple lines
     ;* are displayed.
     (get-tk-option '-justify string->symbol 'get-justify)]
    [set-width! (val)
     ;* \ret{unspecified}
     ;* Lines longer than \var{val} characters will be wrapped onto the
     ;* next line.  If zero, lines break only where there are
     ;* newline characters in the displayed string.
     (set-tk-option '-width val swl:distance-unit? 'set-width!)]
    [get-width ()
     ;* \ret{integer}
     ;* Returns the width of the canvas text used in line breaking.
     (get-tk-option '-width string->number 'get-width)]))


;; Eventually
;; want to use this to make it all transparent so that people can
;; create buttons, etc. and just specify that they want to put it on
;; a canvas at x,y and bang, it happens.

(swl:api-class (<canvas-sub-window> canvas x1 y1) (<1pt-figure> canvas x1 y1)
  ;% \swlapi{class}{<canvas-sub-window>}{(create <canvas-sub-window> canvas x y)}
  ;* \ret{instance}
  ;* A canvas-sub-window allows other widgets to be placed on a canvas
  ;* anchored at a particular point.  We hope to make this process
  ;* transparent soon. 
  (ivars)
  (inherited parent handle)
  (inheritable)
  (private)
  (protected
    [tcl-name () 'window])
  (public
    [set-window! (val)
     ;* \ret{unspecified}
     ;* \var{val} is a widget to be placed on the canvas.
     (prop-set! 'window val)
     (set-tk-option '-window val isa-tk-widget? 'set-window!)]
    [get-window ()
     ;* \ret{see below}
     ;* Returns the widget placed on the canvas in this sub-window.
     (prop-ref 'window)]
    [set-height! (val)
     ;* \ret{unspecified}
     ;* Sets the height in pixels of the sub-window.
     (set-tk-option '-height val swl:distance-unit? 'set-height!)]
    [get-height ()
     ;* \ret{integer}
     ;* Returns the height in pixels of the sub-window.
     (get-tk-option '-height string->number 'get-height)]
    [set-width! (val)
     ;* \ret{unspecified}
     ;* Sets the width in pixels of the sub-window.
     (set-tk-option '-width val swl:distance-unit? 'set-width!)]
    [get-width ()
     ;* \ret{integer}
     ;* Returns the width in pixels of the sub-window.
     (get-tk-option '-width string->number 'get-width)]))

(define isa-canvas-item? (lambda (x) (send x isa? <canvas-item>)))

