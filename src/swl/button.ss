;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Button
;;

;;   its kind of up to the users, it may be syntactically more convenient to
;;   change the style by typing <checkbutton> instead of <button>
;;
;; currently somewhat inclined to merge all these button type into one thing
;; and specify some kind of (button-style: 'plain / 'check / 'radio / 'menu)
;; thing.  after considering the above paragraph I'm not sure anymore.

;; anyway, I'm imagining we'll have higher-level classes that, say, do the
;; right thing when you want to have several exclusive choice button things,
;; etc.

;;; Button, Checkbutton, Radiobutton, and Menubutton
;;;
;;;  Note:
;;;    the activate and deactivate methods are marked as "obsolete" and
;;;    "to be removed" in the Tk docs.  they can be easily replaced by
;;;    using the configure -state {active,normal} business.
;;
;;   I'm inclined to think enable / disable methods make more sense than
;;   the -state stuff, should also work for menus  (we're inheriting these
;;   from <modal-widget> now)
;;
;;   these names also make more sense in light of checkbutton select deselect

  ; may want to get rid of active-{fore,back}ground and just have the user
  ; bind the enter & leave events if they care about the color...

  ; skipping padx: pady: for now in case we make those into tk-widget opts


(swl:api-class (<button> parent) (<proto-button> parent 'button)
  ;% \swlapi{class}{<button>}{(create <button> parent)}
  ;* \ret{instance}
  ;* A button displays a text string or image and performs the
  ;* action specified by \scheme{set-action!} when pressed.  By default
  ;* a button can be pressed by clicking mouse button 1 when
  ;* the mouse cursor is over the button, or by pressing \mytt{Return} or
  ;* \mytt{Space} when the button has keyboard focus.
  ;* 
  ;* Other default bindings arrange to change the appearance of the
  ;* button when the mouse enters or leaves the button or when the
  ;* button has keyboard focus.
  ;* When the button is pressed, its 3-D border is changed so that it
  ;* appears to have been pressed.  These behaviors
  ;* can be changed by the \scheme{bind} method.
  ;*
  ;* Buttons can be made responsive or unresponsive to user input
  ;* via the \scheme{set-enabled!} method.
  ;*
  ;* Text is displayed in a uniform font (determined by \scheme{set-font!})
  ;* and is displayed as multiple lines if the text contains newlines
  ;* or its length exceeds the wrap length (see \scheme{set-wrap-length!}).
  ;* The text displayed by a button can be changed by the \scheme{set-title!}
  ;* method.
  (ivars)
  (inherited handle)
  (inheritable handle)
  (private)
  (protected)
  (public))


(swl:api-class (<checkbutton> parent) (<proto-fancybutton> parent 'checkbutton)
  ;% \swlapi{class}{<checkbutton>}{(create <checkbutton> parent)}
  ;* \ret{instance}
  ;* A checkbutton displays a text string or image and a small square called
  ;* the indicator, whichs shows whether or not the button is selected.
  ;* When the button is selected the indicator is colored.
  ;* The \scheme{set-draw-indicator!} method determines whether the state
  ;* is depicted by drawing an indicator or by changing the appearance of the
  ;* whole button.
  ;* The action specified by \scheme{set-action!} is performed if 
  ;* mouse button 1 is clicked when
  ;* the mouse cursor is over the checkbutton, or if the button has
  ;* keyboard focus and \mytt{Return} or \mytt{Space} is pressed.
  ;* 
  ;* Other default bindings arrange to change the appearance of the
  ;* checkbutton when the mouse enters or leaves the checkbutton or when the
  ;* checkbutton has keyboard focus.
  ;* When the checkbutton is pressed, its 3-D border is changed so that it
  ;* appears to have been pressed.
  ;* These behaviors can be changed by the \scheme{bind} method.
  ;*
  ;* Checkbuttons can be made responsive or unresponsive to user input
  ;* via the \scheme{set-enabled!} method.
  ;*
  ;* Text is displayed in a uniform font (determined by \scheme{set-font!})
  ;* and is displayed as multiple lines if the text contains newlines
  ;* or its length exceeds the wrap length (see \scheme{set-wrap-length!}).
  ;* The text displayed by a checkbutton can be changed by the \scheme{set-title!}
  ;* method.
  (ivars)
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public
    [toggle ()
     ;* \ret{unspecified}
     ;* toggles the selection state of the button.
     (button-action 'toggle)]))


(swl:api-class (<radiobutton> parent) (<proto-fancybutton> parent 'radiobutton)
  ;% \swlapi{class}{<radiobutton>}{(create <radiobutton> parent)}
  ;* \ret{instance}
  ;* A radiobutton displays a text string or image and a small diamond called
  ;* the indicator, whichs shows whether or not the button is selected.
  ;* When a radiobutton is selected the indicator is colored and all
  ;* other radiobuttons with the same parent are unselected.  Thus a group
  ;* of mutually-exclusive radiobuttons can be made by creating a frame
  ;* and creating the radiobuttons as children of the frame.
  ;* 
  ;* The \scheme{set-draw-indicator!} method determines whether the state
  ;* of the radiobutton
  ;* is depicted by drawing an indicator or by changing the appearance of the
  ;* whole button.
  ;* The action specified by \scheme{set-action!} is performed if 
  ;* mouse button 1 is clicked when
  ;* the mouse cursor is over the radiobutton, or if the button has
  ;* keyboard focus and \mytt{Return} or \mytt{Space} is pressed.
  ;* 
  ;* Other default bindings arrange to change the appearance of the
  ;* radiobutton when the mouse enters or leaves the radiobutton or when the
  ;* radiobutton has keyboard focus.
  ;* When the radiobutton is pressed, its 3-D border is changed so that it
  ;* appears to have been pressed.
  ;* These behaviors can be changed by the \scheme{bind} method.
  ;*
  ;* Radiobuttons can be made responsive or unresponsive to user input
  ;* via the \scheme{set-enabled!} method.
  ;*
  ;* Text is displayed in a uniform font (determined by \scheme{set-font!})
  ;* and is displayed as multiple lines if the text contains newlines
  ;* or its length exceeds the wrap length (see \scheme{set-wrap-length!}).
  ;* The text displayed by a radiobutton can be changed by the \scheme{set-title!}
  ;* method.
  (ivars)
  (inherited handle parent)
  (inheritable)
  (private)
  (protected)
  (public
    [init ignore-args
     ;; hack around Tk misfeature
     (swl:tcl-eval handle 'config '-val handle '-var parent)
     (send-base self init)]))

