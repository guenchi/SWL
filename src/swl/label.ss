;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Label
;;

;; currently leaving off padx: pady: since we may make that a widget option

(swl:api-class (<label> parent) (<proto-label> parent 'label)
  ;% \swlapi{class}{<label>}{(create <label> parent)}
  ;* \ret{instance}
  ;;
  ;* A label displays static (non-editable) text or an image.
  ;* Text is displayed in a uniform font (determined by \scheme{set-font!})
  ;* and is displayed as multiple lines if the text contains newlines
  ;* or its length exceeds the wrap length (see \scheme{set-wrap-length!}).
  ;* The text displayed by a label can be changed by the \scheme{set-title!}
  ;* method.  By default labels are not responsive to user interaction,
  ;* but this can be changed via \scheme{bind}.
  ;*
  ;* Support for bitmap, GIF, PPM, and PGM images is in the works.
  (ivars)
  (inherited)
  (inheritable)
  (private)
  (protected
    [class-name () '<label>])
  (public))

