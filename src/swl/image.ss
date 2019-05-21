;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Photo, Bitmap
;;

; have the objects that have a set-image! method hold onto the image (GC)

(define-swl-class (<image> type)
              (<tk-prewidget>
                (let ([handle (swl:new-handle #f)])
                  (swl:tcl-eval 'image 'create type handle)
                  handle))
  ;* Images are used to display GIF and raw PPM pictures in other
  ;* widgets.  The \var{filename} argument specifies the file containing
  ;* the image data.
  (ivars)
  (inherited handle)
  (inheritable handle)
  (private)
  (protected
    ;; should rearrange to get this via inheritance
    [set-tk-option (tclname value err-check name)
     (swl:safety-check
       (unless (err-check value) (assertion-violationf name "bad value ~s" value)))
     (swl:tcl-eval handle 'config tclname value)
     (#3%void)]
    [get-tk-option (tclname conversion name)
     (conversion (swl:tcl-eval handle 'cget tclname))])
  (public
    [destroy ()
     (swl:tcl-eval 'image 'delete handle)
     (send-base self destroy)]
    [set-filename! (fn)
     ;* \ret{unspecified}
     ;* Set the image filename.
     (set-tk-option '-file fn string? 'set-filename!)]
    [get-filename ()
     ;* \ret{string}
     ;* Returns the name of the file associated with the instance.
     (get-tk-option '-file (lambda (x) x) 'get-filename)]
    [get-height ()
     ;* \ret{integer}
     ;* Returns the height, in pixels, of the given image.
     (string->number (swl:tcl-eval 'image 'height handle))]
    [get-width ()
     ;* \ret{integer}
     ;* Returns the width, in pixels, of the given image.
     (string->number (swl:tcl-eval 'image 'width handle))]))

(define isa-image? (lambda (x) (and (instance? x) (isa? x <image>))))

(swl:api-class (<bitmap>) (<image> 'bitmap)
  ;% \swlapi{class}{<bitmap>}{(create <bitmap>)}
  ;* \ret{instance}
  ;;
  ;* This class manipulates monochrome bitmap images which can
  ;* be used as the \scheme{image:} attribute
  ;* for widgets such as \scheme{<label>} that can display images.
  ;* The \var{filename} argument specifies the name of a file
  ;* containing the (X11) bitmap data.
  (ivars)
  (inherited handle)
  (inheritable)
  (private)
  (protected)
  (public
    [set-data! (string)
     ;* \ret{unspecified}
     ;* Sets the bitmap data to be displayed by this \scheme{<bitmap>}.
     ;* The string data should be in the format of an X11 bitmap file
     ;* (a stylized C program).
     ;; (The procedure swl:file->string is convenient for reading the
     ;; data from a file.)
     (set-tk-option '-data string string? 'set-data!)]
    [get-data ()
     ;* \ret{see below}
     ;* No idea if this will really work reliably.
     (get-tk-option '-data (lambda (x) x) 'get-data)]
    [set-mask-data! (string)
     ;* \ret{unspecified}
     ;* Sets the mask bitmap for this \scheme{<bitmap>}.
     ;* The string should be in the format of an X11 bitmap file
     ;* (a stylized C program).
     ;; (The procedure swl:file->string is convenient for reading the
     ;; mask data from a file.)
     (set-tk-option '-maskdata string string? 'set-mask-data!)]
    [get-mask-data ()
     ;* \ret{see below}
     ;* No idea if this will really work reliably.
     (get-tk-option '-maskdata (lambda (x) x) 'get-mask-data)]
    [set-foreground-color! (val)
     ;* \ret{unspecified}
     ;* Sets the foreground color for the bitmap to \var{val},
     ;* which is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (set-tk-option '-foreground val swl:color? 'set-foreground-color!)]
    [get-foreground-color ()
     ;* \ret{see below}
     ;* Returns the foreground color for the bitmap.
     ;* The value returned is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (get-tk-option '-foreground tk->color 'get-foreground-color)]
    [set-background-color! (val)
     ;* \ret{unspecified}
     ;* Sets the background color for the bitmap to \var{val},
     ;* which is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (set-tk-option '-background val swl:color? 'set-background-color!)]
    [get-background-color ()
     ;* \ret{see below}
     ;* Returns the background color for the bitmap.
     ;* The value returned is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (get-tk-option '-background tk->color 'get-background-color)]
   ))

(swl:api-class (<photo>) (<image> 'photo)
  ;% \swlapi{class}{<photo>}{(create <photo>)}
  ;* \ret{instance}
  ;;
  ;* This class manipulates full color images which can
  ;* be used as the \scheme{image:} attribute
  ;* for widgets such as \scheme{<label>} that can display images.
  ;* The \var{filename} argument specifies the name of a file
  ;* containing the (GIF or PPM) photo data.
  (ivars)
  (inherited handle)
  (inheritable)
  (private)
  (protected)
  (public
    [set-filename! (fn)
     ;* \ret{unspecified}
     ;* Unfortunately the Tk documentation is pretty sketchy about photo
     ;* support, so it is only possible to specify image data via filename
     ;* at present.
     (set-tk-option '-file fn string? 'set-filename!)]
    [get-filename ()
     ;* \ret{string}
     ;* Returns the name of the file associated with the \scheme{<photo>}.
     (get-tk-option '-file (lambda (x) x) 'get-filename)]
    [set-data! (string)
     ;* \ret{unspecified}
     ;* Sets the bitmap data to be displayed by this \scheme{<photo>}.
     ;* The string data should be base64-encoded GIF or PBM data.
     ;* (
     ;; The procedure \scheme{swl:file->string} is convenient for reading
     ;; the data from a file, and
     ;* The \scheme{swl:base64-encode} procedure in the \scheme{swl:base64}
     ;* module can be used to
     ;* encode the data for consumption by the underlying Tk library.)
     (set-tk-option '-data string string? 'set-data!)]
    [get-data ()
     ;* \ret{see below}
     ;* Returns a base64-encoded string representing the image data.
     (get-tk-option '-data (lambda (x) x) 'get-data)]
  ))

; (define swl:file->string
;   (lambda (filename)
;     (let* ([ip (open-input-file filename)]
;            [len (file-length ip)]
;            [s (make-string len)])
;       (let ([ok? (= len (block-read ip s size))])
;         (close-input-port ip)
;         (unless ok? (assertion-violationf 'swl:file->string "unable to read the entire file?"))
;         s))))
 
