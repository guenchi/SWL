;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; predicates and converters
;;

(module swl:option (mouse-cursor?
                    tk->mouse-cursor
                    <rgb>
                    string->color
                    tk->color
                    tk->boolean
                    swl:anchor?
                    swl:color?
                    swl:distance-unit?
                    swl:relief?
                    swl:oknum?
                    swl:justify?
                    cm->pixels
                    in->pixels
                    pt->pixels
                    <font>
                    swl:font?
                    tk->font
                    swl:font-families
                    make-font
                    <tab-stop>
                    swl:tabs?
                    tk->tabs
                    tab-stops
                    swl:font-available?
                    swl:get-named-colors
                    swl:get-rgb
                   )

(import swl:oop)
(import swl:macros)

(define mouse-cursor?
 ;; This is going to be totally non-portable, why did I even bother?
  (let ((x-mouse-cursors '(
        ; Because Tk returns "" for default mouse cursor, we use
        ; default as forged name instead.  Must keep this in sync
        ; with base2.ss mouse-cursor setter/getters, and in sync
        ; with tk->mouse-cursor below.
          default
          |X_cursor| arrow based_arrow_down based_arrow_up boat bogosity
          bottom_left_corner bottom_right_corner bottom_side bottom_tee
          box_spiral center_ptr circle clock coffee_mug cross
          cross_reverse crosshair diamond_cross dot dotbox double_arrow
          draft_large draft_small draped_box exchange fleur gobbler
          gumby hand1 hand2 heart icon iron_cross left_ptr left_side
          left_tee leftbutton ll_angle lr_angle man middlebutton mouse
          pencil pirate plus question_arrow right_ptr right_side
          right_tee rightbutton rtl_logo sailboat sb_down_arrow
          sb_h_double_arrow sb_left_arrow sb_right_arrow sb_up_arrow
          sb_v_double_arrow shuttle sizing spider spraycan star target
          tcross top_left_arrow top_left_corner top_right_corner
          top_side top_tee trek ul_angle umbrella ur_angle watch xterm)))
  (lambda (x)
    (or (memq x x-mouse-cursors)
        (and (pair? x)
             (let ((len (length x)))
               (if (fx= len 2)
                   (swl:color? (cadr x))
                   (and (fx= len 3)
                        (swl:color? (cadr x))
                        (swl:color? (caddr x))))))))))

;; it appears that the default Tk mouse cursor is top_left_arrow,
;; so we return that when Tk doesn't tell us what the cursor is
;; (ie. Tk returns "" and we don't have a provision yet for saying
;; set it to the default cursor)
(define tk->mouse-cursor
  (lambda (s)
    (let ((p (open-input-string s)))
      (let ((cursor-name
              (critical-section
                (parameterize ((case-sensitive #t))
                  (read p)))))
        (if (not (symbol? cursor-name))
            'default   ;; see mouse-cursor?
            (if (eof-object? (peek-char p))
                cursor-name
                (call-with-values
                  (lambda () (string->color s (port-input-index p)))
                  (lambda (fore next)
                    (call-with-values
                      (lambda () (string->color s next))
                      (lambda (back ignore)
                          (if back
                              (list cursor-name fore back)
                              (if fore
                                  (list cursor-name fore)
                                  cursor-name))))))))))))

(swl:api-class (<rgb> r g b) (<tk-object>)
  ;* \formdef{<rgb>}{class}{(make <rgb> \var{red} \var{green} \var{blue})}
  ;* \ret{instance}
  ;;
  ;* An instance of \scheme{<rgb>} contains three integer values
  ;* representing the red, green and blue components of
  ;* a particular color.
  (ivars (r (if (and (fixnum? r) (fx>= r 0))
                r
                (assertion-violationf 'rgb "bad value ~s" r)))
         (g (if (and (fixnum? g) (fx>= g 0))
                g
                (assertion-violationf 'rgb "bad value ~s" g)))
         (b (if (and (fixnum? b) (fx>= b 0))
                b
                (assertion-violationf 'rgb "bad value ~s" b)))
         (pad #f))
  (inherited)
  (inheritable)
  (private
    [digit->hex (n) (integer->char (fx+ n (if (fx< n 10) 48 55)))]
    [display-hex (n op)
     (unless pad
       ;; compute and cache value of pad for subsequent printings
       (let loop ([n (max r g b)] [c 1])
         (if (fx< n 16) (set! pad c) (loop (fxsra n 4) (fx+ c 1)))))
     (let loop ((n n) (digs 0))
       (if (fxzero? n)
           (let loop ((diff (fx- pad digs)))
             (when (fxpositive? diff)
               (write-char #\0 op)
               (loop (fx- diff 1))))
           (begin
             (loop (fxquotient n 16) (fx+ digs 1))
             (write-char (digit->hex (fxmodulo n 16)) op))))])
  (protected)
  (public
    [scheme->tcl (op)
     ;% \ret{unspecified}
     (display #\# op)
     (display-hex r op)
     (display-hex g op)
     (display-hex b op)]
    [rgb-red ()
     ;* \ret{an integer}
     ;* Returns the red component of the color.
      r]
    [rgb-green ()
     ;* \ret{an integer}
     ;* Returns the green component of the color.
     g]
    [rgb-blue ()
     ;* \ret{an integer}
     ;* Returns the blue component of the color.
     b]
    [rgb-values ()
     ;* \ret{see below}
     ;* Returns the red, green, and blue components of the color.
     (values r g b)]))

;; brutally inefficient
(define string->color
  (lambda (s start)
    (let ((end (string-length s)))
      (define scan-for
        (lambda (f s i)
          (cond
            ((fx>= i end) #f)
            ((f (string-ref s i)) i)
            (else (scan-for f s (fx+ i 1))))))
      (define getnum
        (lambda (s start digs)
          (let loop ((i (fx+ start digs -1)) (base 1) (ac 0))
            (if (fx< i start)
                ac
                (loop (fx- i 1)
                      (fx* base 16)
                      (fx+ ac (fx* base (char->value (string-ref s i)))))))))
      (define char->value
        (lambda (ch)
          (let ((n (char->integer ch)))
            (cond
              ((and (fx>= n 48) (fx<= n 57))
               (fx- n 48))
              ((and (fx>= n 65) (fx<= n 70))
               (fx- n 55))
              ((and (fx>= n 97) (fx<= n 102))
               (fx- n 87))))))
      (let ((start (scan-for (lambda (x) (char=? x #\#)) s start)))
        (if (not start)
            (let ([start
                   (scan-for (lambda (x) (not (char-whitespace? x)))
                      s (or start 0))])
              (values
                (if (and start (fx= start end))
                    #f
                    (string->symbol (substring s start end)))
                end))
            (let ((end (or (scan-for char-whitespace? s start) end))
                  (start (fx+ start 1)))
               (let ((digs (fxquotient (fx- end start) 3)))
                 (values
                   (make <rgb> (getnum s start digs)
                               (getnum s (fx+ start digs) digs)
                               (getnum s (fx+ start digs digs) digs))
                   end))))))))

(define swl:get-named-colors
  (lambda ()
    (include "../swl/rgb.ss")))

(define swl:get-rgb
  (lambda (x)
    (if (not (swl:color? x))
        (assertion-violationf 'swl:get-rgb "~s is not a valid color" x)
        (if (instance? x)
            (send x rgb-values)
            (let f ([ls (swl:get-named-colors)])
              (if (null? ls)
                  (assertion-violationf 'swl:get-rgb "~s is not a valid color" x)
                  (let ([info (car ls)])
                    (if (memq x (car info))
                        (apply values (cdr info))
                        (f (cdr ls))))))))))

(define tk->color
  (lambda (s)
    (call-with-values
      (lambda () (string->color s 0))
      (lambda (color ignore) color))))

(define swl:bitmap? 'unimplemented)
(define tk->bitmap 'unimplemented)

(define swl:image? 'unimplemented)
(define tk->image 'unimplemented)

(define tk->boolean (lambda (s) (string=? s "1")))

(define swl:anchor?
  (lambda (x) (and (memq x '(center n s e w nw sw ne se)) #t)))
(define swl:color?
  (let ([color-names (apply append (map car (include "../swl/rgb.ss")))])
    (lambda (x)
      (or (memq x color-names) (and (instance? x) (send x isa? <rgb>))))))
;(define swl:fill?
;  (lambda (x) (and (memq x '(both x y)) #t)))
(define swl:distance-unit? (lambda (x) (and (fixnum? x) (fx>= x 0))))
(define swl:relief?
  (lambda (x) (and (memq x '(raised sunken flat groove ridge)) #t)))
(define swl:oknum? (lambda (x) (or (fixnum? x) (flonum? x))))
(define swl:justify?
  (lambda (x) (and (memq x '(left right center)) #t)))

;; Optional widget can be passed in to get the conversion relative to
;; that widget (since it could be on a different display).

(define-syntax define-conversion
  (syntax-rules ()
    ((_ name r)
     (define name
       (lambda (n . widget)
         (unless (or (integer? n) (flonum? n))
           (assertion-violationf 'name "~s is not an integer or float" n))
         (let ((widget (if (null? widget)
                           #\.
                           (let ((t (car widget)))
                             (unless (isa? t <tk-widget>)
                               (assertion-violationf 'name "~s is not a widget" t))
                             t))))
           (let ([val
                  (string->number
                    (swl:tcl-eval 'winfo 'fpixels widget
                      (string-append
                        (number->string n 10)
                        (symbol->string 'r))))])
             (if (flonum? val) (flonum->fixnum val) val))))))))

(define-conversion
  ;* missing documentation
   cm->pixels c)
(define-conversion
  ;* missing documentation
   in->pixels i)
(define-conversion
  ;* missing documentation
   pt->pixels p)

(swl:api-class (<font> family size style) (<tk-object>)
  ;; \formdef{<font>}{class}{(create <font> \var{family} \var{size} \var{weight} \var{slant} \var{line})}
  ;* \ret{instance}
  ;;
  ;* Fonts are represented by instances of the \scheme{<font>} class.
  ;* (See also the \hpageref{make-font}{\scheme{make-font}} macro.)
  ;* A font is characterized by three parameters:
  ;*  \begin{description}
  ;*    \item[\var{family}] is a symbol that describes the typeface
  ;*         (eg. \scheme{times}, \scheme{helvetica}, \scheme{fixed}, etc.).
  ;*         If the requested family is not known to the local display,
  ;*         the behavior is unspecified.
  ;*         See \hpageref{swl:font-families}{\scheme{swl:font-families}}.
  ;*    \item[\var{size}] is an integer indicating the font size.
  ;*         Positive values indicate size in points, negative values
  ;*         indicate size in pixels.
  ;*    \item[\var{style}] is a list of symbols describing other
  ;*         desired attributes of the font.  The list may contain
  ;*         zero or more of the following symbols:  \scheme{normal},
  ;*         \scheme{bold}, \scheme{roman}, \scheme{italic},
  ;*         \scheme{underline}, and \scheme{overstrike}.
  ;*         If the list contains duplicates or opposites (eg. \scheme{roman}
  ;*         and \scheme{italic}), the behavior is unspecified.
  ;*  \end{description}
  ;* The display engine attempts to find the closest match for the
  ;* requested font.  Unfortunately the underlying graphics toolkit
  ;* provides no apparent way to determine whether a precise match was
  ;* found or some substitution was made.  By comparing actual font
  ;* attributes with the requested values, it is possible to determine
  ;* when the family or slant (\scheme{roman} or \scheme{italic})
  ;* request could not be satisfied.  However, there seems to be no way
  ;* to determine when a font was scaled to meet the requested size.
  ;* Scaling bitmap fonts often yields poor results.
  (ivars (family family) (size size) (style style) (prrep #f))
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public
    [scheme->tcl (op)
     ;% \ret{unspecified}
     (display prrep op)]

    [get-values ()
     ;* \ret{see below}
     ;* Returns, as multiple values, the family, size, and style
     ;* requested when the instance was created.
     (values family size (list-copy style))]

    [get-actual-values ()
     ;* \ret{see below}
     ;* Returns, as multiple values, the actual family, size, and style
     ;* used by the display engine when rendering this font.
     (parse-tk-font 'get-actual-values (swl:tcl-eval 'font 'actual self))]

    [get-family ()
     ;* \ret{a symbol}
     ;* Returns the family requested when the instance was created.
     family]
    [get-size ()
     ;* \ret{an integer}
     ;* Returns the size requested when the instance was created.
     ;* Positive values indicate size in points;
     ;* negative values indicate size in pixels.
     size]
    [get-style ()
     ;* \ret{see below}
     ;* Returns a list of the other properties ( \scheme{normal},
     ;*         \scheme{bold}, \scheme{roman}, \scheme{italic},
     ;*         \scheme{underline}, or \scheme{overstrike})
     ;* requested when the instance was created.
     (list-copy style)]
 
    [init (family size style)
     ;* \ret{unspecified}
     ;* Initializes the instance.
     (unless (symbol? family)
       (assertion-violationf 'create "<font> expected symbol for family attribute, got ~s" family))
     (unless (integer? size)
       (assertion-violationf 'create "<font> expected integer for size attribute, got ~s" size))
     (unless (or (null? style)
                 (and (pair? style)
                      (andmap
                        (lambda (x)
                          (memq x '(bold italic roman normal underline overstrike)))
                        style)))
       (assertion-violationf 'create
         "<font> expected list of normal, bold, roman, italic, underline, or overstrike for style attribute: ~s"
         style))
     (set! prrep (format-font "" family size style))]))

(define format-font
  (lambda (prefix family size style)
    (let ([op (open-output-string)])
      (define contains-space
        (lambda (str)
          (let f ([i (fx- (string-length str) 1)])
            (and (not (fx< i 1))
                 (or (char=? (string-ref str i) #\space) (f (fx- i 1)))))))
      (let ([fam (symbol->string family)])
        (unless (eq? prefix "") (display prefix op))
        (fprintf op (if (contains-space fam) "{{~a} ~s" "{~a ~s") fam size)
        (let f ([style style])
          (unless (null? style)
            (write-char #\space op)
            (display (car style) op)
            (f (cdr style))))
        (write-char #\} op)
        (get-output-string op)))))

(define parse-tk-font
  (lambda (who str)
    (let ([ip (open-input-string str)])
      (let lp ([family #f] [size #f] [style '()])
        (case (read ip)
          [(-family) (lp (string->symbol (read-family ip)) size style)]
          [(-size) (lp family (read ip) style)]
          [(-weight -slant) (lp family size (cons (read ip) style))]
          [(-underline) (lp family size (if (zero? (read ip)) style (cons 'underline style)))]
          [(-overstrike) (lp family size (if (zero? (read ip)) style (cons 'overstrike style)))]
          [(#!eof)
           (unless (and family size)
             (assertion-violationf who "unexpected value from graphics library"))
           (values family size style)])))))

(module (swl:font-available?)

  (define max-bit #b100000)

  (define hash-style
   ; could make this a bit smarter so it recognizes mutually exclusive options
   ; could try harder to hash options together that have same defaults
    (lambda (who style)
     ; would be cool if a partial evaluator could rewrite this as
     ; (if (null? style) #b001100 (let hash ...))
      (let hash ([style (if (null? style) '(normal roman) style)])
        (if (null? style)
            0
            (fxlogor
              (hash (cdr style))
              (case (car style)
                [(bold)       #b000001]
                [(italic)     #b000010]
                [(roman)      #b000100]
                [(normal)     #b001000]
                [(underline)  #b010000]
                [(overstrike) max-bit]
                [else (assertion-violationf who "invalid font style ~s" (car style))]))))))

  (swl:api-procedure define swl:font-available?
   ;* \formdef{swl:font-available?}{procedure}{(swl:font-available? family size file)}
   ;* \ret{true if the specified font is available, otherwise false}
   ; Would be kind of cool to have a multi-dimensional alist thing
   ; that let you store and fetch information on multiple keys.
   ; then we could split it out to key on family, then size, then style.
   ; As it is, I'm too rushed to cook up such a thing.
   ; This is a place where aspect-oriented programming could be cool
   ; with libraries (or maybe like active libraries).  In a single-threaded
   ; setting, the library could dismiss with the critical sections.
   ; In a multi-threaded setting, we could do a better job of critical
   ; section than I've done here (e.g. row-level locking).
    (let ([als '()])
      (lambda (family size style)
        (define prehash (hash-style 'swl:font-available? style))
        (define exists?
          (lambda ()
            (let-values ([(fam sz stl)
                          (parse-tk-font 'swl:font-available?
                            (swl:raw-tcl-eval
                              (format-font "font actual " family size style)))])
              (and (eq? fam family)
                   (eqv? sz size)
                   (eqv? prehash (hash-style 'swl:font-available? stl))))))
        (if (fx>= (fxabs size)
                  (1- (fxsll 1
                        (- (integer-length (most-positive-fixnum))
                           (integer-length max-bit)))))
            #f
            (let ([subkey (fxlogor prehash (fxsll size (integer-length max-bit)))])
              (cond
                [(assq family als) =>
                 (lambda (hit)
                   (cond
                     [(assv subkey (cdr hit)) => cdr]
                     [else
                      (critical-section
                        (let ([ans (exists?)])
                          (set-cdr! hit (cons (cons subkey ans) (cdr hit)))
                          ans))]))]
                [else
                 (critical-section
                   (let ([ans (exists?)])
                     (set! als
                       (cons (cons family (list (cons subkey ans))) als))
                     ans))]))))))

)

(define swl:font? (lambda (x) (and (instance? x) (send x isa? <font>))))

(define read-family
  (lambda (ip)
    (let ([op (open-output-string)]) ; local op for thread safety
      (let skip-white ()
        (when (char-whitespace? (peek-char ip))
          (read-char ip)
          (skip-white)))
      (let copy ([nbrack 0])
        (let ([c (peek-char ip)])
          (case c
            [(#!eof) (get-output-string op)]
            [(#\{) (read-char ip) (copy (fx+ nbrack 1))]
            [(#\})
             (read-char ip)
             (if (fx= nbrack 1)
                 (get-output-string op)
                 (copy (fx- nbrack 1)))]
            [(#\space)
             (if (fx= nbrack 0)
                 (get-output-string op)
                 (begin (write-char (read-char ip) op) (copy nbrack)))]
            [else (write-char (char-downcase (read-char ip)) op) (copy nbrack)]))))))

(define tk->font
  (lambda (s)
    (let ([ip (open-input-string s)])
      (define read-style
        (lambda ()
          (let f ([ls '()])
            (let ([x (read ip)])
              (if (eof-object? x) ls (f (cons x ls)))))))
      (let* ([family (string->symbol (read-family ip))]
             [size (read ip)]
             [style (read-style)])
        (let ([size
               (if (eof-object? size)
                   (string->number (swl:tcl-eval 'font 'actual s '-size))
                   size)])
          (create <font> family size style))))))

(swl:api-procedure define swl:font-families
  ;* \formdef{swl:font-families}{procedure}{(swl:font-families)}\label{swl:font-families}
  ;* \formdef{swl:font-families}{procedure}{(swl:font-families constraint)}
  ;* \ret{a list of symbols}
  ;* Returns a list of symbols naming font families known to the display.
  ;* Items in the returned list are legal values for the \scheme{family}
  ;* attribute of \scheme{<font>} instances created on the local display.
  ;* If the optional \scheme{constraint} is \scheme{'fixed}, only the fixed-width
  ;* font families are returned.
  ;;   Todo:  compute known-fixed dynamically (currently too slow using
  ;;          (string=? "1" (swl:tcl-eval 'font 'metrics fnt '-fixed))
  ;;          ie. it took more than 10 seconds on my machine at home)
  ;;          (also it returned as fixed some non-text fonts like
  ;;           |open look cursor|)
  ;;   Could provide an option for updating the list of fixed,
  ;;   with a suitable disclaimer about how long it will take.
  ;;
  ;;   With newer machines it's down to about 3 seconds, but that's still
  ;;   unacceptable.  Note that the system is frozen during that time, so
  ;;   we can't simply fork a thread to initialize this in the background.
  (let ([known-fixed
         (if (memq (machine-type) '(i3nt ppcnt))
             '(systemfixed ansifixed oemfixed courier
               |lucida console|
               |courier new|
               fixedsys terminal)
             '(clean courier fixed lucidatypewriter terminal))])
    (rec swl:font-families
      (case-lambda
        [() (swl:font-families #f)]
        [(filter)
         (let ([ip (open-input-string (swl:tcl-eval 'font 'families))])
           (let lp ([ls '()])
             (let ([c (peek-char ip)])
               (if (eof-object? c)
                   (let ([all (map string->symbol (sort string-ci<? ls))])
                     (if (eq? filter 'fixed)
                         (let f ([all all])
                           (cond
                             [(null? all) '()]
                             [(memq (car all) known-fixed)
                              (cons (car all) (f (cdr all)))]
                             [else (f (cdr all))]))
                         all))
                   (lp (cons (read-family ip) ls))))))]))))

(module ((make-font scratch))

(swl:api-syntax make-font
  ;* \formdef{make-font}{syntax}{(make-font \var{base-font} (keyword \var{value}) \dots)}\label{make-font}
  ;* \ret{an instance of \scheme{<font>}}
  ;* This macro provides convenient syntax for overriding particular attributes
  ;* of an existing \scheme{<font>} instance when constructing a new instance.
  ;* The valid keywords are \scheme{family:}, \scheme{size:}, \scheme{add:},
  ;* and \scheme{cancel:}.  The latter two are used to add or cancel sets of
  ;* font style properties, such as \scheme{bold}, \scheme{italic}, etc.
  ;* For example
  ;*
  ;* \scheme{(make-font template-font (size: 16) (add: '(bold)) (cancel: '(italic underline)))}
  ;*
  ;* \noindent requests a 16-point bold version of \var{template-font} that
  ;* is not italic or underlined.
  (lambda (form)
    (syntax-case form ()
      [(_ font mod ...)
       (let f ([mods #'(mod ...)] [family #f] [size #f] [add #f] [cancel #f])
         (if (not (null? mods))
             (syntax-case (car mods) (family: size: add: cancel:)
               [(family: val) (not family) (f (cdr mods) #'val size add cancel)]
               [(size: val) (not size) (f (cdr mods) family #'val add cancel)]
               [(add: val) (not add) (f (cdr mods) family size #'val cancel)]
               [(cancel: val) (not cancel) (f (cdr mods) family size add #'val)]
               [else (syntax-error form (format "invalid or duplicate keyword ~s" (syntax-object->datum (car mods))))])
             (with-syntax ([family family] [size size] [add add] [cancel cancel])
               #'(call-with-values
                   (lambda () (send font get-values))
                   (lambda (default-family default-size style)
                     (create <font>
                       (or family default-family)
                       (or size default-size)
                       (let ([ad add] [can cancel])
                         (let ([ls (if ad (append ad style) style)])
                           (if can (scratch can ls) ls)))))))))])))

(define scratch
  (lambda (ls1 ls2)
    (cond
      [(null? ls2) '()]
      [(memq (car ls2) ls1) (scratch ls1 (cdr ls2))]
      [else (cons (car ls2) (scratch ls1 (cdr ls2)))])))

) ; end module

;; swl:tabs? tk->tabs
(swl:api-class (<tab-stop> position alignment) (<tk-object>)
  ;* \formdef{<tab-stop>}{class}{(make <tab-stop> \var{position} \var{alignment})}
  ;* \ret{instance}
  ;;
  ;* A tab stop has a pixel position and an alignment
  ;* that is either \scheme{left}, \scheme{right}, \scheme{center}, or \scheme{numeric}.
  ;* A \scheme{numeric} tab centers text with respect to the decimal point.
   (ivars
     [position
       (if (swl:distance-unit? position)
           position
           (assertion-violationf '<tab-stop> "invalid tab position ~s" position))]
     [alignment
       (if (memq alignment '(left right center numeric))
           alignment
           (assertion-violationf '<tab-stop> "alignment must be left right center or numeric"))])
   (inherited)
   (inheritable)
   (private)
   (protected)
   (public
     [scheme->tcl (op)
      ;% \ret{unspecified}
      (swl:display* op position #\space alignment)]
     [tab-position ()
      ;* \ret{an integer}
      ;* Returns the position of this tab stop as an offset in pixels.
      ; is it an offset from the left margin?
      position]
     [tab-alignment ()
      ;* \ret{see below}
      ;* Returns the alignment (\scheme{left}, \scheme{right}, \scheme{center}, or \scheme{numeric})
      ;* for this tab stop.
      alignment]))

(define swl:tabs?
  (lambda (ls)
    (andmap (lambda (x) (isa? x <tab-stop>)) ls)))

(define tk->tabs
  (lambda (s)
    (if (string=? s "")
        '()
        (let ((p (open-input-string s)))
          (let loop ((pos (read p)))
            (if (eof-object? pos)
                '()
                (let ((align (read p)))
                  (cons (make <tab-stop> pos align) (loop (read p))))))))))

(swl:api-syntax tab-stops
  ;* \formdef{tab-stops}{syntax}{(tab-stops (keyword \var{value}) \dots)}
  ;* \ret{a list of \scheme{<tab-stop>} instances}
  ;*
  ;* This macro simplifies the construction of lists of
  ;* \scheme{<tab-stop>} instances used when setting the tab stops
  ;* of text widgets and markups.
  ;*
  ;* Each \var{keyword} must be \scheme{left:}, for left-aligned
  ;* tabs, \scheme{right:}, for right-aligned tabs, \scheme{center:}
  ;* for centered tabs, or \scheme{numeric:} for tabs that are
  ;* aligned on the decimal point.
  ;*
  ;* Each \var{value} must be a non-negative integer.
  (lambda (x)
    (syntax-case x (left: right: center: numeric:)
      ((_) (syntax '()))
      ((_ (left: n) . rest)
       (syntax (cons (make <tab-stop> n 'left) (tab-stops . rest))))
      ((_ (right: n) . rest)
       (syntax (cons (make <tab-stop> n 'right) (tab-stops . rest))))
      ((_ (center: n) . rest)
       (syntax (cons (make <tab-stop> n 'center) (tab-stops . rest))))
      ((_ (numeric: n) . rest)
       (syntax (cons (make <tab-stop> n 'numeric) (tab-stops . rest)))))))

)

(eval-when (compile) (import swl:option))
