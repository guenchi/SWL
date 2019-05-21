;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Markup
;;

;; need to fix the GC stuff here to be a bit more like canvasitem.ss

;;  darn:
;;    * markups can be applied to ranges that overlap
;;    * when two markups have attributes in common, then
;;      the last one applied to a range wins
;;        - notion of raising markups
;;        - would be much nicer if they composed better (they do to some extent)
;;    * tags can be removed from ranges
;;    * tags can be deleted (easier to implement)

;; events that tags can deal with:
;;   Enter, Leave, ButtonPress, Motion, KeyPress, KeyRelease, ButtonRelease 

;;   Control Flow for markups in a text:
;;     - The text seems to get the event anyway, even if there is a break.
;;     - Break only affects execution of scripts on the other tags?
;;   
;;   Enter, Leave
;;     - don't get the mouse-button modifiers, but get kbd mods
;;   ButtonPress, ButtonRelease, ButtonMotion
;;     - naturally ButtonPress doesn't get the mouse button mods
;;     - for button-press/release we need to have callbacks for
;;         * double
;;         * triple
;;   KeyPress, KeyRelease
;;     - only generates these events when mouse is over a range of text that
;;       has the markup (kind of makes sense in terms of Focus follows mouse)
;;
;;
;; johnz - 9/24/96 - commented out the efficiency hacks in markup
;;   initialization, so that mouse method redefinition can take place
;;   in subclasses.
;;
;; johnz - 9/26/96 - the use of numeric constants to represent bit flags
;;   is gross and probably unportable.  Suppose the bit assignments
;;   change in a future release of Tk?
;;

;; for now, conservatively assume the event methods for <markups> are
;; always redefined in any subclass (and in the current class)
;; may eventually permit this kind of reflection in the object system

; (define method-redefined-in-subclass?
;   (lambda (self method-name)
;     #t))

(define ignore-event-method (lambda ignore #f))
  ;; new convention is to return #f if markup ignores the event
  ;; see ormap in code under init method in <text> class to understand why.

;; using interned symbols instead of gensyms so we can hash back to them
;; if we want to support a method on texts that returns a list of all
;; markups applied to a given position.

(define new-markup-handle
  (let ((n -1))
    (lambda ()
      (set! n (+ n 1))
      (string->symbol (format "#~x" n)))))

(swl:api-class (<markup>) (<tk-prewidget> (new-markup-handle))
  ;% \swlapi{class}{<markup>}{(create <markup> with (keyword: value) \dots)}
  ;* \ret{instance}
  ;;
  ;* Markups can be applied to regions of text in a \scheme{<text>} widget
  ;* to control the display and behavioral properties of that region.
  ;* A given markup may be applied to regions of text in several different
  ;* text widgets.
  ;* See the \scheme{apply-markup} method for more detail.
  ;*
  ;* Markups exist in a stacking order (latest applied is topmost) that 
  ;* determines which display properties are visible.  Many text display
  ;* attributes can be set on a markup which is otherwise transparent.
  ;* Setting a markup attribute to \scheme{#f} makes it transparent again for
  ;* that attribute.  For example, setting the font of a markup to \scheme{#f}
  ;* permits the font of underlying markups (or the text widget) to show
  ;* through.
  ;*
  ;* By default the event notification methods can be changed dynamically
  ;* for markups.  Since several markups may apply to a range of text
  ;* it may be desirable to invoke the notification methods of more than one markup
  ;* for a given event.  To permit notification methods of other markups to be invoked,
  ;* a notification method can simply return \scheme{#f}.  Any other return value
  ;* prevents other methods from running.  Markups are notified of events by stacking order.
  ;  I think.
  (ivars (texts '()) (text-options '()) (callbacks '())
         (mouse-enter-method ignore-event-method)
         (mouse-leave-method ignore-event-method)
         (key-press-method ignore-event-method)
         (key-release-method ignore-event-method)
         (mouse-motion-method ignore-event-method)
         (mouse-press-method ignore-event-method)
         (mouse-release-method ignore-event-method)
         (need-callback-tag? #f))
  (inherited handle)
  (inheritable)
  (private
    [ensure-configured (txt)
     ;; invariant:  if txt is among texts, then it's binding and display
     ;;   attribute info is up-to-date
     (or (memq txt texts) (send txt tag-config handle text-options self))]
    [update-options (key value)
     ; This representation permits <text> tag-config to set all the
     ; options in one call.
     (set! text-options
       (let loop ([ls text-options])
         (cond
           [(null? ls) (if value (list key value) '())]
           [(eq? key (car ls)) (cons key (cons value (cddr ls)))]
           [else (cons (car ls) (cons (cadr ls) (loop (cddr ls))))])))]
    [old-update-options (key value)
     (if (not value)
         (set! text-options (remq (car ls) text-options))
         (let loop ([ls text-options])
           (cond
             [(null? ls)
              (set! text-options (cons (cons key value) text-options))]
             [(eq? key (caar ls)) (set-cdr! (car ls) value)]
             [else (loop (cdr ls))])))]
    [lookup-option (key)
     (let lp ([ls text-options])
       (cond
         [(null? ls) #f]
         [(eq? (car ls) key) (cadr ls)]
         [else (lp (cddr ls))]))]
    [old-lookup-option (key)
     (let ((x (assq key text-options)))
       (and x (cdr x)))])
  (protected
    [set-tk-option (tclname value err-check name)
     ;; It seems that attributes for Tk tags can be un-set by setting them
     ;; to {} (ie. what scheme->tcl turns () into).  This is handy for cases
     ;; where you want to temporarily alter the appearance of some marked
     ;; range of text, but want to be able to reset it to a transparent
     ;; default later on.  Now we'll be able to do that by passing in #f.
     (thread-critical-section
       (swl:safety-check
         (unless (or (not value) (err-check value))
           (assertion-violationf name "bad value ~s" value)))
       (update-options tclname value)
; Comment out for-each for experimental (buffering) version:
       (for-each
         (lambda (txt)
           (swl:tcl-eval txt 'tag 'config handle tclname (or value '())))
         texts)
       (#3%void))]
    [get-tk-option (tclname conversion name)
     ; cp0 will inline this for us
     (lookup-option tclname)])
  (public
    [mouse-enter (text x y mods)
     ;* \ret{unspecified}
     ;* this method is called to notify the markup that the mouse
     ;* has entered a range of text with this markup in the widget
     ;* \var{text} at coordinates \var{x}, \var{y}, with modifiers \var{mods}.
     (mouse-enter-method self text x y mods)]
    [mouse-leave (text x y mods)
     ;* \ret{unspecified}
     ;* this method is called to notify the markup that the mouse
     ;* has left a range of text with this markup in the widget
     ;* \var{text} at coordinates \var{x}, \var{y}, with modifiers \var{mods}.
     (mouse-leave-method self text x y mods)]
    [key-press (text key mods)
     ;* \ret{unspecified}
     ;* this method is called to notify the markup that the specified
     ;* \var{key} has been pressed with the modifiers \var{mods} while the
     ;* mouse is over a range of text with this markup
     ;; note:  we could have the bind stuff call directly to the appropriate
     ;; proc, but this fits better into the overall event framework.  maybe.
     ;; not sure what to make of inheritance.
     (key-press-method self text key mods)]
    [key-release (text key mods)
     ;* \ret{unspecified}
     ;* this method is called to notify the markup that the specified
     ;* \var{key} has been released with the modifiers \var{mods} while the
     ;* mouse is over a range of text with this markup
     (key-release-method self text key mods)]
    [mouse-motion (text x y mods)
     ;* \ret{unspecified}
     ;* this method is called to notify the markup that the mouse
     ;* has moved over a range of text with this markup in the widget
     ;* \var{text} at coordinates \var{x}, \var{y}, with modifiers \var{mods}.
     (mouse-motion-method self text x y mods)]
    [mouse-press (text x y mods)
     ;* \ret{unspecified}
     ;* this method is called to notify the markup that a mouse button
     ;* has been clicked over a range of text with this markup in the widget
     ;* \var{text} at coordinates \var{x}, \var{y}, with modifiers \var{mods}.
     (mouse-press-method self text x y mods)]
    [mouse-release (text x y mods)
     ;* \ret{unspecified}
     ;* this method is called to notify the markup that a mouse button
     ;* has been released over a range of text with this markup in the widget
     ;* \var{text} at coordinates \var{x}, \var{y}, with modifiers \var{mods}.
     (mouse-release-method self text x y mods)]
    [set-key-press-method! (val)
     ;* \ret{unspecified}
     ;; the point here is that the mouse has to be in the marked-up text
     ;; for that piece of text to get the events
     ;* specifies a procedure to be called when a key is pressed while
     ;* the mouse is over a range of text with this markup.
     ;* \var{val} is a procedure of four arguments:  \var{self}, \var{text},
     ;* \var{key}, and \var{mods}.  The first two correspond to the markup
     ;* and the particular text that generated the event, and the latter
     ;* two are suitable for use with \scheme{event-case}.
     ;* Note that the text widget is also notified of the \scheme{key-press} event.
     (unless (procedure? val)  ;; eventually support callback object for Wally
       (assertion-violationf 'set-key-press-method! "~s is not a procedure" val))
     (set! need-callback-tag? #t)
     (set! key-press-method val)]
    [get-key-press-method ()
     ;* \ret{see below}
     ;* returns the method that is notified when a key is pressed while
     ;* the mouse is over a range of text with this markup.
     key-press-method]
    [set-key-release-method! (val)
     ;* \ret{unspecified}
     ;* specifies a procedure to be called when a key is released while
     ;* the mouse is over a range of text with this markup.
     ;* \var{val} is a procedure of four arguments:  \var{self}, \var{text},
     ;* \var{key}, and \var{mods}.  The first two correspond to the markup
     ;* and the particular text that generated the event, and the latter
     ;* two are suitable for use with \scheme{event-case}.
     ;* Note that the text widget is also notified of the \scheme{key-release} event.
     (unless (procedure? val)  ;; eventually support callback object for Wally
       (assertion-violationf 'set-key-release-method! "~s is not a procedure" val))
     (set! need-callback-tag? #t)
     (set! key-release-method val)]
    [get-key-release-method ()
     ;* \ret{see below}
     ;* returns the method that is notified when a key is released while
     ;* the mouse is over a range of text with this markup.
     key-release-method]
    [set-mouse-press-method! (val)
     ;* \ret{unspecified}
     ;* specifies a procedure to be called when a mouse button is pressed
     ;* over a range of text with this markup.
     ;* \var{val} is a procedure of five arguments:  \var{self}, \var{text},
     ;* \var{x}, \var{y}, and \var{mods}.  The first two correspond to the markup
     ;* and the particular text that generated the event, \var{x} and \var{y}
     ;* are the coordinates of the mouse within the text widget, and \var{mods}
     ;* is suitable for use with \scheme{event-case}.
     ;* Note that the text widget is also notified of the \scheme{mouse-press} event.
     (unless (procedure? val)  ;; eventually support callback object for Wally
       (assertion-violationf 'set-mouse-press-method! "~s is not a procedure" val))
     (set! need-callback-tag? #t)
     (set! mouse-press-method val)]
    [get-mouse-press-method ()
     ;* \ret{see below}
     ;* returns the procedure that is notified when a mouse button is pressed
     ;* over a range of text with this markup.
     mouse-press-method]
    [set-mouse-release-method! (val)
     ;* \ret{unspecified}
     ;* specifies a procedure to be called when a mouse button is released
     ;* over a range of text with this markup.
     ;* \var{val} is a procedure of five arguments:  \var{self}, \var{text},
     ;* \var{x}, \var{y}, and \var{mods}.  The first two correspond to the markup
     ;* and the particular text that generated the event, \var{x} and \var{y}
     ;* are the coordinates of the mouse within the text widget, and \var{mods}
     ;* is suitable for use with \scheme{event-case}.
     ;* Note that the text widget is also notified of the \scheme{mouse-release} event.
     (unless (procedure? val)  ;; eventually support callback object for Wally
       (assertion-violationf 'set-mouse-release-method! "~s is not a procedure" val))
     (set! need-callback-tag? #t)
     (set! mouse-release-method val)]
    [get-mouse-release-method ()
     ;* \ret{see below}
     ;* returns the procedure that is notified when a mouse button is released
     ;* over a range of text with this markup.
     mouse-release-method]
    [set-mouse-motion-method! (val)
     ;* \ret{unspecified}
     ;* specifies a procedure to be called when the mouse moves
     ;* over a range of text with this markup.
     ;* \var{val} is a procedure of five arguments:  \var{self}, \var{text},
     ;* \var{x}, \var{y}, and \var{mods}.  The first two correspond to the markup
     ;* and the particular text that generated the event, \var{x} and \var{y}
     ;* are the coordinates of the mouse within the text widget, and \var{mods}
     ;* is suitable for use with \scheme{event-case}.
     ;* Note that the text widget is also notified of the \scheme{mouse-motion} event.
     (unless (procedure? val)  ;; eventually support callback object for Wally
       (assertion-violationf 'set-mouse-motion-method! "~s is not a procedure" val))
     (set! need-callback-tag? #t)
     (set! mouse-motion-method val)]
    [get-mouse-motion-method ()
     ;* \ret{see below}
     ;* returns the procedure that is notified when the mouse moves
     ;* over a range of text with this markup.
     mouse-motion-method]
    [set-mouse-enter-method! (val)
     ;* \ret{unspecified}
     ;* specifies a procedure to be called when the mouse
     ;* enters a range of text with this markup.
     ;* \var{val} is a procedure of five arguments:  \var{self}, \var{text},
     ;* \var{x}, \var{y}, and \var{mods}.  The first two correspond to the markup
     ;* and the particular text that generated the event, \var{x} and \var{y}
     ;* are the coordinates of the mouse within the text widget, and \var{mods}
     ;* is suitable for use with \scheme{event-case}.
     ;* Note that the text widget is also notified of the \scheme{mouse-enter} event.
     ;* There seems to be an underlying Tk bug that makes the mouse button
     ;* modifiers unavailable for \scheme{mouse-enter} and \scheme{mouse-leave} events.
     ;* The keyboard modifiers are unaffected.
     (unless (procedure? val)  ;; eventually support callback object for Wally
       (assertion-violationf 'set-mouse-enter-method! "~s is not a procedure" val))
     (set! need-callback-tag? #t)
     (set! mouse-enter-method val)]
    [get-mouse-enter-method ()
     ;* \ret{see below}
     ;* returns the procedure that is notified when the mouse
     ;* enters a range of text with this markup.
     mouse-enter-method]
    [set-mouse-leave-method! (val)
     ;* \ret{unspecified}
     ;* specifies a procedure to be called when the mouse
     ;* leaves a range of text with this markup.
     ;* \var{val} is a procedure of five arguments:  \var{self}, \var{text},
     ;* \var{x}, \var{y}, and \var{mods}.  The first two correspond to the markup
     ;* and the particular text that generated the event, \var{x} and \var{y}
     ;* are the coordinates of the mouse within the text widget, and \var{mods}
     ;* is suitable for use with \scheme{event-case}.
     ;* Note that the text widget is also notified of the \scheme{leave} event.
     ;* There seems to be an underlying Tk bug that makes the mouse button
     ;* modifiers unavailable for \scheme{mouse-enter} and \scheme{mouse-leave} events.
     ;* The keyboard modifiers are unaffected.
     (unless (procedure? val)  ;; eventually support callback object for Wally
       (assertion-violationf 'set-mouse-leave-method! "~s is not a procedure" val))
     (set! need-callback-tag? #t)
     (set! mouse-leave-method val)]
    [get-mouse-leave-method ()
     ;* \ret{see below}
     ;* returns the procedure that is notified when the mouse
     ;* leaves a range of text with this markup.
     mouse-leave-method]
    [set-font! (val)
     ;* \ret{unspecified}
     ;* Sets the font for text with this markup.
     ;* \var{val} is an instance of \scheme{<font>} (see the description
     ;* of the \scheme{make-font} macro) or \scheme{#f} which cancels the effect
     ;* of this markup on font.
     (set-tk-option '-font val swl:font? 'set-font!)]
    [get-font ()
     ;* \ret{see below}
     ;* Returns an instance of \scheme{<font>} describing the font
     ;* for text with this markup or \scheme{#f} to indicate that this
     ;* markup does not override the text font.
     (get-tk-option '-font tk->font 'get-font)]
    [set-background-color! (val)
     ;* \ret{unspecified}
     ;* Sets the background color for text with this markup to \var{val},
     ;* which is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt}, an instance of \scheme{<rgb>}, or
     ;* \scheme{#f} which cancels the effect of this markup on background color.
     (set-tk-option '-background val swl:color? 'set-background-color!)]
    [get-background-color ()
     ;* \ret{see below}
     ;* Returns the background color for text with this markup.
     ;* The value returned is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt}, an instance of \scheme{<rgb>}, or \scheme{#f}.
     (get-tk-option '-background tk->color 'get-background-color)]
    [set-foreground-color! (val)
     ;* \ret{unspecified}
     ;* Sets the foreground color for text with this markup to \var{val},
     ;* which is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt}, an instance of \scheme{<rgb>}, or 
     ;* \scheme{#f} which cancels the effect of this markup on foreground color.
     (set-tk-option '-foreground val swl:color? 'set-foreground-color!)]
    [get-foreground-color ()
     ;* \ret{see below}
     ;* Returns the foreground color for text with this markup.
     ;* The value returned is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt}, an instance of \scheme{<rgb>}, or \scheme{#f}.
     (get-tk-option '-foreground tk->color 'get-foreground-color)]
    [set-above-paragraph-space! (val)
     ;* \ret{unspecified}
     ;* Sets in pixels the amount of extra space to insert above each
     ;* paragraph in the text.  (\scheme{#f} cancels the extra space for this
     ;* markup)
     (set-tk-option '-spacing1 val swl:distance-unit? 'set-above-paragraph-space!)]
    [get-above-paragraph-space ()
     ;* \ret{see below}
     ;* Returns in pixels the amount of extra space to insert above each
     ;* paragraph in the text, or \scheme{#f}.
     (get-tk-option '-spacing1 string->number 'get-above-paragraph-space)]
    [set-below-paragraph-space! (val)
     ;* \ret{unspecified}
     ;* Sets in pixels the amount of extra space to insert below each
     ;* paragraph in the text.  (\scheme{#f} cancels the extra space for this
     ;* markup)
     (set-tk-option '-spacing3 val swl:distance-unit? 'set-below-paragraph-space!)]
    [get-below-paragraph-space ()
     ;* \ret{see below}
     ;* Returns in pixels the amount of extra space to insert below each
     ;* paragraph in the text, or \scheme{#f}.
     (get-tk-option '-spacing3 string->number 'get-below-paragraph-space)]
    [set-line-space! (val)
     ;* \ret{unspecified}
     ;* Sets in pixels the amount of extra space to insert between each line
     ;* in the text.  (\scheme{#f} cancels the extra space for this
     ;* markup)
     (set-tk-option '-spacing2 val swl:distance-unit? 'set-line-space!)]
    [get-line-space ()
     ;* \ret{see below}
     ;* Returns in pixels the amount of extra space to insert between each line
     ;* in the text, or \scheme{#f}.
     (get-tk-option '-spacing2 string->number 'get-line-space)]
    [set-tabs! (val)
     ;* \ret{unspecified}
     ;* This sets the tab stops for regions of text bearing this markup to
     ;* those supplied by the list of \scheme{<tab-stop>} instances, or cancels
     ;* tab settings for this markup if \scheme{#f}.
     ;* Four kinds of tabs are supported:  left aligned, right aligned,
     ;* centered, and numeric.
     ;* (See the \scheme{tab-stops} macro.)
     (set-tk-option '-tabs val swl:tabs? 'set-tabs!)]
    [get-tabs ()
     ;* \ret{see below}
     ;* Returns a list of the tab stops in effect for text with this markup
     ;* or \scheme{#f} if none.
     (get-tk-option '-tabs tk->tabs 'get-tabs)]
    [set-border-width! (val)
     ;* \ret{unspecified}
     ;* Sets the width in pixels of the border drawn around text with this
     ;* markup, or cancels the border if \scheme{#f}.
     (set-tk-option '-borderwidth val swl:distance-unit? 'set-border-width!)]
    [get-border-width ()
     ;* \ret{see below}
     ;* Returns the width in pixels of the border drawn around
     ;* text with this markup, or \scheme{#f} if none.
     (get-tk-option '-borderwidth string->number 'get-border-width)]
    [set-relief! (val)
     ;* \ret{unspecified}
     ;* Determines how the border of the affected region of text is drawn.
     ;* Legal values are \scheme{flat}, \scheme{sunken}, \scheme{raised},
     ;* \scheme{ridge}, \scheme{groove}, and \scheme{#f} which cancels the effect of
     ;* this markup on relief.  See also \scheme{set-border-width!}.
     ;* Note:  If the widget border width is set to zero, setting relief
     ;* will have no visible effect.
     (set-tk-option '-relief val swl:relief? 'set-relief!)]
    [get-relief ()
     ;* \ret{see below}
     ;* Returns a symbol describing how the border of the widget is drawn,
     ;* or \scheme{#f}.
     (get-tk-option '-relief string->symbol 'get-relief)]
    [set-wrap! (val)
     ;* \ret{unspecified}
     ;* Determines how lines that are too long to be displayed will be
     ;* wrapped.  \var{val} can be either \scheme{char} for wrapping lines at
     ;* character boundaries, \scheme{word} for wrapping lines a word boundaries,
     ;* \scheme{none} which prevents lines from being wrapped, or \scheme{#f} which
     ;* cancels the effect of this markup on text wrapping.
     (set-tk-option '-wrap val
       (lambda (x) (memq x '(word char none))) 'set-wrap!)]
    [get-wrap ()
     ;* \ret{see below}
     ;* Returns a symbol indicating how long lines are wrapped, or \scheme{#f} if
     ;* none.
     (get-tk-option '-wrap string->symbol 'get-wrap)]
    [set-paragraph-indent! (val)
     ;* \ret{unspecified}
     ;* Determines the left margin in pixels for the first line of
     ;* a paragraph with this markup.
     ;* If \scheme{val} is \scheme{#f}, this markup will have no effect on indent.
     (set-tk-option '-lmargin1 val swl:distance-unit? 'set-paragraph-index!)]
    [get-paragraph-indent ()
     ;* \ret{see below}
     ;* Returns the number of pixels that the first line of a paragraph
     ;* with this markup is indented, or \scheme{#f} if none.
     (get-tk-option '-lmargin1 tk->boolean 'get-paragraph-index)]
    [set-left-margin! (val)
     ;* \ret{unspecified}
     ;* Determines the left margin in pixels for text other than
     ;* the first line of a paragraph with this markup.
     ;* If \scheme{val} is \scheme{#f}, this markup will have no effect on left margin.
     (set-tk-option '-lmargin2 val swl:distance-unit? 'set-left-margin!)]
    [get-left-margin ()
     ;* \ret{see below}
     ;* Returns the left margin in pixels for text with this markup, or \scheme{#f}
     ;* if none.
     (get-tk-option '-lmargin2 tk->boolean 'get-left-margin)]
    [set-right-margin! (val)
     ;* \ret{unspecified}
     ;* Determines the right margin in pixels for text with this markup.
     ;* If \scheme{val} is \scheme{#f}, this markup will have no effect on right margin.
     (set-tk-option '-rmargin val swl:distance-unit? 'set-right-margin!)]
    [get-right-margin ()
     ;* \ret{see below}
     ;* Returns the right margin in pixels for text with this markup, or \scheme{#f} 
     ;* if none. 
     (get-tk-option '-rmargin tk->boolean 'get-right-margin)]
    [set-underline! (val)
     ;* \ret{unspecified}
     ;* Determines whether or not text with this markup is underlined.
     (set-tk-option '-underline val boolean? 'set-underline!)]
    [get-underline ()
     ;* \ret{see below}
     ;* Returns a boolean indicating whether or not text with this
     ;* markup is underlined.
     (get-tk-option '-underline tk->boolean 'get-underline)]
    [set-overstrike! (val)
     ;* \ret{unspecified}
     ;* Determines whether or not text with this markup is overstricken.
     (set-tk-option '-overstrike val boolean? 'set-overstrike!)]
    [get-overstrike ()
     ;* \ret{see below}
     ;* Returns a boolean indicating whether or not text with this
     ;* markup is overstricken.
     (get-tk-option '-overstrike tk->boolean 'get-overstrike)]
    [marked-ranges (txt)
     ;* \ret{see below}
; why did I make this (and some others)
; a <markup> method rather than a <text> method?
     ;* Returns a list of positions describing ranges in the text where this
     ;* markup is applied, or an empty list if none.  If a non-empty list is
     ;* returned, every
     ;* pair of elements describes the start and end position of a range with
     ;* this markup.  The end positions indicate the place just after the last
     ;* marked character in each range.
     (thread-critical-section
       (or (and (memq txt texts) (send txt tag-ranges handle)) '()))]
    [ensure-configured-for (txt)
     ;; NOT an API method
     ;; really a hack needed (?) by text.ss
     (ensure-configured txt)]
    [apply-markup (txt index1 index2)
     ;* \ret{unspecified}
     ;* applies the given markup to the range of text beginning
     ;* at \var{index1} and ending at \var{index2} within the text widget
     ;* \var{txt}.  Subsequent operations on the markup affect this
     ;* region of text until the markup is removed, or that text
     ;* is deleted from the widget.
     (thread-critical-section
       (and (ensure-configured txt)
            (send txt tag-range handle 'add index1 index2 need-callback-tag?)
            (set! texts (weak-cons txt texts))))
     (void)]
    [remove-markup (txt index1 index2)
     ;* \ret{unspecified}
     ;* removes the given markup from the range of text beginning
     ;* at \var{index1} and ending at \var{index2} within the text widget
     ;* \var{txt}.
     (send txt tag-range handle 'remove index1 index2 #f)
     (void)]
    [destroy ()
     ;; This egregious hack is needed because Tk sends a mouse-leave
     ;; notification when the mouse is over a markup when the markup
     ;; is removed.  When removing a markup, the <text> class drops
     ;; its reference to the markup if Tk says the markup isn't applied
     ;; to any ranges.  A race condition exists between the time that
     ;; Tk decides to send the notification and the time the GC gets
     ;; around to destroying the unreferenced markup.  The following
     ;; hack prevents this race condition from biting us, but it does
     ;; change Tk's notify semantics in this case.
     (send-base self destroy)
     (critical-section
       (vector-set! self 0
         (let ([err (lambda (msg . args)
                      (assertion-violationf msg "instance has been destroyed"))])
           (swl:dispatch-table err
             [scheme->tcl (op) (display handle op)]
; this is gross, but then so is it all
             [key-press (w k m) (void)]
             [key-release (w k m) (void)]
             [mouse-press (w x y m) (void)]
             [mouse-enter (w x y m) (void)]
             [mouse-leave (w x y m) (void)]
             [mouse-motion (w x y m) (void)]
             [mouse-release (w x y m) (void)]
             [hide () (void)]
             [destroy () (void)]))))]
))

(define isa-markup? (lambda (markup) (send markup isa? <markup>)))

(define-generic apply-markup)
(define-generic remove-markup)

