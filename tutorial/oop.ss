;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;;
;; This tutorial doesn't do anything interactive.
;; (in fact, will error out since it's formatted for printing)
;;

(module ()
(import swl:oop)
(import swl:macros)
(swl:tutorial define-class
  (illustrates
     define-class send send-base define-generic private protected public)
  (follows intro)
  ;* Classes are defined using the \scheme{define-class} macro.
  ;* Below we define a class \scheme{point} with instance variables
  ;* \scheme{x} and \scheme{y}, and methods \scheme{distance},
  ;* \scheme{get-x}, \scheme{get-y}, \scheme{set-x!}, and \scheme{set-y!}.

  (let ()
    (define-class (point x y) (<base>)
      (ivars (x x) (y y))
      (inherited)
      (inheritable)
      (private
        [delta-x (pt) (abs (- x (send pt get-x)))]
        [delta-y (pt) (abs (- y (send pt get-y)))])
      (protected
        [combine-deltas (x y)
         (assertion-violationf 'point "not sure how to combine deltas")])
      (public
        [distance (pt)
         (combine-deltas (delta-x pt) (delta-y pt))]
        [get-x () x]
        [get-y () y]
        [set-x! (val) (set! x val)]
        [set-y! (val) (set! y val)]))
  
    ;* Note that the syntax for classes is currently very rigid:
    ;* no part of the class definition is optional.  If this becomes
    ;* tedious it is straightforward to define macros that expand into
    ;* the full class definition.
    ;*
    ;* The first subform of the \scheme{define-class} expression
    ;* gives the name of the class and a list of formal
    ;* parameters for which arguments must be supplied when an
    ;* instance of the class is constructed.
    ;* These formals are visible in the expressions that initialize
    ;* the instance variables and the arguments to the
    ;* base class.
    ;* The second subform of the \scheme{define-class} expression
    ;* names the base class and supplies the actual values
    ;* needed to initialize the base class.  In the example
    ;* above, the base class is \scheme{<base>} which takes no arguments.
    ;*
    ;* \scheme{ivars} introduces a binding list of expressions
    ;* much like \scheme{let}.
    ;* The class formals are visible within these expressions.
    ;* \scheme{inherited} introduces a list of identifiers naming
    ;* instance variables inherited from the base class.  These
    ;* variables are initialized by a super class.
    ;* \scheme{inheritable} introduces a list of identifiers naming
    ;* instance variables that may be inherited by subclasses derived
    ;* from this class.  This list may include identifiers bound in
    ;* the \scheme{ivars} or listed among the \scheme{inherited}.
    ;* 
    ;* Methods are divided into three groups: \scheme{private} methods visible
    ;* only within a class, \scheme{protected} methods visible within a class
    ;* and its subclasses,
    ;* and \scheme{public} methods visible outside the class as well.
    ;* Syntactically, a method is a list consisting of the method name,
    ;* the method formals (in any form accepted by \scheme{lambda}), and
    ;* a sequence of expressions forming the method body.
    ;* The instance variables and inherited instance variables
    ;* are visible within each method body, as are the other
    ;* methods and an implicit binding of \scheme{self} to the instance.
    ;* Calls to private and protected methods have the same syntax
    ;* as ordinary function calls.  Public methods
    ;* are called using the \scheme{send} macro:
    ;*
    ;* \scheme{(send instance message arg ...)}
    ;*
    ;* \noindent where \var{message} is the name of a public method in the class
    ;* of which \var{instance} is an instance.  The \scheme{define-generic}
    ;* macro takes the name of a public method and binds it to a macro
    ;* that expands into the appropriate use of \scheme{send}.
    ;* Thus, given
    ;*
    ;* \scheme{(define-generic message)}
    ;*
    ;* \noindent the schema for calling public methods above becomes
    ;*
    ;* \scheme{(message instance arg ...)}
    ;*
    ;* When a subclass extends a public method it is often convenient to
    ;* call the base method.  This is possible using the \scheme{send-base}
    ;* macro, which has syntax identical to \scheme{send}, but which calls
    ;* the specified method in the base class.
    ;*
    ;* In the \scheme{point} example above, \scheme{delta-x} and \scheme{delta-y} are private
    ;* methods that compute the absolute difference in the \scheme{x} and
    ;* \scheme{y} values for two points.  \scheme{combine-deltas} is a protected
    ;* method that is redefined in the \scheme{cartesian-point} and
    ;* \scheme{manhattan-point} subclasses.  \scheme{point} provides public methods
    ;* for setting and accessing the instance variables, and a \scheme{distance}
    ;* method that relies on a suitable implementation of \scheme{combine-deltas}.
    ;* Note that we must define \scheme{combine-deltas} as a protected method
    ;* in \scheme{point} so that the \scheme{distance} method recognizes it as a protected
    ;* method.
    ;* This is necessary because the \scheme{distance} method is compiled without
    ;* knowlege of its subclasses, and has the added benefit that we get
    ;* a better error message if a subclass fails to define \scheme{combine-deltas}
    ;* as a protected method.
    ;* When we redefine \scheme{combine-deltas}
    ;* in the \scheme{cartesian-point} and \scheme{manhattan-point} classes below,
    ;* the inherited
    ;* \scheme{distance} method works as intended.
    ;*
    ;* Now we define a \scheme{cartesian-point} class that
    ;* redefines the protected method \scheme{combine-deltas}
    ;* so that the inherited \scheme{distance} method computes the
    ;* cartesian distance between two points.
    ;* Note that the base class \scheme{point} is initialized with the
    ;* \scheme{x} and \scheme{y} values supplied to \scheme{cartesian-point}.
  
    (define-class (cartesian-point x y) (point x y)
      (ivars)
      (inherited)
      (inheritable)
      (private)
      (protected
        [combine-deltas (dx dy) (sqrt (+ (* dx dx) (* dy dy)))])
      (public))
  
    ;* \noindent For example
    (let ((a (make cartesian-point 1 1))
          (b (make cartesian-point 4 5)))
      (send a distance b))
    ;* \noindent evaluates to \scheme{5}.
    ;*
    ;* Next we define a \scheme{manhattan-point} class that
    ;* redefines the protected method \scheme{combine-deltas}
    ;* so that the inherited \scheme{distance} method computes the
    ;* manhattan distance between two points.
    ;* Note that the base class \scheme{point} is initialized with the
    ;* \scheme{x} and \scheme{y} values supplied to \scheme{manhattan-point}.
  
    (let ()
      (define-class (manhattan-point x y) (point x y)
        (ivars)
        (inherited)
        (inheritable)
        (private)
        (protected
          [combine-deltas (delta-x delta-y) (+ delta-x delta-y)])
        (public))
  
      ;* \noindent For example
      (let ((a (make manhattan-point 1 1))
            (b (make manhattan-point 4 5)))
        (send a distance b))))
  ;* \noindent evaluates to \scheme{7}.

);end tutorial
)

;;
;; This tutorial doesn't do anything interactive.
;;

