(lambda (machine)

(import swl:oop)
(import swl:macros)
(import swl:generics)
(import swl:option)
(import turing-machine)

(define q12 (create <state> machine 'q12 393 354))
(define q11 (create <state> machine 'q11 253 354))
(define q10 (create <state> machine 'q10 199 278))
(define q9 (create <state> machine 'q9 600 180))
(define q8 (create <state> machine 'q8 497 200))
(define q7 (create <state> machine 'q7 430 144))
(define q6 (create <state> machine 'q6 383 239))
(define q5 (create <state> machine 'q5 336 113))
(define q4 (create <state> machine 'q4 293 195))
(define q3 (create <state> machine 'q3 200 196))
(define q2 (create <state> machine 'q2 201 107))
(define q1 (create <state> machine 'q1 112 148))
(define q0 (create <state> machine 'q0 79 77))
(let ([x (create <transition> machine q7 'blank 'right q7)])
  (send x
        suggest-points
        '(420.0440939907941
           131.47482792390227
           399
           105
           444
           92
           434.15957557516595
           128.55014786366928))
  (send x show-label 423 100))
(let ([x (create <transition> machine q9 'blank 'left q10)])
  (send x
        suggest-points
        '(587.4841940293062
           189.9676778089957
           433
           313
           214.8239723001255
           280.366833463694))
  (send x show-label 485 253))
(let ([x (create <transition> machine q9 '1 'blank q2)])
  (send x
        suggest-points
        '(593.6258030885377
           165.32452338988932
           557
           81
           274
           77
           215.79904454270974
           100.918200872859))
  (send x show-label 517 97))
(let ([x (create <transition> machine q8 'blank 'right q9)])
  #f
  (send x show-label 540 187))
(let ([x (create <transition> machine q7 '1 'blank q8)])
  #f
  (send x show-label 462 168))
(let ([x (create <transition> machine q6 '1 'right q7)])
  #f
  (send x show-label 407 191))
(let ([x (create <transition> machine q5 'blank '1 q6)])
  #f
  (send x show-label 359 168))
(let ([x (create <transition> machine q4 '1 'right q5)])
  #f
  (send x show-label 293 142))
(let ([x (create <transition> machine q4 'blank 'left q4)])
  (send x
        suggest-points
        '(284.6893552358867
           208.6723510635412
           262
           246
           303
           258
           295.5082807072113
           210.8021684554314))
  (send x show-label 278 252))
(let ([x (create <transition> machine q3 '1 'left q4)])
  #f
  (send x show-label 239 195))
(let ([x (create <transition> machine q3 'blank 'left q10)])
  #f
  (send x show-label 199 232))
(let ([x (create <transition> machine q2 'blank 'right q3)])
  #f
  (send x show-label 199 149))
(let ([x (create <transition> machine q1 '1 'blank q2)])
  #f
  (send x show-label 150 130))
(let ([x (create <transition> machine q11 '1 'left q11)])
  (send x
        suggest-points
        '(237.76754301943578
           358.8961468866099
           197
           372
           227
           404
           245.61835621372816
           368.1954688197535))
  (send x show-label 206 377))
(let ([x (create <transition> machine q10 'blank 'left q10)])
  (send x
        suggest-points
        '(184.0083167234189
           283.5901191878777
           140
           300
           173
           336
           192.45510553451345
           292.6001491922392))
  (send x show-label 150 311))
(let ([x
       (create <transition> machine q11 'blank 'right q12)])
  #f
  (send x show-label 318 354))
(let ([x (create <transition> machine q10 '1 'left q11)])
  #f
  (send x show-label 222 311))
(let ([x (create <transition> machine q1 'blank 'left q10)])
  (send x
        suggest-points
        '(113.57259549909492
           163.92252942833602
           120
           229
           185.4030987643427
           269.56647898041507))
  (send x show-label 124 213))
(let ([x (create <transition> machine q0 '1 'right q1)])
  #f
  (send x show-label 110 102))
)
