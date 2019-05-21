;; This machine inverts a sequence of 1's and 0's

(lambda (machine)

(import swl:oop)
(import swl:macros)
(import swl:generics)
(import swl:option)
(import turing-machine)

(define q8 (create <state> machine 'q8 436 274))
(define q7 (create <state> machine 'q7 318 319))
(define q6 (create <state> machine 'q6 195 309))
(define q5 (create <state> machine 'q5 55 281))
(define q4 (create <state> machine 'q4 55 186))
(define q2 (create <state> machine 'q2 420 189))
(define q1 (create <state> machine 'q1 280 189))
(define q3 (create <state> machine 'q3 53 102))
(let ([x (create <transition> machine q3 'blank 'blank q8)])
  (send x
        suggest-points
        '(68.95643723760823
           100.82012259862756
           337
           81
           467
           136
           439.506811310665
           258.38903352026557))
  (send x show-label 151 96))
(let ([x (create <transition> machine q7 'blank 'right q8)])
  (send x suggest-points '())
  (send x show-label 362 299))
(let ([x (create <transition> machine q6 'm 'blank q7)])
  (send x suggest-points '())
  (send x show-label 249 314))
(let ([x (create <transition> machine q6 '0 'left q6)])
  (send x
        suggest-points
        '(184.35016244866557
           320.9407269514962
           162
           346
           200
           363
           196.4751713689718
           324.93185078489523))
  (send x show-label 179 357))
(let ([x (create <transition> machine q6 '1 'left q6)])
  (send x
        suggest-points
        '(183.84674570733108
           297.5280812989691
           160
           273
           136
           308
           179.00229769513643
           308.7288525033074))
  (send x show-label 148 292))
(let ([x (create <transition> machine q1 'blank 'left q6)])
  (send x suggest-points '())
  (send x show-label 235 249))
(let ([x (create <transition> machine q5 'm 'right q1)])
  (send x suggest-points '())
  (send x show-label 164 236))
(let ([x (create <transition> machine q3 '1 'left q4)])
  (send x
        suggest-points
        '(62.54724426151297
           114.83939745513814
           82
           141
           63.23193208684042
           172.2801131885993))
  (send x show-label 69 139))
(let ([x (create <transition> machine q3 '0 'left q4)])
  (send x
        suggest-points
        '(43.551840089852526
           114.91248521053488
           23
           143
           45.447827690876736
           173.1642684596156))
  (send x show-label 30 142))
(let ([x (create <transition> machine q4 'blank 'm q5)])
  (send x suggest-points '())
  (send x show-label 54 232))
(let ([x (create <transition> machine q2 '0 'right q1)])
  (send x
        suggest-points
        '(404.6235905100001
           193.42335067520546
           347
           210
           295.26761942280973
           193.78537325192542))
  (send x show-label 349 197))
(let ([x (create <transition> machine q1 '1 '0 q2)])
  (send x
        suggest-points
        '(289.89901756494476
           201.5701810348505
           343
           269
           408.90450245825997
           200.52778965375586))
  (send x show-label 342 237))
(let ([x (create <transition> machine q2 '1 'right q1)])
  (send x
        suggest-points
        '(404.96924453023263
           183.5158054367065
           346
           162
           294.8087512992908
           182.94187446847192))
  (send x show-label 347 168))
(let ([x (create <transition> machine q1 '0 '1 q2)])
  (send x
        suggest-points
        '(288.2928274574935
           175.31683469513573
           340
           90
           409.94368059856345
           176.55530474072225))
  (send x show-label 333 128))
)
