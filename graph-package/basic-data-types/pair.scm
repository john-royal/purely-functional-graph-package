(define (pair a b) (list a b))
(define (first pair) (car pair))
(define (second pair) (car (cdr pair)))

;(define p1 (pair 1 2))
;(first p1)  ; 1
;(second p1) ; 2

;(define p2 (pair '3 '(4 5 6)))
;(first p2)  ; 3
;(second p2) ; (4 5 6)