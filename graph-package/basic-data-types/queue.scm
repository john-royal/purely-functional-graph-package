; "Queues can be represented as stacks or lists"
; "Do we care about time efficiency?" "Not so much"

(define (make-empty-queue) '())

(define (enqueue x Q)
  (if (null? Q) (list x)
      (cons (car Q) (enqueue x (cdr Q)))))

(define (dequeue Q)
  (cdr Q))

(define (front Q)
  (car Q))

(define (empty? Q)
  (null? Q))

(define (size Q)
  (if (null? Q) 0
      (+ (size (cdr Q)))))

(define q0 (make-empty-queue))
(define q1 (enqueue 1 q0))
(define q2 (enqueue 2 q1))
(define q3 (enqueue 3 q2))

(enqueue 4 q3) ; (1 2 3 4)
(dequeue q3)   ; (2 3)
(front q3)     ; 1
(empty? q3)    ; #f
(size q3)      ; 3
