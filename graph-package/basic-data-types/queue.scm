; I have a feeling that the professor would prefer a procedural implementation of queues, considering his comments in `stack.scm`.
; However, I couldn’t figure that out, so here’s a list-based implementation for now.
; You know you love me.
; x o x o
; gossip squirrel

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
