
; This file, provided by the professor, contains two draft implementations of the stack data structure:
; - 'list, where data is stored more intuitively in lists
; - 'proc, where data is stored using functions (less intuitive, but more robust - seems to be the professor's preference)
; We’ll need to decide which implementation to go with. We can also come up with our own, Troeger says.

; --------------------------------------------------

;; Here is a way to have multiple representations for the same ADT (or
;; interface) without resorting to the name-space clutter displayed in
;; stackADT.scm

;; One can add as many representations as one wishes inside the
;; function stack-reps

;; Status: DRAFT



;; m is one of 'proc or 'list

(define (stack-reps m)

  (define (procedural-rep)
    
    (define (make-empty)
      (lambda (m)
        (eq? m 'empty?)))

    ;; m is either 'top or 'pop
    ;; s is a stack
    (define (push x s)
      (lambda (m)
        (cond ((eq? m 'top) x)
              ((eq? m 'pop) s)
              ((eq? m 'empty?) #f))))

    ;; s is a non-empty stack
    (define (top s)
      (s 'top))

    ;; s is a non-empty stack
    (define (pop s)
      (s 'pop))

    ;; s is a stack
    (define (empty? s)
      (s 'empty?))

;; m is one of 'make-empty, 'push, 'top, 'pop, or 'empty?
(define (dispatch m)
  (cond ((eq? m 'make-empty) (make-empty))
        ((eq? m 'push) push)
        ((eq? m 'top) top)
        ((eq? m 'pop) pop)
        ((eq? m 'empty?) empty?)))

    dispatch)



(define (list-rep)

  (define make-empty
    '())

  (define (push x s)
    (cons x s))

  (define (top s)
    (car s))

  (define (pop s)
    (cdr s))

  (define (empty? s)
    (null? s))

  ;; m is one of 'make-empty, 'push, 'top, 'pop, or 'empty?
  (define (dispatch m)
    (cond ((eq? m 'make-empty) make-empty)
          ((eq? m 'push) push)
          ((eq? m 'top) top)
          ((eq? m 'pop) pop)
          ((eq? m 'empty?) empty?)))

  dispatch)


  

  (cond ((eq? m 'proc) (procedural-rep))
        ((eq? m 'list) (list-rep)))

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; usage -- comment out the list stacks section to experiment with
;; procedural stacks -- comment out the procedural stacks section to
;; experiment with list stacks.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; observe how the function stack-reps works to avoid cluttering the
;; namespace with different versions of (for example) push: we no longer
;; need disambiguation via names such as push-procedural and push-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; procedural stacks  -- comment out the list stacks section to
;;; experiment with the procedural representation

;;; first define the basic operations at the top level

(define make-empty-stack ((stack-reps 'proc) 'make-empty))
(define push-stack ((stack-reps 'proc) 'push))
(define top-stack ((stack-reps 'proc) 'top))
(define pop-stack ((stack-reps 'proc) 'pop))
(define empty-stack? ((stack-reps 'proc) 'empty?))

;;; now define some stacks

(define stack-0 make-empty-stack)

(define stack-1 (push-stack 'a stack-0))

(define stack-2 (push-stack 'b stack-1))

(define stack-3 (push-stack 'c (pop-stack stack-2)))


;;; and now use them

(list (top-stack stack-3) (top-stack stack-2) (top-stack stack-1))

(define (demo-proc)  
  (newline)
  (display (top-stack stack-1))
  (newline)
  (display (empty-stack? (pop-stack stack-1)))
  (newline)
  (display (top-stack stack-1))
  (newline))


;; list stacks

;;; first define the basic operations at the top level

(define make-empty-stack ((stack-reps 'list) 'make-empty))
(define push-stack ((stack-reps 'list) 'push))
(define top-stack ((stack-reps 'list) 'top))
(define pop-stack ((stack-reps 'list) 'pop))
(define empty-stack? ((stack-reps 'list) 'empty?))

;;; now define some stacks

(define stack-0 make-empty-stack)

(define stack-1 (push-stack 'a stack-0))

(define stack-2 (push-stack 'b stack-1))

(define stack-3 (push-stack 'c (pop-stack stack-2)))


;;; and now use them

(list (top-stack stack-3) (top-stack stack-2) (top-stack stack-1))

(define (demo-list)  
  (newline)
  (display (top-stack stack-1))
  (newline)
  (display (empty-stack? (pop-stack stack-1)))
  (newline)
  (display (top-stack stack-1))
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Notice that this approach allows us to have stacks represented
;; procedurally along side stacks represented as lists.  For example,

(define stack-4 ((stack-reps 'proc) 'make-empty))

(define stack-5 ((stack-reps 'list) 'make-empty))

;; and then

(((stack-reps 'proc) 'empty?) stack-4)

(newline)

(((stack-reps 'list) 'empty?) stack-5)
    


