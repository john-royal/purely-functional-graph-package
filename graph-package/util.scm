; util.scm


(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(define (some predicate sequence)
  (not (null? (filter predicate sequence))))


(define (sort pred lst)
  (define (insert el lst)
    (cond ((null? lst) (list el))
          ((pred el (car lst)) (cons el lst))
          (else (cons (car lst) (insert el (cdr lst))))))
  (if (null? lst) '()
      (insert (car lst) (sort pred (cdr lst)))))


(define (accumulate op init seq)
  (if (null? seq) init
      (op (car seq) (accumulate op init (cdr seq)))))

;every: This function takes a predicate function pred and a list lst and returns true if the predicate
; is true for all elements of the list; otherwise, it returns false.
(define (every pred lst)
  (cond ((null? lst) #t)
        ((pred (car lst)) (every pred (cdr lst)))
        (else #f)))

(define (insertion-sort lst less-than?)
  (define (insert x sorted-lst)
    (if (null? sorted-lst)
        (list x)
        (if (less-than? x (car sorted-lst))
            (cons x sorted-lst)
            (cons (car sorted-lst) (insert x (cdr sorted-lst))))))
  (if (null? lst)
      '()
      (insert (car lst) (insertion-sort (cdr lst) less-than?))))