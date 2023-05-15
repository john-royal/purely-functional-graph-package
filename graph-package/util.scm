; util.scm


(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))


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

