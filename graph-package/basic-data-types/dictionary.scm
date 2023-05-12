; I created this to help with some algorithms, but I don’t think we’ll end up needing it.
; Keeping it here for now, just in case.

(define (empty-dict) '())
(define example-dict '((1 ())
                       (4 ())))

(define (false? val) (eq? val #f))

(define (dict-get key dict)
  (let ((entry (assoc key dict)))
    (if (false? entry) #f
        (car (cdr entry)))))

(dict-get '1 example-dict)

(define (dict-set key val dict)
  (cond ((null? dict) (list (list key val)))
        ((equal? (car (car dict)) key) (cons (list key val) (cdr dict)))
        (else (cons (car dict) (dict-set key val (cdr dict))))))

(define (dict-push key val dict)
  (let ((entry (dict-get key dict)))
    (if (false? entry) (cons (list key (list val)) dict)
        (dict-set key (cons val entry) dict))))

(define (dict-keys dict) (map car dict))

(display example-dict)
(newline)
(dict-push '1 '2 example-dict)
(dict-push '1 '2 (dict-push '1 '3 example-dict))
(dict-keys example-dict)