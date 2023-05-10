(define (set-member? el set)
  (cond ((null? set) #f)
        ((equal? (car set) el) #t)
        (else (set-member? el (cdr set)))))

; (set-member? 1 '(1 2 3)) ; #t
; (set-member? 4 '(1 2 3)) ; #f

(define (set-insert el set)
  (cond ((null? set) (list el))
        ((equal? (car set) el) set)
        (else (cons (car set) (set-insert el (cdr set))))))

; (set-insert 5 '(4 5 6))  ; (4 5 6)
; (set-insert 7 '(4 5 6))  ; (4 5 6 7)

(define (set-remove el set)
  (cond ((null? set) '())
        ((equal? (car set) el) (cdr set))
        (else (cons (car set) (set-remove el (cdr set))))))

; (set-remove 7 '(7 8 9))  ; (8 9)
; (set-remove 6 '(7 8 9))  ; (7 8 9)

(define (set-create lst)
  (let iter ((lst-remaining lst)
             (set '()))
    (if (null? lst-remaining) set
        (iter (cdr lst-remaining) (set-insert (car lst-remaining) set)))))

; (set-create '(1 2 3))           ; (1 2 3)
; (set-create '(1 1 2 2 3 3))     ; (1 2 3)
; (set-create '(1 4 2 3 1 2 3 4)) ; (1 4 2 3)

(define (set-union A B)
  (if (null? B) A
      (set-union (set-insert (car B) A) (cdr B))))

; (set-union '(1 2 3 4) '(3 4 5 6)) ; (1 2 3 4 5 6)

(define (set-intersection A B)
  (let iter ((A’ A)
             (B’ B)
             (result '()))
    (cond ((null? B’) result)
          ((set-member? (car B’) A’) (iter (set-remove (car B’) A’) (cdr B’) (set-insert (car B’) result)))
          (else (iter A’ (cdr B’) result)))))

; (set-intersection '(1 2 3 4) '(3 4 5 6)) ; (3 4)

(define (set-difference A B)
  (cond ((null? B) A)
        ((set-member? (car B) A) (set-difference (set-remove (car B) A) (cdr B)))
        (else (set-difference A (cdr B)))))

; (set-difference '(1 2 3 4) '(3 4 5 6)) ; (1 2)

(define (set-symmetric-difference A B)
  (set-union (set-difference A B) (set-difference B A)))

; (set-symmetric-difference '(1 2 3 4) '(3 4 5 6)) ; (1 2 5 6)

(define (subset? A B)
  (cond ((null? A) #t)
        ((set-member? (car A) B) (subset? (cdr A) B))
        (else #f)))

; (subset? '(1 2 3) '(1 2 3)) ; #t
; (subset? '(1 2) '(1 2 3))   ; #t
; (subset? '() '(1 2 3))      ; #t
; (subset? '(1 2 3) '(1))     ; #f
; (subset? '(1 2 3) '(1 2))   ; #f
; (subset? '(1 2 3) '(1 2 4)) ; #f

(define (superset? A B) (subset? B A))

; (superset? '(1 2 3) '(1 2 3)) ; #t
; (superset? '(1 2 3) '(1 2))   ; #t
; (superset? '(1 2) '(1 2 3))   ; #f
; (superset? '(1) '(1 2 3))     ; #f

(define (set? S)
  (cond ((null? S) #t)
        ((set-member? (car S) (cdr S)) #f)
        (else (set? (cdr S)))))

; (set? '(1 2 3))   ; #t
; (set? '(1 2 2 3)) ; #f
