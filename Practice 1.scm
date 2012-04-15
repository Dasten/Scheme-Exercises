;;Scheme exercises for practice 1 - Carlos Belmonte

;;Exercise 1 - (list-operation '(a b c d e f g h) 'c) -> '(a b d e f g h)
(define (list-operation l argl)
  (cond
   ((null? l) '())
   ((eq? (car l) argl) 
    (list-operation (cdr l) argl))
   (else
    (cons (car l) (list-operation (cdr l) argl)))))

(pp (list-operation '(a b c d e f) 'b))

;;Exercise 2 - (insert-right '(a b c d e f g h) 'c 'M) -> '(a b c M d e f g h)
(define (insert-right l argl1 argl2)
(cond 
   ((null? l)  '())
   ((eq? (car l) argl1)
    (cons argl1 (cons argl2 (insert-right (cdr l) argl1 argl2))))
   (else
    (cons (car l) (insert-right (cdr l) argl1 argl2)))))

(pp (insert-right '(a b c d e f) 'b 'Z))

;;Exercise 3 - (insert-left '(a b c d e f g h) 'c 'M) -> '(a b M c d e f g h)
(define (insert-left l argl1 argl2)
  (cond
   ((null? l) '())
   ((eq? (car l) argl1) 
    (cons argl2 (cons argl1 (insert-left (cdr l) argl1 argl2))))
   (else
    (cons (car l) (insert-left (cdr l) argl1 argl2)))))

(pp (insert-left '(a b c d e f) 'b 'Z))

;;Exercise 4 -  (sust '(a b c a e f a h) 'a 'M) -> '(M b c a e f a h)
(define (sust l argl1 argl2)
  (cond
   ((null? l) '())
   ((eq? (car l) argl1)
    (cons argl2 (sust (cdr l) argl1 argl2)))
   (else
    (cons (car l) (sust (cdr l) argl1 argl2)))))

(pp (sust '(a b c d e f g) 'b 'Z))