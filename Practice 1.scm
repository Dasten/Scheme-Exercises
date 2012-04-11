;;Scheme exercises for practice 1 - Carlos Belmonte

;;Exercise 1 - (list-operation '(a b c d e f g h) 'c) -> '(a b d e f g h)
(define (list-operation l argl)
  (cond
   ((null? l) '())
   ((eq? (car l) argl) (list-operation (cdr l) argl))
   (else
    (cons (car l) (list-operation (cdr l) argl)))))

(pp (list-operation '(a b c d e f) 'b))

;;Exercise 2 - (insert-right '(a b c d e f g h) 'c 'M) -> '(a b c M d e f g h)
(define (insert-right l argl1 argl2)
  (cond 
   ((null? '())
    (eq? (car l) argl1) (insert-right (cons 
                                      (cons (car l) argl2) 
                                      (cdr l)) argl1 argl2))
   (else
    (insert-right (cdr l) argl1 argl2))))

(pp (insert-right '(a b c d e f) 'b 'Z))