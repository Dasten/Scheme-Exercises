;;Scheme exercises for practice 2 - Carlos Belmonte

;;Exercise 1 - (list-operation '(a b c d e f g h) 'c) -> '(a b d e f g h)
(define (list-operation l argl)
  (let recur ((list l))
    (cond
     ((null? list) '())
     ((eq? (car list) argl)
      (recur (cdr list)))
     (else
      (cons (car list) (recur (cdr list)))))))

(pp (list-operation '(a b c d e f g h) 'c))

;;Exercise 2 - (insert-right '(a b c d e f g h) 'c 'M) -> '(a b c M d e f g h)
(define (insert-right l argl1 argl2)
  (let recur ((list l))
    (cond
     ((null? list) '())
     ((eq? (car list) argl1)
      (cons argl1 (cons argl2 (recur (cdr list)))))
     (else
      (cons (car list) (recur (cdr list)))))))

(pp (insert-right '(a b c d e f g h) 'c 'M))

;;Exercise 3 - (insert-left '(a b c d e f g h) 'c 'M) -> '(a b M c d e f g h)
(define (insert-left l argl1 argl2)
  (let recur ((list l))
    (cond 
     ((null? list) '())
     ((eq? (car list) argl1)
      (cons argl2 (cons argl1 (recur (cdr list)))))
     (else
      (cons (car list) (recur (cdr list)))))))

(pp (insert-left '(a b c d e f g h) 'c 'M))

;;Exercise 4 - (sust '(a b c a e f a h) 'a 'M) -> '(M b c a e f a h)
(define (sust l argl1 argl2)
  (let recur ((list l))
    (cond
     ((null? list) '())
     ((eq?  (car l) argl1)
      (cons argl2 (cdr list)))
     (else
      (recur (cons (car list) (cdr list)))))))

(pp (sust '(a b c a e f a h) 'a 'M))

;;Exercise 5 - (element-append '(a b c d e f g h) 'M) -> '(a b c d e f g h M)
(define (element-append l argl)
  (let recur ((list l))
    (cond
     ((null? list) '())
     ((eq? (cdr list) '())
      (cons argl (recur (cdr list))))
     (else
      (cons (car list) (recur (cdr list)))))))

(pp (element-append '(a b c d e f g h) 'M))

;;Exercise 6 - (msust '(a b c a e f a h) 'a 'M) -> '(M b c M e f M h)