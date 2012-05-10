;;Scheme exercises for practice 2 - Carlos Belmonte

;;Exercise 1 - (list-operation '(a b c d e f g h) 'c) -> '(a b d e f g h)
(define (list-operation l argl)
  (let recur ((list l))
    (cond
     ((null? list) '())
     ((eq? (car list) argl)
      (recur (cdr list)))
     (else
      (cons 
       (car list) (recur (cdr list)))))))

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
     ((eq?  (car list) argl1)
      (cons argl2 (cdr list)))
     (else
      (recur (cons (car list) (cdr list)))))))

(pp (sust '(a b c a e f a h) 'a 'M))

;;Exercise 5 - (element-append '(a b c d e f g h) 'M) -> '(a b c d e f g h M)
(define (element-append l argl)
  (let recur ((list l))
    (cond
     ((null? list) (cons argl '()))
     (else
      (cons (car list) (recur (cdr list)))))))

(pp (element-append '(a b c d e f g h) 'M))

;;Exercise 6 - (msust '(a b c a e f a h) 'a 'M) -> '(M b c M e f M h)
(define (msust l argl1 argl2)
  (let recur ((list l))
    (cond
     ((null? list) '())
     ((eq? (car list) argl1)
      (cons argl2 (recur (cdr list))))
     (else
      (cons (car list) (recur (cdr list)))))))

(pp (msust '(a b c a e f a h) 'a 'M))

;;Exercise 7 - (mrember '(a b c a e f a h) 'a) -> '(b c e f h)
(define (mrember l argl)
  (let recur ((list l))
    (cond
     ((null? list) '())
     ((eq? (car list) argl)
      (recur (cdr list)))
     (else
      (cons (car list) (recur (cdr list)))))))

(pp (mrember '(a b c a e f a h) 'a))

;;Exercise 8 - (nth-element '(a b c d e f g h) 3) -> 'd
(define (nth-element l argl)
  (let recur ((list l) 
              (counter argl))
    (cond
     ((null? list) '())
     ((eq? counter 0)
      (car list))
     (else
      (recur (cdr list) (- counter 1))))))

(pp (nth-element '(a b c d e f g h) 3))

;;Exercise  9 - (nth-rember '(a b c d e f g h) 3) -> '(a b c e f g h)
(define (nth-rember l argl)
  (let recur ((list l)
              (counter argl))
    (cond
     ((eq? counter 0)
      (cdr list))
     (else
      (cons (car list) (recur (cdr list) (- counter 1)))))))

(pp (nth-rember '(a b c d e f g h) 3))

;;Exercise 10 - (element-occur '(a b c a e f a h) 'a) -> 3
(define (element-occur l argl)
  (let recur ((list l))
    (cond
     ((null? list) 0)
     ((eq? (car list) argl)
      (+ 1 (recur (cdr list))))
     (else
      (recur (cdr list))))))

(pp (element-occur '(a b c a e f a h) 'a))

;;Exercise  11 - (length '(a b c d e f g h)) -> 8
(define (length l)
  (let recur ((list l))
    (cond 
     ((null? list) 0)
     (else
      (+ 1 (recur (cdr list)))))))

(pp (length '(a b c d e f g)))