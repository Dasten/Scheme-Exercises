Statements of the Scheme Exercises

;; 1-  (list-operation '(a b c d e f g h) 'c) -> '(a b d e f g h)
;; 2-  (insert-right '(a b c d e f g h) 'c 'M) -> '(a b c M d e f g h)
;; 3-  (insert-left '(a b c d e f g h) 'c 'M) -> '(a b M c d e f g h)
;; 4-  (sust '(a b c a e f a h) 'a 'M) -> '(M b c a e f a h)
;; 5-  (element-append '(a b c d e f g h) 'M) -> '(a b c d e f g h M) 
;; 6-  (msust '(a b c a e f a h) 'a 'M) -> '(M b c M e f M h)
;; 7-  (mrember '(a b c a e f a h) 'a) -> '(b c e f h)
;; 8-  (nth-element '(a b c d e f g h) 3) -> 'd
;; 9-  (nth-rember '(a b c d e f g h) 3) -> '(a b c e f g h)
;; 10- (element-occur '(a b c a e f a h) 'a) -> 3
;; 11- (length '(a b c d e f g h)) -> 8