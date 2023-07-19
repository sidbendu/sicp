#lang sicp

; 1.1
;
; 10 => 10
; (+ 5 3 4) => 12
; (- 9 1) => 8
; (+ (* 2 4) (- 4 6))
; (define a 3)
; (define b (+ a 1)) => 1
; (+ a b (* a b)) => 19
; (= a b) => #f
; (if (and (> b a) (<  b (* a b)))
;   b
;   a) = 4

;(cond ((= a 4) 6)
;      ((= b 4) (+ 6 7 a))
;      (else 25)) => 16

;(+ 2 (if (> b a) b a)) => 6
;
;(* (cond ((> a b) a)
;         ((< a b) b)
;         (else - 1))
;   (+ a 1)) => 16


; 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

; 1.3
(define (square x) (* x x))
(define (sum-of-larger a b c)
  (cond ((and (<= a b) (<= a c)) (+ (square b) (square c)))
        ((and (<= b a) (<= b c)) (+ (square a) (square c)))
        ((and (<= c a) (<= c b)) (+ (square a) (square b)))))

; 1.4
; it adds absolute value of b to a

; 1.5
; evaluating `(p)` causes infinite recursion. this is fine in normal order evaluation, since that branch of the if statement is never evaluated. but in applicative order, arguments to `test` are evaluated first and then it gets stuck

; 1.6
; unlike `if`, which is a special form, `new-if` is just a regular lisp function, which means all the arguments are evaluated first, which means every call evalutes 'sqrt-iter', regardless of condition, which results in infinite recursion

; 1.7
; when the number is very small, our margin of error is similar in magnitude to the result so it will be off by a lot, and for very large numbers it's too small relative to result, so it will take forever to converge

(define (sqrt-improved x)
  (define (good-enough? guess new-guess)
    (< (abs (/ (- guess new-guess) guess)) 0.0001))
  (define (average a b) (/ (+ a b) 2))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess (improve guess))
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0)
  )

; 1.8
(define (cube-root x)
  (define (good-enough? guess new-guess)
    (< (abs (/ (- guess new-guess) guess)) 0.0001))
  (define (average a b) (/ (+ a b) 2))
  (define (improve guess)
    (average guess (/ (+ (/ x (square guess)) (* 2 guess)) 3)))
  (define (sqrt-iter guess)
    (if (good-enough? guess (improve guess))
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0)
  )
(cube-root 64)