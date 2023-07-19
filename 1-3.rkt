#lang sicp

(define (identity x) x)
(define (square x) (* x x))
(define (average a b) (/ (+ a b) 2))
(define (cube x) (* x x x))
(define (inc x) (+ x 1))
(define (dec x) (- x 1))
(define (close-enough? a b) (< (abs (- a b)) 0.0001))

; 1.29
; TODO
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(integral cube 0 1 0.01)

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (sum k)
    (define term (f (+ a (* k h))))
    (cond ((= k n) term)
          ((= k 0) (+ term (sum (+ k 1))))
          (else (+ (* (if (even? k) 2 4) term)
                   (sum (+ k 1))))))
  (* (/ h 3) (sum 0))
  )

(simpson cube 0 1 1000)

; it generates the exact result (1/4), because it keeps rational representation of numbers so there's no error


;1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))
    ))
  (iter a 0)
  )

;1.31
(define (product term a next b)
  (if (> a b)
    1
    (* (term a) (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a prod)
    (if (> a b)
      prod
      (iter (next a) (* prod (term a)))))
  (iter a 1))

(define (fact n)
  (product-acc identity 1 inc n))

(define (pi4 n)
  (define (term x) (/ (square (+ x 1)) (square x)))
  (define (next x) (+ x 2))
  (/ (* 8 (product term 3 next n)) n))

(exact->inexact( pi4 1000))

;1.32

(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
     (if (> a b)
       result
       (iter (next a) (combiner result (term a) null-value))))
  (iter a null-value))

(define (sum-acc term a next b)
  (accumulate + 0 term a next b))

(define (product-acc term a next b)
  (accumulate-iter * 1 term a next b))

;1.33
(define (filtered-accumulate combiner null-value term a next b pred)
  (if (> a b)
    null-value
    (combiner (if (pred a) (term a) null-value) (filtered-accumulate combiner null-value term (next a) b ))))

(define (prime? n) #t)
(lambda (a b) (filtered-accumulate + 0 square a b prime?))
(lambda (n) (filtered-accumulate * 1 identity 1 n (lambda (x) (= (gcd x n) 1))))

; 1.34
; (f f) => (f 2) => (2 2) => error

(define (search f neg-point pos-point)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.0001))
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
      midpoint
      (let ((test-value (f midpoint)))
        (cond ((positive? test-value)
               (search f neg-point midpoint))
              ((negative? test-value)
               (search f midpoint pos-point))
              (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else (error "Not opposite sign")))))

(define (fixed-point f first-guess print)
  (define tolerance 0.00001)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.0001))
  (define (try guess)
    (if print (begin (display guess) (newline)) 0)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

; 1.35
(define golden-ratio (fixed-point (lambda (x) (+ 1 (/ 1 x))) 0.1 #f))

; 1.36
(define (logp x) (/ (log 1000) (log x)))
#| (fixed-point logp 2.0 #t) |#
#| (newline) |#
#| (fixed-point (average-damp logp) 2.0 #t) |#
; it's much faster with averrage damping, there's less oscilation around the result

; 1.37
(define (cont-frac n d k)
  (define (f i)
    (if (= i k)
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (f (+ i 1))))))
  (f 0))

; it takes k=10 for 4 decimal places

(define (cont-frac-iter n d k)
  (define (iter res i)
    (if (< i 0)
      res
      (iter (/ (n i) (+ res (d i))) (- i 1))))

  (iter 0.0 k)
  )

; 1.38
(define (d i)
  (if (= (remainder i 3) 1)
    (+ 2 (* 2 (quotient i 3)))
    1))
(cont-frac (lambda (i) 1.0) d 10)

; 1.39
(define (tan-cf x k)
  (cont-frac-iter
    (lambda (i)
      (if (= i 0) x (- (square x))))
    (lambda (i) (+ (* 2 i) 1))
     k))

; 1.40
(define (deriv f)
  (define dx 0.0001)
  (lambda (x)
    (/ (- (f (+ x dx)) (f x)) dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess #f))

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

; 1.41
(define (double f)
  (lambda (x) (f (f x))))

(((double (double double)) inc) 5)

; 1.42
(define (compose f g)
  (lambda (x) (f (g x))))
((compose square inc) 6)

; 1.43
(define (repeated f n)
  (if (= n 1)
    f
    (compose f (repeated f (dec n)))))
((repeated square 2) 5)

; 1.44
(define (smooth f)
  (define dx 0.1)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-smooth f n)
  ((repeated smooth n) f))

; 1.45
(define (nth-root n x)
  (define repeats (floor (log n 2)))
  (fixed-point ((repeated average-damp repeats) (lambda (y) (/ x (expt y (- n 1))))) 1.0 #f))

; 1.46
(define (iterative-improve good-enough? improve)
  (define (search guess)
    (if (good-enough? guess)
      guess
      (search (improve guess))))
  search)

(define (fixed-point-iter f first-guess)
  ((iterative-improve (lambda (x) (close-enough? x (f x))) f) first-guess))

(define (sqrt2 x)
  ((iterative-improve
     (lambda (g) (close-enough? (square g) x))
     (lambda (y) (average y (/ x y)))) 1.0))
