#lang sicp
(define (square x) (* x x))

; 1.9

;(+ 4 5)
;(inc (+ 3 5))
;(inc (inc (+ 2 5)))
;(inc (inc (inc (+ 1 5))))
;(inc (inc (inc (inc (+ 0 5)))))
;(inc (inc (inc (inc 5))))
;(inc (inc (inc 6)))
;(inc (inc 7))
;(inc 8)
;9

; (+ 4 5)
; (+ 3 6)
; (+ 2 7)
; (+ 1 8)
; (+ 0 9)
; 9

; 1.10
; (A 1 10) => (A 0 (A 1 9))
; => (A 0 (A 0 (A 1 8)))
; => (A 0 (A 0 (A 0 (A 1 7))))
; => (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
; => (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
; => (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
; => 1024
;
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
; (A 3 3) => 65536
; (A 2 4) => 65536
; (A 1 10) => 1024

; f(n) = 2 * n
; g(n) = 2 ^ n
; h(n) = 2 ^ (2 ^ n)
; k(n) = 5 * n ^ 2


; 1.11
(define (f n)
  (if (< n 3) n
    (+ (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

(define (f-iter n)
  (define (iter a b c x)
    (cond ((< n 3) n)
      ((= x n) c)
      (else (iter b c (+ (* 3 a) (* 2 b) c) (inc x)))))
  (iter 1 2 4 3)
  )

; 1.12
(define (pascal x y)
  (cond ((or (= y 1) (= x y)) 1)
        (else (+ (pascal (dec x) (dec y)) (pascal (dec x) y)))))

; 1.13 and 1.14 done on paper

; 1.15
; p is called 5 times when called with 12.15.
; growth is O(log(n)), because every iteration reduces number 3x

; 1.16
(define (fast-exp a b n)
  (cond ((= n 0) a)
        ((even? n) (fast-exp a (square b) (/ n 2)))
        (else (fast-exp (* b a) b (dec n)))))

; 1.17
(define (double x) (+ x x))
(define (halve x) (/ x 2))
(define (mult a b)
   (cond ((= b 0) 0)
         ((even? b) (double (mult a (halve b))))
         (else (+ a (mult a (- b 1))))))

; 1.18
(define (mult2 a b s)
  (cond ((= b 0) s)
        ((even? b) (mult2 (double a) (halve b) s))
        (else (mult2 a (dec b) (+ s a) ))))

; 1.19
; p' = p ^ 2 + q ^ 2
; q' = 2 * p * q + q ^ 2


; 1.20
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))
; applicative order:
; (gcd 206 40)
; (gcd 40 (remainder 206 40))
; (gcd 40 6)
; (gcd 6 (remainder 40 6))
; (gcd 6 4)
; (gcd 4 (remainder 6 4))
; (gcd 4 2)
; (gcd 4 (remainder 4 2))
; (gcd 2 0)
; 2
; so a total of 4 evaluations

; normal order
; (gcd 206 40)
; (gcd 40 (remainder 206 40))
; (if (= (remainder 206 40)) 0) => eval
; (if (= 6 0))
; (gcd 6 (remainder 6 4))
; (if (= (remainder 6 4) 0)) => eval
; (if (= 2 0))
; (gcd 4 (remainder 4 2))
; (if (= (remainder 4 2) 0)) => eval
; (if (= 0 0)) => 2
; so a total of 3 evaluations

; 1.21
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (define (next n)
    (if (= n 2) 3
      (+ n 2)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (dec exp) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (inc (random (dec n)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (dec times)))
        (else false)))


(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

(define (prime? n)
  (= n (smallest-divisor n)))

; 1.22

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
    (report-prime n (- (runtime) start-time))
    #f))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (display "\n")
  #t)

(define (search-for-primes start n)
  (cond ((= n 0) (display "done\n"))
        ((start-prime-test start (runtime)) (search-for-primes (inc start) (dec n)))
        (else (search-for-primes (inc start) n))))

#| (search-for-primes 100000 3) |#
#| (search-for-primes 1000000 3) |#
#| (search-for-primes 10000000 3) |#
#| (search-for-primes 100000000 3) |#

; i've multiplied all numbers 100x because computers got so fast differences were not significant. with these higher numbers every 10x step is roughly sqrt(10) slower

; 1.23
;it's about 1.6x faster. we do half as many numbers, but have to do an extra if so that slows it down. also it probably has more branch mispredictions

; 1.24
; it is pretty close, every 10x increase in numbers adds about 10 microseconds

; 1.25
; no, we have to remainder along the way, otherwise the exponentiation gives huge results which slows down the program

; 1.26
; he calls expmod twice when exponent is even, which cancels out the speedup you get from succesive squaring

; 1.27
(define (test-carmichael n)
  (define (try-it a)
    (if (= a (dec n))
      #t
      (and (= (expmod a n n) a) (try-it (inc a)))))
  (try-it 1))

#| (test-carmichael 561) |#
#| (test-carmichael 1105) |#
#| (test-carmichael 1729) |#
#| (test-carmichael 2465) |#
#| (test-carmichael 2821) |#
#| (test-carmichael 6601) |#

; 1.28
; skip for now