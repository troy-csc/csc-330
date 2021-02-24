#lang racket


(provide (all-defined-out)) ;; so we can put tests in a second file

; part 1
(define nat-num-stream
  (letrec
      ([f (lambda (x)
            (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

;#1
(define add-pointwise  #f)

;#2
(define add-pointwise-lists #f)

;#3
(define add-pointwise-lists-2 #f)

;#4
(define stream-for-n-steps #f)

;#5
(define fibo-stream #f)

;#6
(define filter-stream #f)

;#7
(define palyndromic-numbers #f)

;#8 macro create-stream

; part 2

;#1
(define vector-assoc #f)

;#2
(define cached-assoc #f)
