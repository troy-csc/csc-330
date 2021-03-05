; CSC 330
; Programming Languages
; Assignment 4

; Student Information
; Name: Tarush Roy
; VNum: V00883469

#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file


; ---------- part 1 ---------- ;
(define nat-num-stream
  (letrec
      ([f (lambda (x)
            (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

; #1: add-pointwise
(define (add-pointwise ls1 ls2)
  (if (list? ls1)     ; check both parameters
      (if (list? ls2) ; for lists
          (if (null? ls1)
              ls2
              (if (null? ls2)
                  ls1
                  (cons (+ (car ls1) (car ls2)) (add-pointwise (cdr ls1) (cdr ls2))))) ; both lists are non-empty here
          (error "illegal parameter"))
      (error "illegal parameter")))
          

; #2: add-pointwise-lists
(define (add-pointwise-lists ls)
  (letrec (; helper function to sum list
           [sum (lambda (xs) (if (null? xs) 0 (+ (car xs) (sum (cdr xs)))))]
           ; get a list of heads of a list of lists
           [get-heads (lambda (ls) (map (lambda (l) (if (null? l) 0 (car l))) ls))]
           ; get a list of tails of a list of lists
           [get-tails (lambda (ls) (map (lambda (l) (if (null? l) null (cdr l))) ls))]
           ; checks list of lists for empty lists
           [list-nulls? (lambda (ls) (if (null? ls) #t (if (null? (car ls)) (list-nulls? (cdr ls)) #f)))]
           ; checks list of lists for illegal parameters
           [list-lists? (lambda (ls) (if (null? ls) #t (if (list? (car ls)) (list-lists? (cdr ls)) #f)))])
    (if (list? ls)
        (if (null? ls)
            null
            (if (list-nulls? ls)
                null
                (if (list-lists? ls)
                    (cons (sum (get-heads ls)) (add-pointwise-lists (get-tails ls))) ; sum heads, recurse of list of tails
                    (error "illegal parameter"))))
        (error "illegal parameter"))))

; #3: add-pointwise-lists-2
;     non-recursive
(define (add-pointwise-lists-2 ls)
  (if (list? ls)
      (foldr add-pointwise null ls) ; simple fold call
      (error "illegal parameter")))

; #4: stream-for-n-steps
(define (stream-for-n-steps s n)
  (letrec ([f (lambda (stream counter acc)
                (if (= counter n)
                    acc
                    (let ([pr (stream)])
                      (f (cdr pr) (+ counter 1) (append acc (list (car pr)))))))])
    (f s 0 null)))

; #5: fibo-stream
(define fibo-stream
  (letrec ([f (lambda (x1 x2)
                (if (= x1 0)
                    (cons x1 (lambda () (cons x2 (lambda () (cons x2 (lambda () (f x2 x2))))))) ; Manually inserting first exception values
                    (cons (+ x1 x2) (lambda () (f x2 (+ x1 x2))))))])
    (lambda () (f 0 1))))

; #6: filter-stream
(define (filter-stream fn s)
  (letrec ([f (lambda (stream)
                (let ([pr (stream)])
                  (if (fn (car pr))
                      (cons (car pr) (lambda () (f (cdr pr))))
                      (f (cdr pr)))))])
    (lambda () (f s))))

; #7: palyndromic-numbers stream
(define palyndromic-numbers
  (letrec ([palindrome? (lambda (n)
                          (equal? (string->list (~a n)) (reverse (string->list (~a n)))))])
    (filter-stream palindrome? nat-num-stream)))
                
; #8: macro create-stream
(define-syntax create-stream
  (syntax-rules (using starting at with increment)
    [(create-stream name using f starting at i0 with increment delta)
     (define (name) ; define stream with name
       (letrec ([fn (lambda (x)
                     (cons (f x) (lambda () (fn (+ x delta)))))]) ; add f(x) and call with increment
         (fn i0)))])) ; start with initial value


; ---------- part 2 ---------- ;

; #1: vector-assoc
(define (vector-assoc v vec)
  (if (= (vector-length vec) 0)
      #f
      (letrec ([f (lambda (pos)
                    (if (= pos (vector-length vec)) ; if last item
                        (if (pair? (vector-ref vec (- pos 1))) ; checks for pair
                            (if (= (car (vector-ref vec (- pos 1))) v) ; check value for equal
                                (vector-ref vec pos) ; return value
                                #f) ; return false because nothing was found in the vector
                            #f)
                        (if (pair? (vector-ref vec pos)) ; not last item
                            (if (= (car (vector-ref vec pos)) v)
                                (vector-ref vec pos)
                                (f (+ pos 1))) ; recursive call with next position accumulator
                            (f (+ pos 1)))))]) ; recursive call
        (f 0)))) ; call to helper function

; #2: cached-assoc
(define (cached-assoc xs n)
  (letrec ([memo (make-vector n [(lambda () #f)])] ; cache initialized with #f
           [pos 0] ; counter for current position in cache
           [f (lambda (v)
                (let ([ans (vector-assoc v memo)]) ; search cache for match
                  (if ans
                      ans ; return ans
                      (let ([new-ans (assoc v xs)]) ; find match is list
                        (if new-ans
                            (begin
                              (vector-set! memo pos new-ans) ; insert pair into cache
                              (if (= pos (vector-length memo)) ; check for last position in cache
                                  (set! pos 0) ; set current position counter to 0
                                  (set! pos (+ pos 1))) ; increment pos counter
                              new-ans) ; return ans
                            #f)))))]) ; not found
    f))
