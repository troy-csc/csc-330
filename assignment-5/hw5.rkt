#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent)

;; Problem A

;; CHANGE (put your solutions here)
(define (mupllist->racketlist lst)
  (cond [(aunit? lst) null]
        [(apair? lst) (if (apair? (apair-e1 lst)) ; for nested lists
                          (cons (mupllist->racketlist (apair-e1 lst))
                                (mupllist->racketlist (apair-e2 lst)))
                          (cons (apair-e1 lst)
                                (mupllist->racketlist (apair-e2 lst))))]))
(define (racketlist->mupllist lst)
  (if (null? lst)
      (aunit)
      (if (list? (car lst)) ; for nested lists
          (apair (racketlist->mupllist (car lst))
                 (racketlist->mupllist (cdr lst)))
          (apair (car lst)
                 (racketlist->mupllist (cdr lst))))))

;; Problem B

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp (see below).
(define (eval-under-env e env)
  (cond [(var? e)
         (envlookup env (var-string e))]
        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1)
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; "CHANGE" add more cases here
        ;; one for each type of expression
        [(int? e) (if (number? (int-num e))
                      e ; return value itself
                      (eval-under-env (int-num e) env))] ; if value is an expression, return evaluated expression
        [(ifgreater? e) (let ([v1 (eval-under-env (ifgreater-e1 e) env)] ; eval first 2 subexpressions
                              [v2 (eval-under-env (ifgreater-e2 e) env)])
                          (if (and (int? v1) ; check for correct type
                                   (int? v2))
                              (if (> (int-num v1) ; if greater, evaluate e3 else e4
                                     (int-num v2))
                                  (eval-under-env (ifgreater-e3 e) env)
                                  (eval-under-env (ifgreater-e4 e) env))
                              (error "MUPL values not integers")))]
        [(fst? e) (let ([v1 (eval-under-env (fst-e e) env)]) ; evaluate in environment
                    (if (apair? v1) ; check if pair
                        (apair-e1 v1) ; return first part
                        (error "MUPL value not a pair")))]
        [(snd? e) (let ([v1 (eval-under-env (snd-e e) env)])
                    (if (apair? v1)
                        (apair-e2 v1)
                        (error "MUPL value not a pair")))]
        [(isaunit? e) (if (aunit? (eval-under-env (isaunit-e e) env))
                          (int 1)
                          (int 0))]
        [(aunit? e) e]
        [(apair? e) (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(mlet? e) (eval-under-env (mlet-body e)
                                   (cons (cons (mlet-var e)
                                               (eval-under-env (mlet-e e) env))
                                         env))]
        [(fun? e) (closure env e)]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (if (fun-nameopt (closure-fun v1))
                   (eval-under-env (fun-body (closure-fun v1)) (cons (cons (fun-formal (closure-fun v1))
                                                                           (eval-under-env (call-actual e) env))
                                                                     (cons (cons (fun-nameopt (closure-fun v1)) v1)
                                                                     (closure-env v1))))
                   (eval-under-env (fun-body (closure-fun v1)) (cons (cons (fun-formal (closure-fun v1))
                                                                           (eval-under-env (call-actual e) env))
                                                                     (closure-env v1))))
               (error "MUPL function did not evaluate to a closure")))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
;; note how evaluating an expression start with an empty environment
(define (eval-exp e)
  (eval-under-env e null))

;; Problem C

(define (ifaunit e1 e2 e3) (if (aunit? e1) e2 e3))

(define (mlet* lstlst e2)
  (letrec ([f (lambda (lst)
                (if (null? lst) ; base case returns final expression to be evaluated
                    e2
                    (mlet (car (car lst)) ; get variable name from head of pair of the head of the lst
                          (cdr (car lst)) ; get expression
                          (f (cdr lst)))))]) ; recursive call to helper function as the body of the mlet
    (f lstlst)))

; this function works by using ifgreater
; it checks if e1 > e2
; if yes, return e4
; if no, check if e2 > e1
;        if yes, return e4
;        if no, e1 = e2, and return e3
(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y") e4
                    (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem D

(define mupl-map
  (fun "mupl-map" "f"
       (fun #f "lst"
            (ifeq (isaunit (var "lst")) (int 1) ; check if list is empty
                  (aunit) ; if yes, return empty list
                  (apair (call (var "f") (fst (var "lst")))  ; else return pair of result of call of f on first val in list
                         (call (call (var "mupl-map") (var "f")) (snd (var "lst")))))))) ; and the recursive call to mupl-map with the rest of the list
;; this binding is a bit tricky. it must return a function.
;; the first two lines should be something like this:
;;
;;   (fun "mupl-map" "f"    ;; it is  function "mupl-map" that takes a function f
;;       (fun #f "lst"      ;; and it returns an anonymous function
;;          ...
;;
;; also remember that we can only call functions with one parameter, but
;; because they are curried instead of
;;    (call funexp1 funexp2 exp3)
;; we do
;;    (call (call funexp1 funexp2) exp3)
;; 

(define mupl-mapAddN
  (mlet "map" mupl-map
        (fun #f "i" ; function that takes in the integer      | curried inputs
             (fun #f "lst" ; function that takes in the list  |
                  (call (call (var "map") (fun #f "x" (add (var "i") (var "x")))) (var "lst")))))) ; call mupl-map with an anonymous function that adds i to an element on the list
