;;; Eliza Nip
;;; CS381 Lab4
;;; Self-Check Questions 
#lang racket
(define (f lst)
; (a) ;
    (if (null? lst)
    ; (b) ;
    '()
    ; (c) ;
    (cons (+ 1 (car lst))  (f (cdr lst)))))
;;; Run this function as follows.  
;;; Question 1: What output do you get?(f '(3 1 4 1 5 9))
;;;     '(4 2 5 2 6 10)
;;; Question 2:  What does this function f do?
;;;     This function f will add 1 to items in list 
;;; Question 3:  Give a comment that explains the line following(a),(b), and(c).
;;; (a) Check if the list argument is null.
;;; (b) return parameter without evaluation. If the list is empty, return empty list.
;;; (c) condition statement if lst is not null. Add 1 to the first value of list, then move to the next value of list. Recusivly use function f to add 1 to every value in list.
;;; Question 4:  Trace the call given in Question 1, showing each recursive call to the function.
;;;              Expand the termlstto specific lists or atoms at each depth of the recursion.
;;;       First value  3 + 1 = 4, next value in list is 1.
;;;       Second value 1 + 1 = 2, next value in list is 4.
;;;       Third value 4 + 1 = 5, next value in list is 1.
;;;       Fourth value 1 + 1 = 2, next value in list is 5.
;;;      Fifth value 5 + 1 = 6, next value in list is 9.
;;;     Sixth value 9 + 1 = 10, since this is the last value of list, there is no pair value with sixth value, so we exit and return lst.
;;; Member function 
(define (member? e lst)
    ;;; Check if lst is empty, return false if it is.lst
    (cond((null? lst) #f) 
    ;;; Check if first value of lst equals to e. Return true if so 
    ((equal? e (car lst)) #t)
    ;;; If e isn't the first value of lst, check the rest of the lst.
    (else (member? e (cdr lst)))))

;;; Set function
(define (set? lst)
    ;;; Check if lst is empty, return true if its empty set/
    (cond((null? lst) #t)
    ;;;Use the member function to check if the first element of lst is duplicated with other in lst.Return false if so
    ((member? (car lst) (cdr lst)) #f)
    ;;;Use set functon to check the rest of the list recusivly
    (else (set? (cdr lst)))))

;;; Union function 
(define (union lst1 lst2)
    ;;;Check if lst1 is empty, return lst2
    (if(null? lst1)lst2
    ;;; Union the rest of lst1
    (union (cdr lst1)
            ;;; Use member? function to see if the first element in lst1 is in lst2
           (if(member? (car lst1)lst2)
            ;;; Call lst2   
              lst2
    ;;; If not, make a join lst with first element of lst1 + lst2
    (cons (car lst1) lst2)) )))

;;; Intersect function
(define (intersect lst1 lst2)
    ;;; Check if lst1 is empty, return empty set
    (cond((null? lst1)'())
    ;;; Use member? function to see if the first element in lst1 is in lst2, then concatinate first element in lst1 and the intersect of rest of the lists usinh intersect funciton
    ((member? (car lst1)lst2)(cons (car lst1)(intersect (cdr lst1) lst2)))
    ;;; If not, run intersect again on lst1, lst2
    (else (intersect (cdr lst1) lst2))))

;;; Difference function
(define (difference lst1 lst2)
    ;;; Check if lst1 is empty, return empty set 
    (cond((null? lst1)'())
    ;;; Use member? function to see if the first element in lst1 is in lst2, then call difference for the rest of the lst1,lsts2.difference
    ((member? (car lst1) lst2)(difference(cdr lst1)lst2))
    ;;; If not, concatinate the first element in lst1, and the res of lst1 - lst2
    (else (cons (car lst1) (difference(cdr lst1)lst2)))))

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))
(let loop ()
    (define line (read-line (current-input-port) 'any))
    (if (eof-object? line)(display "")
    (begin  (print (eval (read (open-input-string line)) ns)) (newline) (loop))))