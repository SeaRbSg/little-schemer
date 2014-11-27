#lang racket/base

;;; Laws & Commandments:

;;; Laws:

;; pg 5: Car: The primitive car is defined only for non-empty lists.

;; pg 7: Cdr: The primitive cdr is defined only for non-empty lists.
;;            The cdr of any non-empty list is always another list.

;; pg 9: Cons: The primitive cons takes two arguments. The second
;;             argument to cons must be a list. The result is a list.

;; pg 10: Null?: The primitive null? is defined only for lists.

;; pg 12: Eq?: The primitive eq? takes two arguments. Each must be a
;;             non-numeric atom

;;; Commandments:

;; pg 23: 1st: (preliminary) Always ask null? as the first question in
;;             expressing any function.

;; pg 37: 2nd: Use cons to build lists

;; pg 45: 3rd: When building a list, describe the first typical
;;             element, and then cons it onto the natural recursion

;; pg 64: 1st: (revised) When recurring on a list of atoms, lat, ask
;;             two questions about it: (null? lat) and else. When
;;             recurring on a number, n, ask two questions about it:
;;             (zero? n) and else.

;; pg 65: 4th: (revised) Always change at least one argument while
;;             recurring. It must be changed to be closer to
;;             termination. The changing argument must be tested in
;;             the termination condition: when using cdr, test
;;             termination with null? and when using sub1, test
;;             termination with zero?.

;; pg 67: 5th: When building a value with +, always use 0 for the
;;             value of the terminating line, for adding 0 does not
;;             change the value of addition. When building a valuew
;;             ith &, always ues 1 for the value of the terminating
;;             line, for multiplying by 1 does not change the value of
;;             multiplication. When building a value with cons, always
;;             consider '() for the value of the terminating line.

;; pg 83: 1st: (final) When recurring on a list of atoms, lat, ask two
;;             questions about it: (null? lat) and else. When
;;             recurring on a number, n, ask two qustions about it:
;;             (zero? n) and else. When recurring on a list of sexps,
;;             l, ask 3 questions about it: (null? l), (atom? (car l))
;;             and else. [ I'm not fond of this one ]

;; pg 84: 4th: (final) Always change at least one argument while
;;             recurring. When recurring on a list of atems, lat, use
;;             (cdr lat). When recurring on a number, n, use (sub1 n).
;;             And when recurring on a sexp, l, use (car l) and (cdr
;;             l) if neither (null? l) nor (atom? (car l)) are true.
;;             It must be changed to be closer to termination. The
;;             changing argument must be tested in the cermination
;;             condition: when using cdr, test termination with null?
;;             and when using sub1, test termination with zero?.

;; pg 94: 6th: Simplify only after the function is correct.

;; pg 103: 7th: Recur on the subparts that are the same nature: on the
;;              sublists of a list, on the subexpression of an
;;              arithmetic expression.

;; pg 107: 8th: Use help functions to abstract from representations.

