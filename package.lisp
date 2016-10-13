(defpackage :lambda-star
  (:use :common-lisp
	:lambda-star.term
	:lambda-star.parse
	:lambda-star.print
	:lambda-star.reduce
	:lambda-star.eval))

(in-package :lambda-star)

(defparameter quo (parse '(fn Y (fn x (1 x Y)))))
(defparameter unq (parse '(fn Z (0 Z (fn Y Y)))))
