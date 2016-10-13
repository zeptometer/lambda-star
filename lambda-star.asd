;;; -*- lisp-mode -*-

(defpackage :lambda-star.asdf
  (:use :common-lisp
	:asdf))

(in-package :lambda-star.asdf)

(defsystem lambda-star
  :serial t
  :components ((:file "term")
	       (:file "parse")
	       (:file "print")
	       (:file "reduce")
	       (:file "eval")
	       (:file "package"))
  :depends-on ("optima"))
