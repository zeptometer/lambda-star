(defpackage :coffee.acupof.lambda-star.term
  (:use :common-lisp)
  (:export :app
	   :make-app
	   :app-p
	   :app-level
	   :app-fun
	   :app-arg
	   :name
	   :make-name
	   :name-p
	   :name-level
	   :name-str
	   :name=
	   :var
	   :make-var
	   :var-p
	   :var-name
	   :var-skip
	   :var-sub
	   :abst
	   :make-abst
	   :abst-p
	   :abst-bind
	   :abst-body
	   :spush
	   :make-spush
	   :spush-p
	   :spush-name
	   :spush-term
	   :spop
	   :make-spop
	   :spop-p
	   :spop-name))

(in-package :coffee.acupof.lambda-star.term)

(defstruct app
  level fun arg)

(defstruct name
  level str)

(defun name= (a b)
  (and (string= (name-str a) (name-str b))
       (= (name-level a) (name-level b))))

(defstruct var
  name skip sub)

(defstruct abst
  bind body)

;;; Substitution
;; a substitution is represented as a list of push and pop
(defstruct spush
  name term)

(defstruct spop
  name)
