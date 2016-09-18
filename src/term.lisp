(defpackage :coffee.acupof.lambda-star.term
  (:use :common-lisp
	:optima)
  (:shadow :abs :push :pop))

(in-package :coffee.acupof.lambda-star.term)

(defstruct app
  level fun arg)

(defstruct name
  level str)

(defstruct var
  name skip sub)

(defstruct abs
  bind body)

(defstruct push
  name term rest)

(defstruct pop
  name rest)

;;; printer
(defun stringfy-name (name)
  (with-output-to-string (out)
    (format out "~a~a" (name-str name) (name-level name))))

(defun stringfy-term (term)
  (with-output-to-string (out)
    (match term
      ((var- (name (name- level str))
	     skip sub)
       (format out "~a~a^~a[~a]" str level skip (stringfy-subst sub)))
      ((app- level fun arg)
       (format out "~a @~a ~a"
	       (stringfy-term fun)
	       level
	       (stringfy-term arg)))
      ((abs (bind (name- level str)) body)
       (format out "\\~a~a.~a" str level (stringfy-term body))))))

(defun stringfy-subst (sub)
  (with-output-to-string (out)
    (match sub
      (:id nil)
      ((push- name term rest)
       (format out "+~a(~a)~a" name (stringfy-term term) (stringfy-subst rest)))
      ((pop- name rest)
       (format out "-~a~a" name (stringfy-subst rest))))))

;;; parser
;; TERM := (var NAME SKIP SUB)
;;      |  (LEVEL TERM TERM)
;;      |  (fn NAME TERM)
;; SUB := ()
;;     |  ((+ NAME TERM) . SUB)
;;     |  ((- NAME) . SUB)
;; NAME := (STR LEVEL)

(defun parse (sexp)
  (match sexp
    ((list 'var name skip sub)
     (make-var :name (parse-name name) :skip skip :sub (parse-sub sub)))
    ((list 'fn name body)
     (make-abs :bind (parse-name name) :body (parse body)))
    ((list level fun arg)
     (make-app :level level :fun (parse fun) :arg (parse arg)))))

(defun parse-name (sexp)
  (make-name :str (car sexp) :level (cadr sexp)))

(defun parse-sub (sexp)
  (match sexp
    (nil :id)
    ((cons (list '+ name term) rest)
     (make-push :name (parse-name name)
		:term (parse term)
		:rest (parse-sub rest)))
    ((cons (list '- name) rest)
     (make-pop :name (parse-name name)
	       :rest (parse-sub rest)))))

;;;; Substitution
;;; <v, d>-component
(defun component (sub name% skip)
  (match sub
    (:id (make-var :name name% :skip skip :sub sub))
    ((push- name term rest)
     (if (and (equal name name%) (zerop skip))
	 term
	 (component rest name (- skip (if (equal name name%) 1 0)))))
    ((pop- name rest)
     (component rest name (+ skip (if (equal name name%) 1 0))))))

;;; S-restriction
(defun restrict (sub pred)
  (match sub
    (:id :id)
    ((push- name term rest)
     (if (funcall pred name)
	 (make-push :name name
		    :term term
		    :rest (restrict rest pred))
	 (restrict rest pred)))
    ((pop- name rest)
     (if (funcall pred name)
	 (make-pop :name name
		   :rest (restrict rest pred))
	 (restrict rest pred)))))

(defun restrict< (sub level)
  (restrict sub (lambda (x) (< (name-level x) level))))

(defun restrict>= (sub level)
  (restrict sub (lambda (x) (>= (name-level x) level))))

;;; apply substitution
(defun apply-subst (term sub%)
  (match term
    ((var- name skip sub) (if (eq :id (restrict>= sub (name-level name)))
			      (make-var :name name
					:skip skip
					:sub sub%)
			      (apply-subst (component sub% name skip)
					   (restrict< (compose-subst sub sub%)
						      (name-level name)))))
    ((abs- bind body)
     (make-abs :bind bind
	       :body (apply-subst body
				  (make-push :name bind
					     :term (make-var :name bind
							     :skip 0
							     :sub :id)
					     :rest (compose-subst sub%
								  (make-pop :name bind :rest :id))))))
    ((app- level fun arg)
     (make-app :level level
	       :fun (apply-subst fun sub%)
	       :arg (apply-subst arg (restrict>= sub% level))))))

;;; subst composition
(defun compose-subst (a b)
  (match a
    (:id b)
    ((push- name term rest) (make-push :name name
				       :term (apply-subst term (restrict>= b (name-level name)))
				       :rest (compose-subst rest b)))
    ((pop- name rest) (make-pop :name name
				:rest (compose-subst rest b)))))

;;;; Reduction
(defun is-beta-redex (term)

  (match term
    ((guard (app- level
		  (fun (abs- (bind (name- (level level%))))))
	    (= level level%))
     t)
    (_ nil)))

(defun beta-reduction (term)
  (match term
    ((app- (fun (abs- bind body))
	   arg)
     (apply-subst body
		  (make-push :name bind
			     :term arg)))))
