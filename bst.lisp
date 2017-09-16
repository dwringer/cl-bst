;;;; BST.LISP - Binary Search Tree template macro
;;;; Copyright 2017 Darren W. Ringer <dwringer@gmail.com>

;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use,
;;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following
;;;; conditions:

;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.
(when (not (find-package 'deftest)) (load "deftest"))
(defpackage :bst
  (:use :common-lisp
	:deftest)
  (:export :make-bst
	   :value
	   :left
	   :right
	   :bst-min
	   :bst-max
	   :bst-insert
	   :bst-remove
	   :bst-member
	   :bst-empty
	   :bst-to-list
	   :define-bst-prototype))
(in-package :bst)

;;; MAKE-BST template macro:
;; Create a typed binary search tree using a custom template tree type.
;;
;;  Keyword parameters:
;;    ELEMENT-TYPE: Unquoted type specifier (as used in method parameter types)
;;    TEST: Function by which to compare values left-to-right when ordering
;;
;;  Returns:
;;    New instance of a custom BST type created from the template.
;;
;;  Example:
;;   > (progn 
;;       (defparameter *t* (make-bst :element-type string :test #'string<)) 
;;       *t*)
;;   #S(BST-1173 :LEFT NIL :VALUE NIL :RIGHT NIL)

(defgeneric bst-insert (x tr &key unique-only overwrite))
;; Nondestructive insert of value X into binary search tree TR.
;;
;;  Parameters:
;;    X: The element (of type ELEMENT-TYPE) to be inserted to the BST
;;    TR: The binary search tree into which X will be inserted
;;
;;  Keyword parameters:
;;    UNIQUE-ONLY: If true, disallow multiple nodes with the same value
;;    OVERWRITE: If true and UNIQUE-ONLY, new insertions overwrite matches
;;
;;  Returns:
;;    A BST created from TR with a [possibly additional] node representing X.
;;
;;  Example:
;;   > (setf *t* (bst-insert "world" *t*))
;;   #S(BST-1173 :LEFT NIL :VALUE "world" :RIGHT NIL)
;;   > (setf *t* (bst-insert "hello" *t*))
;;   #S(BST-1173
;;      :LEFT #S(BST-1173 :LEFT NIL :VALUE "hello" :RIGHT NIL)
;;      :VALUE "world"
;;      :RIGHT NIL)

(defgeneric bst-min (tr))
;; Find the minimum (leftmost branch) value in the given bst TR.
;;
;;  Parameters:
;;    TR: The binary search tree from which to produce the minimum element
;;  
;;  Returns:
;;    Minimum value contained in the binary search tree TR.
;;
;;  Example:
;;    > (bst-min *t*)
;;    "hello"

(defgeneric bst-max (tr))
;; Find the maximum (rightmost branch) value in the given bst TR.
;;
;;  Parameters:
;;    TR: The binary search tree from which to produce the maximum element
;;
;;  Returns:
;;    Maximum value contained in the binary search tree TR.
;;
;;  Example:
;;    > (bst-max *t*)
;;    "world"

(defgeneric bst-remove (x tr &optional first-only))
;;  Return a copy of the bst TR sans elements matching X.
;;
;;   Parameters:
;;     X: Element against which to match candidates for deletion
;;     TR: Binary search tree from which to remove elements matching X
;;
;;   Returns:
;;     A bst created from nodes of TR with matches of X removed.
;;
;;   Example:
;;    > (bst-remove "world" *t*)
;;    #S(BST-1173 :LEFT NIL :VALUE "hello" :RIGHT NIL)

(defgeneric bst-member (x tr))
;; If found, retrieve the subtree of binary search tree TR containing element X.
;;
;;  Parameters:
;;    X: The element (of type ELEMENT-TYPE) for which to search
;;    TR: The binary search tree in which X is to be sought
;;
;;  Returns:
;;    BST with the root node containing a match of X, or NIL if not found.
;;
;;  Example:
;;   > (bst-member "foo" *t*)
;;   NIL
;;   > (bst-member "hello" *t*)
;;   #S(BST-1173 :LEFT NIL :VALUE "hello" :RIGHT NIL)
;;   > (bst-member "world" *t*)
;;   #S(BST-1173
;;      :LEFT #S(BST-1173 :LEFT NIL :VALUE "hello" :RIGHT NIL)
;;      :VALUE "world"
;;      :RIGHT NIL)

(defgeneric bst-empty (tr))
;; Return whether the given binary search tree TR is empty.
;;
;;  Parameters:
;;    TR: The binary search tree to be checked for contents.
;;
;;  Returns:
;;    T if TR is empty [any/all nodes contain NIL], otherwise NIL.
;;
;;  Example:
;;   > (bst-empty *t*)
;;   NIL
;;   > (bst-empty (make-bst))
;;   T

(defgeneric bst-to-list (tr))
;; Convert the binary search tree TR into an ordered list.
;;
;;  Parameters:
;;    TR: Binary search tree to be converted into a list
;;
;;  Returns:
;;    A list composed of the elements of TR ordered from left-to-right.
;;
;;  Example:
;;   > (bst-to-list *t*)
;;   ("hello" "world")

(defvar *next-bst-id* 0)
(defvar *cached-bst-ids* (make-hash-table :test #'equal))

(defun bst-id (element-type test pkg-name)
  (let* ((args (list element-type test pkg-name))
	 (cached-id (gethash args *cached-bst-ids*)))
    (if (null cached-id)
	(values (let ((id (format nil "~A" *next-bst-id*)))
		  (setf (gethash args *cached-bst-ids*) id)
		  (setf *next-bst-id* (+ *next-bst-id* 1))
		  id)
		nil)
	(values cached-id t))))

(defmacro make-bst (&key (element-type 'real) (test #'<))
  "Create a typed binary search tree using a custom template tree type."
  (multiple-value-bind (id cached)
      (bst-id element-type test (package-name *package*))
    (let ((constructor
	   (intern (concatenate 'string "MAKE-BST-" id))))
      (if cached
	  `(,constructor)
	  `(make-bst! :element-type ,element-type
		      :test ,test
		      :id ,id
		      :constructor ,constructor)))))

(defmacro make-bst! (&key (element-type 'real) (test #'<) id constructor)
  (let* ((type (intern (concatenate 'string "BST-NODE-" id)))
	 (elem-type (intern (concatenate 'string "BST-ELEM-" id)))
	 (struct (intern (concatenate 'string "BST-" id))))
    `(progn
       (deftype ,type () '(or null ,struct))
       (deftype ,elem-type () '(or null ,element-type))
       (defstruct ,struct
	 (left nil :type ,type)
	 (value nil :type ,elem-type)
	 (right nil :type ,type))
       
       (defmethod bst-insert ((x ,element-type) (tr ,struct)
			      &key unique-only overwrite)
	 "Nondestructive insert of value X into binary search tree TR."
	 (with-slots ((l left) (v value) (r right)) tr
	   (cond ((null v) (,constructor :value x))
		 ((funcall ,test v x)
		  (,constructor :left l
				:value v
				:right (if (null r)
					   (,constructor :value x)
					   (bst-insert x r))))
		 ((or (not unique-only)
		      (funcall ,test x v))
		  (,constructor :left (if (null l)
					  (,constructor
					   :value x)
					  (bst-insert x l))
				:value v
				:right r))
		 (t (if overwrite
			(,constructor :left l :value x :right r)
			tr)))))

       (defmethod bst-min ((tr ,struct))
	 "Find the minimum (leftmost branch) value in the given bst TR."
	 (let ((l (slot-value tr 'left)))
	   (if (null l)
	       (slot-value tr 'value)
	       (bst-min l))))

       (defmethod bst-max ((tr ,struct))
	 "Find the maximum (rightmost branch) value in the given bst TR."
	 (let ((r (slot-value tr 'right)))
	   (if (null r)
	       (slot-value tr 'value)
	       (bst-max r))))
       
       (defmethod bst-remove ((x ,element-type) (tr ,struct)
			      &optional (first-only t))
	 "Return a copy of the bst TR sans elements matching X."
	 (with-slots ((l left) (v value) (r right)) tr
	   (cond ((null v) (values (,constructor) t))
		 ((funcall ,test v x)
		  (let ((nextr (when r
				 (multiple-value-bind (next empty)
				     (bst-remove x r first-only)
				   (when (not empty) next)))))
		    (values (,constructor :left l :value v :right nextr)
			    nil)))
		 ((funcall ,test x v)
		  (let ((nextl (when l
				 (multiple-value-bind (next empty)
				     (bst-remove x l first-only)
				   (when (not empty) next)))))
		    (values (,constructor :left nextl :value v :right r) nil)))
		 ((and (null l) (null r)) (values (,constructor) t))
		 ((null l) (if first-only r (bst-remove x r nil)))
		 ((null r) (if first-only l (bst-remove x l nil)))
		 (t (let* ((nextl (if first-only
				      l
				      (multiple-value-bind (next empty)
					  (bst-remove x l nil)
					(when (not empty) next))))
			   (nextv (bst-min r))
			   (nextr (multiple-value-bind (next empty)
				      (bst-remove nextv r t)
				    (when (not empty) next))))
		      (values
		       (,constructor :left nextl :value nextv :right nextr)
		       nil))))))
       
       (defmethod bst-member ((x ,element-type) (tr ,struct))
	 "If found, retrieve the subtree of TR containing element X."
	 (with-slots ((l left) (v value) (r right)) tr
	   (cond ((null v) nil)
		 ((funcall ,test x v) (when (not (null l)) (bst-member x l)))
		 ((funcall ,test v x) (when (not (null r)) (bst-member x r)))
		 (t tr))))
       
       (defmethod bst-empty ((tr ,struct))
	 "Return whether the given binary search tree TR is empty."
	 (with-slots ((l left) (v value) (r right)) tr
	   (and (null v)
		(or (null l) (bst-empty l))
		(or (null r) (bst-empty r)))))
       
       (defmethod bst-to-list ((tr ,struct))
	 "Convert the binary search tree TR into an ordered list."
	 (with-slots ((l left) (v value) (r right)) tr
	   (append
	    (when (not (null l)) (bst-to-list l))
	    (list v)
	    (when (not (null r)) (bst-to-list r)))))
       
       (,constructor))))

(setf (macro-function 'define-bst-prototype) (macro-function 'make-bst))

(defmacro assert-null-bst (tr)
  `(progn
     (assert (not (null ,tr)))
     (with-slots ((l left) (v value) (r right)) ,tr
       (assert (null l))
       (assert (null v))
       (assert (null r)))))

(defmacro make-test-reals-bst (from to &key (step 1) (apply-fn #'identity))
  `(do ((bst (make-bst))
	(i ,from (+ i ,step)))
       ((= i ,to) bst)
     (setf bst (bst-insert (funcall ,apply-fn i) bst))))

(defmacro make-tests ()
  `(progn
     (define-bst-prototype)
     (define-bst-prototype :element-type integer)
     (define-bst-prototype :element-type string :test #'string<)
     (deftests
       (test-inst make-bst ()
		  (tr)
	  (assert-null-bst tr))
       
       (test-inst make-bst (:element-type integer)
		  (tr)
	  (assert-null-bst tr)
	  (setf (slot-value tr 'value) 5)
	  (assert (= (slot-value tr 'value) 5)))
     
       (test-inst make-bst (:element-type string :test #'string<)
		  (tr)
	  (assert-null-bst tr)
	  (setf (slot-value tr 'value) "hello")
	  (assert (string= (slot-value tr 'value) "hello")))
     
       (test-pre-method bst-insert (1)
			(make-bst)
			(orig) (result)
	(assert (null (slot-value orig 'value)))
	(assert (= (slot-value result 'value) 1)))
       
       (test-pre-method bst-insert ("test")
			(make-bst :element-type string :test #'string<)
			(orig) (result)
	(assert (string= (slot-value result 'value) "test"))
	(let ((new (bst-insert "hello"
			       (bst-insert "world" result))))
	  (with-slots ((l left) (v value) (r right)) new
	    (assert (string= "test" v))
	    (assert (string= (slot-value l 'value) "hello"))
	    (assert (string= (slot-value r 'value) "world")))))
       
       (test-post-method bst-to-list
			 (make-test-reals-bst 0 5
			  :apply-fn #'(lambda (x) (* (expt -1 x) x)))
			 ()
			 (orig) (result)
	 (assert (equal '(-3 -1 0 2 4) result)))
       
       (test-post-method bst-min
			 (make-test-reals-bst -5 5 :step 0.5)
			 ()
			 (orig) (result)
	 (assert (= -5 result)))
       
       (test-post-method bst-max
			 (make-test-reals-bst -5 5 :step 0.5)
			 ()
			 (orig) (result)
	 (assert (= 4.5 result))))))
