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
(declaim (optimize (speed 3)
		   (compilation-speed 1)
		   (safety 2)
		   (debug 0)
		   (space 0)))
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
	   :bst-test
	   :bst-insert
	   :bst-fast-insert
	   :bst-set-insert
	   :bst-remove
	   :bst-clear
	   :bst-member
	   :bst-fast-member
	   :bst-empty
	   :bst-to-list
	   :bst-insert-list
	   :bst-constructor
	   :bst-map
	   :define-bst-prototype
	   :bstins
	   :bstrem
	   :bstmem
	   :mkbst))
(in-package :bst)

;;; bstins (elt tr &key (test #'<))
;; Insert element to abstract BST
;;
;;  Parameters:
;;    ELT: Element (of any type) to insert to bst TR using TEST
;;    TR: Tree (as created with MKBST) into which to insert the value
;;    TEST: Test by which to compare values and insert ELT in order
;;
;;  Returns:
;;    A new BST made from elements of TR with ELT inserted

;;; bstrem (elt tr &key (all nil) (test #'<))
;; Remove matching element[s] from abstract BST
;;
;;  Parameters:
;;    ELT: Element to be removed from the bst TR (matched by TEST)
;;    TR: Tree (as created with MKBST) from which to remove the match(es)
;;
;;  Keyword parameters:
;;    ALL: Remove all matches instead of just the first?
;;    TEST: Test by which the value to be removed will be matched in the tree
;;
;;  Returns:
;;    A new BST condensed from elements of TR with ELT(s) removed

;;; bstmem (elt tr &key (test #'<))
;; Determine membership of element in abstract BST
;;
;;  Parameters:
;;    ELT: Element to be checked for membership in the bst TR
;;    TR: Tree to be searched for a matching element
;;
;;  Keyword parameters:
;;    TEST: Test by which to determine if a value in the tree matches ELT
;;
;;  Returns:
;;    The sub-tree of bst TR with a value matching ELT at its root.

;;; mkbst (&optional initial-contents (test #'<))
;; Make an asbtract BST, accepting any type but requiring explicit comparators
;;
;;  Parameters:
;;    INITIAL-CONTENTS: If supplied, fill newly created BST with these values
;;    TEST: Test argument to supply to BSTINS when inserting initial contents
;;
;;  Returns:
;;    A new instance of a BST supporting any value type by explicit comparisons.

;;; MAKE-BST template macro:
;; Create a typed binary search tree using a custom template tree type.
;;
;;  Keyword parameters:
;;    ELEMENT-TYPE: Unquoted type specifier (as used in method parameter types)
;;    TEST: Function by which to compare values left-to-right when ordering
;;
;;  Returns:
;;    New instance of a custom BST type created from the template.

(defgeneric bst-test (tr))
;; Return the test function used by binary search trees of the same type as TR.
;;
;;  Parameters:
;;    TR: BST, the type of which to query for the associated test function
;;
;;  Returns:
;;    A comparator function implementing the test used by the given BST

(defgeneric bst-insert (x tr &key unique-only overwrite test))
;; Nondestructive insert of value X into binary search tree TR.
;;
;;  Parameters:
;;    X: The element (of type ELEMENT-TYPE) to be inserted to the BST
;;    TR: The binary search tree into which X will be inserted
;;
;;  Keyword parameters:
;;    UNIQUE-ONLY: If true, disallow multiple nodes with the same value
;;    OVERWRITE: If true and UNIQUE-ONLY, new insertions overwrite matches
;;    TEST: If present, overrides the BST's comparison function
;;
;;  Returns:
;;    A BST created from TR with a [possibly additional] node representing X.

(defgeneric bst-fast-insert (x tr))
;; Nondestructive insert of value X into binary search tree TR.
;;
;;  Parameters:
;;    X: The element (of type ELEMENT-TYPE) to be inserted to the BST
;;    TR: The binary search tree into which X will be inserted
;;
;;  Returns:
;;    A BST created from TR with a [possibly additional] node representing X.

(defgeneric bst-set-insert (x tr))
;; Nondestructive overwriting set insert of X into bst TR.
;;
;;  Parameters:
;;    X: The element (of type ELEMENT-TYPE) to be inserted to the BST
;;    TR: The binary search tree into which X will be inserted
;;
;;  Returns:
;;    A BST created from TR with a [possibly additional] node representing X.

(defgeneric bst-min (tr))
;; Find the minimum (leftmost branch) value in the given bst TR.
;;
;;  Parameters:
;;    TR: The binary search tree from which to produce the minimum element
;;  
;;  Returns:
;;    Minimum value contained in the binary search tree TR.

(defgeneric bst-max (tr))
;; Find the maximum (rightmost branch) value in the given bst TR.
;;
;;  Parameters:
;;    TR: The binary search tree from which to produce the maximum element
;;
;;  Returns:
;;    Maximum value contained in the binary search tree TR.

(defgeneric bst-remove (x tr &key first-only test))
;;  Return a copy of the bst TR sans elements matching X.
;;
;;   Parameters:
;;     X: Element against which to match candidates for deletion
;;     TR: Binary search tree from which to remove elements matching X
;;
;;   Keyword parameters:
;;     FIRST-ONLY: If true, only remove the first matching element
;;     TEST: If present, overrides the BST's comparison function
;;
;;   Returns:
;;     A bst created from nodes of TR with matches of X removed.

(defgeneric bst-clear (tr))
;;  Return an empty binary search tree of the same type as TR.
;;
;;   Parameters:
;;     TR: Tree from which to infer type to create a new tree
;;
;;   Return:
;;     An empty [new] binary search tree, of the same type as TR.

(defgeneric bst-member (x tr &optional test))
;; If found, retrieve subtree of binary search tree TR containing element X.
;;
;;  Parameters:
;;    X: The element (of type ELEMENT-TYPE) for which to search
;;    TR: The binary search tree in which X is to be sought
;;    TEST: If present, overrides the BST's comparison function
;;
;;  Returns:
;;    BST with the root node containing a match of X, or NIL if not found.

(defgeneric bst-fast-member (x tr))
;; With implicit comparisons, find subtree of TR containing element X.
;;
;;  Parameters:
;;    X: The element (of type ELEMENT-TYPE) for which to search
;;    TR: The binary search tree in which X is to be sought
;;
;;  Returns:
;;    BST with the root node containing a match of X, or NIL if not found.

(defgeneric bst-empty (tr))
;; Return whether the given binary search tree TR is empty.
;;
;;  Parameters:
;;    TR: The binary search tree to be checked for contents.
;;
;;  Returns:
;;    T if TR is empty [any/all nodes contain NIL], otherwise NIL.

(defgeneric bst-to-list (tr))
;; Convert the binary search tree TR into an ordered list.
;;
;;  Parameters:
;;    TR: Binary search tree to be converted into a list
;;
;;  Returns:
;;    A list composed of the elements of TR ordered from left-to-right.

(defgeneric bst-insert-list (lst tr &key unique-only overwrite test as-set))
;; Insert all values from LST into the bst TR.
;;
;;  Parameters:
;;    LST: List from which values will be inserted to the tree
;;    TR: Binary search tree into which values will be inserted
;;
;;  Keyword parameters:
;;    - This function uses the same keyword parameters as BST-INSERT -
;;    AS-SET: If t, Ignore all other keyword parameters and uses BST-SET-INSERT

(defgeneric bst-constructor (tr))
;; Return the function used to construct instances of the type of TR.
;;
;;  Parameters:
;;    TR: Binary search tree from which type will be inferred
;;
;;  Returns:
;;    A function to construct instances of the inferred type

(defgeneric bst-map (function tr &optional into-bst))
;; Apply FUNCTION to every value in the binary search tree TR
;;
;;  Parameters:
;;    FUNCTION: Function to be applied to each tree value
;;    TR: Binary search tree to have function applied to elements
;;    INTO-BST: If present, use this BST's type to create the result tree
;;
;;  Returns:
;;    A new BST (defaults to same type as TR) with FUNCTION applied.

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

(defmacro kwarg-cond (use-kw?-var kw-default-var
		      if-specifiable-code
		      if-specified-code
		      default-code)
  `(cond (,use-kw?-var ,if-specifiable-code)
	 (,kw-default-var ,if-specified-code)
	 (t ,default-code)))

(defmacro make-bst! (&key (element-type 'real) (test #'<) id constructor)
  (let* ((type (intern (concatenate 'string "BST-NODE-" id)))
	 (elem-type (intern (concatenate 'string "BST-ELEM-" id)))
	 (struct (intern (concatenate 'string "BST-" id)))
	 (bst-insert-fn (intern (concatenate 'string "BST-INSERT-" id "-FN")))
	 (bst-set-insert-fn
	  (intern (concatenate 'string "BST-SET-INSERT-" id "-FN")))
	 (bst-fast-insert-fn
	  (intern (concatenate 'string "BST-FAST-INSERT-" id "-FN"))))
    
    (defun make-insert-method (name uniques-key? overwrite-key? test-key?
			       &key uniques-val (overwrite-val t) test-val)
      (let ((function-name
	     (intern (concatenate 'string (symbol-name name) "-" id "-FN")))
	    (insert-args nil)
	    (method-args nil)
	    (params '(x tr))
	    (method-params `((x ,element-type) (tr ,struct)))
	    (test-form (cond (test-key? 'test)
			     (test-val test-val)
			     (t test))))
	(when (or uniques-key? overwrite-key? test-key?)
	  (setf method-params (append method-params (list '&key)))
	  (setf params (append params (list '&key))))
	(macrolet ((create-keyword-param (kwname include? default)
		     `(when ,include?
			(let ((next-param (if ,default
					      (list ',kwname ,default)
					      ',kwname)))
			  (setf params (append params (list next-param)))
			  (setf method-params
				(append method-params (list next-param))))
			(setf insert-args
			      (append insert-args
				      (list '',(intern (symbol-name kwname)
						       "KEYWORD")
					    '',kwname)))
			(setf method-args
			      (append method-args
				      (list ',(intern (symbol-name kwname)
						      "KEYWORD")
					    ',kwname))))))			
	  (create-keyword-param unique-only uniques-key? uniques-val)
	  (create-keyword-param overwrite overwrite-key? overwrite-val)
	  (create-keyword-param test test-key? test-val))
	`(progn
	   (defun ,function-name ,params
	     (declare (,element-type x) (,struct tr))
	     (macrolet ((%bst-insert-x (bst)
			  `(,',function-name x ,bst ,,@insert-args))
			(%make-bst (l v r)
			  `(,',constructor :left ,l :value ,v :right ,r)))
	       (with-slots ((l left) (v value) (r right)) tr
		 ,@(when test-key? `((when (not test) (setf test ,test))))
		 (cond ((null v) (,constructor :value x))
		       ((funcall ,test-form v x)
			(%make-bst l v (if (null r)
					   (,constructor :value x)
					   (%bst-insert-x r))))
		       (,(kwarg-cond uniques-key? uniques-val
				     `(or (not unique-only)
					  (funcall ,test-form x v))
				     `(funcall ,test-form x v)
				     't)
			(%make-bst (if (null l)
				       (,constructor :value x)
				       (%bst-insert-x l))
				   v
				   r))
		       ,@(if (or uniques-key? uniques-val)
			     (kwarg-cond overwrite-key? overwrite-val
				 '((t (if overwrite (%make-bst l x r) tr)))
				 '((t (%make-bst l x r)))
				 '((t tr))))))))
	   (defmethod ,name ,method-params
	     (declare (,element-type x) (,struct tr))
	     (,function-name x tr ,@method-args)))))
    
    (defun make-remove-method (name first-only-key? test-key?
			       &key first-only-val test-val)
      (let ((function-name
	     (intern (concatenate 'string (symbol-name name)
				  "-" id "-FN")))
	    (remove-args nil)
	    (method-args nil)
	    (params '(x tr))
	    (method-params  `((x ,element-type) (tr ,struct)))
	    (test-form (cond (test-key? 'test)
			     (test-val test-val)
			     (t test))))
	(when (or first-only-key? test-key?)
	  (setf method-params (append method-params (list '&key)))
	  (setf params (append params (list '&key))))
	(macrolet ((create-keyword-param (name include? default)
		     `(when ,include?
			(let ((next-param (if ,default
					      (list ',name ,default)
					      ',name)))
			  (setf params (append params (list next-param)))
			  (setf method-params
				(append method-params (list next-param))))
			(setf remove-args
			      (append remove-args
				      (list '',(intern (symbol-name name)
						       "KEYWORD")
					    '',name)))
			(setf method-args
			      (append method-args
				      (list ',(intern (symbol-name name)
						      "KEYWORD")
					    ',name))))))
	  (create-keyword-param first-only first-only-key? first-only-val)
	  (create-keyword-param test test-key? test-val)
	  `(progn
	     (defun ,function-name ,params
	       (declare (,element-type x) (,struct tr))
	       (macrolet ((%bst-remove-x (bst)
			    `(,',function-name x ,bst ,,@remove-args))
			  (%make-bst (l v r)
			    `(,',constructor :left ,l :value ,v :right ,r))
			  (%when-not-empty (bst-remove-form)
			    `(multiple-value-bind (next empty) ,bst-remove-form
			       (when (not empty) next))))
		 (with-slots ((l left) (v value) (r right)) tr
		   ,@(when test-key? `((when (not test) (setf test ,test))))
		   (cond ((null v) (values (,constructor) t))
			 ((funcall ,test-form v x)
			  (values (%make-bst l v (when r (%when-not-empty
							  (%bst-remove-x r))))
				  nil))
			 ((funcall ,test-form x v)
			  (values (%make-bst (when l (%when-not-empty
						      (%bst-remove-x l))) v r)
				  nil))
			 ((and (null l) (null r)) (values (,constructor) t))
			 ((null l)
			  ,(kwarg-cond first-only-key? first-only-val
			       '(if first-only
				 (values r nil)
				 (%bst-remove-x r))
			       '(values r nil)
			       '(%bst-remove-x r)))
			 ((null r)
			  ,(kwarg-cond first-only-key? first-only-val
			       '(if first-only
				 (values l nil)
				 (%bst-remove-x l))
			       '(values l nil)
			       '(%bst-remove-x l)))
			 (t (let* ((nextv (bst-min r)))
			      (values (%make-bst
				       ,(kwarg-cond first-only-key?
						    first-only-val
					    '(if first-only
					      l
					      (%when-not-empty
					       (%bst-remove-x l)))
					    'l
					    '(%when-not-empty
					      (%bst-remove-x l)))
				       nextv
				       (%when-not-empty
					(,name nextv r
					       ,@(when first-only-key?
						       '(:first-only t))
					       ,@(when test-key?
						       '(:test test)))))
				      nil)))))))
	     (defmethod ,name ,method-params
	       (declare (,element-type x) (,struct tr))
	       (,function-name x tr ,@method-args))))))
    
    `(progn
       (deftype ,type () '(or null ,struct))
       (deftype ,elem-type () '(or null ,element-type))
       (defstruct ,struct
	 (left nil :type ,type)
	 (value nil :type ,elem-type)
	 (right nil :type ,type))
       
       (defmethod bst-test ((tr ,struct))
	 "Return the test function used by the type of binary search tree TR."
	 ,test)

       ,(make-insert-method 'bst-insert t t t :overwrite-val nil)
       ;;  "Nondestructive insert of value X into binary search tree TR."

       ,(make-insert-method 'bst-fast-insert nil nil nil)
       ;;  "Uncustomizable faster bst-insert, with only implicit comparisons"

       ,(make-insert-method 'bst-set-insert nil nil nil
			    :uniques-val t :overwrite-val t)
       ;;  "Nondestructive overwriting set insert of X into bst TR."
       
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

       ,(make-remove-method 'bst-remove t t :first-only-val t)
       ;;  "Return a copy of the bst TR sans elements matching X."
       
       (defmethod bst-clear ((tr ,struct))
	 "Return an empty binary search tree of the same type as TR."
	 (,constructor))
       
       (defmethod bst-member ((x ,element-type) (tr ,struct) &optional test)
	 "If found, retrieve the subtree of TR containing element X."
	 (with-slots ((l left) (v value) (r right)) tr
	   (when (not test) (setf test ,test))
	   (cond ((null v) nil)
		 ((funcall test x v)
		  (when (not (null l)) (bst-member x l test)))
		 ((funcall test v x)
		  (when (not (null r)) (bst-member x r test)))
		 (t tr))))

       (defmethod bst-fast-member ((x ,element-type) (tr ,struct))
	 "With implicit comparisons, find subtree of TR containing element X."
	 (labels ((bst-fast-member-fn (x tr)
		    (with-slots ((l left) (v value) (r right)) tr
		      (cond ((null v) nil)
			    ((funcall ,test x v)
			     (When (not (null l)) (bst-fast-member-fn x l)))
			    ((funcall ,test v x)
			     (when (not (null r)) (bst-fast-member-fn x r)))
			    (t tr)))))
	   (bst-fast-member-fn x tr)))
       
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
       
       (defmethod bst-insert-list (lst (tr ,struct)
				   &key unique-only overwrite test as-set)
	 "Insert all values from LST into the bst TR."
	 (if as-set
	     (macrolet ((bst-update (x bst)
			  `(,',bst-set-insert-fn ,x ,bst)))
	       (dolist (x lst tr) (setf tr (bst-update x tr))))
	     (if (not (or unique-only test))
		 (macrolet ((bst-update (x bst)
			      `(,',bst-fast-insert-fn ,x ,bst)))
		   (dolist (x lst tr) (setf tr (bst-update x tr))))
		 (macrolet ((bst-update (x bst)
			      `(,',bst-insert-fn ,x ,bst
						 :unique-only unique-only
						 :overwrite overwrite
						 :test test)))
		   (dolist (x lst tr) (setf tr (bst-update x tr)))))))
       
       (defmethod bst-constructor ((tr ,struct))
	 "Return the function used to construct instances of the type of TR."
	 (function ,constructor))

       (defmethod bst-map (function (tr ,struct) &optional into-bst)
	 "Apply FUNCTION to every value in the binary search tree TR"
	 (with-slots ((l left) (v value) (r right)) tr
	   (let ((newl (when l (bst-map function l into-bst)))
		 (newv (funcall function v))
		 (newr (when r (bst-map function r into-bst))))
	     (if into-bst
		 (funcall (bst-constructor into-bst)
			  :left newl
			  :value newv
			  :right newr)
		 (,constructor :left (when l (bst-map function l))
			       :value (funcall function v)
			       :right (when r (bst-map function r)))))))
       
       (,constructor))))

(setf (macro-function 'define-bst-prototype) (macro-function 'make-bst))

(define-bst-prototype :element-type t :test #'(lambda (a b) (declare (ignore a)) b))

(defun bstins (elt tr &key (test #'<))
  "Insert element to abstract BST"
  (bst-insert elt tr :test test))

(defun bstrem (elt tr &key (all nil) (test #'<))
  "Remove matching element[s] from abstract BST"
  (bst-remove elt tr :first-only (not all) :test test))

(defun bstmem (elt tr &key (test #'<))
  "Determine membership of element in abstract BST"
  (bst-member elt tr test))

(defun mkbst (&optional initial-contents (test #'<))
  "Make an asbtract BST, accepting any type but requiring explicit comparators"
  (do ((bst
	(make-bst :element-type t
		  :test #'(lambda (a b) (declare (ignore a)) b)))
       (i 0 (+ i 1)))
      ((= i (length initial-contents)) bst)
    (setf bst (bstins (elt initial-contents i) bst :test test))))

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
	 (assert (= 4.5 result)))

       (test-pre-method bst-insert-list ((list 5 3 8 2 9 7 1 4 6))
			(make-bst :element-type integer)
			(orig) (result)
	(assert (equal (bst-to-list result)
		       '(1 2 3 4 5 6 7 8 9))))

       (test-pre-method bst-remove (7 :first-only nil
				      :test #'<)
			(mkbst '(3 7 6 9 5 6 3 7 45 9 7))
			(orig) (result)
        (assert (equal '(3 3 5 6 6 9 9 45)
		       (bst-to-list result))))

       (test-pre-method bst-remove (7)
			(bst-insert-list '(3 7 6 9 5 6 3 7 45 9 4 7)
					 (make-bst :element-type integer))
			(orig) (result)
	(assert (equal '(3 3 4 5 6 6 7 7 9 9 45)
		       (bst-to-list result))))

       (test-pre-method bst-insert-list ((do (lst
					      (i 0 (1+ i)))
					     ((= i 1000000) lst)
					   (push (random 10000000) lst)))
			(make-bst :element-type integer)
			(orig) (result)
	(assert (equal 1000000 (length (bst-to-list result))))))))

