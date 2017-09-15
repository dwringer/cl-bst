;;;; FINITE-MAP.LISP - Finite Map implemented with BST template macro
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
(when (not (find-package 'bst)) (load "bst"))
(when (not (find-package 'deftest)) (load "deftest"))
(defpackage :finite-map
  (:nicknames :fm)
  (:use :common-lisp
	:deftest)
  (:export :make-finite-map
	   :bst
	   :finite-map-bind
	   :finite-map-unbind
	   :finite-map-lookup
	   :finite-map-key-not-found))
(in-package :finite-map)

;;; MAKE-FINITE-MAP template macro:
;; Create a typed finite map using a custom-typed BST implementation.
;;
;;  Keyword parameters:
;;    KEY-ELEMENT-TYPE: Unquoted type specifier applied to keys
;;    VALUE-ELEMENT-TYPE: Unquoted type specifier applied to values
;;    TEST: Function by which to compare record keys
;;
;;  Returns:
;;    New instance of a finite map built on a BST, created from templates.
;;
;;  Example:
;;   > (progn
;;       (defparameter *fm* (make-finite-map
;;                            :key-element-type symbol
;;                            :value-element-type integer
;;                            :test #'(lambda (a b)
;;                                      (string< (symbol-name a)
;;                                               (symbol-name b)))))
;;       *fm*)
;;   #S(FINITE-MAP-986 :BST #S(BST-1000 :LEFT NIL :VALUE NIL :RIGHT NIL))

(defgeneric finite-map-bind (k v fm &key overwrite))
;; Nondestructive insert record to the finite map FM binding key K to value V.
;;
;;  Parameters:
;;    K: Key (of type KEY-ELEMENT-TYPE) under which to index the new record.
;;    V: Value (of type VALUE-ELEMENT-TYPE) to be held in the new record.
;;    FM: A finite map to which the desired new record will be added.
;;
;;  Keyword parameters:
;;    OVERWRITE: If true, binding an existing key updates its value
;;
;;  Returns:
;;    New instance of a finite map containing the desired new record.
;;
;;  Example:
;;    > (setf *fm* (finite-map-bind 'a 1 *fm*))
;;    #S(FINITE-MAP-986
;;       :BST #S(BST-1000
;;               :LEFT NIL
;;               :VALUE #S(FINITE-MAP-RECORD-986 :KEY A :VALUE 1)
;;               :RIGHT NIL))
;;    > (setf *fm* (finite-map-bind 'b 3 *fm*))
;;    #S(FINITE-MAP-986
;;       :BST #S(BST-1000
;;               :LEFT NIL
;;               :VALUE #S(FINITE-MAP-RECORD-986 :KEY A :VALUE 1)
;;               :RIGHT #S(BST-1000
;;                         :LEFT NIL
;;                         :VALUE #S(FINITE-MAP-RECORD-986 :KEY B :VALUE 3)
;;                         :RIGHT NIL)))

(defgeneric finite-map-unbind (k fm))
;; Return finite map copy w/its BST stripped of records w/key K.
;;
;;  Parameters:
;;    K: Key of record to be matched and unbound
;;    FM: A finite map from which the record of key K is to be removed.
;;
;;  Returns:
;;    A new finite map built from FM except for records matching key K.
;;
;;  Example:
;;    > (finite-map-unbind 'a *fm*)
;;    #S(FINITE-MAP-986
;;       :BST #S(BST-1000
;;               :LEFT NIL
;;               :VALUE #S(FINITE-MAP-RECORD-986 :KEY B :VALUE 3)
;;               :RIGHT NIL))

(defgeneric finite-map-lookup (k fm))
;; If finite map FM contains a record with key K, retrieve its associated value.
;;
;;  Parameters:
;;    K: Key for which to search records
;;    FM: Finite map in which the desired record may be found.
;;
;;  Returns:
;;    If a matching-key record is found, return its stored value.
;;
;;  Example:
;;   > (finite-map-lookup 'c *fm*)
;;   ; Evaluation aborted on #<FINITE-MAP-KEY-NOT-FOUND {100A321503}>.
;;   > (finite-map-lookup 'b *fm*)
;;   3

(define-condition finite-map-key-not-found (error) (e))

(defvar *next-fm-id* 0)
(defvar *cached-fm-ids* (make-hash-table :test #'equal))

(defun finite-map-id (key-element-type value-element-type test)
  (let* ((args (list key-element-type value-element-type test))
	 (cached-id (gethash args *cached-fm-ids*)))
    (if (null cached-id)
	(values (let ((id (format nil "~A" *next-fm-id*)))
		  (setf (gethash args *cached-fm-ids*) id)
		  (setf *next-fm-id* (+ *next-fm-id* 1))
		  id)
		nil)
	(values cached-id t))))

(defmacro make-finite-map (&key
 			     (key-element-type 'string)
 			     (value-element-type 't)
 			     (test #'string<))
  "Create a typed finite map using a custom-typed BST implementation"
  (multiple-value-bind (id cached)
      (finite-map-id key-element-type value-element-type test)
    (let ((constructor (intern (concatenate 'string "MAKE-FINITE-MAP-" id)))
	  (record-struct
	   (intern (concatenate 'string "FINITE-MAP-RECORD-" id))))
      (if cached
	  `(,constructor
	    :bst (bst:make-bst :element-type ,record-struct
			       :test #'(lambda (a b) (funcall ,test
							 (slot-value a 'key)
							 (slot-value b 'key)))))
	  `(make-finite-map! :key-element-type ,key-element-type
			     :value-element-type ,value-element-type
			     :test ,test
			     :id ,id
			     :constructor ,constructor
			     :record-struct ,record-struct)))))

(defmacro make-finite-map! (&key
			      key-element-type
			      value-element-type
			      test
			      id
			      constructor
			      record-struct)
  (let* ((struct
	  (intern (concatenate 'string "FINITE-MAP-" id)))
 	 (key-type
	  (intern (concatenate 'string "FINITE-MAP-KEY-" id)))
 	 (value-type
	  (intern (concatenate 'string "FINITE-MAP-VALUE-" id)))
	 (record-constructor
	  (intern (concatenate 'string "MAKE-FINITE-MAP-RECORD-" id))))
    `(progn
       (defstruct ,struct bst)
       (deftype ,key-type () '(or null ,key-element-type))
       (deftype ,value-type () '(or null ,value-element-type))
       (defstruct ,record-struct
 	 (key nil :type ,key-type)
 	 (value nil :type ,value-type))
       
       (defmethod finite-map-bind ((k ,key-element-type)
				   (v ,value-element-type)
				   (fm ,struct)
				   &key (overwrite t))
	 "Nondestructive insert of a record to finite map FM, binding K->V."
	 (,constructor
	  :bst (bst:bst-insert (,record-constructor :key k :value v)
			       (slot-value fm 'bst)
			       :unique-only t
			       :overwrite overwrite)))

       (defmethod finite-map-unbind ((k ,key-element-type)
				     (fm ,struct))
	 "Return finite map copy w/its BST stripped of records w/key K"
	 (,constructor
	  :bst (bst:bst-remove (,record-constructor :key k)
			       (slot-value fm 'bst))))
       
       (defmethod finite-map-lookup ((k ,key-element-type)
				     (fm ,struct))
	 "If finite map FM contains record w/key K, retrieve associated value."
	 (let ((found (bst:bst-member (,record-constructor :key k)
				      (slot-value fm 'bst))))
	   (if found
	       (slot-value (slot-value found 'bst::value) 'value)
	       (error 'finite-map-key-not-found k))))
       
       (,constructor
	:bst (bst:make-bst :element-type ,record-struct
			   :test #'(lambda (a b) (funcall ,test
						     (slot-value a 'key)
						     (slot-value b 'key))))))))

(setf (macro-function 'define-fm-prototype) (macro-function 'make-finite-map))

(defmacro make-tests ()
  `(progn
     (define-fm-prototype)
     (deftests
       (test-inst make-finite-map ()
		  (fm)
	  (assert (not (null fm)))
	  (let ((bst (slot-value fm 'bst)))
	    (bst::assert-null-bst bst)))

       (test-pre-method finite-map-bind ("hello" "world")
			(make-finite-map)
			(orig) (result)
		(assert (not (null orig)))
		(assert (not (null result)))
		(let* ((bst (slot-value result 'bst))
		       (record (slot-value bst 'bst::value)))
		  (with-slots ((k key) (v value)) record
		    (assert (string= k "hello"))
		    (assert (string= v "world")))))
       
       (test-pre-method finite-map-unbind ("hello")
			(let ((fm (make-finite-map)))
			  (finite-map-bind "hello" "world" fm))
			(orig) (result)
		(assert (not (null orig)))
		(let ((bst (slot-value result 'bst)))
		  (bst::assert-null-bst bst))))))
