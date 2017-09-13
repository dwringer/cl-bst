(defpackage :bst
  (:use :common-lisp)
  (:export :make-bst
	   :bst-insert
	   :bst-member
	   :bst-empty
	   :bst-to-list
	   :make-finite-map
	   :bst
	   :finite-map-bind
	   :finite-map-lookup
	   :finite-map-key-not-found))
(in-package :bst)


(defgeneric bst-insert (x tr))
(defgeneric bst-member (x tr))
(defgeneric bst-empty (tr))
(defgeneric bst-to-list (tr))


(defmacro make-bst (&key (element-type 'real) (test #'<) unique-only)
  (let* ((id (string (gensym "")))
	 (type-name (intern (concatenate 'string "BST-NODE-" id)))
	 (elem-type-name (intern (concatenate 'string "BST-ELEM-" id)))
	 (struct-name (intern (concatenate 'string "BST-" id)))
	 (constructor-name (intern (concatenate 'string "MAKE-BST-" id))))
    `(progn
       (deftype ,type-name () '(or null ,struct-name))
       (deftype ,elem-type-name () '(or null ,element-type))
       (defstruct ,struct-name
	 (left nil :type ,type-name)
	 (value nil :type ,elem-type-name)
	 (right nil :type ,type-name))
       (defmethod bst-insert ((x ,element-type) (tr ,struct-name))
	 (with-slots ((l left) (v value) (r right)) tr
	   (cond ((null v) (,constructor-name :value x))
		 ((funcall ,test v x)
		  (,constructor-name :left l
				     :value v
				     :right (if (null r)
						(,constructor-name :value x)
						(bst-insert x r))))
		 ((or (not ,unique-only)
		      (funcall ,test x v))
		  (,constructor-name :left (if (null l)
					       (,constructor-name
						:value x)
					       (bst-insert x l))
				     :value v
				     :right r))
		 (t tr))))
       (defmethod bst-member ((x ,element-type) (tr ,struct-name))
	 (with-slots ((l left) (v value) (r right)) tr
	   (cond ((null v) nil)
		 ((funcall ,test x v) (when (not (null l)) (bst-member x l)))
		 ((funcall ,test v x) (when (not (null r)) (bst-member x r)))
		 (t tr))))
       (defmethod bst-empty ((tr ,struct-name))
	 (with-slots ((l left) (v value) (r right)) tr
	   (and (null v)
		(or (null l) (bst-empty l))
		(or (null r) (bst-empty r)))))
       (defmethod bst-to-list ((tr ,struct-name))
	 (with-slots ((l left) (v value) (r right)) tr
	   (concatenate 'list
			(when (not (null l)) (bst-to-list l))
			(list v)
			(when (not (null r)) (bst-to-list r)))))
       (values (,constructor-name) ',struct-name))))


(defgeneric finite-map-bind (k v fm))
(defgeneric finite-map-lookup (k fm))


(define-condition finite-map-key-not-found (error) (e))


(defmacro make-finite-map (&key
 			     (key-element-type 'string)
 			     (value-element-type 't)
 			     (test #'string<))
  (let* ((id (string (gensym "")))
	 (struct-name
	  (intern (concatenate 'string "FINITE-MAP-" id)))
 	 (key-type-name
	  (intern (concatenate 'string "FINITE-MAP-KEY-" id)))
 	 (value-type-name
	  (intern (concatenate 'string "FINITE-MAP-VALUE-" id)))
 	 (record-struct-name
	  (intern (concatenate 'string "FINITE-MAP-RECORD-" id)))
	 (constructor-name
	  (intern (concatenate 'string "MAKE-FINITE-MAP-" id)))
	 (record-constructor-name
	  (intern (concatenate 'string "MAKE-FINITE-MAP-RECORD-" id))))
    `(progn
       (defstruct ,struct-name bst)
       (deftype ,key-type-name () '(or null ,key-element-type))
       (deftype ,value-type-name () '(or null ,value-element-type))
       (defstruct ,record-struct-name
 	 (key nil :type ,key-type-name)
 	 (value nil :type ,value-element-type))
       (defmethod finite-map-bind ((k ,key-element-type)
				   (v ,value-element-type)
				   (fm ,struct-name))
	 (,constructor-name
	  :bst (bst-insert (,record-constructor-name :key k :value v)
			   (slot-value fm 'bst))))
       (defmethod finite-map-lookup ((k ,key-element-type)
				     (fm ,struct-name))
	 (with-slots ((tr bst)) fm
	   (with-slots ((l left) (v value) (r right)) tr
	     (if (null v)
		 (error 'finite-map-key-not-found k)
		 (let ((this-key (slot-value v 'key)))
		   (cond ((and (null this-key) (null l) (null r))
			  (error 'finite-map-key-not-found k))
			 ((funcall ,test k this-key)
			  (if (null l)
			      (error 'finite-map-key-not-found k)
			      (finite-map-lookup k (,constructor-name :bst l))))
			 ((funcall ,test this-key k)
			  (if (null r)
			      (error 'finite-map-key-not-found k)
			      (finite-map-lookup k (,constructor-name :bst r))))
			 (t (slot-value v 'value))))))))
       (,constructor-name
	:bst (make-bst :element-type ,record-struct-name
		       :test #'(lambda (a b) (funcall ,test
						 (slot-value a 'key)
						 (slot-value b 'key)))
		       :unique-only t)))))
