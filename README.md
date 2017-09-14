# cl-bst

CL-BST is a generic typed binary-search-tree implementation and
associated finite-map implementation made using template macros.

## MAKE-BST template macro:
Create a typed binary search tree using a custom template tree type.

#### Keyword parameters:
   ELEMENT-TYPE: Unquoted type specifier (as used in method parameter types)  
   TEST: Function by which to compare values left-to-right when ordering  
   UNIQUE-ONLY: If true, disallow multiple nodes with the same value  
   OVERWRITES: If true and UNIQUE-ONLY, new insertions overwrite matches  

#### Returns:
   New instance of a custom BST type created from the template.

#### Example:
```
  > (progn 
      (defparameter *t* (make-bst :element-type string :test #'string<)) 
      *t*)
  #S(BST-1173 :LEFT NIL :VALUE NIL :RIGHT NIL)
```

### bst-min (tr)
Find the minimum (leftmost branch) value in the given bst TR.

#### Parameters:
   TR: The binary search tree from which to produce the minimum element
   
#### Returns:
   Minimum value contained in the binary search tree TR
   
#### Example:
```
  > (bst-min *bst-containing-values-1-through-10*)
  1
```

### bst-max (tr)
Find the maximum (rightmost branch) value in the given bst TR.

#### Parameters:
   TR: The binary search tree from which to produce the maximum element

#### Returns:
   Maximum value contained in the binary search tree TR.
   
#### Example:
```
  > (bst-max *bst-containing-values-1-through-10*)
  10
```

### bst-insert (x tr)
Nondestructive insert of value X into binary search tree TR.

#### Parameters:
   X: The element (of type ELEMENT-TYPE) to be inserted to the BST  
   TR: The binary search tree into which X will be inserted  

#### Returns:
   A BST created from TR with a [possibly additional] node representing X.

#### Example:
```
  > (setf *t* (bst-insert "world" *t*))
  #S(BST-1173 :LEFT NIL :VALUE "world" :RIGHT NIL)
  > (setf *t* (bst-insert "hello" *t*))
  #S(BST-1173
     :LEFT #S(BST-1173 :LEFT NIL :VALUE "hello" :RIGHT NIL)
     :VALUE "world"
     :RIGHT NIL)
```

### bst-remove (x tr)
Return a copy of the bst TR sans elements matching X.

#### Parameters:
   X: Element against which to match candidates for deletion
   TR: Binary search tree from which to remove elements matching X

#### Returns:
   A bst created from nodes of TR with matches of X removed.

#### Example:
```
  > (bst-remove "world" *t*)
  #S(BST-1173
     :LEFT NIL
     :VALUE "hello"
     :RIGHT NIL)
```

### bst-member (x tr)
If found, retrieve the subtree of binary search tree TR containing element X.

#### Parameters:
   X: The element (of type ELEMENT-TYPE) for which to search  
   TR: The binary search tree in which X is to be sought  

#### Returns:
   BST with the root node containing a match of X, or NIL if not found.

#### Example:
```
  > (bst-member "foo" *t*)
  NIL
  > (bst-member "hello" *t*)
  #S(BST-1173 :LEFT NIL :VALUE "hello" :RIGHT NIL)
  > (bst-member "world" *t*)
  #S(BST-1173
     :LEFT #S(BST-1173 :LEFT NIL :VALUE "hello" :RIGHT NIL)
     :VALUE "world"
     :RIGHT NIL)
```

### bst-empty (tr)
Return whether the given binary search tree TR is empty.

#### Parameters:
   TR: The binary search tree to be checked for contents.

#### Returns:
   T if TR is empty [any/all nodes contain NIL], otherwise NIL.

#### Example:
```
  > (bst-empty *t*)
  NIL
  > (bst-empty (make-bst))
  T
```

### bst-to-list (tr)
Convert the binary search tree TR into an ordered list.

#### Parameters:
   TR: Binary search tree to be converted into a list

#### Returns:
   A list composed of the elements of TR ordered from left-to-right.

#### Example:
```
  > (bst-to-list *t*)
  ("hello" "world")
```

## MAKE-FINITE-MAP template macro:
Create a typed finite map using a custom-typed BST implementation.

#### Keyword parameters:
   KEY-ELEMENT-TYPE: Unquoted type specifier applied to keys  
   VALUE-ELEMENT-TYPE: Unquoted type specifier applied to values  
   TEST: Function by which to compare record keys  
   OVERWRITES: If true, binding an existing key updates its value  

#### Returns:
   New instance of a finite map built on a BST, created from templates.

#### Example:
```
  > (progn
      (defparameter *fm* (make-finite-map
                           :key-element-type symbol
                           :value-element-type integer
                           :test #'(lambda (a b)
                                     (string< (symbol-name a)
                                              (symbol-name b)))))
      *fm*)
  #S(FINITE-MAP-986 :BST #S(BST-1000 :LEFT NIL :VALUE NIL :RIGHT NIL))
```

### finite-map-bind (k v fm)
Nondestructive insert record to the finite map FM binding key K to value V.

#### Parameters:
   K: Key (of type KEY-ELEMENT-TYPE) under which to index the new record.  
   V: Value (of type VALUE-ELEMENT-TYPE) to be held in the new record.  
   FM: A finite map to which the desired new record will be added.  

#### Returns:
   New instance of a finite map containing the desired new record.

#### Example:
```
   > (setf *fm* (finite-map-bind 'a 1 *fm*))
   #S(FINITE-MAP-986
      :BST #S(BST-1000
              :LEFT NIL
              :VALUE #S(FINITE-MAP-RECORD-986 :KEY A :VALUE 1)
              :RIGHT NIL))
   > (setf *fm* (finite-map-bind 'b 3 *fm*))
   #S(FINITE-MAP-986
      :BST #S(BST-1000
              :LEFT NIL
              :VALUE #S(FINITE-MAP-RECORD-986 :KEY A :VALUE 1)
              :RIGHT #S(BST-1000
                        :LEFT NIL
                        :VALUE #S(FINITE-MAP-RECORD-986 :KEY B :VALUE 3)
                        :RIGHT NIL)))
```

### finite-map-unbind (k fm)
Return finite map copy w/its BST stripped of records w/key K.

#### Parameters:
   K: Key of record to be matched and unbound  
   FM: A finite map from which the record of key K is to be removed.  

#### Returns:
   A new finite map built from FM except for records matching key K.

#### Example:
```
   > (finite-map-unbind 'a *fm*)
   #S(FINITE-MAP-986
      :BST #S(BST-1000
              :LEFT NIL
              :VALUE #S(FINITE-MAP-RECORD-986 :KEY B :VALUE 3)
              :RIGHT NIL))
```

### finite-map-lookup (k fm)
If finite map FM contains a record with key K, retrieve its associated value.

#### Parameters:
   K: Key for which to search records  
   FM: Finite map in which the desired record may be found.  

#### Returns:
   If a matching-key record is found, return its stored value.

#### Example:
```
  > (finite-map-lookup 'c *fm*)
  ; Evaluation aborted on #<FINITE-MAP-KEY-NOT-FOUND {100A321503}>.
  > (finite-map-lookup 'b *fm*)
  3
```
