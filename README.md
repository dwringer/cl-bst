# cl-bst

CL-BST is a generic typed binary-search-tree implementation and
associated finite-map implementation made using template macros.  To
use, just load "bst.lisp" or "finite-map.lisp" (the latter will
automatically load the former) and use the :bst or :fm/:finite-map
package(s).

NOTE: Because of the way these data structures use
custom-instanced types and structs, the compiler may generate
style-warnings if MAKE-BST or MAKE-FINITE-MAP are called for the first
time (for a given set of params) inside some lexical scope.  Creating
an empty prototype instance of the desired type at the toplevel will
allow subsequent such instances to be used without any problems.  The
aliases DEFINE-BST-PROTOTYPE and DEFINE-FM-PROTOTYPE can be used for
clarity in this purpose.

## MAKE-BST template macro:
Create a typed binary search tree using a custom template tree type.

#### Keyword parameters:
   ELEMENT-TYPE: Unquoted type specifier (as used in method parameter types)  
   TEST: Function by which to compare values left-to-right when ordering  

#### Returns:
   New instance of a custom BST type created from the template.

#### Example:
```
  > (progn 
      (defparameter *t* (make-bst :element-type string :test #'string<)) 
      *t*)
  #S(BST-1173 :LEFT NIL :VALUE NIL :RIGHT NIL)
```

### bst-insert (x tr &key unique-only overwrite)
Nondestructive insert of value X into binary search tree TR.

#### Parameters:
   X: The element (of type ELEMENT-TYPE) to be inserted to the BST  
   TR: The binary search tree into which X will be inserted  

#### Keyword parameters:
   UNIQUE-ONLY: If true, disallow multiple nodes with the same value  
   OVERWRITE: If true and UNIQUE-ONLY, new insertions overwrite matches  

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

### bst-min (tr)
Find the minimum (leftmost branch) value in the given bst TR.

#### Parameters:
   TR: The binary search tree from which to produce the minimum element
   
#### Returns:
   Minimum value contained in the binary search tree TR
   
#### Example:
```
  > (bst-min *t*)
  "hello"
```

### bst-max (tr)
Find the maximum (rightmost branch) value in the given bst TR.

#### Parameters:
   TR: The binary search tree from which to produce the maximum element

#### Returns:
   Maximum value contained in the binary search tree TR.
   
#### Example:
```
  > (bst-max *t*)
  "hello"
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
  #S(BST-1173 :LEFT NIL :VALUE "hello" :RIGHT NIL)
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

### finite-map-bind (k v fm &key overwrite)
Nondestructive insert record to the finite map FM binding key K to value V.

#### Parameters:
   K: Key (of type KEY-ELEMENT-TYPE) under which to index the new record.  
   V: Value (of type VALUE-ELEMENT-TYPE) to be held in the new record.  
   FM: A finite map to which the desired new record will be added.  

#### Keyword parameters:
   OVERWRITE: If true, binding an existing key updates its value  

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

## LICENSE:
> CL-BST
> Copyright 2017 Darren W. Ringer <dwringer@gmail.com>

> Permission is hereby granted, free of charge, to any person
> obtaining a copy of this software and associated documentation
> files (the "Software"), to deal in the Software without
> restriction, including without limitation the rights to use,
> copy, modify, merge, publish, distribute, sublicense, and/or sell
> copies of the Software, and to permit persons to whom the
> Software is furnished to do so, subject to the following
> conditions:

> The above copyright notice and this permission notice shall be
> included in all copies or substantial portions of the Software.

> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
> EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
> OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
> NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
> HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
> WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
> FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
> OTHER DEALINGS IN THE SOFTWARE.
