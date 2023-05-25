(in-package :glib-test)

(def-suite glib-misc :in glib-suite)
(in-suite glib-misc)

;;;   g_malloc
;;;   g_free
  
(test malloc
  (let ((mem nil))
    (is-true (cffi:pointerp (setq mem (g:malloc 10))))
    (g:free mem)
    (is-true (cffi:null-pointer-p (g:malloc 0)))))

;;;   GStrv

(test strv-t.1
  (is (eq :pointer (cffi::canonicalize-foreign-type 'g:strv-t)))
  (let ((ptr (cffi:convert-to-foreign (list "Hello" "World") 'g:strv-t)))
    (is-true (cffi:pointerp ptr))
    (is (equal '("Hello" "World") (cffi:convert-from-foreign ptr 'g:strv-t)))))

(test strv-t.2
  (let ((ptr (cffi:convert-to-foreign (list "a" "b" "c" "d" "" "e") 'g:strv-t)))
    (is (equal '("a" "b" "c" "d" "" "e") 
               (cffi:convert-from-foreign ptr 'g:strv-t)))))

;;;   GList

;; a list with pointers to objects
(test list-t.1
  (let ((ptr (cffi:convert-to-foreign
                 (list (g:object-pointer (make-instance 'g:simple-action))
                       (g:object-pointer (make-instance 'g:menu-item)))
                 '(g:list-t :pointer))))
    (is (every #'g:is-object
               (cffi:convert-from-foreign ptr '(g:list-t g:object))))))

;; a list with strings
(test list-t.2
  (let ((ptr (cffi:convert-to-foreign (list "a" "b" "c") '(g:list-t :string))))
    (is (equal '("a" "b" "c")
               (cffi:convert-from-foreign ptr '(g:list-t :string))))))

;; a list with objects
(test list-t.3
  (let ((ptr (cffi:convert-to-foreign (list (make-instance 'g:simple-action)
                                            (make-instance 'g:menu-item))
                                      '(g:list-t g:object))))
    (is (every #'g:is-object
               (cffi:convert-from-foreign ptr '(g:list-t g:object))))))

;;;   GSList

;; a list with pointers to objects
(test slist-t.1
  (let ((ptr (cffi:convert-to-foreign
                 (list (g:object-pointer (make-instance 'g:simple-action))
                       (g:object-pointer (make-instance 'g:menu-item)))
                 '(g:slist-t :pointer))))
    (is (every #'g:is-object
               (cffi:convert-from-foreign ptr '(g:slist-t g:object))))))

;; a list with strings
(test slist-t.2
  (let ((ptr (cffi:convert-to-foreign (list "a" "b" "c") '(g:slist-t :string))))
    (is (equal '("a" "b" "c") 
               (cffi:convert-from-foreign ptr '(g:slist-t :string))))))

;; a list with objects
(test slist-t.3
  (let ((ptr (cffi:convert-to-foreign (list (make-instance 'g:simple-action)
                                            (make-instance 'g:menu-item))
                                      '(g:slist-t g:object))))
    (is (every #'g:is-object
               (cffi:convert-from-foreign ptr '(g:slist-t g:object))))))

;;;     gunichar

(test unichar.1
  (is (= 97 (cffi:convert-to-foreign #\a 'g:unichar))))

(test unichar.2
  (is (eql #\a (cffi:convert-from-foreign 97 'g:unichar))))

;;; --- 2023-5-25 --------------------------------------------------------------
