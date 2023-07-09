(in-package :glib-test)

(def-suite glib-misc :in glib-suite)
(in-suite glib-misc)

;;;   GStrv

(test g-strv-t.1
  (is (eq :pointer (cffi::canonicalize-foreign-type 'g:strv-t)))
  (let ((ptr (cffi:convert-to-foreign (list "Hello" "World") 'g:strv-t)))
    (is-true (cffi:pointerp ptr))
    (is (equal '("Hello" "World") (cffi:convert-from-foreign ptr 'g:strv-t)))))

(test g-strv-t.2
  (let ((ptr (cffi:convert-to-foreign (list "a" "b" "c" "d" "" "e") 'g:strv-t)))
    (is (equal '("a" "b" "c" "d" "" "e") 
               (cffi:convert-from-foreign ptr 'g:strv-t)))))

;;;   GList

;; a list with pointers to objects
(test g-list-t.1
  (let ((ptr (cffi:convert-to-foreign
                 (list (g:object-pointer (make-instance 'g:simple-action))
                       (g:object-pointer (make-instance 'g:menu-item)))
                 '(g:list-t :pointer))))
    (is (every #'g:is-object
               (cffi:convert-from-foreign ptr '(g:list-t g:object))))))

;; a list with strings
(test g-list-t.2
  (let ((ptr (cffi:convert-to-foreign (list "a" "b" "c") '(g:list-t :string))))
    (is (equal '("a" "b" "c")
               (cffi:convert-from-foreign ptr '(g:list-t :string))))))

;; a list with objects
(test g-list-t.3
  (let ((ptr (cffi:convert-to-foreign (list (make-instance 'g:simple-action)
                                            (make-instance 'g:menu-item))
                                      '(g:list-t g:object))))
    (is (every #'g:is-object
               (cffi:convert-from-foreign ptr '(g:list-t g:object))))))

;;;   GSList

;; a list with pointers to objects
(test g-slist-t.1
  (let ((ptr (cffi:convert-to-foreign
                 (list (g:object-pointer (make-instance 'g:simple-action))
                       (g:object-pointer (make-instance 'g:menu-item)))
                 '(g:slist-t :pointer))))
    (is (every #'g:is-object
               (cffi:convert-from-foreign ptr '(g:slist-t g:object))))))

;; a list with strings
(test g-slist-t.2
  (let ((ptr (cffi:convert-to-foreign (list "a" "b" "c") '(g:slist-t :string))))
    (is (equal '("a" "b" "c") 
               (cffi:convert-from-foreign ptr '(g:slist-t :string))))))

;; a list with objects
(test g-slist-t.3
  (let ((ptr (cffi:convert-to-foreign (list (make-instance 'g:simple-action)
                                            (make-instance 'g:menu-item))
                                      '(g:slist-t g:object))))
    (is (every #'g:is-object
               (cffi:convert-from-foreign ptr '(g:slist-t g:object))))))

;;;     GQuark

(test g-quark
  (is (= 9 (cffi:convert-to-foreign "gboolean" 'g:quark-as-string)))
  (is (string= "gboolean" (cffi:convert-from-foreign 9 'g:quark-as-string)))
  (is (= 0 (cffi:convert-to-foreign nil 'g:quark-as-string)))
  (is-false (cffi:convert-from-foreign 0 'g:quark-as-string)))

;;;     GDateTime

(test g-date-time
  (let ((utime (get-universal-time))
        (ptr nil))
    (is (cffi:pointerp (setf ptr
                             (cffi:convert-to-foreign utime 'g:date-time))))
    (is (= utime
           (cffi:convert-from-foreign ptr 'g:date-time)))))

;;;     gunichar

(test g-unichar.1
  (is (= 97 (cffi:convert-to-foreign #\a 'g:unichar))))

(test g-unichar.2
  (is (eql #\a (cffi:convert-from-foreign 97 'g:unichar))))

;;;   g_malloc
;;;   g_free
  
(test g-malloc/free
  (let ((mem nil))
    (is-true (cffi:pointerp (setq mem (g:malloc 10))))
    (is-false (g:free mem))
    (is-true (cffi:null-pointer-p (g:malloc 0)))))

;;;     g_application_name

(defvar *first-run-application* t)

#+nil ; Do not set the application name in the testsuite
(test g-application-name
  (when *first-run-application*
    #+(and sbcl (not windows))
    (is (string= "sbcl" (g:application-name)))
    #+(and sbcl windows)
    (is (string= "sbcl" (g:application-name)))
    #+(and ccl (not windows))
    (is (string= "Program" (g:application-name)))
    (is (string= "Application" (setf (g:application-name) "Application"))))
  (is (string= "Application" (g:application-name)))
  (setf *first-run-application* nil))

;;;     g_prgname

;; The PRGNAME is set in the rtest-glib.lisp file.

(test g-prgname
  (is (string= "glib-test" (g:prgname))))

;;; --- 2023-6-22 --------------------------------------------------------------
