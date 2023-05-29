(in-package :glib-test)

(def-suite glib-gtype :in glib-suite)
(in-suite glib-gtype)

(test list-name-to-gtypes
  (is (every #'stringp
             (iter (for (name gtype) in-hashtable glib::*name-to-gtype*)
                   (collect name)))))

(test list-id-to-gtypes
  (is (every #'integerp
             (iter (for (id gtype) in-hashtable glib::*id-to-gtype*)
                   (collect id)))))

(test gtype-from-name.1
  (let ((gtype (glib::gtype-from-name "void")))
    (is (= 4 (glib::gtype-id gtype)))
    (is (string= "void" (glib::gtype-name gtype)))))

(test gtype-from-name.2
  (is-false (glib::gtype-from-name "unknown")))

(test gtype-from-name.3
  (let ((gtypes (iter (for (name gtype) in-hashtable glib::*name-to-gtype*)
                      (collect name))))
    (is (every #'stringp gtypes))
    (is (every (lambda (x) (typep x 'glib:gtype))
               (mapcar #'glib::gtype-from-name gtypes)))))

(test gtype-from-id.1
  (let ((gtype (glib::gtype-from-id 4)))
    (is (= 4 (glib::gtype-id gtype)))
    (is (string= "void" (glib::gtype-name gtype)))))

(test gtype-from-id.2
  (let ((gtype (glib::gtype-from-id 0)))
    (is (= 0 (glib::gtype-id gtype)))
    (is-false gtype)))

(test gtype-from-id.3
  (let ((gtypes (iter (for (id gtype) in-hashtable glib::*id-to-gtype*)
                      (collect id))))
    (is (every #'integerp gtypes))
    (is (every (lambda (x) (typep x 'glib:gtype))
               (mapcar #'glib::gtype-from-id gtypes)))))

(test symbol-for-gtype
  (is (eq 'glib:error (glib:symbol-for-gtype "GError")))
  (is (eq 'glib:bytes (glib:symbol-for-gtype "GBytes")))
  (is-false (glib:symbol-for-gtype "unknown")))

(test list-symbol-for-gtypes
  (let ((names (iter (for (name sym) in-hashtable glib::*symbol-for-gtype*)
                     (collect name))))
    (is (every #'symbolp
               (mapcar #'glib:symbol-for-gtype names)))))

;;; --- 2023-5-29 --------------------------------------------------------------
