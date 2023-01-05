(in-package :glib-test)

(def-suite gobject-boxed-lisp :in gobject-suite)
(in-suite gobject-boxed-lisp)

(test registered-gtype.1
  (is (eq 'g:bytes
          (setf (gobject:symbol-for-gtype "GBytes") 'g:bytes)))
  (is (eq 'g:bytes (gobject:symbol-for-gtype "GBytes"))))

(test registered-gtype.2
  (is (eq 'g:resource
          (setf (gobject:symbol-for-gtype "GResource") 'g:resource)))
  (is (eq 'g:resource (gobject:symbol-for-gtype "GResource"))))

(test registered-gtype.3
  (is (eq 'g:variant-type
          (setf (gobject:symbol-for-gtype (g:gtype "GVariantType"))
                'g:variant-type)))
  (is (eq 'g:variant-type (gobject:symbol-for-gtype (g:gtype "GVariantType")))))


(test get-boxed-info.1
  (let ((info (gobject::get-boxed-info "GBytes")))
    (is (typep (setf (gobject::get-boxed-info "GBytes") info)
               'gobject::boxed-opaque-info))
    (is (typep (gobject::get-boxed-info "GBytes")
               'gobject::boxed-opaque-info))
    (is (typep (gobject::get-boxed-info 'glib:bytes)
               'gobject::boxed-opaque-info))
    (is (typep (gobject::get-boxed-info (g:gtype "GBytes"))
               'gobject::boxed-opaque-info))))

(test get-boxed-info.2
  (let ((info (gobject::get-boxed-info "GResource")))
    (is (typep (setf (gobject::get-boxed-info 'g:resource) info)
               'gobject::boxed-opaque-info))
    (is (typep (gobject::get-boxed-info 'g:resource)
               'gobject::boxed-opaque-info))
    (is (typep (gobject::get-boxed-info "GResource")
               'gobject::boxed-opaque-info))
    (is (typep (gobject::get-boxed-info (g:gtype "GResource"))
               'gobject::boxed-opaque-info))))

(test get-boxed-info.3
  (let ((info (gobject::get-boxed-info "GVariantType")))
    (is (typep (setf (gobject::get-boxed-info (g:gtype "GVariantType")) info)
               'gobject::boxed-opaque-info))
    (is (typep (gobject::get-boxed-info (g:gtype "GVariantType"))
               'gobject::boxed-opaque-info))
    (is (typep (gobject::get-boxed-info "GVariantType")
               'gobject::boxed-opaque-info))
    (is (typep (gobject::get-boxed-info 'g:variant-type)
               'gobject::boxed-opaque-info))))

;;; --- 2023-1-2 ---------------------------------------------------------------
