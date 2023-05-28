(in-package :glib-test)

(def-suite gobject-boxed :in gobject-suite)
(in-suite gobject-boxed)

(defvar *verbose-gobject-boxed* nil)

(test registered-gtype.1
  (is (eq 'g:bytes
          (setf (glib:symbol-for-gtype "GBytes") 'g:bytes)))
  (is (eq 'g:bytes (glib:symbol-for-gtype "GBytes"))))

(test registered-gtype.2
  (is (eq 'g:resource
          (setf (glib:symbol-for-gtype "GResource") 'g:resource)))
  (is (eq 'g:resource (glib:symbol-for-gtype "GResource"))))

(test registered-gtype.3
  (is (eq 'g:variant-type
          (setf (glib:symbol-for-gtype (g:gtype "GVariantType"))
                'g:variant-type)))
  (is (eq 'g:variant-type (glib:symbol-for-gtype (g:gtype "GVariantType")))))

(test get-boxed-info.1
  (let ((info (glib::get-boxed-info "GBytes")))
    (is (typep (setf (glib::get-boxed-info "GBytes") info)
               'glib::boxed-opaque-info))
    (is (typep (glib::get-boxed-info "GBytes")
               'glib::boxed-opaque-info))
    (is (typep (glib::get-boxed-info 'glib:bytes)
               'glib::boxed-opaque-info))
    (is (typep (glib::get-boxed-info (g:gtype "GBytes"))
               'glib::boxed-opaque-info))))

(test get-boxed-info.2
  (let ((info (glib::get-boxed-info "GResource")))
    (is (typep (setf (glib::get-boxed-info 'g:resource) info)
               'glib::boxed-opaque-info))
    (is (typep (glib::get-boxed-info 'g:resource)
               'glib::boxed-opaque-info))
    (is (typep (glib::get-boxed-info "GResource")
               'glib::boxed-opaque-info))
    (is (typep (glib::get-boxed-info (g:gtype "GResource"))
               'glib::boxed-opaque-info))))

(test get-boxed-info.3
  (let ((info (glib::get-boxed-info "GVariantType")))
    (is (typep (setf (glib::get-boxed-info (g:gtype "GVariantType")) info)
               'glib::boxed-opaque-info))
    (is (typep (glib::get-boxed-info (g:gtype "GVariantType"))
               'glib::boxed-opaque-info))
    (is (typep (glib::get-boxed-info "GVariantType")
               'glib::boxed-opaque-info))
    (is (typep (glib::get-boxed-info 'g:variant-type)
               'glib::boxed-opaque-info))))

(test make-bytes.1
  (when *verbose-gobject-boxed*
    (trace tg:finalize)
    (trace glib::register-gboxed-for-gc)
    (trace cffi:translate-from-foreign)
    (trace cffi:translate-to-foreign)
    (trace g:bytes-data)
    (trace g:bytes-size))

  (let ((bytes (make-instance 'glib:bytes)))
    (is (typep bytes 'g:bytes))
    (is (cffi:pointerp (glib::boxed-opaque-pointer bytes)))
    (is (cffi:pointerp (g:bytes-data bytes)))
    (is (= 0 (g:bytes-size bytes))))

  (when *verbose-gobject-boxed*
    (untrace tg:finalize)
    (untrace glib::register-gboxed-for-gc)
    (untrace cffi:translate-from-foreign)
    (untrace cffi:translate-to-foreign)
    (untrace g:bytes-data)
    (untrace g:bytes-size)))

(test make-bytes.2
  (when *verbose-gobject-boxed*
    (trace tg:finalize)
    (trace tg:cancel-finalization)
    (trace cffi:translate-from-foreign)
    (trace cffi:translate-to-foreign)
    (trace g:bytes-new)
    (trace g:bytes-data)
    (trace g:bytes-size))

  (let ((bytes (g:bytes-new (cffi:null-pointer) 0)))
    (is (typep bytes 'g:bytes))
    (is (cffi:pointerp (glib::boxed-opaque-pointer bytes)))
    (is (cffi:pointerp (g:bytes-data bytes)))
    (is (= 0 (g:bytes-size bytes))))

  (when *verbose-gobject-boxed*
    (untrace tg:finalize)
    (untrace tg:cancel-finalization)
    (untrace cffi:translate-from-foreign)
    (untrace cffi:translate-to-foreign)
    (untrace g:bytes-new)
    (untrace g:bytes-data)
    (untrace g:bytes-size)))

(test convert-to/from-foreign-for-bytes
  (multiple-value-bind (data len)
      (cffi:foreign-string-alloc "A test string")
    (let* ((bytes (g:bytes-new data len))
           (bytes-ptr (cffi:convert-to-foreign bytes '(g:boxed g:bytes))))

      (is (typep bytes 'g:bytes))
      (is (string= "A test string"
                   (cffi:foreign-string-to-lisp (g:bytes-data bytes))))
      (is (cffi:pointerp (glib::boxed-opaque-pointer bytes)))
      (is (cffi:pointerp bytes-ptr))
      (is (eq bytes-ptr
              (glib::boxed-opaque-pointer bytes)))

      (is (cffi:pointerp (setf bytes-ptr
                               (cffi:convert-to-foreign bytes
                                                        '(g:boxed g:bytes
                                                                  :return)))))
      (is (typep bytes 'g:bytes))
      (is-false (glib::boxed-opaque-pointer bytes))
      (is (cffi:pointerp bytes-ptr))

      (is (typep (setf bytes (cffi:convert-from-foreign bytes-ptr
                                                        '(g:boxed g:bytes)))
                 'g:bytes))
      (is (string= "A test string"
                   (cffi:foreign-string-to-lisp (g:bytes-data bytes))))
      (is (cffi:pointerp (glib::boxed-opaque-pointer bytes)))
      (is (cffi:pointerp bytes-ptr))
      (is (eq bytes-ptr
              (glib::boxed-opaque-pointer bytes)))

      (is (typep (setf bytes (cffi:convert-from-foreign bytes-ptr
                                                        '(g:boxed g:bytes
                                                                  :return)))
                 'g:bytes))
      (is (string= "A test string"
                   (cffi:foreign-string-to-lisp (g:bytes-data bytes))))
      (is (cffi:pointerp (glib::boxed-opaque-pointer bytes)))
      (is (cffi:pointerp bytes-ptr))
      (is (eq bytes-ptr
              (glib::boxed-opaque-pointer bytes)))
)))

(test define-g-boxed-opaque
  (when *verbose-gobject-boxed*
    (format t "~%~%")
    (trace glib::make-boxed-free-finalizer)
    (trace glib::register-gboxed-for-gc)
    (trace glib::boxed-free-fn)
    (trace glib::boxed-copy-fn))

  (let ((boxed-info (glib::get-boxed-info "GBytes"))
        (bytes (make-instance 'glib:bytes)))

    (when *verbose-gobject-boxed*

      (format t "GBOXED-GC-HOOKS-HOOKS : ~a~%" glib::*gboxed-gc-hooks*)
      (format t "~&~&DEFINE-G-BOXED-OPAQUE~%~a~%"
                (macroexpand '(define-g-boxed-opaque bytes "GBytes"
                                :alloc (bytes-new (cffi:null-pointer) 0))))
      (format t "~%BOXED-OPAQUE-INFO~%~a~%" boxed-info)
    )

    ;; boxed-opaque-info
    (is (eq 'glib:bytes (glib::boxed-info-name boxed-info)))
    (is (string= "GBytes" (glib::boxed-info-gtype boxed-info)))
    (is-false (glib::boxed-opaque-info-alloc boxed-info))
    (is-false (glib::boxed-opaque-info-free boxed-info))

    ;; instance of class BYTES
    (is (cffi:pointerp (glib::boxed-opaque-pointer bytes)))

    (when *verbose-gobject-boxed*
      (format t "~%~%")
      (format t "GBOXED-GC-HOOKS-HOOKS : ~a~%" glib::*gboxed-gc-hooks*)

      (untrace glib::make-boxed-free-finalizer)
      (untrace glib::register-gboxed-for-gc)
      (untrace glib::boxed-free-fn)
      (untrace glib::boxed-copy-fn)
    )))

;;; --- 2023-1-27 --------------------------------------------------------------
