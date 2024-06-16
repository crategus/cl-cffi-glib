(in-package :glib-test)

(def-suite glib-boxed-type :in glib-suite)
(in-suite glib-boxed-type)

(defparameter glib-boxed-type
              '(glib::type-initializer-call
                glib::boxed-copy
                glib::boxed-free
                glib::activate-gboxed-gc-hooks
                glib::register-gboxed-for-gc

                glib::get-boxed-info
                glib::make-boxed-type
                glib::cleanup-translated-object-for-callback
                glib::has-callback-cleanup
                glib::boxed-copy-fn
                glib::boxed-free-fn
                glib::pointer

                glib::make-boxed-free-finalizer
               ))

(export 'glib-boxed-type)

;;; 2024-6-15
