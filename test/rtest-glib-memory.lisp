(in-package :glib-test)

(def-suite glib-memory :in glib-suite)
(in-suite glib-memory)

#+nil
(test gboxed-gc-hooks
  (is-false glib::*gboxed-gc-hooks*)
  (is (= 0 glib::*gboxed-gc-hooks-counter*))

  (let ((variant (g:variant-type-new "b")))

    (is (typep variant 'glib:variant-type))

  )
  
;  (tg:gc :full t :verbose t)
)

(test stable-pointers
  (when (> 1 (glib::get-stable-pointers-length))
    (is-false (glib::get-stable-pointers-length))
    (is-false (glib::get-stable-pointers-counter))
    (is-false (glib::get-stable-pointers))
))

;;; --- 2023-5-28 --------------------------------------------------------------
