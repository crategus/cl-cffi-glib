(in-package :glib-test)

(def-suite glib-variant :in glib-suite)
(in-suite glib-variant)

;;;     GVariant

;;;     g:with-variant

(test g-with-variant.1
  (g:with-variant (var :boolean t)
    (is-true (g:variant-get var))))

(test g-with-variant.2
  (g:with-variant (var :int16 #x7fff)
    (is (= 32767 (g:variant-get var)))))

(test g-with-variant.3
  (g:with-variant (var :int16 #x-8000)
    (is (= -32768 (g:variant-get var)))))

(test g-with-variant.4
  (g:with-variant (var :uint16 #xffff)
    (is (= 65535 (g:variant-get var)))))

(test g-with-variant.5
  (g:with-variant (var :int32 #x7fffffff)
    (is (= 2147483647 (g:variant-get var)))))

(test g-with-variant.6
  (g:with-variant (var :int32 #x-80000000)
    (is (= -2147483648 (g:variant-get var)))))

(test g-with-variant.7
  (g:with-variant (var :uint32 #xffffffff)
    (is (= 4294967295 (g:variant-get var)))))

(test g-with-variant.5
  (g:with-variant (var :int64 #x7fffffffffffffff)
    (is (= 9223372036854775807 (g:variant-get var)))))

(test g-with-variant.6
  (g:with-variant (var :int64 #x-8000000000000000)
    (is (= -9223372036854775808 (g:variant-get var)))))

(test g-with-variant.7
  (g:with-variant (var :uint64 #xffffffffffffffff)
    (is (= 18446744073709551615 (g:variant-get var)))))

(test g-with-variant.8
  (g:with-variant (var :handle #x7fffffff)
    (is (= 2147483647 (g:variant-get var)))))

(test g-with-variant.9
  (g:with-variant (var :double 99.99d99)
    (is (= 9.999d100 (g:variant-get var)))))

(test g-with-variant.10
  (g:with-variant (var :string "This is a string.")
    (is (string= "This is a string." (g:variant-get var)))))

(test g-with-variant.11
  (let ((path "/com/crategus/glib/test"))
    (g:with-variant (var :object-path path)
      (is-true (g:variant-is-object-path path))
      (is (string= "/com/crategus/glib/test" (g:variant-get var))))))

(test g-with-variant.12
  (let ((signature "iii"))
    (g:with-variant (var :signature signature)
      (is-true (g:variant-is-signature signature))
      (is (string= "iii" (g:variant-get var))))))

(test g-with-variant.13
  (g:with-variant (var1 :int16 100)
    (g:with-variant (var2 :variant var1)
      (is (= 100 (g:variant-get var1)))
      (is (= 100 (g:variant-get (g:variant-get var2)))))))

;;;   g_variant_unref

(test g-variant-unref
  (let ((bool (g:variant-new-boolean t)))
    (is-true (g:variant-boolean bool))
    (g:variant-unref bool)))

;;;   g_variant_ref

(test g-variant-ref
  (let ((bool (g:variant-new-boolean t)))
    (is-true (g:variant-boolean bool))
    (setf bool (g:variant-ref bool))
    (is-true (g:variant-boolean bool))
    (g:variant-unref bool)
    (is-true (g:variant-boolean bool))
    (is-false (g:variant-unref bool))))

;;;   g_variant_ref_sink

(test g-variant-ref-sink
  (let ((bool (g:variant-new-boolean t)))
    (is-true (g:variant-is-floating bool))
    (setf bool (g:variant-ref-sink bool))
    (is-false (g:variant-is-floating bool))
    (is-false (g:variant-unref bool))))

;;;   g_variant_is_floating

(test g-variant-is-floating
  (let ((bool (g:variant-new-boolean t)))
    (is-true (g:variant-is-floating bool))
    (is-false (g:variant-unref bool))))

;;;   g_variant_take_ref

;;;   g_variant_type

(test g-variant-type
  (let ((bool (g:variant-new-boolean t)))
    (is (equal "b" (g:variant-type-dup-string (g:variant-type bool))))
    (is-false (g:variant-unref bool))))

;;;   g_variant_type_string

(test g-variant-type-string
  (let ((bool (g:variant-new-boolean t)))
    (is (equal "b" (g:variant-type-string bool)))
    (is-false (g:variant-unref bool))))

;;;     g_variant_is_of_type

(test g-variant-is-of-type
  (let ((bool (g:variant-new-boolean t)))
    (is-true (g:variant-is-of-type bool (g:variant-type-new "b")))
    (is-false (g:variant-unref bool))))

;;;   g_variant_is_container

(test g-variant-is-container
  (let* ((bool (g:variant-ref (g:variant-new-boolean t)))
         (container (g:variant-ref-sink (g:variant-new-variant bool))))
    (is-false (g:variant-is-container bool))
    (is-true (g:variant-is-container container))
    (is-false (g:variant-unref bool))
    (is-false (g:variant-unref container))))

;;;   g_variant_compare

(test g-variant-compare
  (let ((int1 (g:variant-new-int16 2))
        (int2 (g:variant-new-int16 4)))
    (is (=  0 (g:variant-compare int1 int1)))
    (is (= -2 (g:variant-compare int1 int2)))
    (is (=  2 (g:variant-compare int2 int1)))
    (is-false (g:variant-unref int1))
    (is-false (g:variant-unref int2))))

;;;   GVariantClass

;;;   g_variant_classify

(test g-variant-classify
  (let ((bool (g:variant-new-boolean t)))
    (is (eq :boolean (g:variant-classify bool)))
    (is-false (g:variant-unref bool))))

;;;     g_variant_check_format_string
;;;     g_variant_get
;;;     g_variant_get_va
;;;     g_variant_new
;;;     g_variant_new_va
;;;
;;;     g_variant_new_boolean
;;;     g_variant_new_byte
;;;     g_variant_new_int16
;;;     g_variant_new_uint16
;;;     g_variant_new_int32
;;;     g_variant_new_uint32
;;;     g_variant_new_int64
;;;     g_variant_new_uint64
;;;     g_variant_new_handle
;;;     g_variant_new_double
;;;     g_variant_new_string
;;;     g_variant_new_object_path
;;;     g_variant_is_object_path
;;;     g_variant_new_signature
;;;     g_variant_is_signature
;;;     g_variant_new_variant
;;;     g_variant_new_strv
;;;     g_variant_new_objv
;;;     g_variant_new_bytestring
;;;     g_variant_new_bytestring_array
;;;
;;;     g_variant_get_boolean
;;;     g_variant_get_byte
;;;     g_variant_get_int16
;;;     g_variant_get_uint16
;;;     g_variant_get_int32
;;;     g_variant_get_uint32
;;;     g_variant_get_int64
;;;     g_variant_get_uint64
;;;     g_variant_get_handle
;;;     g_variant_get_double
;;;     g_variant_get_string
;;;     g_variant_dup_string
;;;     g_variant_get_variant
;;;     g_variant_get_strv
;;;     g_variant_dup_strv
;;;     g_variant_get_objv
;;;     g_variant_dup_objv
;;;     g_variant_get_bytestring
;;;     g_variant_dup_bytestring
;;;     g_variant_get_bytestring_array
;;;     g_variant_dup_bytestring_array
;;;
;;;     g_variant_new_maybe
;;;     g_variant_new_array

;;;     g_variant_new_tuple

(test g-variant-new-tuple
  (let* ((vtype1 (g:variant-type-new "i"))
         (child1 (g:variant-new-int16 10))
         (vtype2 (g:variant-type-new "i"))
         (child2 (g:variant-new-int16 20))
         ;; Tuple type for two integer
         (tuple (g:variant-type-new-tuple vtype1 vtype2))
         value)
    ;; Tuple from g:variant-parse
    (is-true (setf value (g:variant-parse tuple "(10,20)")))
    (is (string= "(10, 20)" (g:variant-print value)))
    ;; Tuple from g:variant-new-tuple for two values
    (is-true (setf value (g:variant-new-tuple child1 child2)))
    (is (string= "(10, 20)" (g:variant-print value)))
    ;; Tuple from g:variant-new-tuple for one value
    (is-true (setf value (g:variant-new-tuple child1)))
    (is (string= "(10,)" (g:variant-print value)))
    ;; Tuple from g:variant-new-tuple for no value
    (is-true (setf value (g:variant-new-tuple)))
    (is (string= "()" (g:variant-print value)))))

;;;     g_variant_new_dict_entry
;;;     g_variant_new_fixed_array
;;;
;;;     g_variant_get_maybe
;;;     g_variant_n_children
;;;     g_variant_get_child_value
;;;     g_variant_get_child
;;;     g_variant_lookup_value
;;;     g_variant_lookup
;;;     g_variant_get_fixed_array
;;;
;;;     g_variant_get_size
;;;     g_variant_get_data
;;;     g_variant_get_data_as_bytes
;;;     g_variant_store
;;;     g_variant_new_from_data
;;;     g_variant_new_from_bytes
;;;     g_variant_byteswap
;;;     g_variant_get_normal_form
;;;     g_variant_is_normal_form
;;;
;;;     g_variant_hash
;;;     g_variant_equal

;;;     g_variant_print

(test g-variant-print
  (let (value)
    (is (string= "false"
                 (g:variant-print (setf value (g:variant-new-boolean nil)))))
    (is-false (g:variant-unref value))
    (is (string= "true"
                 (g:variant-print (setf value (g:variant-new-boolean t)))))
    (is-false (g:variant-unref value))
    (is (string= "0xff"
                 (g:variant-print (setf value (g:variant-new-byte #xff)))))
    (is-false (g:variant-unref value))
    (is (string= "10.0"
                 (g:variant-print (setf value (g:variant-new-double 10.0d0)))))
    (is-false (g:variant-unref value))
    (is (string= "16777215"
                 (g:variant-print (setf value (g:variant-new-handle #xffffff)))))
    (is-false (g:variant-unref value))
    (is (string= "4095"
                 (g:variant-print (setf value (g:variant-new-int16 #xfff)))))
    (is-false (g:variant-unref value))
    (is (string= "4095"
                 (g:variant-print (setf value (g:variant-new-uint16 #xfff)))))
    (is-false (g:variant-unref value))
    (is (string= "65535"
                 (g:variant-print (setf value (g:variant-new-int32 #xffff)))))
    (is-false (g:variant-unref value))
    (is (string= "65535"
                 (g:variant-print (setf value (g:variant-new-uint32 #xffff)))))
    (is-false (g:variant-unref value))
    (is (string= "1048575"
                 (g:variant-print (setf value (g:variant-new-int64 #xfffff)))))
    (is-false (g:variant-unref value))
    (is (string= "1048575"
                 (g:variant-print (setf value (g:variant-new-uint64 #xfffff)))))
    (is-false (g:variant-unref value))
    (is (string= "'test'"
                 (g:variant-print (setf value (g:variant-new-string "test")))))
    (is-false (g:variant-unref value))))

;;;     g_variant_print_string
;;;
;;;     GVariantIter
;;;
;;;     g_variant_iter_copy
;;;     g_variant_iter_free
;;;     g_variant_iter_init
;;;     g_variant_iter_n_children
;;;     g_variant_iter_new
;;;     g_variant_iter_next_value
;;;     g_variant_iter_next
;;;     g_variant_iter_loop
;;;
;;;     g_variant_dict_unref
;;;     g_variant_dict_ref

;;;     g_variant_dict_new

(test g-variant-dict-new
  (let* ((vtype (g:variant-type-new "a{sv}"))
         (asv "[{'first', <1>}, {'second', <2>}]")
         (variant (g:variant-parse vtype asv)))
    (is (typep (g:variant-dict-new (cffi:null-pointer)) 'g:variant-dict))
    (is (typep (g:variant-dict-new variant) 'g:variant-dict))
    (is-false (g:variant-unref variant))))

;;;     g_variant_dict_init
;;;     g_variant_dict_clear

;;;     g_variant_dict_contains

(test g-variant-dict-contains
  (let* ((vtype (g:variant-type-new "a{sv}"))
         (asv "[{'first', <1>}, {'second', <2>}]")
         (variant (g:variant-parse vtype asv))
         (dict (g:variant-dict-new variant)))
    (is (typep dict 'g:variant-dict))
    (is-true (g:variant-dict-contains dict "first"))
    (is-true (g:variant-dict-contains dict "second"))
    (is-false (g:variant-dict-contains dict "third"))
    (is-false (g:variant-unref variant))))

;;;     g_variant_dict_lookup

;;;     g_variant_dict_lookup_value

(test g-variant-dict-lookup-value
  (let* ((vtype (g:variant-type-new "a{sv}"))
         (asv "[{'first', <int32 1>}, {'second', <int32 2>}]")
         (variant (g:variant-parse vtype asv))
         (dict (g:variant-dict-new variant)))
    (is (typep dict 'g:variant-dict))
    (is-true (g:variant-dict-contains dict "first"))
    (is (string= "i"
                 (g:variant-type-string
                     (g:variant-dict-lookup-value dict "first"))))
    (is (= 1 (g:variant-int32 (g:variant-dict-lookup-value dict "first"))))
    (is (= 1 (g:variant-int32 (g:variant-dict-lookup-value dict "first" nil))))
    (is (= 1 (g:variant-int32 (g:variant-dict-lookup-value dict "first" "i"))))
    (is (= 1 (g:variant-int32
                 (g:variant-dict-lookup-value dict
                                              "first"
                                              (g:variant-type-new "i")))))
    (is-false (g:variant-unref variant))))

;;;     g_variant_dict_insert

;;;     g_variant_dict_insert_value
;;;     g_variant_dict_remove

(test g-variant-dict-insert-value/remove
  (let* ((vtype (g:variant-type-new "a{sv}"))
         (asv "[{'first', <int32 1>}, {'second', <int32 2>}]")
         (variant (g:variant-parse vtype asv))
         (dict (g:variant-dict-new variant)))
    (is (string= "{'first': <1>, 'second': <2>}" (g:variant-print variant)))
    (is (= 1 (g:variant-int32 (g:variant-dict-lookup-value dict "first"))))
    (is-false (g:variant-dict-insert-value dict
                                           "first"
                                           (g:variant-new-int32 10)))
    (is (= 10 (g:variant-int32 (g:variant-dict-lookup-value dict "first"))))
    (is-true (g:variant-dict-contains dict "second"))
    (is-true (g:variant-dict-remove dict "second"))
    (is-false (g:variant-dict-contains dict "secton"))
    (is (string= "{'first': <10>}"
                 (g:variant-print (g:variant-dict-end dict))))
    (is-false (g:variant-unref variant))))

;;;     g_variant_dict_end

(test g-variant-dict-end
  (let* ((vtype (g:variant-type-new "a{sv}"))
         (asv "[{'first', <1>}, {'second', <2>}]")
         (variant (g:variant-parse vtype asv))
         (dict (g:variant-dict-new variant)))
    (is (string= "{'first': <1>, 'second': <2>}"
                 (g:variant-print (g:variant-dict-end dict))))
    (is-false (g:variant-unref variant))))

;;;     Example from the documentation

(defun add-to-count (orig)
  (let* ((dict (g:variant-dict-new orig))
         (variant (g:variant-dict-lookup-value dict "count")))
    (when variant
      (let ((value (1+ (g:variant-int32 variant))))
        (g:variant-dict-insert-value dict "count" (g:variant-new-int32 value))
        (g:variant-dict-end dict)))))

(test g-variant-add-to-count
  (let* ((vtype (g:variant-type-new "a{sv}"))
         (asv "[{'count', <int32 1>}, {'second', <int32 2>}]")
         (variant (g:variant-parse vtype asv)))
    (is (string= "{'count': <1>, 'second': <2>}" (g:variant-print variant)))
    (is (string= "{'count': <2>, 'second': <2>}"
                 (g:variant-print (setf variant (add-to-count variant)))))
    (is (string= "{'count': <3>, 'second': <2>}"
                 (g:variant-print (add-to-count variant))))
    (is-false (g:variant-unref variant))))

;;;     g_variant_parse

(test g-variant-parse
  (let ((value nil))
    ;; Parse boolean
    (setf value (g:variant-parse nil "true"))
    (is (string= "b" (g:variant-type-string value)))
    (is (string= "true" (g:variant-print value)))
    (setf value (g:variant-parse (g:variant-type-new "b") "false"))
    (is (string= "b" (g:variant-type-string value)))
    (is (string= "false" (g:variant-print value)))
    (setf value (g:variant-parse "b" "false"))
    (is (string= "b" (g:variant-type-string value)))
    (is (string= "false" (g:variant-print value)))

    ;; Parse byte
    (setf value (g:variant-parse nil "0xff"))
    (is (string= "i" (g:variant-type-string value)))
    (is (string= "255" (g:variant-print value)))
    (setf value (g:variant-parse (g:variant-type-new "y") "0xff"))
    (is (string= "y" (g:variant-type-string value)))
    (is (string= "0xff" (g:variant-print value)))
    (setf value (g:variant-parse "y" "0xff"))
    (is (string= "y" (g:variant-type-string value)))
    (is (string= "0xff" (g:variant-print value)))

    ;; Parse double
    (setf value (g:variant-parse nil "10.0"))
    (is (string= "d" (g:variant-type-string value)))
    (is (string= "10.0" (g:variant-print value)))
    (setf value (g:variant-parse (g:variant-type-new "d") "10.0"))
    (is (string= "d" (g:variant-type-string value)))
    (is (string= "10.0" (g:variant-print value)))
    (setf value (g:variant-parse "d" "10.0"))
    (is (string= "d" (g:variant-type-string value)))
    (is (string= "10.0" (g:variant-print value)))

    ;; Parse handle
    (setf value (g:variant-parse nil "0xffffff"))
    (is (string= "i" (g:variant-type-string value)))
    (is (string= "16777215" (g:variant-print value)))
    (setf value (g:variant-parse (g:variant-type-new "h") "0xffffff"))
    (is (string= "h" (g:variant-type-string value)))
    (is (string= "16777215" (g:variant-print value)))
    (setf value (g:variant-parse "h" "0xffffff"))
    (is (string= "h" (g:variant-type-string value)))
    (is (string= "16777215" (g:variant-print value)))

    ;; Parse int16
    (setf value (g:variant-parse nil "0xfff"))
    (is (string= "i" (g:variant-type-string value)))
    (is (string= "4095" (g:variant-print value)))
    (setf value (g:variant-parse (g:variant-type-new "n") "0xfff"))
    (is (string= "n" (g:variant-type-string value)))
    (is (string= "4095" (g:variant-print value)))
    (setf value (g:variant-parse "n" "0xfff"))
    (is (string= "n" (g:variant-type-string value)))
    (is (string= "4095" (g:variant-print value)))
    (setf value (g:variant-parse (g:variant-type-new "q") "0xfff"))
    (is (string= "q" (g:variant-type-string value)))
    (is (string= "4095" (g:variant-print value)))
    (setf value (g:variant-parse "q" "0xfff"))
    (is (string= "q" (g:variant-type-string value)))
    (is (string= "4095" (g:variant-print value)))

    ;; Parse int32
    (setf value (g:variant-parse nil "0xffff"))
    (is (string= "i" (g:variant-type-string value)))
    (is (string= "65535" (g:variant-print value)))
    (setf value (g:variant-parse (g:variant-type-new "i") "0xffff"))
    (is (string= "i" (g:variant-type-string value)))
    (is (string= "65535" (g:variant-print value)))
    (setf value (g:variant-parse "i" "0xffff"))
    (is (string= "i" (g:variant-type-string value)))
    (is (string= "65535" (g:variant-print value)))
    (setf value (g:variant-parse (g:variant-type-new "u") "0xffff"))
    (is (string= "u" (g:variant-type-string value)))
    (is (string= "65535" (g:variant-print value)))
    (setf value (g:variant-parse "u" "0xffff"))
    (is (string= "u" (g:variant-type-string value)))
    (is (string= "65535" (g:variant-print value)))

    ;; Parse int64
    (setf value (g:variant-parse nil "0xfffff"))
    (is (string= "i" (g:variant-type-string value)))
    (is (string= "1048575" (g:variant-print value)))
    (setf value (g:variant-parse (g:variant-type-new "x") "0xfffff"))
    (is (string= "x" (g:variant-type-string value)))
    (is (string= "1048575" (g:variant-print value)))
    (setf value (g:variant-parse "x" "0xfffff"))
    (is (string= "x" (g:variant-type-string value)))
    (is (string= "1048575" (g:variant-print value)))
    (setf value (g:variant-parse (g:variant-type-new "t") "0xfffff"))
    (is (string= "t" (g:variant-type-string value)))
    (is (string= "1048575" (g:variant-print value)))
    (setf value (g:variant-parse "t" "0xfffff"))
    (is (string= "t" (g:variant-type-string value)))
    (is (string= "1048575" (g:variant-print value)))

    ;; Parse string
    (setf value (g:variant-parse nil "'test'"))
    (is (string= "s" (g:variant-type-string value)))
    (is (string= "'test'" (g:variant-print value)))
    (setf value (g:variant-parse (g:variant-type-new "s") "'test'"))
    (is (string= "s" (g:variant-type-string value)))
    (is (string= "'test'" (g:variant-print value)))
    (setf value (g:variant-parse "s" "'test'"))
    (is (string= "s" (g:variant-type-string value)))
    (is (string= "'test'" (g:variant-print value)))))

;;;     g_variant_new_parsed_va
;;;     g_variant_new_parsed

;;; 2025-05-25
