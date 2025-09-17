(in-package :glib-test)

(def-suite gobject-param :in gobject-suite)
(in-suite gobject-param)

;;;     GParamSpecBoolean

(test g-param-spec-boolean-struct
  (is (= 16 (cffi:foreign-type-size '(:struct g:param-spec-boolean))))
  (is (equal '(:parent-instance :default-value)
             (cffi:foreign-slot-names '(:struct g:param-spec-boolean)))))

;;;     g_param_spec_boolean

(test g-param-spec-boolean
  (let (pspec)
    ;; Create a GParamSpecBoolean
    (is (g:is-param-spec (setf pspec
                               (g:param-spec-boolean "myBoolean"
                                                     "myBool"
                                                     "Documentation"
                                                     t
                                                     '(:readable :writable)))))
    ;; Check type
    (is-true (g:type-is-param (g:type-from-instance pspec)))
    (is (eq (g:gtype "GParamBoolean") (g:param-spec-type pspec)))
    (is (string= "GParamBoolean" (g:param-spec-type-name pspec)))
    (is (eq (g:gtype "gboolean") (g:param-spec-value-type pspec)))
    ;; Check infos about the parameter
    (is (string= "myBoolean" (g:param-spec-name pspec)))
    (is (string= "myBool" (g:param-spec-nick pspec)))
    (is (string= "Documentation" (g:param-spec-blurb pspec)))
    ;; Check default value
    (g:with-value (gvalue "gboolean")
      (is (eq (g:gtype "gboolean")
              (g:value-type (g:param-spec-default-value pspec))))
      (is-true (g:value-get (g:param-spec-default-value pspec)))
      (is-false (g:param-value-set-default pspec gvalue))
      ;; Default value is T
      (is-true (g:value-get gvalue)))
    (is-false (g:param-spec-unref pspec))))

;;;     g_value_set_boolean
;;;     g_value_get_boolean

(test g-value-boolean
  (g:with-value (gvalue "gboolean")
    (is-true (setf (g:value-boolean gvalue) t))
    (is-true (g:value-boolean gvalue))
    (is-false (setf (g:value-boolean gvalue) nil))
    (is-false (g:value-boolean gvalue))))

;;;     GParamSpecChar

(test g-param-spec-char-struct
  (is (= 16 (cffi:foreign-type-size '(:struct g:param-spec-char))))
  (is (equal '(:DEFAULT-VALUE :MAXIMUM :MINIMUM :PARENT-INSTANCE)
             (sort (cffi:foreign-slot-names '(:struct g:param-spec-char))
                   #'string<))))

;;;     g_param_spec_char

(test g-param-spec-char
  (let (pspec)
    ;; Create a GParamSpecChar
    (is (g:is-param-spec (setf pspec
                               (g:param-spec-char "myChar"
                                                  "myChar"
                                                  "Documentation"
                                                  (char-code #\A)
                                                  (char-code #\Z)
                                                  (char-code #\D)
                                                  '(:readable :writable)))))
    ;; Check type
    (is-true (g:type-is-param (g:type-from-instance pspec)))
    (is (eq (g:gtype "GParamChar") (g:param-spec-type pspec)))
    (is (string= "GParamChar" (g:param-spec-type-name pspec)))
    (is (eq (g:gtype "gchar") (g:param-spec-value-type pspec)))
    ;; Check infos about the parameter
    (is (string= "myChar" (g:param-spec-name pspec)))
    (is (string= "myChar" (g:param-spec-nick pspec)))
    (is (string= "Documentation" (g:param-spec-blurb pspec)))
    (g:with-value (gvalue "gchar")
      ;; Check default value
      (is (eq (g:gtype "gchar")
              (g:value-type (g:param-spec-default-value pspec))))
      (is-true (g:value-get (g:param-spec-default-value pspec)))
      (is-false (g:param-value-set-default pspec gvalue))
      (is (= (char-code #\D)
             (g:value-get gvalue))) ; Default value is #\D
      ;; Validate a value
      (is (= 0 (setf (g:value-char gvalue) 0)))
      (is-true (g:param-value-validate pspec gvalue))
      (is (= (char-code #\A) (g:value-get gvalue)))
      (is (= (char-code #\a) (setf (g:value-char gvalue) (char-code #\a))))
      (is-true (g:param-value-validate pspec gvalue))
      (is (= (char-code #\Z) (g:value-get gvalue)))
      ;; More checks for a default value
      (is-false (g:param-value-defaults pspec gvalue))
      (is-false (g:param-value-set-default pspec gvalue))
      (is-true (g:param-value-defaults pspec gvalue)))
    (is-false (g:param-spec-unref pspec))))

;;;     g_value_set_char
;;;     g_value_get_char

(test g-value-char
  (g:with-value (gvalue "gchar")
    (is (= (char-code #\A) (setf (g:value-char gvalue) (char-code #\A))))
    (is (= (char-code #\A) (g:value-char gvalue)))))

;;;     g_value_get_schar
;;;     g_value_set_schar

(test g-value-schar
  (g:with-value (gvalue "gchar")
    (is (= (char-code #\A) (setf (g:value-schar gvalue) (char-code #\A))))
    (is (= (char-code #\A) (g:value-schar gvalue)))))

;;;     GParamSpecUChar

(test g-param-spec-uchar-struct
  (is (= 16 (cffi:foreign-type-size '(:struct g:param-spec-uchar))))
  (is (equal '(:DEFAULT-VALUE :MAXIMUM :MINIMUM :PARENT-INSTANCE)
             (sort (cffi:foreign-slot-names '(:struct g:param-spec-uchar))
                   #'string<))))

;;;     g_param_spec_uchar

(test g-param-spec-uchar
  (let (pspec)
    ;; Create a GParamSpecUChar
    (is (g:is-param-spec (setf pspec
                               (g:param-spec-uchar "myUChar"
                                                   "myUChar"
                                                   "Documentation"
                                                   (char-code #\A)
                                                   (char-code #\Z)
                                                   (char-code #\D)
                                                   '(:readable :writable)))))
    ;; Check type
    (is-true (g:type-is-param (g:type-from-instance pspec)))
    (is (eq (g:gtype "GParamUChar") (g:param-spec-type pspec)))
    (is (string= "GParamUChar" (g:param-spec-type-name pspec)))
    (is (eq (g:gtype "guchar") (g:param-spec-value-type pspec)))

    ;; Check infos about the parameter
    (is (string= "myUChar" (g:param-spec-name pspec)))
    (is (string= "myUChar" (g:param-spec-nick pspec)))
    (is (string= "Documentation" (g:param-spec-blurb pspec)))

    ;; Check default value
    (g:with-value (gvalue "guchar")
      (is (eq (g:gtype "guchar")
              (g:value-type (g:param-spec-default-value pspec))))
      (is-true (g:value-get (g:param-spec-default-value pspec)))
      (is-false (g:param-value-set-default pspec gvalue))
      (is (= (char-code #\D)
             (g:value-get gvalue))) ; the default value is #\D
      ;; Validate a value
      (is (= 48 (setf (g:value-uchar gvalue) (char-code #\0))))
      (is-true (g:param-value-validate pspec gvalue))
      (is (= (char-code #\A) (g:value-get gvalue)))
      (is (= (char-code #\a) (setf (g:value-uchar gvalue) (char-code #\a))))
      (is-true (g:param-value-validate pspec gvalue))
      (is (= (char-code #\Z) (g:value-get gvalue)))
      ;; More checks for a default value
      (is-false (g:param-value-defaults pspec gvalue))
      (is-false (g:param-value-set-default pspec gvalue))
      (is-true (g:param-value-defaults pspec gvalue)))
    (is-false (g:param-spec-unref pspec))))

;;;     g_value_set_uchar
;;;     g_value_get_uchar

(test g-value-uchar
  (g:with-value (gvalue "guchar")
    (is (= (char-code #\A) (setf (g:value-uchar gvalue) (char-code #\A))))
    (is (= (char-code #\A) (g:value-uchar gvalue)))))

;;;     GParamSpecInt

(test g-param-spec-int-struct
  (is (= 24 (cffi:foreign-type-size '(:struct g:param-spec-int))))
  (is (equal '(:DEFAULT-VALUE :MAXIMUM :MINIMUM :PARENT-INSTANCE)
             (sort (cffi:foreign-slot-names '(:struct g:param-spec-int))
                   #'string<))))

;;;   g_param_spec_int

(test g-param-spec-int
  (let (pspec)
    ;; Create a GParamSpecInt
    (is (g:is-param-spec (setf pspec
                               (g:param-spec-int "myInteger"
                                                 "myInt"
                                                 "Documentation"
                                                 50
                                                 150
                                                 100
                                                 '(:readable :writable)))))
    ;; Check type
    (is-true (g:type-is-param (g:type-from-instance pspec)))
    (is (eq (g:gtype "GParamInt") (g:param-spec-type pspec)))
    (is (string= "GParamInt" (g:param-spec-type-name pspec)))
    (is (eq (g:gtype "gint") (g:param-spec-value-type pspec)))
    ;; Check infos about the parameter
    (is (string= "myInteger" (g:param-spec-name pspec)))
    (is (string= "myInt" (g:param-spec-nick pspec)))
    (is (string= "Documentation" (g:param-spec-blurb pspec)))
    ;; Check default value
    (g:with-value (value "gint")
      (is (eq (g:gtype "gint")
              (g:value-type (g:param-spec-default-value pspec))))
      (is-true (g:value-get (g:param-spec-default-value pspec)))
      (is-false (g:param-value-set-default pspec value))
      (is (= 100 (g:value-get value))) ; the default value is 100
      ;; Validate a value
      (is (= 0 (setf (g:value-int value) 0)))
      (is-true (g:param-value-validate pspec value))
      (is (= 50 (g:value-get value)))
      (is (= 200 (setf (g:value-int value) 200)))
      (is-true (g:param-value-validate pspec value))
      (is (= 150 (g:value-get value)))
      ;; More checks for a default value
      (is-false (g:param-value-defaults pspec value))
      (is-false (g:param-value-set-default pspec value))
      (is-true (g:param-value-defaults pspec value)))
    (is-false (g:param-spec-unref pspec))))

;;;     g_value_set_int
;;;     g_value_get_int

(test g-value-int
  (g:with-value (value "gint")
    (g:value-init value "gint")
    (is (= 65000 (setf (g:value-int value) 65000)))
    (is (= 65000 (g:value-int value)))))

;;;     GParamSpecUInt

(test g-param-spec-uint-struct
  (is (= 24 (cffi:foreign-type-size '(:struct g:param-spec-uint))))
  (is (equal '(:DEFAULT-VALUE :MAXIMUM :MINIMUM :PARENT-INSTANCE)
             (sort (cffi:foreign-slot-names '(:struct g:param-spec-uint))
                   #'string<))))

;;;     g_param_spec_uint

(test g-param-spec-uint
  (let (pspec)
    ;; Create a GParamSpecInt
    (is (g:is-param-spec (setf pspec
                               (g:param-spec-uint "myUnsignedInteger"
                                                  "myUInt"
                                                  "Documentation"
                                                  50
                                                  150
                                                  100
                                                  '(:readable :writable)))))
    ;; Check type
    (is-true (g:type-is-param (g:type-from-instance pspec)))
    (is (eq (g:gtype "GParamUInt") (g:param-spec-type pspec)))
    (is (string= "GParamUInt" (g:param-spec-type-name pspec)))
    (is (eq (g:gtype "guint") (g:param-spec-value-type pspec)))
    ;; Check infos about the parameter
    (is (string= "myUnsignedInteger" (g:param-spec-name pspec)))
    (is (string= "myUInt" (g:param-spec-nick pspec)))
    (is (string= "Documentation" (g:param-spec-blurb pspec)))
    ;; Check default value
    (g:with-value (value "guint")
      (is (eq (g:gtype "guint")
              (g:value-type (g:param-spec-default-value pspec))))
      (is-true (g:value-get (g:param-spec-default-value pspec)))
      (is-false (g:param-value-set-default pspec value))
      (is (= 100 (g:value-get value))) ; the default value is 100
      ;; Validate a value
      (is (= 0 (setf (g:value-uint value) 0)))
      (is-true (g:param-value-validate pspec value))
      (is (= 50 (g:value-get value)))
      (is (= 200 (setf (g:value-uint value) 200)))
      (is-true (g:param-value-validate pspec value))
      (is (= 150 (g:value-get value)))
      ;; More checks for a default value
      (is-false (g:param-value-defaults pspec value))
      (is-false (g:param-value-set-default pspec value))
      (is-true (g:param-value-defaults pspec value)))
    (is-false (g:param-spec-unref pspec))))

;;;     g_value_set_uint
;;;     g_value_get_uint

(test g-value-uint
  (g:with-value (value "guint")
    (is (= 65000 (setf (g:value-uint value) 65000)))
    (is (= 65000 (g:value-uint value)))))

;;;     GParamSpecLong

(test g-param-spec-long-struct
  #-windows
  (is (= 32 (cffi:foreign-type-size '(:struct g:param-spec-long))))
  #+windows
  (is (= 24 (cffi:foreign-type-size '(:struct g:param-spec-long))))
  (is (equal '(:DEFAULT-VALUE :MAXIMUM :MINIMUM :PARENT-INSTANCE)
             (sort (cffi:foreign-slot-names '(:struct g:param-spec-long))
                   #'string<))))

;;;     g_param_spec_long

(test g-param-spec-long
  (let (pspec)
    ;; Create a GParamSpecInt
    (is (g:is-param-spec (setf pspec
                               (g:param-spec-long "myLong"
                                                  "myLong"
                                                  "Documentation"
                                                  50
                                                  150
                                                  100
                                                  '(:readable :writable)))))
    ;; Check type
    (is-true (g:type-is-param (g:type-from-instance pspec)))
    (is (eq (g:gtype "GParamLong") (g:param-spec-type pspec)))
    (is (string= "GParamLong" (g:param-spec-type-name pspec)))
    (is (eq (g:gtype "glong") (g:param-spec-value-type pspec)))
    ;; Check default value
    (g:with-value (value "glong")
      (is (eq (g:gtype "glong")
              (g:value-type (g:param-spec-default-value pspec))))
      (is-true (g:value-get (g:param-spec-default-value pspec)))
      (is-false (g:param-value-set-default pspec value))
      (is (= 100 (g:value-get value))) ; the default value is 100
      ;; Validate a value
      (is (= 0 (setf (g:value-long value) 0)))
      (is-true (g:param-value-validate pspec value))
      (is (= 50 (g:value-get value)))
      (is (= 200 (setf (g:value-long value) 200)))
      (is-true (g:param-value-validate pspec value))
      (is (= 150 (g:value-get value)))
      ;; More checks for a default value
      (is-false (g:param-value-defaults pspec value))
      (is-false (g:param-value-set-default pspec value))
      (is-true (g:param-value-defaults pspec value))
      ;; Check infos about the parameter
      (is (string= "myLong" (g:param-spec-name pspec)))
      (is (string= "myLong" (g:param-spec-nick pspec)))
      (is (string= "Documentation" (g:param-spec-blurb pspec))))
    (is-false (g:param-spec-unref pspec))))

;;;     g_value_set_long
;;;     g_value_get_long

(test g-value-long
  (g:with-value (value "glong")
    (g:value-init value "glong")
    (is (= 65000 (setf (g:value-long value) 65000)))
    (is (= 65000 (g:value-long value)))))

;;;     GParamSpecULong

(test g-param-spec-ulong-struct
  #-windows
  (is (= 32 (cffi:foreign-type-size '(:struct g:param-spec-ulong))))
  #+windows
  (is (= 24 (cffi:foreign-type-size '(:struct g:param-spec-ulong))))
  (is (equal '(:DEFAULT-VALUE :MAXIMUM :MINIMUM :PARENT-INSTANCE)
             (sort (cffi:foreign-slot-names '(:struct g:param-spec-ulong))
                   #'string<))))

;;;     g_param_spec_ulong

(test g-param-spec-ulong
  (let (pspec)
    (cffi:with-foreign-object (value '(:struct g:value))
      ;; Initialize a GValue for further checks
      (is (cffi:pointerp (g:value-init value "gulong")))
      ;; Create a GParamSpecInt
      (is (cffi:pointerp (setf pspec
                          (g:param-spec-ulong "myUnsignedLong"
                                              "myULong"
                                              "Documentation"
                                              50
                                              150
                                              100
                                              '(:readable :writable)))))
      ;; Check type
      (is-true (g:type-is-param (g:type-from-instance pspec)))
      (is-true (g:is-param-spec pspec))
      (is (eq (g:gtype "GParamULong") (g:param-spec-type pspec)))
      (is (string= "GParamULong" (g:param-spec-type-name pspec)))
      (is (eq (g:gtype "gulong") (g:param-spec-value-type pspec)))
      ;; Check default value
      (is (eq (g:gtype "gulong")
              (g:value-type (g:param-spec-default-value pspec))))
      (is-true (g:value-get (g:param-spec-default-value pspec)))
      (is-false (g:param-value-set-default pspec value))
      (is (= 100 (g:value-get value))) ; the default value is 100
      ;; Validate a value
      (is (= 0 (setf (g:value-ulong value) 0)))
      (is-true (g:param-value-validate pspec value))
      (is (= 50 (g:value-get value)))
      (is (= 200 (setf (g:value-ulong value) 200)))
      (is-true (g:param-value-validate pspec value))
      (is (= 150 (g:value-get value)))
      ;; More checks for a default value
      (is-false (g:param-value-defaults pspec value))
      (is-false (g:param-value-set-default pspec value))
      (is-true (g:param-value-defaults pspec value))
      ;; Check infos about the parameter
      (is (string= "myUnsignedLong" (g:param-spec-name pspec)))
      (is (string= "myULong" (g:param-spec-nick pspec)))
      (is (string= "Documentation" (g:param-spec-blurb pspec)))
      ;; Unset the GValue
      (is-false (g:value-unset value))
      (is-false (g:param-spec-unref pspec)))))

;;;     g_value_set_ulong
;;;     g_value_get_ulong

(test g-value-ulong
  (cffi:with-foreign-object (value '(:struct g:value))
    (g:value-init value "gulong")
    (is (= 65000 (setf (g:value-ulong value) 65000)))
    (is (= 65000 (g:value-ulong value)))
    (g:value-unset value)))

;;;     GParamSpecInt64

(test g-param-spec-int64-struct
  (is (= 32 (cffi:foreign-type-size '(:struct g:param-spec-int64))))
  (is (equal '(:DEFAULT-VALUE :MAXIMUM :MINIMUM :PARENT-INSTANCE)
             (sort (cffi:foreign-slot-names '(:struct g:param-spec-int64))
                   #'string<))))

;;;     g_param_spec_int64

(test g-param-spec-int64
  (let (pspec)
    (cffi:with-foreign-object (value '(:struct g:value))
      ;; Initialize a GValue for further checks
      (is (cffi:pointerp (g:value-init value "gint64")))
      ;; Create a GParamSpecInt
      (is (cffi:pointerp (setf pspec
                          (g:param-spec-int64 "myInt64"
                                              "myInt64"
                                              "Documentation"
                                              50
                                              150
                                              100
                                              '(:readable :writable)))))
      ;; Check type
      (is-true (g:type-is-param (g:type-from-instance pspec)))
      (is-true (g:is-param-spec pspec))
      (is (eq (g:gtype "GParamInt64") (g:param-spec-type pspec)))
      (is (string= "GParamInt64" (g:param-spec-type-name pspec)))
      (is (eq (g:gtype "gint64") (g:param-spec-value-type pspec)))
      ;; Check default value
      (is (eq (g:gtype "gint64")
              (g:value-type (g:param-spec-default-value pspec))))
      (is-true (g:value-get (g:param-spec-default-value pspec)))
      (is-false (g:param-value-set-default pspec value))
      (is (= 100 (g:value-get value))) ; the default value is 100
      ;; Validate a value
      (is (= 0 (setf (g:value-int64 value) 0)))
      (is-true (g:param-value-validate pspec value))
      (is (= 50 (g:value-get value)))
      (is (= 200 (setf (g:value-int64 value) 200)))
      (is-true (g:param-value-validate pspec value))
      (is (= 150 (g:value-get value)))
      ;; More checks for a default value
      (is-false (g:param-value-defaults pspec value))
      (is-false (g:param-value-set-default pspec value))
      (is-true (g:param-value-defaults pspec value))
      ;; Check infos about the parameter
      (is (string= "myInt64" (g:param-spec-name pspec)))
      (is (string= "myInt64" (g:param-spec-nick pspec)))
      (is (string= "Documentation" (g:param-spec-blurb pspec)))
      ;; Unset the GValue
      (is-false (g:value-unset value))
      (is-false (g:param-spec-unref pspec)))))

;;;     g_value_set_int64
;;;     g_value_get_int64

(test g-value-int64
  (cffi:with-foreign-object (value '(:struct g:value))
    (g:value-init value "gint64")
    (is (= 65000 (setf (g:value-int64 value) 65000)))
    (is (= 65000 (g:value-int64 value)))
    (g:value-unset value)))

;;;     GParamSpecUInt64

(test g-param-spec-uint64-struct
  (is (= 32 (cffi:foreign-type-size '(:struct g:param-spec-uint64))))
  (is (equal '(:DEFAULT-VALUE :MAXIMUM :MINIMUM :PARENT-INSTANCE)
             (sort (cffi:foreign-slot-names '(:struct g:param-spec-uint64))
                   #'string<))))

;;;     g_param_spec_uint64

(test g-param-spec-uint64
  (let (pspec)
    (cffi:with-foreign-object (value '(:struct g:value))
      ;; Initialize a GValue for further checks
      (is (cffi:pointerp (g:value-init value "guint64")))
      ;; Create a GParamSpecInt
      (is (cffi:pointerp (setf pspec
                          (g:param-spec-uint64 "myUsignedInt64"
                                               "myUInt64"
                                               "Documentation"
                                               50
                                               150
                                               100
                                               '(:readable :writable)))))
      ;; Check type
      (is-true (g:type-is-param (g:type-from-instance pspec)))
      (is-true (g:is-param-spec pspec))
      (is (eq (g:gtype "GParamUInt64") (g:param-spec-type pspec)))
      (is (string= "GParamUInt64" (g:param-spec-type-name pspec)))
      (is (eq (g:gtype "guint64") (g:param-spec-value-type pspec)))
      ;; Check default value
      (is (eq (g:gtype "guint64")
              (g:value-type (g:param-spec-default-value pspec))))
      (is-true (g:value-get (g:param-spec-default-value pspec)))
      (is-false (g:param-value-set-default pspec value))
      (is (= 100 (g:value-get value))) ; the default value is 100
      ;; Validate a value
      (is (= 0 (setf (g:value-uint64 value) 0)))
      (is-true (g:param-value-validate pspec value))
      (is (= 50 (g:value-get value)))
      (is (= 200 (setf (g:value-uint64 value) 200)))
      (is-true (g:param-value-validate pspec value))
      (is (= 150 (g:value-get value)))
      ;; More checks for a default value
      (is-false (g:param-value-defaults pspec value))
      (is-false (g:param-value-set-default pspec value))
      (is-true (g:param-value-defaults pspec value))
      ;; Check infos about the parameter
      (is (string= "myUsignedInt64" (g:param-spec-name pspec)))
      (is (string= "myUInt64" (g:param-spec-nick pspec)))
      (is (string= "Documentation" (g:param-spec-blurb pspec)))
      ;; Unset the GValue
      (is-false (g:value-unset value))
      (is-false (g:param-spec-unref pspec)))))

;;;     g_value_set_uint64
;;;     g_value_get_uint64

(test g-value-uint64
  (cffi:with-foreign-object (value '(:struct g:value))
    (g:value-init value "guint64")
    (is (= 65000 (setf (g:value-uint64 value) 65000)))
    (is (= 65000 (g:value-uint64 value)))
    (g:value-unset value)))

;;;     GParamSpecFloat

(test g-param-spec-float-struct
  (is (= 24 (cffi:foreign-type-size '(:struct g:param-spec-float))))
  (is (equal '(:DEFAULT-VALUE :EPSILON :MAXIMUM :MINIMUM :PARENT-INSTANCE)
             (sort (cffi:foreign-slot-names '(:struct g:param-spec-float))
                   #'string<))))

;;;     g_param_spec_float

(test g-param-spec-float
  (let (pspec)
    (cffi:with-foreign-object (value '(:struct g:value))
      ;; Initialize a GValue for further checks
      (is (cffi:pointerp (g:value-init value "gfloat")))
      ;; Create a GParamSpecInt
      (is (cffi:pointerp (setf pspec
                          (g:param-spec-float "myFloat"
                                              "myFloat"
                                              "Documentation"
                                              50.0
                                              150.0
                                              100.0
                                              '(:readable :writable)))))
      ;; Check type
      (is-true (g:type-is-param (g:type-from-instance pspec)))
      (is-true (g:is-param-spec pspec))
      (is (eq (g:gtype "GParamFloat") (g:param-spec-type pspec)))
      (is (string= "GParamFloat" (g:param-spec-type-name pspec)))
      (is (eq (g:gtype "gfloat") (g:param-spec-value-type pspec)))
      ;; Check the default value
      (is (eq (g:gtype "gfloat")
              (g:value-type (g:param-spec-default-value pspec))))
      (is-true (g:value-get (g:param-spec-default-value pspec)))
      (is-false (g:param-value-set-default pspec value))
      (is (= 100 (g:value-get value))) ; the default value is 100
      ;; Validate a value
      (is (= 0 (setf (g:value-float value) 0.0)))
      (is-true (g:param-value-validate pspec value))
      (is (= 50 (g:value-get value)))
      (is (= 200 (setf (g:value-float value) 200.0)))
      (is-true (g:param-value-validate pspec value))
      (is (= 150 (g:value-get value)))
      ;; More checks for a default value
      (is-false (g:param-value-defaults pspec value))
      (is-false (g:param-value-set-default pspec value))
      (is-true (g:param-value-defaults pspec value))
      ;; Check infos about the parameter
      (is (string= "myFloat" (g:param-spec-name pspec)))
      (is (string= "myFloat" (g:param-spec-nick pspec)))
      (is (string= "Documentation" (g:param-spec-blurb pspec)))
      ;; Unset the GValue
      (is-false (g:value-unset value))
      (is-false (g:param-spec-unref pspec)))))

;;;     g_value_set_float
;;;     g_value_get_float

(test g-value-float
  (cffi:with-foreign-object (value '(:struct g:value))
    (g:value-init value "gfloat")
    (is (= 65000 (setf (g:value-float value) 65000.0)))
    (is (= 65000 (g:value-float value)))
    (g:value-unset value)))

;;;     GParamSpecDouble

(test g-param-spec-double-struct
  (is (= 40 (cffi:foreign-type-size '(:struct g:param-spec-double))))
  (is (equal '(:DEFAULT-VALUE :EPSILON :MAXIMUM :MINIMUM :PARENT-INSTANCE)
             (sort (cffi:foreign-slot-names '(:struct g:param-spec-double))
                   #'string<))))

;;;     g_param_spec_double

(test g-param-spec-double
  (let (pspec)
    (cffi:with-foreign-object (value '(:struct g:value))
      ;; Initialize a GValue for further checks
      (is (cffi:pointerp (g:value-init value "gdouble")))
      ;; Create a GParamSpecDouble
      (is (cffi:pointerp (setf pspec
                          (g:param-spec-double "myDouble"
                                               "myDouble"
                                               "Documentation"
                                               50.0d0
                                               150.0d0
                                               100.0d0
                                               '(:readable :writable)))))
      ;; Check type
      (is-true (g:type-is-param (g:type-from-instance pspec)))
      (is-true (g:is-param-spec pspec))
      (is (eq (g:gtype "GParamDouble") (g:param-spec-type pspec)))
      (is (string= "GParamDouble" (g:param-spec-type-name pspec)))
      (is (eq (g:gtype "gdouble") (g:param-spec-value-type pspec)))
      ;; Check default value
      (is (eq (g:gtype "gdouble")
              (g:value-type (g:param-spec-default-value pspec))))
      (is-true (g:value-get (g:param-spec-default-value pspec)))
      (is-false (g:param-value-set-default pspec value))
      (is (= 100 (g:value-get value))) ; the default value is 100
      ;; Validate a value
      (is (= 0 (setf (g:value-double value) 0.0d0)))
      (is-true (g:param-value-validate pspec value))
      (is (= 50 (g:value-get value)))
      (is (= 200 (setf (g:value-double value) 200.0d0)))
      (is-true (g:param-value-validate pspec value))
      (is (= 150 (g:value-get value)))
      ;; More checks for a default value
      (is-false (g:param-value-defaults pspec value))
      (is-false (g:param-value-set-default pspec value))
      (is-true (g:param-value-defaults pspec value))
      ;; Check infos about the parameter
      (is (string= "myDouble" (g:param-spec-name pspec)))
      (is (string= "myDouble" (g:param-spec-nick pspec)))
      (is (string= "Documentation" (g:param-spec-blurb pspec)))
      ;; Unset the GValue
      (is-false (g:value-unset value))
      (is-false (g:param-spec-unref pspec)))))

;;;     g_value_set_double
;;;     g_value_get_double

(test g-value-double
  (cffi:with-foreign-object (value '(:struct g:value))
    (g:value-init value "gdouble")
    (is (= 65000 (setf (g:value-double value) 65000.0d0)))
    (is (= 65000 (g:value-double value)))
    (g:value-unset value)))

;;;     GParamSpecEnum

(test g-param-spec-enum-struct
  (is (= 24 (cffi:foreign-type-size '(:struct g:param-spec-enum))))
  (is (equal '(:DEFAULT-VALUE :ENUM-CLASS :PARENT-INSTANCE)
             (sort (cffi:foreign-slot-names '(:struct g:param-spec-enum))
                   #'string<))))

;;;     g_param_spec_enum

(test g-param-spec-enum
  (let (pspec)
    (cffi:with-foreign-object (value '(:struct g:value))
      ;; Initialize a GValue for further checks
      (is (cffi:pointerp (g:value-init value "GEmblemOrigin")))
      ;; Create a GParamSpec
      (is (cffi:pointerp (setf pspec
                               (g:param-spec-enum "myEnumeration"
                                                  "myEnum"
                                                  "Documentation"
                                                  "GEmblemOrigin"
                                                  0 ; for :unkown
                                                  '(:readable :writable)))))
      ;; Check type
      (is-true (g:type-is-param (g:type-from-instance pspec)))
      (is-true (g:is-param-spec pspec))
      (is (eq (g:gtype "GParamEnum") (g:param-spec-type pspec)))
      (is (string= "GParamEnum" (g:param-spec-type-name pspec)))
      (is (eq (g:gtype "GEmblemOrigin") (g:param-spec-value-type pspec)))
      ;; Check the default value
      (is (eq (g:gtype "GEmblemOrigin")
              (g:value-type (g:param-spec-default-value pspec))))
      (is-true (g:value-get (g:param-spec-default-value pspec)))
      (is-false (g:param-value-set-default pspec value))
      (is (eq :unknown
              (g:value-get value))) ; the default value is :empty
      ;; More checks for a default value
      (is-true (g:param-value-defaults pspec value))
      (is-false (g:param-value-set-default pspec value))
      (is-true (g:param-value-defaults pspec value))
      ;; Check the infos about the parameter
      (is (string= "myEnumeration" (g:param-spec-name pspec)))
      (is (string= "myEnum" (g:param-spec-nick pspec)))
      (is (string= "Documentation" (g:param-spec-blurb pspec)))
      ;; Unset the GValue
      (is-false (g:value-unset value))
      (is-false (g:param-spec-unref pspec)))))

;;;     g_value_set_enum
;;;     g_value_get_enum

(test g-value-enum
  (cffi:with-foreign-object (value '(:struct g:value))
    (g:value-init value "GEmblemOrigin")
    (is (= 0 (setf (gobject::%value-enum value) 0)))
    (is (= 0 (gobject::%value-enum value)))
    ;; Accepts any integer, must not be valid
    (is (= 2 (setf (gobject::%value-enum value) 2)))
    (is (= 2 (gobject::%value-enum value)))
    (g:value-unset value)))

;;;     GParamSpecFlags

(test g-param-spec-flags-struct
  (is (= 24 (cffi:foreign-type-size '(:struct g:param-spec-flags))))
  (is (equal '(:DEFAULT-VALUE :FLAGS-CLASS :PARENT-INSTANCE)
             (sort (cffi:foreign-slot-names '(:struct g:param-spec-flags))
                   #'string<))))

;;;     g_param_spec_flags

(test g-param-spec-flags
  (let (pspec)
    (cffi:with-foreign-object (value '(:struct g:value))
      ;; Initialize a GValue for further checks
      (is (cffi:pointerp (g:value-init value "GApplicationFlags")))
      ;; Create a GParamSpec
      (is (cffi:pointerp (setf pspec
                          (g:param-spec-flags "myFlags"
                                              "myFlags"
                                              "Documentation"
                                              "GApplicationFlags"
                                              1 ; for :is-service
                                              '(:readable :writable)))))
      ;; Check type
      (is-true (g:type-is-param (g:type-from-instance pspec)))
      (is-true (g:is-param-spec pspec))
      (is (eq (g:gtype "GParamFlags") (g:param-spec-type pspec)))
      (is (string= "GParamFlags" (g:param-spec-type-name pspec)))
      (is (eq (g:gtype "GApplicationFlags") (g:param-spec-value-type pspec)))
      ;; Check the default value
      (is (eq (g:gtype "GApplicationFlags")
              (g:value-type (g:param-spec-default-value pspec))))
      (is (equal '(:is-service)
                 (g:value-get (g:param-spec-default-value pspec))))
      (is-false (g:param-value-set-default pspec value))
      (is (equal '(:is-service) (g:value-get value))) ; default value
      ;; More checks for a default value
      (is-true (g:param-value-defaults pspec value))
      (is-false (g:param-value-set-default pspec value))
      (is-true (g:param-value-defaults pspec value))
      ;; Check the infos about the parameter
      (is (string= "myFlags" (g:param-spec-name pspec)))
      (is (string= "myFlags" (g:param-spec-nick pspec)))
      (is (string= "Documentation" (g:param-spec-blurb pspec)))
      ;; Unset the GValue
      (is-false (g:value-unset value))
      (is-false (g:param-spec-unref pspec)))))

;;;     g_value_set_flags
;;;     g_value_get_flags

(test g-value-flags
  (cffi:with-foreign-object (value '(:struct g:value))
    (g:value-init value "GApplicationFlags")
    (is (= 0 (setf (gobject::%value-flags value) 0)))
    (is (= 0 (gobject::%value-flags value)))
    ;; Accepts any integer, must not be valid
    (is (= 99 (setf (gobject::%value-flags value) 99)))
    (is (= 99 (gobject::%value-flags value)))
    (g:value-unset value)))

;;;     GParamSpecString
;;;     gchararray

(test g-param-spec-string-struct
  (is (= 40 (cffi:foreign-type-size '(:struct g:param-spec-string))))
  (is (equal '(:CSET-FIRST :CSET-NTH :DEFAULT-VALUE :FLAGS-FOR-NULL
               :PARENT-INSTANCE :SUBSTITUTOR)
             (sort (cffi:foreign-slot-names '(:struct g:param-spec-string))
                   #'string<))))

;;;     g_param_spec_string

(test g-param-spec-string
  (let (pspec)
    (cffi:with-foreign-object (value '(:struct g:value))
      ;; Initialize a GValue for further checks
      (is (cffi:pointerp (g:value-init value "gchararray")))
      ;; Create a GParamSpec
      (is (cffi:pointerp (setf pspec
                          (g:param-spec-string "myString"
                                               "myString"
                                               "Documentation"
                                               "string"
                                               '(:readable :writable)))))
      ;; Check type
      (is-true (g:type-is-param (g:type-from-instance pspec)))
      (is-true (g:is-param-spec pspec))
      (is (eq (g:gtype "GParamString") (g:param-spec-type pspec)))
      (is (string= "GParamString" (g:param-spec-type-name pspec)))
      (is (eq (g:gtype "gchararray") (g:param-spec-value-type pspec)))
      ;; Check the default value
      (is (eq (g:gtype "gchararray")
              (g:value-type (g:param-spec-default-value pspec))))
      (is (string= "string"
                   (g:value-get (g:param-spec-default-value pspec))))
      (is-false (g:param-value-set-default pspec value))
      (is (string= "string" (g:value-get value)))
      ;; More checks for a default value
      (is-true (g:param-value-defaults pspec value))
      (is-false (g:param-value-set-default pspec value))
      (is-true (g:param-value-defaults pspec value))
      ;; Check the infos about the parameter
      (is (string= "myString" (g:param-spec-name pspec)))
      (is (string= "myString" (g:param-spec-nick pspec)))
      (is (string= "Documentation" (g:param-spec-blurb pspec)))
      ;; Unset the GValue
      (is-false (g:value-unset value))
      (is-false (g:param-spec-unref pspec)))))

;;;     g_value_set_string
;;;     g_value_get_string

(test g-value-string
  (cffi:with-foreign-object (value '(:struct g:value))
    (g:value-init value "gchararray")
    (is (string= "string" (setf (g:value-string value) "string")))
    (is (string= "string" (g:value-string value)))
    (g:value-unset value)))

;;;     GParamSpecParam

(test g-param-spec-param-struct
  (is (= 8 (cffi:foreign-type-size '(:struct g:param-spec-param))))
  (is (equal '(:parent-instance)
             (cffi:foreign-slot-names '(:struct g:param-spec-param)))))

;;;     g_param_spec_param

(test g-param-spec-param
  (let (pspec)
    (cffi:with-foreign-object (value '(:struct g:value))
      ;; Initialize a GValue for further checks
      (is (cffi:pointerp (g:value-init value "GParamParam")))
      ;; Create a GParamSpec
      (is (cffi:pointerp (setf pspec
                          (g:param-spec-param "myParameter"
                                              "myParam"
                                              "Documentation"
                                              "GParamBoolean"
                                              '(:readable :writable)))))
      ;; Check type
      (is-true (g:type-is-param (g:type-from-instance pspec)))
      (is-true (g:is-param-spec pspec))
      (is (eq (g:gtype "GParamParam") (g:param-spec-type pspec)))
      (is (string= "GParamParam" (g:param-spec-type-name pspec)))
      (is (eq (g:gtype "GParamBoolean") (g:param-spec-value-type pspec)))
      ;; Check the default value
      (is (eq (g:gtype "GParamBoolean")
              (g:value-type (g:param-spec-default-value pspec))))
;     (is-false (g:value-get (g-param-spec-default-value pspec)))
;     (is-false (g-param-value-set-default pspec value))
;     (is (string= "string" (g:value-get value)))
      ;; More checks for a default value
;     (is-true (g-param-value-defaults pspec value))
;     (is-false (g-param-value-set-default pspec value))
;     (is-true (g-param-value-defaults pspec value))
      ;; Check the infos about the parameter
      (is (string= "myParameter" (g:param-spec-name pspec)))
      (is (string= "myParam" (g:param-spec-nick pspec)))
      (is (string= "Documentation" (g:param-spec-blurb pspec)))
      ;; Unset the GValue
      (is-false (g:value-unset value))
      (is-false (g:param-spec-unref pspec)))))

;;;     g_value_set_param
;;;     g_value_get_param

(test g-value-param
  (cffi:with-foreign-object (value '(:struct g:value))
    (g:value-init value "GParamBoolean")
    (is (eq (g:gtype "GParamBoolean")
            (g:param-spec-type (setf (g:value-param value)
                                     (g:param-spec-boolean "myBool"
                                                           "myBool"
                                                           "Doku" t '())))))
    (is (eq (g:gtype "GParamBoolean") (g:param-spec-type (g:value-param value))))
    (g:value-unset value)))

;;;     GParamSpecBoxed

(test g-param-spec-boxed-struct
  (is (= 8 (cffi:foreign-type-size '(:struct g:param-spec-boxed))))
  (is (equal '(:parent-instance)
             (cffi:foreign-slot-names '(:struct g:param-spec-boxed)))))

;;;     g_param_spec_boxed

(test g-param-spec-boxed
  (let (pspec)
    (cffi:with-foreign-object (value '(:struct g:value))
      ;; Initialize a GValue for further checks
      (is (cffi:pointerp (g:value-init value "GParamBoxed")))
      ;; Create a GParamSpec
      (is (cffi:pointerp (setf pspec
                               (g:param-spec-boxed "myBoxed"
                                                   "myBoxed"
                                                   "Documentation"
                                                   "GBytes"
                                                   '(:readable :writable)))))
      ;; Check type
      (is-true (g:type-is-param (g:type-from-instance pspec)))
      (is-true (g:is-param-spec pspec))
      (is (eq (g:gtype "GParamBoxed") (g:param-spec-type pspec)))
      (is (string= "GParamBoxed" (g:param-spec-type-name pspec)))
      (is (eq (g:gtype "GBytes") (g:param-spec-value-type pspec)))
      ;; Check the default value
      ;  no default value
      ;; Check the infos about the parameter
      (is (string= "myBoxed" (g:param-spec-name pspec)))
      (is (string= "myBoxed" (g:param-spec-nick pspec)))
      (is (string= "Documentation" (g:param-spec-blurb pspec)))
      ;; Unset the GValue
      (is-false (g:value-unset value))
      (is-false (g:param-spec-unref pspec)))))

;;;     g_value_set_boxed
;;;     g_value_set_static_boxed
;;;     g_value_take_boxed
;;;     g_value_set_boxed_take_ownership
;;;     g_value_get_boxed
;;;     g_value_dup_boxed

;; TODO: Improve the implementation of g:value-boxed

#+nil
(test g-value-boxed
  (cffi:with-foreign-object (value '(:struct g:value))
    (g:value-init value "GdkRectangle")
    (is (eq 'gdk:rectangle
            (type-of (setf (g:value-boxed value) (gdk:rectangle-new)))))
;    (is-false (g:param-spec-type (g:value-boxed value)))
    (g:value-unset value)))

;;;     GParamSpecPointer

;;;     g_param_spec_pointer
;;;     g_value_set_pointer
;;;     g_value_get_pointer

;;;     GParamSpecObject

;;;     g_param_spec_object
;;;     g_value_set_object
;;;     g_value_take_object
;;;     g_value_set_object_take_ownership
;;;     g_value_get_object
;;;     g_value_dup_object

;;;     GParamSpecUnichar

;;;     g_param_spec_unichar

;;;     GParamSpecValueArray

;;;     g_param_spec_value_array

;;;     GParamSpecOverride

;;;     g_param_spec_override

;;;     GParamSpecGType

(test g-param-spec-gtype-struct
  (is (= 16 (cffi:foreign-type-size '(:struct g:param-spec-gtype))))
  (is (equal '(:PARENT-INSTANCE :IS-A-TYPE)
             (cffi:foreign-slot-names '(:struct g:param-spec-gtype)))))

;;;     g_param_spec_gtype

(test g-param-spec-gtype
  (let (pspec)
    ;; Create a GParamSpecGtype
    (is (g:is-param-spec (setf pspec
                               (g:param-spec-gtype "myGType"
                                                   "myGType"
                                                   "Documentation"
                                                   "GObject"
                                                   '(:readable :writable)))))
    ;; Check type
    (is-true (g:type-is-param (g:type-from-instance pspec)))
    (is (eq (g:gtype "GParamGType") (g:param-spec-type pspec)))
    (is (string= "GParamGType" (g:param-spec-type-name pspec)))
    (is (eq (g:gtype "GType") (g:param-spec-value-type pspec)))
    ;; Check infos about the parameter
    (is (string= "myGType" (g:param-spec-name pspec)))
    (is (string= "myGType" (g:param-spec-nick pspec)))
    (is (string= "Documentation" (g:param-spec-blurb pspec)))
    (is-false (g:param-spec-unref pspec))))

;;;     g_value_get_gtype
;;;     g_value_set_gtype

(test g-value-gtype
  (g:with-value (gvalue "GType")
    (is (eq (g:gtype "GObject") (setf (g:value-gtype gvalue) "GObject")))
    (is (eq (g:gtype "GObject") (g:value-gtype gvalue)))))

;;;     GParamSpecVariant

;;;     g_param_spec_variant
;;;     g_value_get_variant
;;;     g_value_dup_variant
;;;     g_value_set_variant
;;;     g_value_take_variant

;;; 2025-09-17
