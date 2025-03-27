(in-package :glib-test)

(def-suite gobject-type-info :in gobject-suite)
(in-suite gobject-type-info)

(g:type-ensure "GChecksum")

;;; ----------------------------------------------------------------------------

(defparameter gobject-type-info
  '(g:gtype
    g:gtype-name
    g:gtype-id
    g:type-fundamental
    g:type-is-abstract
    g:type-is-derived
    g:type-is-fundamental
    g:type-is-value-type
    g:type-is-classed
    g:type-is-interface
    g:type-from-instance
    g:type-from-class
    g:type-from-interface
    g:type-instance-class
    g:type-check-instance-type
    g:type-check-class-type
    g:type-name
    g:type-parent
    g:type-children
    g:type-depth
    g:type-next-base
    g:type-is-a
    g:type-class-ref
    g:type-class-peek
    g:type-class-unref
    g:type-interface-peek
    g:type-default-interface-ref
    g:type-default-interface-unref
    g:type-interfaces
    g:type-interface-prerequisites
    g:type-qdata
    g:type-ensure))

(export 'gobject-type-info)


;;; --- Types and Values -------------------------------------------------------

(test g-type-constants.1
  (is (= (ash  1 2) (glib:gtype-id (g:gtype "void"))))
  (is (= (ash  2 2) (glib:gtype-id (g:gtype "GInterface"))))
  (is (= (ash  3 2) (glib:gtype-id (g:gtype "gchar"))))
  (is (= (ash  4 2) (glib:gtype-id (g:gtype "guchar"))))
  (is (= (ash  5 2) (glib:gtype-id (g:gtype "gboolean"))))
  (is (= (ash  6 2) (glib:gtype-id (g:gtype "gint"))))
  (is (= (ash  7 2) (glib:gtype-id (g:gtype "guint"))))
  (is (= (ash  8 2) (glib:gtype-id (g:gtype "glong"))))
  (is (= (ash  9 2) (glib:gtype-id (g:gtype "gulong"))))
  (is (= (ash 10 2) (glib:gtype-id (g:gtype "gint64"))))
  (is (= (ash 11 2) (glib:gtype-id (g:gtype "guint64"))))
  (is (= (ash 12 2) (glib:gtype-id (g:gtype "GEnum"))))
  (is (= (ash 13 2) (glib:gtype-id (g:gtype "GFlags"))))
  (is (= (ash 14 2) (glib:gtype-id (g:gtype "gfloat"))))
  (is (= (ash 15 2) (glib:gtype-id (g:gtype "gdouble"))))
  (is (= (ash 16 2) (glib:gtype-id (g:gtype "gchararray"))))
  (is (= (ash 17 2) (glib:gtype-id (g:gtype "gpointer"))))
  (is (= (ash 18 2) (glib:gtype-id (g:gtype "GBoxed"))))
  (is (= (ash 19 2) (glib:gtype-id (g:gtype "GParam"))))
  (is (= (ash 20 2) (glib:gtype-id (g:gtype "GObject"))))
  (is (= (ash 21 2) (glib:gtype-id (g:gtype "GVariant")))))

(test g-type-constants.2
  (let ((types '("void" "GInterface")))
    (is (equal types
               (mapcar #'g:gtype-name
                       (mapcar #'g:gtype types)))))
  (is (string= "void" (glib:gtype-name (funcall #'g:gtype "void"))))
  (is (string= "GInterface" (glib:gtype-name (g:gtype "GInterface"))))
  (is (string= "gchar" (glib:gtype-name (g:gtype "gchar"))))
  (is (string= "guchar" (glib:gtype-name (g:gtype "guchar"))))
  (is (string= "gboolean" (glib:gtype-name (g:gtype "gboolean"))))
  (is (string= "gint" (glib:gtype-name (g:gtype "gint"))))
  (is (string= "guint" (glib:gtype-name (g:gtype "guint"))))
  (is (string= "glong" (glib:gtype-name (g:gtype "glong"))))
  (is (string= "gulong" (glib:gtype-name (g:gtype "gulong"))))
  (is (string= "gint64" (glib:gtype-name (g:gtype "gint64"))))
  (is (string= "guint64" (glib:gtype-name (g:gtype "guint64"))))
  (is (string= "GEnum" (glib:gtype-name (g:gtype "GEnum"))))
  (is (string= "GFlags" (glib:gtype-name (g:gtype "GFlags"))))
  (is (string= "gfloat" (glib:gtype-name (g:gtype "gfloat"))))
  (is (string= "gdouble" (glib:gtype-name (g:gtype "gdouble"))))
  (is (string= "gchararray" (glib:gtype-name (g:gtype "gchararray"))))
  (is (string= "gpointer" (glib:gtype-name (g:gtype "gpointer"))))
  (is (string= "GBoxed" (glib:gtype-name (g:gtype "GBoxed"))))
  (is (string= "GParam" (glib:gtype-name (g:gtype "GParam"))))
  (is (string= "GObject" (glib:gtype-name (g:gtype "GObject"))))
  (is (string= "GVariant" (glib:gtype-name (g:gtype "GVariant")))))

(test g-type-fundamentals
  (let* ((glib::*warn-unknown-gtype* nil)
         (nmax (cffi:foreign-funcall "g_type_fundamental_next" :size))
         (gtypes (iter (for x from 1 below nmax)
                       (for gtype = (g:gtype (ash x 2)))
                       (when gtype (collect gtype)))))
  (is (equal '("void"
               "GInterface"
               "gchar"
               "guchar"
               "gboolean"
               "gint"
               "guint"
               "glong"
               "gulong"
               "gint64"
               "guint64"
               "GEnum"
               "GFlags"
               "gfloat"
               "gdouble"
               "gchararray"
               "gpointer"
               "GBoxed"
               "GParam"
               "GObject"
               "GVariant")
             (mapcar #'glib:gtype-name gtypes)))))

;;;     GType

;;;     GTypeInterface

(test g-type-interface-structure
  (let ((interface (g:type-default-interface-ref "GAction")))
    (is (= 16 (cffi:foreign-type-size '(:struct g:type-interface))))
    (is (equal '(:instance-type :type)
               (sort (cffi:foreign-slot-names '(:struct g:type-interface))
                     #'string-lessp)))
    (is (eq (g:gtype "GAction")
            (cffi:foreign-slot-value interface
                                     '(:struct g:type-interface) :type)))
    (is-false (cffi:foreign-slot-value interface
                                       '(:struct g:type-interface)
                                       :instance-type))
    (is-false (g:type-default-interface-unref interface))))

;;;     GTypeInstance

(test g-type-instance-structure
  (let* ((button (make-instance 'g:simple-action))
         (class (cffi:foreign-slot-value (glib:pointer button)
                                         '(:struct g:type-instance) :class)))
    (is (= 8 (cffi:foreign-type-size '(:struct g:type-instance))))
    (is (equal '(:class) (cffi:foreign-slot-names '(:struct g:type-instance))))
    (is (eq (g:gtype "GSimpleAction") (g:type-from-class class)))))

;;;     GTypeClass

(test g-type-class-structure
  (let ((gclass (g:type-class-ref "GSimpleAction")))
    (is (= 8 (cffi:foreign-type-size '(:struct g:type-class))))
    (is (equal '(:type) (cffi:foreign-slot-names '(:struct g:type-class))))
    (is (eq (g:gtype "GSimpleAction")
            (cffi:foreign-slot-value gclass '(:struct g:type-class) :type)))
    (is-false (g:type-class-unref gclass))))

;;;     GTypeInfo
;;;     GTypeFundamentalInfo
;;;     GInterfaceInfo
;;;     GTypeValueTable
;;;     GTypeDebugFlags                                    not implemented
;;;     GTypeQuery
;;;     GTypeFlags
;;;     GTypeFundamentalFlags

;;; --- Functions --------------------------------------------------------------

;;;   g_type_fundamental

(test g-type-fundamental
  (is (eq (g:gtype "GObject") (g:type-fundamental "GApplication")))
  (is (eq (g:gtype "GObject") (g:type-fundamental "GSimpleAction")))
  (is (eq (g:gtype "GInterface") (g:type-fundamental "GAction")))
  (is (eq (g:gtype "GFlags") (g:type-fundamental "GApplicationFlags")))
  (is (eq (g:gtype "GEnum") (g:type-fundamental "GEmblemOrigin")))
  (is (eq (g:gtype "GBoxed") (g:type-fundamental "GVariantType"))))

;;;   g_type_make_fundamental                              not exported

;;;   g_type_is_abstract

(test g-type-is-abstract
  (is-false (g:type-is-abstract (g:gtype "void")))
  (is-false (g:type-is-abstract (g:gtype "GInterface")))
  (is-false (g:type-is-abstract (g:gtype "gchar")))
  (is-false (g:type-is-abstract (g:gtype "guchar")))
  (is-false (g:type-is-abstract (g:gtype "gboolean")))
  (is-false (g:type-is-abstract (g:gtype "gint")))
  (is-false (g:type-is-abstract (g:gtype "guint")))
  (is-false (g:type-is-abstract (g:gtype "glong")))
  (is-false (g:type-is-abstract (g:gtype "gulong")))
  (is-false (g:type-is-abstract (g:gtype "gint64")))
  (is-false (g:type-is-abstract (g:gtype "guint64")))
  (is-true  (g:type-is-abstract (g:gtype "GEnum")))
  (is-true  (g:type-is-abstract (g:gtype "GFlags")))
  (is-false (g:type-is-abstract (g:gtype "gfloat")))
  (is-false (g:type-is-abstract (g:gtype "gdouble")))
  (is-false (g:type-is-abstract (g:gtype "gchararray")))
  (is-false (g:type-is-abstract (g:gtype "gpointer")))
  (is-true  (g:type-is-abstract (g:gtype "GBoxed")))
  (is-true  (g:type-is-abstract (g:gtype "GParam")))
  (is-false (g:type-is-abstract (g:gtype "GObject")))
  (is-false (g:type-is-abstract (g:gtype "GType")))
  (is-false (g:type-is-abstract (g:gtype "GVariant")))
  (is-false (g:type-is-abstract (g:gtype "GChecksum")))

  (is-false (g:type-is-abstract "GApplication"))
  (is-false (g:type-is-abstract "GAction"))
  (is-false (g:type-is-abstract "GSimpleAction"))
  (is-false (g:type-is-abstract "GApplicationFlags"))
  (is-false (g:type-is-abstract "GEmblemOrigin"))
  (is-false (g:type-is-abstract "GVariantType")))

;;;   g_type_is_dervied

(test g-type-is-derived
  (is-false (g:type-is-derived "void"))
  (is-false (g:type-is-derived "GInterface"))
  (is-false (g:type-is-derived "gchar"))
  (is-false (g:type-is-derived "guchar"))
  (is-false (g:type-is-derived "gboolean"))
  (is-false (g:type-is-derived "gint"))
  (is-false (g:type-is-derived "guint"))
  (is-false (g:type-is-derived "glong"))
  (is-false (g:type-is-derived "gulong"))
  (is-false (g:type-is-derived "gint64"))
  (is-false (g:type-is-derived "guint"))
  (is-false (g:type-is-derived "GEnum"))
  (is-false (g:type-is-derived "GFlags"))
  (is-false (g:type-is-derived "gfloat"))
  (is-false (g:type-is-derived "gdouble"))
  (is-false (g:type-is-derived "gchararray"))
  (is-false (g:type-is-derived "gpointer"))
  (is-false (g:type-is-derived "GBoxed"))
  (is-false (g:type-is-derived "GParam"))
  (is-false (g:type-is-derived "GObject"))
  (is-true  (g:type-is-derived "GType"))
  (is-false (g:type-is-derived "GVariant"))
  (is-true  (g:type-is-derived "GChecksum"))
  (is-true  (g:type-is-derived "GApplication"))
  (is-true  (g:type-is-derived "GAction"))
  (is-true  (g:type-is-derived "GSimpleAction"))
  (is-true  (g:type-is-derived "GApplicationFlags"))
  (is-true  (g:type-is-derived "GEmblemOrigin"))
  (is-true  (g:type-is-derived "GVariantType")))

;;;   g_type_is_fundamental

(test g-type-is-fundamental
  (is-true  (g:type-is-fundamental (g:gtype "void")))
  (is-true  (g:type-is-fundamental (g:gtype "GInterface")))
  (is-true  (g:type-is-fundamental (g:gtype "gchar")))
  (is-true  (g:type-is-fundamental (g:gtype "guchar")))
  (is-true  (g:type-is-fundamental (g:gtype "gboolean")))
  (is-true  (g:type-is-fundamental (g:gtype "gint")))
  (is-true  (g:type-is-fundamental (g:gtype "guint")))
  (is-true  (g:type-is-fundamental (g:gtype "glong")))
  (is-true  (g:type-is-fundamental (g:gtype "gulong")))
  (is-true  (g:type-is-fundamental (g:gtype "gint64")))
  (is-true  (g:type-is-fundamental (g:gtype "guint64")))
  (is-true  (g:type-is-fundamental (g:gtype "GEnum")))
  (is-true  (g:type-is-fundamental (g:gtype "GFlags")))
  (is-true  (g:type-is-fundamental (g:gtype "gfloat")))
  (is-true  (g:type-is-fundamental (g:gtype "gdouble")))
  (is-true  (g:type-is-fundamental (g:gtype "gchararray")))
  (is-true  (g:type-is-fundamental (g:gtype "gpointer")))
  (is-true  (g:type-is-fundamental (g:gtype "GBoxed")))
  (is-true  (g:type-is-fundamental (g:gtype "GParam")))
  (is-true  (g:type-is-fundamental (g:gtype "GObject")))
  (is-false (g:type-is-fundamental (g:gtype "GType")))
  (is-true  (g:type-is-fundamental (g:gtype "GVariant")))
  (is-false (g:type-is-fundamental (g:gtype "GChecksum")))
  (is-false (g:type-is-fundamental "GApplication"))
  (is-false (g:type-is-fundamental "GAction"))
  (is-false (g:type-is-fundamental "GSimpleAction"))
  (is-false (g:type-is-fundamental "GApplicationFlags"))
  (is-false (g:type-is-fundamental "GEmblemOrigin"))
  (is-false (g:type-is-fundamental "GVariantType")))

;;;   g_type_is_value_type

(test g-type-is-value-type
  (is-false (g:type-is-value-type "void"))
  (is-false (g:type-is-value-type "GInterface"))
  (is-true  (g:type-is-value-type "gchar"))
  (is-true  (g:type-is-value-type "guchar"))
  (is-true  (g:type-is-value-type "gboolean"))
  (is-true  (g:type-is-value-type "gint"))
  (is-true  (g:type-is-value-type "guint"))
  (is-true  (g:type-is-value-type "glong"))
  (is-true  (g:type-is-value-type "gulong"))
  (is-true  (g:type-is-value-type "gint64"))
  (is-true  (g:type-is-value-type "guint64"))
  (is-false (g:type-is-value-type "GEnum"))
  (is-false (g:type-is-value-type "GFlags"))
  (is-true  (g:type-is-value-type "gfloat"))
  (is-true  (g:type-is-value-type "gdouble"))
  (is-true  (g:type-is-value-type "gchararray"))
  (is-true  (g:type-is-value-type "gpointer"))
  (is-false (g:type-is-value-type "GBoxed"))
  (is-true  (g:type-is-value-type "GParam"))
  (is-true  (g:type-is-value-type "GObject"))
  (is-true  (g:type-is-value-type "GType"))
  (is-true  (g:type-is-value-type "GVariant"))
  (is-true  (g:type-is-value-type "GChecksum"))
  (is-true  (g:type-is-value-type "GApplication"))
  (is-true  (g:type-is-value-type "GAction"))
  (is-true  (g:type-is-value-type "GSimpleAction"))
  (is-true  (g:type-is-value-type "GApplicationFlags"))
  (is-true  (g:type-is-value-type "GEmblemOrigin"))
  (is-true  (g:type-is-value-type "GVariantType")))

;;;   g_type_has_value_table                               not exported

;;;   g_type_is_classed

(test g-type-is-classed
  (is-false (g:type-is-classed "void"))
  (is-false (g:type-is-classed "GInterface"))
  (is-false (g:type-is-classed "gchar"))
  (is-false (g:type-is-classed "guchar"))
  (is-false (g:type-is-classed "gboolean"))
  (is-false (g:type-is-classed "gint"))
  (is-false (g:type-is-classed "guint"))
  (is-false (g:type-is-classed "glong"))
  (is-false (g:type-is-classed "gulong"))
  (is-false (g:type-is-classed "gint64"))
  (is-false (g:type-is-classed "guint64"))
  (is-true  (g:type-is-classed "GEnum"))
  (is-true  (g:type-is-classed "GFlags"))
  (is-false (g:type-is-classed "gfloat"))
  (is-false (g:type-is-classed "gdouble"))
  (is-false (g:type-is-classed "gchararray"))
  (is-false (g:type-is-classed "gpointer"))
  (is-false (g:type-is-classed "GBoxed"))
  (is-true  (g:type-is-classed "GParam"))
  (is-true  (g:type-is-classed "GObject"))
  (is-false (g:type-is-classed "GVariant"))
  (is-true  (g:type-is-classed "GApplication"))
  (is-false (g:type-is-classed "GAction"))
  (is-true  (g:type-is-classed "GSimpleAction"))
  (is-true  (g:type-is-classed "GApplicationFlags"))
  (is-true  (g:type-is-classed "GEmblemOrigin"))
  (is-false (g:type-is-classed "GVariantType")))

;;;   g_type_is_instantiatable                             not exported
;;;   g_type_is_derivable                                  not exported
;;;   g_type_is_deep_derivable                             not exported

;;; g_type_is_interface

(test g-type-is-interface
  (is-false (g:type-is-interface "void"))
  (is-true  (g:type-is-interface "GInterface"))
  (is-false (g:type-is-interface "gchar"))
  (is-false (g:type-is-interface "guchar"))
  (is-false (g:type-is-interface "gboolean"))
  (is-false (g:type-is-interface "gint"))
  (is-false (g:type-is-interface "guint"))
  (is-false (g:type-is-interface "glong"))
  (is-false (g:type-is-interface "gulong"))
  (is-false (g:type-is-interface "gint64"))
  (is-false (g:type-is-interface "guint64"))
  (is-false (g:type-is-interface "GEnum"))
  (is-false (g:type-is-interface "GFlags"))
  (is-false (g:type-is-interface "gfloat"))
  (is-false (g:type-is-interface "gdouble"))
  (is-false (g:type-is-interface "gchararray"))
  (is-false (g:type-is-interface "gpointer"))
  (is-false (g:type-is-interface "GBoxed"))
  (is-false (g:type-is-interface "GParam"))
  (is-false (g:type-is-interface "GObject"))
  (is-false (g:type-is-interface "GVariant"))
  (is-false (g:type-is-interface "GApplication"))
  (is-true  (g:type-is-interface "GAction"))
  (is-false (g:type-is-interface "GSimpleAction"))
  (is-false (g:type-is-interface "GApplicationFlags"))
  (is-false (g:type-is-interface "GEmblemOrigin"))
  (is-false (g:type-is-interface "GVariantType")))

;;; G_TYPE_FROM_INSTANCE

(test g-type-from-instance
  (signals (error) (g:type-from-instance nil))
  (is (eq (g:gtype "GSimpleAction")
          (g:type-from-instance (make-instance 'g:simple-action)))))

;;;   G_TYPE_FROM_CLASS

(test g-type-from-class
  (let ((gclass nil))
    (is (eq (g:gtype "GApplication")
            (g:type-from-class (setf gclass
                                     (g:type-class-ref "GApplication")))))
    (is-false (g:type-class-unref gclass))
    (is (eq (g:gtype "GSimpleAction")
            (g:type-from-class (setf gclass
                                     (g:type-class-ref "GSimpleAction")))))
    (is-false (g:type-class-unref gclass))))

;;;   G_TYPE_FROM_INTERFACE

(test g-type-from-interface
  (let ((gclass nil))
    (is (eq (g:gtype "GAction")
            (g:type-from-interface
                (setf gclass (g:type-default-interface-ref "GAction")))))
    (is-false (g:type-default-interface-unref gclass))))

;;;     g-type-instance-class

(test g-type-instance-class
  (is (eq (g:gtype "GSimpleAction")
          (g:type-from-class
              (g:type-instance-class (make-instance 'g:simple-action))))))

;;;     G_TYPE_INSTANCE_GET_INTERFACE                      not implemented
;;;     G_TYPE_INSTANCE_GET_PRIVATE                        not implemented
;;;     G_TYPE_CLASS_GET_PRIVATE                           not implemented
;;;     G_TYPE_CHECK_INSTANCE                              not implemented
;;;     G_TYPE_CHECK_INSTANCE_CAST                         not implemented

;;;     G_TYPE_CHECK_INSTANCE_TYPE

(test g-type-check-instance-type
  (let ((action (make-instance 'g:simple-action)))
    (is-true (g:type-check-instance-type action "GObject"))
    (is-true (g:type-check-instance-type action "GAction"))))

;;;     G_TYPE_CHECK_CLASS_CAST                            not implemented

;;;   G_TYPE_CHECK_CLASS_TYPE

(test g-type-check-class-type
  (let ((gclass nil))
    (is-true  (g:type-check-class-type (setf gclass
                                             (g:type-class-ref "GSimpleAction"))
                                       "GObject"))
    (is-false (g:type-class-unref gclass))))

;;;     G_TYPE_CHECK_VALUE                                 not implemented
;;;     G_TYPE_CHECK_VALUE_TYPE                            not implemented
;;;
;;;     g_type_init
;;;     g_type_init_with_debug_flags                       not implemented

;;;   g_type_name

(test g-type-name
  (is (string= "gdouble" (g:type-name (g:gtype "gdouble"))))
  (is (string= "GBoxed" (g:type-name (g:gtype "GBoxed"))))
  (is (string= "GApplication" (g:type-name (g:gtype "GApplication")))))

;;;     g_type_qname

;;;     g_type_parent

(test g-type-parent
  (is (eq (g:gtype "GObject") (g:type-parent "GSimpleAction"))))

;;;     g_type_depth

(test g-type-depth
  (is (= 2 (g:type-depth (g:gtype "GSimpleAction"))))
  (is (= 2 (g:type-depth (g:gtype "GBytes"))))
  (is (= 1 (g:type-depth (g:gtype "gboolean")))))

;;;     g_type_next_base

(test g-type-next-base
  (is (eq (g:gtype "GThemedIcon") (g:type-next-base "GThemedIcon" "GObject")))
)

;;;     g_type_is_a

(test g-type-is-a
  (is-true (g:type-is-a "gboolean" (g:gtype "gboolean")))
  (is-true (g:type-is-a "GResource" (g:gtype "GBoxed")))
  (is-true (g:type-is-a "gchar" (g:gtype "gchar")))
  (is-true (g:type-is-a "GChecksum" (g:gtype "GChecksum")))
  (is-true (g:type-is-a "gdouble" (g:gtype "gdouble")))
  (is-true (g:type-is-a "GEmblemOrigin" (g:gtype "GEnum")))
  (is-true (g:type-is-a "GApplicationFlags" (g:gtype "GFlags")))
  (is-true (g:type-is-a "gfloat" (g:gtype "gfloat")))
  (is-true (g:type-is-a "GType" (g:gtype "GType")))
  (is-true (g:type-is-a "gint" (g:gtype "gint")))
  (is-true (g:type-is-a "gint64" (g:gtype "gint64")))
  (is-true (g:type-is-a "glong" (g:gtype "glong")))
  (is-true (g:type-is-a "void" (g:gtype "void")))
  (is-true (g:type-is-a "GApplication" (g:gtype "GObject")))
  (is-true (g:type-is-a "GParamBoolean" (g:gtype "GParam")))
  (is-true (g:type-is-a "gpointer" (g:gtype "gpointer")))
  (is-true (g:type-is-a "gchararray" (g:gtype "gchararray")))
  (is-true (g:type-is-a "guchar" (g:gtype "guchar")))
  (is-true (g:type-is-a "guint" (g:gtype "guint")))
  (is-true (g:type-is-a "guint64" (g:gtype "guint64")))
  (is-true (g:type-is-a "gulong" (g:gtype "gulong")))
  (is-true (g:type-is-a "GVariant" (g:gtype "GVariant"))))

;;;     g_type_class_ref
;;;     g_type_class_unref
;;;     g_type_class_peek

(test g-type-class-ref/unref/peek
  (let ((gclass nil))
    ;; GTYPE is a string
    (is (cffi:pointerp (setf gclass (g:type-class-ref "GSimpleAction"))))
    (is (eq (g:gtype "GSimpleAction") (g:type-from-class gclass)))
    (is (cffi:pointer-eq gclass
                         (g:type-class-peek "GSimpleAction")))
    (is-false (g:type-class-unref gclass))

    ;; GTYPE is an integer ID
    (is (cffi:pointerp (setf gclass
                             (g:type-class-ref
                               (glib::gtype-id (g:gtype "GSimpleAction"))))))
    (is (eq (g:gtype "GSimpleAction") (g:type-from-class gclass)))
    (is (cffi:pointer-eq gclass
                         (g:type-class-peek
                           (glib:gtype-id (glib:gtype "GSimpleAction")))))
    (is-false (g:type-class-unref gclass))

    ;; GTYPE is a g:type-t type ID
    (is (cffi:pointerp (setf gclass
                             (g:type-class-ref (g:gtype "GSimpleAction")))))
    (is (eq (g:gtype "GSimpleAction") (g:type-from-class gclass)))
    (is (cffi:pointer-eq gclass
                         (g:type-class-peek (glib:gtype "GSimpleAction"))))
    (is-false (g:type-class-unref gclass))

    ;; GTYPE is not classed
    (is-false (g:type-class-ref "gboolean"))
    (is-false (g:type-class-ref "GAction"))
    ;; GTYPE is not known
    (is-false (g:type-class-ref "unknown"))))

;;;     g_type_class_peek_static
;;;     g_type_class_peek_parent
;;;     g_type_class_add_private
;;;     g_type_add_class_private                           not implemented

;;;     g_type_interface_peek

(test g-type-interface-peek
  (let ((gclass (g:type-class-ref "GThemedIcon"))
        (iface nil))
    (is (cffi:pointerp (setf iface (g:type-interface-peek gclass "GIcon"))))
    (is (eq (g:gtype "GIcon") (g:type-from-interface iface)))
    (is-false (g:type-class-unref gclass))))

;;;     g_type_interface_peek_parent                       not implemented

;;;     g_type_default_interface_ref
;;;     g_type_default_interface_peek
;;;     g_type_default_interface_unref

(test g-type-default-interface-ref/unref/peek
  (let ((iface nil))
    ;; VTABLE for GAction interface
    (is (cffi:pointerp (setf iface (g:type-default-interface-ref "GAction"))))
    (is (eq (g:gtype "GAction") (g:type-from-interface iface)))
    (is (cffi:pointer-eq iface (g:type-default-interface-peek "GAction")))
    (is-false (g:type-default-interface-unref iface))
    ;; GSimpleAction is not an interface
    (is-false (g:type-default-interface-ref "GSimpleAction"))
    (is-false (g:type-default-interface-ref "gdouble"))
    (is-false (g:type-default-interface-ref "unknown"))))

;;;     g_type_children

(test g-type-children
  (is (subsetp '("GAppLaunchContext" "GApplication" "GApplicationCommandLine"
                 "GBinding" "GCancellable" "GEmblem" "GEmblemedIcon" "GFileIcon"
                 "GFileInfo" "GInitiallyUnowned" "GListStore" "GMenuItem"
                 "GMenuModel" "GNotification" "GPermission" "GPropertyAction"
                 "GSimpleAction" "GSimpleActionGroup" "GTask" "GThemedIcon")
                 (sort (mapcar #'g:type-name
                               (g:type-children "GObject")) #'string<)
               :test #'string=)))

;;;     g_type_interfaces

(test g-type-interfaces
  (is (equal '("GActionGroup" "GActionMap")
             (mapcar #'g:type-name
                     (g:type-interfaces "GApplication")))))

;;;     g_type_interface_prerequisites

(test g-type-interface-prerequisites
  (is (equal '("GObject")
             (mapcar #'g:type-name
                     (g:type-interface-prerequisites "GAction"))))
  (is (equal '("GObject")
             (mapcar #'g:type-name
                     (g:type-interface-prerequisites "GIcon")))))

;;;     g-type-qdata

(test g-type-qdata.1
  ;; Attach and read data for a "gboolean" type
  (is (string= "a string" (setf (g:type-qdata "gboolean" "mydata") "a string")))
  (is (string= "a string" (g:type-qdata "gboolean" "mydata")))
  (is (equal '(A B C) (setf (g:type-qdata "gboolean" "mydata") '(a b c))))
  (is (equal '(A B C) (g:type-qdata "gboolean" "mydata")))
  (is-false (setf (g:type-qdata "gboolean" "mydata") nil))
  (is-false (g:type-qdata "gboolean" "mydata")))

(test g-type-qdata.2
  ;; Attach and read data for a "GSimpleAction" type
  (is (string= "a string"
               (setf (g:type-qdata "GSimpleAction" "mydata") "a string")))
  (is (string= "a string" (g:type-qdata "GSimpleAction" "mydata")))
  (is (equal '(A B C) (setf (g:type-qdata "GSimpleAction" "mydata") '(a b c))))
  (is (equal '(A B C) (g:type-qdata "GSimpleAction" "mydata")))
  (is-false (setf (g:type-qdata "GSimpleAction" "mydata") nil))
  (is-false (g:type-qdata "GSimpleAction" "mydata")))

;;;     g_type_query                                       not exported
;;;     g_type_register_static                             not exported
;;;     g_type_register_static_simple                      not exported
;;;     g_type_register_dynamic                            not implemented
;;;     g_type_register_fundamental                        not implemented
;;;     g_type_add_interface_static                        not exported
;;;     g_type_add_interface_dynamic                       not implemented
;;;     g_type_interface_add_prerequisite                  not exported
;;;     g_type_get_plugin                                  not implemented
;;;     g_type_interface_get_plugin                        not implemented
;;;     g_type_fundamental_next                            not exported
;;;     g_type_create_instance                             not implemented
;;;     g_type_free_instance                               not implemented
;;;     g_type_add_class_cache_func                        not implemented
;;;     g_type_remove_class_cache_func                     not implemented
;;;     g_type_class_unref_uncached                        not implemented
;;;     g_type_add_interface_check                         not implemented
;;;     g_type_remove_interface_check                      not implemented
;;;     g_type_value_table_peek                            not exported

;;;     g_type_ensure

(test g-type-ensure
  (is (eq (g:gtype "GAction") (g:type-ensure "GAction")))
  (is (eq (g:gtype "GSimpleAction") (g:type-ensure "GSimpleAction")))
  (is (eq (g:gtype "gboolean") (g:type-ensure "gboolean")))
  (is-true (g:type-ensure "GSimpleAsyncResult"))
  ;; GTYPE is not known
  (is-false (g:type-ensure "unknown")))

;;;     g_type_get_type_registration_serial                not implemented
;;;     g_type_get_instance_count                          not implemented
;;;     G_DEFINE_TYPE                                      not implemented
;;;     G_DEFINE_TYPE_WITH_CODE                            not implemented
;;;     G_DEFINE_ABSTRACT_TYPE                             not implemented
;;;     G_DEFINE_ABSTRACT_TYPE_WITH_CODE                   not implemented
;;;     G_DEFINE_INTERFACE                                 not implemented
;;;     G_DEFINE_INTERFACE_WITH_CODE                       not implemented
;;;     G_IMPLEMENT_INTERFACE                              not implemented
;;;     G_DEFINE_TYPE_EXTENDED                             not implemented
;;;     G_DEFINE_BOXED_TYPE                                not implemented
;;;     G_DEFINE_BOXED_TYPE_WITH_CODE                      not implemented
;;;     G_DEFINE_POINTER_TYPE                              not implemented
;;;     G_DEFINE_POINTER_TYPE_WITH_CODE                    not implemented

;;; 2024-12-23
