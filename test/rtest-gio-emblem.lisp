(in-package :glib-test)

(def-suite gio-emblemed :in gio-suite)
(in-suite gio-emblemed)

;;; --- Types and Values -------------------------------------------------------

;;;     GEmblemOrigin

(test emblem-origin-enum
  ;; Check the type
  (is (g:type-is-enum "GEmblemOrigin"))
  ;; Check the type initializer
  (is (eq (g:gtype "GEmblemOrigin")
          (g:gtype (cffi:foreign-funcall "g_emblem_origin_get_type" :size))))
  ;; Check the registered symbol
  (is (eq 'gio:emblem-origin
          (glib:symbol-for-gtype "GEmblemOrigin")))
  ;; Check the names
  (is (equal '("G_EMBLEM_ORIGIN_UNKNOWN" "G_EMBLEM_ORIGIN_DEVICE"
               "G_EMBLEM_ORIGIN_LIVEMETADATA" "G_EMBLEM_ORIGIN_TAG")
             (list-enum-item-name "GEmblemOrigin")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "GEmblemOrigin")))
  ;; Check the nick names
  (is (equal '("unknown" "device" "livemetadata" "tag")
             (list-enum-item-nick "GEmblemOrigin")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GEmblemOrigin"
                             G-EMBLEM-ORIGIN
                             (:EXPORT T)
                             (:UNKNOWN 0)
                             (:DEVICE 1)
                             (:LIVEMETADATA 2)
                             (:TAG 3))
             (gobject:get-g-type-definition "GEmblemOrigin"))))

;;;     GEmblem

(test emblem-class
  ;; Type check
  (is (g:type-is-object "GEmblem"))
  ;; Check the registered symbol
  (is (eq 'gio:emblem
          (glib:symbol-for-gtype "GEmblem")))
  ;; Check the type initializer
  (is (eq (g:gtype "GEmblem")
          (g:gtype (cffi:foreign-funcall "g_emblem_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GEmblem")))
  ;; Check the children
  (is (equal '()
             (list-children "GEmblem")))
  ;; Check the interfaces
  (is (equal '("GIcon")
             (list-interfaces "GEmblem")))
  ;; Check the class properties
  (is (equal '("icon" "origin")
             (list-properties "GEmblem")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GEmblem")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GEmblem" G-EMBLEM
                       (:SUPERCLASS G-OBJECT
                        :EXPORT T
                        :INTERFACES ("GIcon"))
                       ((ICON G-EMBLEM-ICON "icon" "GObject" T NIL)
                        (ORIGIN G-EMBLEM-ORIGIN "origin" "GEmblemOrigin"
                         T NIL)))
             (gobject:get-g-type-definition "GEmblem"))))

;;; --- Functions --------------------------------------------------------------

;;;     g_emblem_new
;;;     g_emblem_new_with_origin
;;;     g_emblem_get_icon
;;;     g_emblem_get_origin

;;; --- 2023-5-29 --------------------------------------------------------------
