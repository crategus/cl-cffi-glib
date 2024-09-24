(in-package :glib-test)

(def-suite gio-emblemed :in gio-suite)
(in-suite gio-emblemed)

;;; --- Types and Values -------------------------------------------------------

;;;     GEmblemOrigin

(test g-emblem-origin-enum
  ;; Check type
  (is (g:type-is-enum "GEmblemOrigin"))
  ;; Check type initializer
  (is (eq (g:gtype "GEmblemOrigin")
          (g:gtype (cffi:foreign-funcall "g_emblem_origin_get_type" :size))))
  ;; Check registered symbol
  (is (eq 'gio:emblem-origin
          (glib:symbol-for-gtype "GEmblemOrigin")))
  ;; Check names
  (is (equal '("G_EMBLEM_ORIGIN_UNKNOWN" "G_EMBLEM_ORIGIN_DEVICE"
               "G_EMBLEM_ORIGIN_LIVEMETADATA" "G_EMBLEM_ORIGIN_TAG")
             (glib-test:list-enum-item-names "GEmblemOrigin")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (glib-test:list-enum-item-values "GEmblemOrigin")))
  ;; Check nick names
  (is (equal '("unknown" "device" "livemetadata" "tag")
             (glib-test:list-enum-item-nicks "GEmblemOrigin")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GEmblemOrigin" GIO:EMBLEM-ORIGIN
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "g_emblem_origin_get_type")
                                    (:UNKNOWN 0)
                                    (:DEVICE 1)
                                    (:LIVEMETADATA 2)
                                    (:TAG 3))
             (gobject:get-gtype-definition "GEmblemOrigin"))))

;;;     GEmblem

(test g-emblem-class
  ;; Check type
  (is (g:type-is-object "GEmblem"))
  ;; Check registered symbol
  (is (eq 'gio:emblem
          (glib:symbol-for-gtype "GEmblem")))
  ;; Check type initializer
  (is (eq (g:gtype "GEmblem")
          (g:gtype (cffi:foreign-funcall "g_emblem_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GEmblem")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GEmblem")))
  ;; Check interfaces
  (is (equal '("GIcon")
             (glib-test:list-interfaces "GEmblem")))
  ;; Check class properties
  (is (equal '("icon" "origin")
             (glib-test:list-properties "GEmblem")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GEmblem")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GEmblem" GIO:EMBLEM
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES ("GIcon")
                        :TYPE-INITIALIZER "g_emblem_get_type")
                       ((ICON EMBLEM-ICON "icon" "GObject" T NIL)
                        (ORIGIN EMBLEM-ORIGIN "origin" "GEmblemOrigin" T NIL)))
             (gobject:get-gtype-definition "GEmblem"))))

;;; --- Functions --------------------------------------------------------------

;;;     g_emblem_new
;;;     g_emblem_new_with_origin
;;;     g_emblem_get_icon
;;;     g_emblem_get_origin

;;; 2024-9-17
