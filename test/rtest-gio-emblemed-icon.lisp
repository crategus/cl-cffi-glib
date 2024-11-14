(in-package :glib-test)

(def-suite gio-emblemed-icon :in gio-suite)
(in-suite gio-emblemed-icon)

;;; --- Types and Values -------------------------------------------------------

;;;     GEmblemedIcon

(test g-emblemed-icon-class
  ;; Check type
  (is (g:type-is-object "GEmblemedIcon"))
  ;; Check the registered symbol
  (is (eq 'gio:emblemed-icon
          (glib:symbol-for-gtype "GEmblemedIcon")))
  ;; Check type initializer
  (is (eq (g:gtype "GEmblemedIcon")
          (g:gtype (cffi:foreign-funcall "g_emblemed_icon_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GEmblemedIcon")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GEmblemedIcon")))
  ;; Check interfaces
  (is (equal '("GIcon")
             (glib-test:list-interfaces "GEmblemedIcon")))
  ;; Check class properties
  (is (equal '("gicon")
             (glib-test:list-properties "GEmblemedIcon")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GEmblemedIcon")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GEmblemedIcon" GIO:EMBLEMED-ICON
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES ("GIcon")
                        :TYPE-INITIALIZER "g_emblemed_icon_get_type")
                       ((GICON EMBLEMED-ICON-GICON "gicon" "GIcon" T NIL)))
             (gobject:get-gtype-definition "GEmblemedIcon"))))

;;; --- Properties -------------------------------------------------------------

;;;     gicon

(test g-emblemed-icon-properties
  (let ((icon (make-instance 'g:emblemed-icon)))
    (is-false (g:emblemed-icon-gicon icon))))

;;; --- Functions --------------------------------------------------------------

;;;     g_emblemed_icon_new

(test g-emblemed-icon-new
  (let* ((icon1 (g:themed-icon-new "emblem-default"))
         (icon2 (g:themed-icon-new "battery"))
         (emblem (g:emblem-new icon1))
         (emblemed (g:emblemed-icon-new icon2 emblem)))
    (is (typep emblemed 'g:emblemed-icon))

    (is-false (g:emblemed-icon-clear-emblems emblemed))

    (is (= 2 (g:object-ref-count icon1)))
    (is (= 2 (g:object-ref-count icon2)))
    (is (= 1 (g:object-ref-count emblem)))
    (is (= 1 (g:object-ref-count emblemed)))))

;;;     g_emblemed_icon_get_icon

(test g-emblemed-icon-icon
  (let* ((icon1 (g:themed-icon-new "emblem-default"))
         (icon2 (g:themed-icon-new "battery"))
         (emblem (g:emblem-new icon1))
         (emblemed (g:emblemed-icon-new icon2 emblem)))
    (is (typep emblemed 'g:emblemed-icon))

    (is (eq icon2 (g:emblemed-icon-icon emblemed)))
    (is (eq (g:emblemed-icon-gicon emblemed) (g:emblemed-icon-icon emblemed)))

    (is-false (g:emblemed-icon-clear-emblems emblemed))

    (is (= 2 (g:object-ref-count icon1)))
    (is (= 2 (g:object-ref-count icon2)))
    (is (= 1 (g:object-ref-count emblem)))
    (is (= 1 (g:object-ref-count emblemed)))))

;;;     g_emblemed_icon_get_emblems
;;;     g_emblemed_icon_add_emblem
;;;     g_emblemed_icon_clear_emblems

(test g-emblemed-icon-emblems
  (let* ((icon1 (g:themed-icon-new "emblem-default"))
         (icon2 (g:themed-icon-new "emblem-new"))
         (icon3 (g:themed-icon-new "battery"))
         (emblem1 (g:emblem-new icon1))
         (emblem2 (g:emblem-new icon3))
         (emblemed (g:emblemed-icon-new icon2 emblem1)))
    (is (typep emblemed 'g:emblemed-icon))

    (is (member emblem1 (g:emblemed-icon-emblems emblemed) :test #'eq))

    (is-false (g:emblemed-icon-add-emblem emblemed emblem2))

    (is (member emblem1 (g:emblemed-icon-emblems emblemed) :test #'eq))
    (is (member emblem2 (g:emblemed-icon-emblems emblemed) :test #'eq))

    (is (eq (g:emblemed-icon-gicon emblemed) (g:emblemed-icon-icon emblemed)))

    (is-false (g:emblemed-icon-clear-emblems emblemed))
    (is-false (g:emblemed-icon-emblems emblemed))

    (is (= 2 (g:object-ref-count icon1)))
    (is (= 2 (g:object-ref-count icon2)))
    (is (= 2 (g:object-ref-count icon3)))
    (is (= 1 (g:object-ref-count emblem1)))
    (is (= 1 (g:object-ref-count emblem2)))
    (is (= 1 (g:object-ref-count emblemed)))))

;;; 2024-10-23
