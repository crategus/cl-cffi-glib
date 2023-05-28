(in-package :glib-test)

(def-suite glib-gtype :in glib-suite)
(in-suite glib-gtype)

(test list-name-to-gtypes
  (is (equal '("GAction" "GActionGroup" "GActionMap" "GAppInfo"
               "GAppInfoCreateFlags" "GAppLaunchContext" "GApplication"
               "GApplicationCommandLine" "GApplicationFlags" "GAsyncResult"
               "GBinding" "GBindingFlags" "GBoxed" "GBytes" "GCancellable"
               "GChecksum" "GClosure" "GEmblem" "GEmblemOrigin" "GEmblemedIcon"
               "GEnum" "GFile" "GFileIcon" "GFilesystemPreviewType" "GFlags"
               "GIcon" "GInitiallyUnowned" "GInterface" "GListModel"
               "GListStore" "GLoadableIcon" "GMenu" "GMenuItem" "GMenuModel"
               "GNotification" "GObject" "GParam" "GParamBoolean" "GParamBoxed"
               "GParamChar" "GParamDouble" "GParamEnum" "GParamFlags"
               "GParamFloat" "GParamInt" "GParamInt64" "GParamLong"
               "GParamParam" "GParamString" "GParamUChar" "GParamUInt"
               "GParamUInt64" "GParamULong" "GPermission" "GPropertyAction"
               "GResource" "GResourceFlags" "GResourceLookupFlags"
               "GSimpleAction" "GSimpleActionGroup" "GSimplePermission" "GStrv"
               "GTask" "GThemedIcon" "GType" "GValue" "GVariant" "GVariantType"
               "gboolean" "gchar" "gchararray" "gdouble" "gfloat" "gint"
               "gint64" "glong" "gpointer" "guchar" "guint" "guint64" "gulong"
               "void")
             (sort (iter (for (name gtype) in-hashtable glib::*name-to-gtype*)
                         (collect name)) #'string<))))

(test list-id-to-gtypes
  (is (every #'integerp
             (iter (for (id gtype) in-hashtable glib::*id-to-gtype*)
                   (collect id)))))

(test gtype-from-name.1
  (let ((gtype (glib::gtype-from-name "void")))
    (is (= 4 (glib::gtype-id gtype)))
    (is (string= "void" (glib::gtype-name gtype)))))

(test gtype-from-name.2
  (let ((gtype (glib::gtype-from-name "unknown")))
    (is (= 0 (glib::gtype-id gtype)))
    (is (string= "unknown" (glib::gtype-name gtype)))
    ;; We remove the entries "unknown" and nil from the hash tables
    ;; TODO: Should we save the values for unknown types?!
    (remhash "unknown" glib::*name-to-gtype*)
    (remhash nil glib::*id-to-gtype*)))

(test gtype-from-name.3
  (let ((gtypes (iter (for (name gtype) in-hashtable glib::*name-to-gtype*)
                      (collect name))))
    (is (every #'stringp gtypes))
    (is (every (lambda (x) (typep x 'glib:gtype))
               (mapcar #'glib::gtype-from-name gtypes)))))

(test gtype-from-id.1
  (let ((gtype (glib::gtype-from-id 4)))
    (is (= 4 (glib::gtype-id gtype)))
    (is (string= "void" (glib::gtype-name gtype)))))

(test gtype-from-id.2
  (let ((gtype (glib::gtype-from-id 0)))
    (is (= 0 (glib::gtype-id gtype)))
    (is-false gtype)))

(test gtype-from-id.3
  (let ((gtypes (iter (for (id gtype) in-hashtable glib::*id-to-gtype*)
                      (collect id))))
    (is (every #'integerp gtypes))
    (is (every (lambda (x) (typep x 'glib:gtype))
               (mapcar #'glib::gtype-from-id gtypes)))))

(test symbol-for-gtype
  (is (eq 'glib:error (glib:symbol-for-gtype "GError")))
  (is (eq 'glib:bytes (glib:symbol-for-gtype "GBytes")))
  (is-false (glib:symbol-for-gtype "unknown")))

(test list-symbol-for-gtypes
  (let ((names (iter (for (name sym) in-hashtable glib::*symbol-for-gtypes*)
                     (collect name))))
    (is (every #'symbolp
               (mapcar #'glib:symbol-for-gtype names)))))

;;; --- 2023-5-27 --------------------------------------------------------------
