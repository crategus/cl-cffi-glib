(in-package :glib-test)

(def-suite gobject-utils :in gobject-suite)
(in-suite gobject-utils)

(test get-enum-items
  (is (equal '("G_EMBLEM_ORIGIN_UNKNOWN" "G_EMBLEM_ORIGIN_DEVICE"
               "G_EMBLEM_ORIGIN_LIVEMETADATA" "G_EMBLEM_ORIGIN_TAG")
             (mapcar #'gobject:enum-item-name
                     (gobject:get-enum-items "GEmblemOrigin"))))
  (is (equal '(0 1 2 3)
             (mapcar #'gobject:enum-item-value
                     (gobject:get-enum-items "GEmblemOrigin"))))
  (is (equal '("unknown" "device" "livemetadata" "tag")
             (mapcar #'gobject:enum-item-nick
                     (gobject:get-enum-items "GEmblemOrigin")))))

(test get-g-enum-definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GEmblemOrigin"
                             G-EMBLEM-ORIGIN
                             (:EXPORT T
                              :TYPE-INITIALIZER "g_emblem_origin_get_type")
                             (:UNKNOWN 0)
                             (:DEVICE 1)
                             (:LIVEMETADATA 2)
                             (:TAG 3))
             (gobject:get-g-enum-definition "GEmblemOrigin"))))

(test get-flags-items
  (is (equal '("G_APPLICATION_FLAGS_NONE" "G_APPLICATION_DEFAULT_FLAGS"
               "G_APPLICATION_IS_SERVICE" "G_APPLICATION_IS_LAUNCHER"
               "G_APPLICATION_HANDLES_OPEN" "G_APPLICATION_HANDLES_COMMAND_LINE"
               "G_APPLICATION_SEND_ENVIRONMENT" "G_APPLICATION_NON_UNIQUE"
               "G_APPLICATION_CAN_OVERRIDE_APP_ID"
               "G_APPLICATION_ALLOW_REPLACEMENT" "G_APPLICATION_REPLACE")
             (mapcar #'gobject:flags-item-name
                     (gobject:get-flags-items "GApplicationFlags"))))
  (is (equal '(0 0 1 2 4 8 16 32 64 128 256)
             (mapcar #'gobject:flags-item-value
                     (gobject:get-flags-items "GApplicationFlags"))))
  (is (equal '("flags-none" "default-flags" "is-service" "is-launcher"
               "handles-open" "handles-command-line" "send-environment"
               "non-unique" "can-override-app-id" "allow-replacement" "replace")
             (mapcar #'gobject:flags-item-nick
                     (gobject:get-flags-items "GApplicationFlags")))))

(test get-g-flags-definition
 (is (equal '(GOBJECT:DEFINE-G-FLAGS "GApplicationFlags"
                             G-APPLICATION-FLAGS
                             (:EXPORT T
                              :TYPE-INITIALIZER "g_application_flags_get_type")
                             (:FLAGS-NONE 0)
                             (:DEFAULT-FLAGS 0)
                             (:IS-SERVICE 1)
                             (:IS-LAUNCHER 2)
                             (:HANDLES-OPEN 4)
                             (:HANDLES-COMMAND-LINE 8)
                             (:SEND-ENVIRONMENT 16)
                             (:NON-UNIQUE 32)
                             (:CAN-OVERRIDE-APP-ID 64)
                             (:ALLOW-REPLACEMENT 128)
                             (:REPLACE 256))
            (gobject:get-g-flags-definition "GApplicationFlags"))))

;;; 2024-6-12
