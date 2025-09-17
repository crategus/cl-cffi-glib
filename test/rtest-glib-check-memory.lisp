(in-package :glib-test)

(def-suite glib-check-memory :in glib-suite)
(in-suite glib-check-memory)

(test check-memory.1
  (glib-test:with-check-memory (object)
    (setf object (g:simple-action-new "action" nil))))

(test check-memory.2
  (glib-test:with-check-memory (object :strong 0)
    (setf object (g:simple-action-new "action" nil))))

(test check-memory.3
  (glib-test:with-check-memory ((object 1) :strong 0)
    (setf object (g:simple-action-new "action" nil))))

(test check-memory.4
  (glib-test:with-check-memory (object1 object2)
    (setf object1 (g:simple-action-new "action1" nil))
    (setf object2 (g:simple-action-new "action2" nil))))

(test check-memory.5
  (glib-test:with-check-memory (object1 object2 :strong 0)
    (setf object1 (g:simple-action-new "action1" nil))
    (setf object2 (g:simple-action-new "action2" nil))))

(test check-memory.6
  (glib-test:with-check-memory ((object1 1) object2 :strong 0)
    (setf object1 (g:simple-action-new "action1" nil))
    (setf object2 (g:simple-action-new "action2" nil))))

;;; 2025-09-17
