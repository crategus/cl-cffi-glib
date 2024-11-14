;;;; Example Application Cmdline - 2023-5-1

(in-package :gio-example)

(defun application-cmdline (&rest argv)
  (let ((app (make-instance 'g:application
                            :application-id
                            "com.crategus.application-cmdline"
                            :flags :handles-command-line))
        (argv (or argv (uiop:command-line-arguments))))
    ;; Print info about the application
    (format t "Start application~%")
    (format t "       argv : ~a~%" argv)
    (format t "    prgname : ~a~%" (g:prgname))
    ;; Signal handler "command-line"
    (g:signal-connect app "command-line"
        (lambda (application cmdline)
          (declare (ignore application))
          (let ((args (g:application-command-line-get-arguments cmdline)))
            (format t "Signal handler COMMAND-LINE~%")
            (format t "  arguments : ~a~%" args)
            ;; Return the exit status
            0)))
    ;; Run the application
    (g:application-run app argv)))
