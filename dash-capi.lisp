;;;; dash-capi.lisp

(in-package #:dash-capi)

;;; "dash-capi" goes here. Hacks and glory await!

;;----------------------------------------------------------------------------
;; The main window
;;----------------------------------------------------------------------------

(capi:define-interface dash-capi-main-window ()
  ((application-interface :initarg :application-interface))
  (:panes
   (input-directory-edit text-input-pane 
                         :title "Path to CAPI HTML documentation"
                         :buttons 
                         '(:browse-file (:directory t :image :std-file-open) :ok nil))
   (output-directory-edit text-input-pane 
                          :title "Output path"
                         :buttons 
                         '(:browse-file (:directory t :image :std-file-open) :ok nil))
   (log-pane collector-pane :buffer-name "Output buffer")
   (generate-button push-button :text "Generate" :callback 'on-generate-button))
  (:layouts
   (main-layout capi:column-layout
                '(input-directory-edit output-directory-edit log-pane generate-button)
                :adjust :center
                :y-ratios '(nil nil 1 nil)))
  (:default-initargs
   :layout 'main-layout
   :best-width 640
   :best-height 480
   :title "Generator of Dash docset for CAPI"
   :destroy-callback 'dash-capi-quit))

(defun dash-capi-quit (self)
  (when-let (application (slot-value self 'application-interface))
    ;; Set drawing-interface to nil to prevent recursion back from
    ;; application-interface's destroy-callback.
    (setf (main-window application)
          nil)
    ;; Quit by destroying the application interface.
    (capi:destroy application)
    ))


(defun on-generate-button (data self)
  ;; could be called from edit fields or as a button itself
  (declare (ignore data))
  (with-slots (input-directory-edit
               output-directory-edit
               log-pane) self
    (let ((source-path (text-input-pane-text input-directory-edit))
          (dest-path (text-input-pane-text output-directory-edit)))
      ;; verify what paths are not empty
      (when (and (> (length source-path) 0) (> (length dest-path) 0))
        ;; some sanity checks. if directories exists at all
        (cond ((not (lw:file-directory-p source-path))
               (display-message "Source path does not exist" source-path))
              ((not (lw:file-directory-p dest-path))
               (display-message "Destination path does not exist" dest-path))
              (t (generate-docset source-path dest-path (collector-pane-stream log-pane))))))))

(defun generate-docset (source dest log-stream)
  (format log-stream "Source path: ~a~%" source)  
  (format log-stream "Destination path: ~a~%" dest)
  )

;;----------------------------------------------------------------------------
;; The application interface
;;----------------------------------------------------------------------------


(capi:define-interface cocoa-application-interface-dash-capi (capi:cocoa-default-application-interface)
  ((main-window :initform nil
                :accessor main-window))
  (:default-initargs
   :title "Generator of Dash docset for CAPI"
   :destroy-callback 'main-window-destroyed))


(defun main-window-destroyed (application)
  (when-let (wnd (main-window application))
    ;; Set application-interface to nil to prevent recursion back from
    ;; main-window's destroy-callback.
    (setf (slot-value wnd 'application-interface) nil)
    ;; Destroy the single main window.  When run as a delivered
    ;; application, this will cause the application to exit because it
    ;; has no more windows.
    (capi:destroy wnd)))


;;----------------------------------------------------------------------------
;; The application entry point
;;----------------------------------------------------------------------------

(defun main ()
  (let ((application (make-instance 'cocoa-application-interface-dash-capi)))
    ;; Set the application interface before using any other CAPI
    ;; functionality.
    (capi:set-application-interface application)
    ;; Start the application with its single window.
    (let ((main-window (make-instance 'dash-capi-main-window
                                      :application-interface application)))
      (setf (main-window application)
            main-window)
      (capi:display main-window))))





