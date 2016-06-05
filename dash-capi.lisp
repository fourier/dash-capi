;;;; dash-capi.lisp

(in-package #:dash-capi)

;;; "dash-capi" goes here. Hacks and glory await!

;;----------------------------------------------------------------------------
;; The main window
;;----------------------------------------------------------------------------

(capi:define-interface dash-capi-main-window ()
  ((application-interface :initarg :application-interface))
  (:panes
   (label capi:title-pane)
   (drawing capi:output-pane))
  (:layouts
   (main-layout capi:column-layout
                '(label drawing)))
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





