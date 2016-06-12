;;;; dash-capi.lisp

(in-package #:dash-capi)

;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(defconstant +docset-name+ "CAPI.docset")
(defconstant +docset-documents-path+ "Contents/Resources/Documents/")
(defconstant +capi-index-files+ '(("capi-m-172.htm" capi) ; CAPI
                                  ("capi-m-688.htm" graphics-ports) ; graphics-ports
                                  ("capi-m-862.htm" lw-gt) ; graphic-tools
                                  ("capi-m-882.htm" color))) ; color
(defconstant +docset-plist-info-filename+ "Contents/Info.plist")
(defconstant +docset-plist-info-contents+
             "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">
<plist version=\"1.0\">
<dict>
  <key>CFBundleIdentifier</key>
  <string>capi</string>
  <key>CFBundleName</key>
  <string>LispWorks CAPI</string>
  <key>DocSetPlatformFamily</key>
  <string>capi</string>
  <key>isDashDocset</key>
  <true/>
  <key>DashDocSetFamily</key>
  <string>dashtoc</string>
  <key>dashIndexFilePath</key>
  <string>capi-m-911.htm</string>
  <key>isJavaScriptEnabled</key><false/>
</dict>
</plist>")
(defconstant +docset-db-filename+ "Contents/Resources/docSet.dsidx")
(defconstant +capi-icon-filename+ "lispworks.gif")
(defconstant +docset-icon-filename+ "icon.png")
(defconstant +docset-icon-filename2x+ "icon@2x.png")


;;----------------------------------------------------------------------------
;; Docset entry
;;----------------------------------------------------------------------------
(defclass docset-entry ()
  ((name :reader ds-name :initarg :name)
   (type :reader ds-type :initarg :type)
   (href :reader ds-href :initarg :href)
   (package :reader ds-package :initarg :package)))

(defmethod initialize-instance :after ((self docset-entry) &key &allow-other-keys)
  "Constructor. Update the type of the entry"
  (with-slots (type package name) self
    ;; either package or symbol inside package
    (if (find-package name)
        (setf type "Package") ; package
        (when-let (symb (find-symbol (string-upcase name) package))
          (setf type 
                (cond ((and (fboundp symb) 
                            (not (macro-function symb))
                            (not (special-operator-p symb))) ; function
                       "Function")
                      ((and (fboundp symb)
                            (macro-function symb)
                            (not (special-operator-p symb))) ; macro
                       "Macro")
                      ((and (fboundp symb) (special-operator-p symb))
                       "Builtin")
                      ((find-class symb nil)
                       "Class")
                      ((boundp symb)
                       "Variable")
                      ((ignore-errors (subtypep symb 't)) "Type")
                      (t "Builtin")))))))


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
  "Callback called when Generate button is pressed"
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
              (t (generate-docset source-path dest-path :log-stream (collector-pane-stream log-pane))))))))

(defun generate-docset (source dest &key log-stream)
  "Generate Dash CAPI docset in from the HTML documentation in SOURCE directory
to the destination DEST directory"
  (unless log-stream
    (setf log-stream *standard-output*))
  (format log-stream "Source path: ~a~%" source)  
  (format log-stream "Destination path: ~a~%" dest)
  (handler-case
      ;; index is a concatenated list of packages and parsed html contents
      (let* ((index (append (parse-html source)
                             ;; insert predefined packages
                             (mapcar (lambda (entry)
                                       (make-instance
                                        'docset-entry
                                        :name (symbol-name (cadr entry))
                                        :href (car entry)))
                                     +capi-index-files+)))
             (docset-path (concatenate 'string
                                       (namestring (truename dest))
                                       +docset-name+))
             (docs-path (concatenate 'string
                                     docset-path
                                     "/"
                                     +docset-documents-path+)))
        (format log-stream "Creating ~a~%" docs-path)
        ;; just do like mkdir -p <docset name>.docset/Contents/Resources/Documents/
        (ensure-directories-exist docs-path)
        ;; and copy all files
        (mapcar (lambda (fname)
                  (let* ((short-name (car (last (split-sequence "/" (namestring fname)))))
                         (dest-name
                          (concatenate 'string
                                       (namestring (truename docs-path))
                                       short-name)))
                    (unless (file-directory-p fname) ;; skip directories
                      (format log-stream "Copying ~a~%" short-name fname)
                      (copy-file fname dest-name))))
                (directory (namestring (truename source))))
        ;; create plist info
        (format log-stream "Creating plist.info~%")
        (create-plist-info dest)
        ;; create an icon
        (format log-stream "Create an icon file~%")
        (create-icon source dest)
        ;; populate database
        (format log-stream "Creating index database~%")
        (create-database dest index)
        (format log-stream "~%The docset ~a has been created~%DONE~%" docset-path))
    (error (err)
      (format log-stream "~a~%" err))))

(defun parse-html (source-path)
  "By given location of the LispWorks HTML documentation SOURCE-PATH,
parses the index files and returns the list of docset-entry"
  ;; concatenate several lists
  (apply (alexandria:curry #'concatenate 'list)
         ;; for every index file ...
         (loop for (index-file package) in +capi-index-files+
               collect
               ;; ... collect the results of this operation into one list (of lists)
               ;; these lists will be contatenated by 'concatenate' call above
               (let* ((index-html ; file with index
                       (truename (concatenate 'string
                                              (namestring (truename source-path))
                                              index-file)))
                      (parsed-html (com.informatimago.common-lisp.html-parser.parse-html:parse-html-file index-html))) ; parsed s-expr from html
                 ;; do the parsing
                 (flet ((find-tag (where tag)
                          (find-if (lambda (x) (and (listp x) (eql (car x) tag))) where)))
                   (when-let* ((html (find-tag parsed-html :html))
                               (body (find-tag html :body))
                               (h4-tags (remove-if-not (lambda (x)
                                                         (and (listp x) (eql (car x) :h4))) body)))
                     (mapcar (lambda (h4)
                               (let* ((h4-flat (alexandria:flatten h4))
                                      (name (string-trim '(#\Space #\Tab) (car (last h4-flat))))
                                      (type "Function")
                                      (href (cadr (member :href h4-flat))))
                                 (make-instance 'docset-entry
                                                :name name
                                                :type type
                                                :href href
                                                :package package))) h4-tags)))))))


(defun create-plist-info (dest)
  "Create the proper Info.plist file in the destination DEST"
  (let ((docset-plist-fname (concatenate 'string
                                         (namestring (truename dest))
                                         +docset-name+
                                         "/"
                                         +docset-plist-info-filename+)))
    (with-open-file (f docset-plist-fname :if-exists :supersede :direction :output)
      (format f +docset-plist-info-contents+))))
                    

(defun create-database (dest index)
  "Create the sqlite database with given index"
  (let* ((docset-db-fname (concatenate 'string
                                       (namestring (truename dest))
                                       +docset-name+
                                       "/"
                                       +docset-db-filename+))
         (db (sqlite:connect docset-db-fname)))
    ;; drop previous table
    (sqlite:execute-non-query db "DROP TABLE IF EXISTS searchIndex")
    ;; create index table
    (sqlite:execute-non-query db "CREATE TABLE searchIndex(id INTEGER PRIMARY KEY, name TEXT, type TEXT, path TEXT)")
    (sqlite:execute-non-query db "CREATE UNIQUE INDEX anchor ON searchIndex (name, type, path)")
    ;; insert symbols
    (mapcar (lambda (entry)
              (sqlite:execute-non-query
               db
               "INSERT OR IGNORE INTO searchIndex(name, type, path) VALUES (?, ?, ?)"
               (ds-name entry)
               (ds-type entry)
               (ds-href entry)))
            index)
    (sqlite:disconnect db)))


(defun create-icon (source-path dest)
  "Convert the lispworks icon to the 32x32 png file and place it to
the appropriate place"
  (let ((source-image 
         (concatenate 'string
                      (namestring (truename source-path))
                      +capi-icon-filename+))
        (dest-image
         (concatenate 'string
                      (namestring (truename dest))
                      +docset-name+
                      "/"
                      +docset-icon-filename+))
        (dest-image2x
         (concatenate 'string
                      (namestring (truename dest))
                      +docset-name+
                      "/"
                      +docset-icon-filename2x+)))
    (opticl:write-png-file dest-image
                           (opticl:resize-image
                            (opticl:read-gif-file source-image) 16 16))
    (opticl:write-png-file dest-image2x
                           (opticl:resize-image
                            (opticl:read-gif-file source-image) 32 32))))


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





