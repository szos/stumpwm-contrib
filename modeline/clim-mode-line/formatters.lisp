(in-package #:clim-mode-line)

(defparameter *background-color* +grey20+)
(defparameter *foreground-color* +grey80+)
(defparameter *highlight-background-color* +black+)
(defparameter *highlight-foreground-color* +white+)

(defun invoke-with-stumpwm-formatting (pane cont highlight-when)
  (let ((highlight (funcall highlight-when)))
    (surrounding-output-with-border (pane :ink (if highlight
                                                   *highlight-background-color*
                                                   *background-color*)
                                          :filled t
                                          :move-cursor nil)
      (with-drawing-options (pane :ink (if highlight
                                           *highlight-foreground-color*
                                           *foreground-color*))
        (funcall cont)))))

(defmacro with-stumpwm-formatting ((pane &key highlight) &body body)
  (alexandria:with-gensyms (cont high)
    `(flet ((,cont () ,@body)
            (,high () ,highlight))
       (declare (dynamic-extent (function ,cont) (function ,high)))
       (invoke-with-stumpwm-formatting ,pane #',cont #',high))))

(defmacro define-formatter ((name &key (auto-call-continuation t))
                            (&optional frame pane other-formatters)
                            &body body)
  (let ((fvar (or frame (gensym "FRAME")))
        (pvar (or pane (gensym "PANE")))
        (ovar (or other-formatters (gensym "REST"))))
    `(defun ,name (,fvar ,pvar ,ovar)
       (declare (ignorable ,fvar ,pvar ,ovar))
       ,@body
       ,@(when auto-call-continuation
           `((call-next-formatter ,ovar ,fvar ,pvar))))))

(defmacro define-simple-formatter (name string)
  `(define-formatter (,(intern (format nil "FORMAT-~A" name))) (pane)
     (with-stumpwm-formatting (pane)
       (format pane "~A" ,string))))

(defun call-next-formatter (formatter-list frame pane
                            &optional (display-delimiter t))
  (when formatter-list
    (when display-delimiter
      (format pane "~A" *text-display-formatter-intermix*))
    (funcall (car formatter-list) frame pane (cdr formatter-list))))

(define-simple-formatter space " ")
(define-simple-formatter bar "|")
(define-simple-formatter left-bracket "[")
(define-simple-formatter right-bracket "]")
(define-simple-formatter backslash "\\")
(define-simple-formatter slash "/")

(defun format-align-right (frame pane other-formatters)
  (let ((record
          (with-output-recording-options (pane :draw nil :record t)
            (with-new-output-record (pane)
              (call-next-formatter other-formatters frame pane nil))))
        (width (mode-line-head-width frame)))
    (multiple-value-bind (x y) (output-record-position record)
      (multiple-value-bind (w h) (bounding-rectangle-size record)
        (setf (output-record-position record) (values (- width w) y))
        (draw-rectangle* pane x y (- width w) h :ink *background-color*)
        (tree-recompute-extent record)
        (replay record pane)))))

(defun make-color-formatter (foreground background highlight-fg highlight-bg)
  (lambda (frame pane others)
    (let ((*foreground-color* foreground)
          (*background-color* background)
          (*highlight-foreground-color* highlight-fg)
          (*highlight-background-color* highlight-bg))
      (call-next-formatter others frame pane nil))))

(defun format-groups (frame pane other-formatters)
  (declare (ignorable frame))
  (let ((current-group (stumpwm:current-group)))
    (do-list-with-interspersed-element
        (group (stumpwm::sort-groups (stumpwm:current-screen))
          (format pane " "))
      (with-stumpwm-formatting (pane :highlight (eq current-group group))
        (present group 'stumpwm::group :stream pane :single-box t))))
  (call-next-formatter other-formatters frame pane))

(defun format-windows (frame pane other-formatters)
  (let ((current-window (stumpwm:current-window)))
    (do-list-with-interspersed-element
        (win (stumpwm::sort-windows-by-number
              (stumpwm:group-windows (stumpwm:current-group)))
          (format pane " "))
      (with-stumpwm-formatting (pane :highlight (eq current-window win))
        (present win 'stumpwm::window :stream pane :single-box t))))
  (call-next-formatter other-formatters frame pane))

;;;; REIMPLEMENT STANDARD FORMATTERS

(define-formatter (fmt-urgent-window-list) (frame pane)
  (let ((curwin (stumpwm:current-window))
        (windows (stumpwm::screen-urgent-windows (stumpwm::current-screen))))
    (do-list-with-interspersed-element (win windows (format pane " "))
      (with-stumpwm-formatting (pane :highlight (eq win curwin))
        (present win 'stumpwm::window :stream pane :single-box t)))))

(define-formatter (fmt-window-list) (frame pane)
  (let ((windows (stumpwm::sort-windows (stumpwm:current-group))))
    (do-list-with-interspersed-element (win windows (format pane " "))
      (with-stumpwm-formatting (pane)
        (present win 'stumpwm::window :stream pane :single-box t)))))

(define-formatter (fmt-group-list) (frame pane)
  (let ((curgrp (stumpwm:current-group))
        (groups (stumpwm::sort-groups (stumpwm:current-screen))))
    (do-list-with-interspersed-element (group groups (format pane " "))
      (with-stumpwm-formatting (pane :highlight (eq group curgrp))
        (present group 'stumpwm::group :stream pane :single-box t)))))

(define-formatter (fmt-head) (frame pane)
  (with-stumpwm-formatting (pane)
    (format pane "~d" (stumpwm::head-number (stumpwm:current-head)))))

(define-formatter (fmt-group) (frame pane)
  (let ((group (stumpwm:current-group)))
    (with-stumpwm-formatting (pane)
      (with-output-as-presentation (pane group 'stumpwm::group :single-box t)
        (format pane "~A" (stumpwm:group-name group))))))

(define-formatter (fmt-head-window-list) (frame pane)
  (let* ((group (stumpwm:current-group))
         (curwin (stumpwm:group-current-window group))
         (windows
           (stumpwm::sort1
            (stumpwm::head-windows group (stumpwm:group-current-head group))
            #'< :key #'stumpwm::window-number)))
    (do-list-with-interspersed-element (win windows (format pane " "))
      (with-stumpwm-formatting (pane :highlight (eq win curwin))
        (present win 'stumpwm::window :stream pane :single-box t)))))

(define-formatter (fmt-head-window-list-hidden-windows) (frame pane)
  (let* ((group (stumpwm:current-group))
         (head (stumpwm:current-head group))
         (curwin (stumpwm:group-current-window group))
         (windows
           (stumpwm::sort1 (stumpwm::head-windows group head)
                           #'<
                           :key #'stumpwm::window-number)))
    (do-list-with-interspersed-element (win windows (format pane " "))
      (with-stumpwm-formatting (pane :highlight (eq win curwin))
        (present win 'stumpwm::window :stream pane :single-box t)))))

(define-formatter (fmt-modeline-time) (frame pane)
  (with-stumpwm-formatting (pane)
    (format pane "~A" (stumpwm:time-format stumpwm:*time-modeline-string*))))
