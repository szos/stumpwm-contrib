(in-package #:clim-mode-line)

;; (defun draw-background (frame pane)
;;   (let (())))

(defun invoke-with-stumpwm-formatting (pane cont highlight-when)
  (let ((highlight (funcall highlight-when)))
    (surrounding-output-with-border (pane :ink (if highlight
                                                   *foreground-color*
                                                   *background-color*)
                                          :filled t
                                          :move-cursor nil)
      (with-drawing-options (pane :ink (if highlight
                                           *background-color*
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
        (rvar (or other-formatters (gensym "OTHER-FORMATTERS"))))
    `(defun ,name (,fvar ,pvar ,rvar)
       ,@body
       ,@(when auto-call-continuation
           `((call-next-formatter ,rvar ,fvar ,pvar))))))

(defmacro define-simple-formatter (name string)
  `(define-formatter (,(intern (format nil "FORMAT-~A" name))) (frame pane)
     (with-stumpwm-formatting (pane)
       (format pane "~A" ,string))))

(defun call-next-formatter (formatter-list frame pane
                            &optional (display-delimiter t))
  (when display-delimiter
    (format pane "~A" *text-display-formatter-intermix*))
  (when formatter-list
    (funcall (car formatter-list) frame pane (cdr formatter-list))))

(define-simple-formatter space " ")
(define-simple-formatter bar "|")
(define-simple-formatter left-bracket "[")
(define-simple-formatter right-bracket "]")
(define-simple-formatter backslash "\\")
(define-simple-formatter slash "/")

(define-formatter (format-align-right :auto-call-continuation nil)
    (frame pane rest)
  (with-right-alignment (frame pane)
    (call-next-formatter rest frame pane nil)))

(define-formatter (format-with-colors :auto-call-continuation nil)
    (frame pane rest)
  (let ((*foreground-color* (mode-line-foreground-color frame))
        (*background-color* (mode-line-background-color frame)))
    (call-next-formatter rest frame pane nil)))

(defun make-color-formatter (foreground-ink background-ink)
  (lambda (frame pane rest)
    (let ((*foreground-color* foreground-ink)
          (*background-color* background-ink))
      ;; (setf (medium-background pane) background-ink
      ;;       (medium-foreground pane) foreground-ink)
      (let ((climi::*foreground-ink* foreground-ink)
            (climi::*background-ink* background-ink))
        (call-next-formatter rest frame pane nil)))))

(defun format-tester-1 (f p r)
  (surrounding-output-with-border (p :move-cursor nil)
    (format p "Hey there"))
  (call-next-formatter r f p))

(defun format-groups-test (frame pane others)
  (declare (ignore frame))
  ;; (let ())
  (let ((current-group (stumpwm:current-group)))
    (do-list-with-interspersed-element
        (group (stumpwm::sort-groups (stumpwm:current-screen))
          (format pane " "))
      ;; (let ((record
      ;;         (with-undrawn-output-record (pane)
      ;;           (format pane "~A" (stumpwm:group-name group))))))
      (surrounding-output-with-border (pane :move-cursor nil
                                            :filled t
                                            :ink +black+)
        (format pane "~A" (stumpwm::group-name group)))
      
      ;; (if (eq current-group group)
      ;;     (let* ((tmp *foreground-color*)
      ;;            (*foreground-color* *background-color*)
      ;;            (*background-color* tmp))
      ;;       (present group 'stumpwm::group :stream pane :single-box t))
      ;;     (present group 'stumpwm::group :stream pane :single-box t))
      ;; (with-stumpwm-formatting (pane :highlight (eq current-group group))
      ;;   (present group 'stumpwm::group :stream pane :single-box t))
      ))
  
  
  ;; (make-clim-application-pane
  ;;  :scroll-bars nil
  ;;  :foreground *foreground-color*
  ;;  :background *background-color*
  ;;  :display-function
  ;;  (lambda (frame pane)
  ;;    ))
  (call-next-formatter others frame pane))

(defun format-groups (frame pane other-formatters)
  (declare (ignorable frame))
  (let ((current-group (stumpwm:current-group)))
    (do-list-with-interspersed-element
        (group (stumpwm::sort-groups (stumpwm:current-screen))
          (format pane " "))
      (if (eq current-group group)
          (let* ((tmp *foreground-color*)
                 (*foreground-color* *background-color*)
                 (*background-color* tmp))
            (present group 'stumpwm::group :stream pane :single-box t))
          (present group 'stumpwm::group :stream pane :single-box t))
      ;; (with-stumpwm-formatting (pane :highlight (eq current-group group))
      ;;   (present group 'stumpwm::group :stream pane :single-box t))
      ))
  (call-next-formatter other-formatters frame pane))

(defun format-windows (frame pane other-formatters)
  (let ((current-window (stumpwm:current-window)))
    (do-list-with-interspersed-element
        (win (stumpwm::sort-windows-by-number
              (stumpwm:group-windows (stumpwm:current-group)))
          (format pane " "))
      ;; (present win 'stumpwm::window :stream pane :single-box t)
      (with-stumpwm-formatting (pane :highlight (eq current-window win))
        (present win 'stumpwm::window :stream pane :single-box t))
      ))
  (call-next-formatter other-formatters frame pane))

(defun format-date (frame pane other-formatters)
  (with-stumpwm-formatting (pane)
    (format pane "~A" (stumpwm:time-format stumpwm:*time-modeline-string*)))
  (call-next-formatter other-formatters frame pane))

;;; Reimplement stumpwm mode-line-formatters.lisp here

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
