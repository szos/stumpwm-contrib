(in-package #:clim-mode-line)

(defparameter *background-color* +black+ )
(defparameter *foreground-color* +white+ )
(defparameter *highlight-background-color* +black+)
(defparameter *highlight-foreground-color* +white+)

(defun invoke-with-stumpwm-formatting (pane cont highlight-when)
  (let ((highlight (funcall highlight-when)))
    (surrounding-output-with-border (pane :ink (if highlight
                                                   *highlight-background-color*
                                                   ;; *background-color*
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
        (rvar (or other-formatters (gensym "OTHER-FORMATTERS"))))
    `(defun ,name (,fvar ,pvar ;; ,rvar
                   )
       ,@body
       ;; ,@(when auto-call-continuation
       ;;     `((call-next-formatter ,rvar ,fvar ,pvar)))
       )))

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

(defun format-align-right (frame pane ;; other-formatters
                           )
  
  ;; (with-right-alignment (frame pane)
  ;;   (call-next-formatter other-formatters frame pane))
  )

(defun format-groups (frame pane ;; other-formatters
                      )
  (declare (ignorable frame))
  (let ((current-group (stumpwm:current-group)))
    (do-list-with-interspersed-element
        (group (stumpwm::sort-groups (stumpwm:current-screen))
          (format pane " "))
      (with-stumpwm-formatting (pane :highlight (eq current-group group))
        (present group 'stumpwm::group :stream pane :single-box t))))
  ;; (call-next-formatter other-formatters frame pane)
  )

(defun format-windows (frame pane ;; other-formatters
                       )
  (let ((current-window (stumpwm:current-window)))
    (do-list-with-interspersed-element
        (win (stumpwm::sort-windows-by-number
              (stumpwm:group-windows (stumpwm:current-group)))
          (format pane " "))
      (with-stumpwm-formatting (pane :highlight (eq current-window win))
        (present win 'stumpwm::window :stream pane :single-box t))))
  ;; (call-next-formatter other-formatters frame pane)
  )
