(in-package #:clim-mode-line)

(defmacro with-fg-bg ((stream) &body body)
  `(surrounding-output-with-border (,stream :ink *background-color*
                                            :shape :rectangle
                                            :filled t
                                            :move-cursor nil)
     (with-drawing-options (,stream :ink *foreground-color*)
       ,@body)))

(define-presentation-method present
    (group (type stumpwm::group) stream view &key)
  (let ((str (string-trim '(#\space)
                          (stumpwm::format-expand stumpwm::*group-formatters*
                                                  stumpwm::*group-format*
                                                  group))))
    (with-fg-bg (stream)
      (format stream "~A" str))
    ;; (surrounding-output-with-border (stream :ink *background-color*
    ;;                                         :shape :rectangle
    ;;                                         :filled t
    ;;                                         :move-cursor nil)
    ;;   (with-drawing-options (stream :ink *foreground-color*)
    ;;     (format stream "~A" str)))
    ))

(define-presentation-method present
    (window (type stumpwm::window) stream view &key)
  (let ((str (string-trim '(#\space)
                          (stumpwm::format-expand stumpwm::*window-formatters*
                                                  stumpwm::*window-format*
                                                  window))))
    (format stream "~A" str)))

;;; Doesnt work, as stumpwm::head is a struct, not a class. 
;; (define-presentation-method present
;;     (head (type stumpwm::head) stream view &key)
;;   (format nil "~d" (stumpwm::head-number (stumpwm:current-head))))
