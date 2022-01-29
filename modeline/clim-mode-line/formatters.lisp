(in-package #:clim-mode-line)

;; (defun invoke-with-normal-stumpwm-highlighting (pane highlight-thunk continuation)
;;   "When HIGHLIGHT-THUNK is returns true, invoke CONTINUATION with the StumpWM
;; manner of highlighting, by swapping the foreground and the background. If this is 
;; called within a table, invoke CONTINUATION within a cell."
;;   (flet ((continue-processing ()
;;            (with-cell (pane)
;;              (funcall continuation))))
;;     (declare (dynamic-extent (function continue-processing)))
;;     (if (funcall highlight-thunk)
;;         (surrounding-output-with-border (pane :filled t
;;                                               :move-cursor nil
;;                                               :ink *foreground-color*)
;;           (with-drawing-options (pane :ink *background-color*)
;;             (continue-processing)))
;;         ;; (with-colors (pane *foreground-color* *background-color*)
;;         ;;   (continue-processing))
;;         ;; (with-inverted-ink (pane)
;;         ;;   (continue-processing))
;;         (continue-processing))))

;; (defmacro with-normal-stumpwm-highlighting ((pane highlight-when)
;;                                             &body body)
;;   (alexandria:with-gensyms (continuation highlight-thunk)
;;     `(flet ((,continuation ()
;;               ,@body)
;;             (,highlight-thunk ()
;;               ,highlight-when))
;;        (declare (dynamic-extent (function ,continuation)
;;                                 (function ,highlight-thunk)))
;;        (invoke-with-normal-stumpwm-highlighting ,pane
;;                                                 #',highlight-thunk
;;                                                 #',continuation))))

;; (defun invoke-with-thin-stumpwm-highlighting (pane highlight cont)
;;   (flet ((continue-processing ()
;;            (with-cell (pane)
;;              (funcall cont))))
;;     (declare (dynamic-extent (function continue-processing)))
;;     (if (funcall highlight)
;;         (let ((record (with-undrawn-output-record (pane)
;;                         (with-drawing-options (pane :ink +background-ink+)
;;                           (continue-processing)))))
;;           (with-output-record-bounds (x y w h) record
;;             (draw-rectangle* pane x y (+ x w) (+ y h))
;;             (replay record pane)))
;;         (continue-processing))))

;; (defmacro with-thin-stumpwm-highlighting ((pane highlight-when) &body body)
;;   (alexandria:with-gensyms (continuation highlight)
;;     `(flet ((,continuation ()
;;               ,@body)
;;             (,highlight ()
;;               ,highlight-when))
;;        (declare (dynamic-extent (function ,continuation)
;;                                 (function ,highlight)))
;;        (invoke-with-thin-stumpwm-highlighting ,pane
;;                                               #',highlight
;;                                               #',continuation))))

;; (defun invoke-with-stumpwm-highlighting (pane highlight cont style)
;;   (case style
;;     ((:normal :thick)
;;      (invoke-with-normal-stumpwm-highlighting pane highlight cont))
;;     ((:thin)
;;      (invoke-with-thin-stumpwm-highlighting pane highlight cont))))

;; (defmacro with-stumpwm-highlighting ((pane highlight-when
;;                                       &optional (style :normal))
;;                                      &body body)
;;   (alexandria:with-gensyms (continuation highlight-thunk contarg)
;;     `(flet ((,continuation (&rest ,contarg)
;;               (declare (ignore ,contarg))
;;               ,@body)
;;             (,highlight-thunk ()
;;               ,highlight-when))
;;        (declare (dynamic-extent (function ,continuation)
;;                                 (function ,highlight-thunk)))
;;        (invoke-with-stumpwm-highlighting ,pane
;;                                          #',highlight-thunk
;;                                          #',continuation
;;                                          ,style))))

;; (defun invoke-with-stumpwm-formatting (pane continuation)
;;   (with-cell (pane)
;;     (funcall continuation)))

;; (defmacro with-stumpwm-formatting ((pane &key (highlight nil) (style :normal))
;;                                    &body body)
;;   (alexandria:with-gensyms (cont high)
;;     `(flet ((,cont ()
;;               ,@body)
;;             ,@(when highlight
;;                 `((,high () ,highlight))))
;;        (declare (dynamic-extent (function ,cont)
;;                                 ,@(when highlight
;;                                     `((function ,high)))))
;;        ,(if highlight
;;             `(invoke-with-stumpwm-highlighting ,pane #',high #',cont ,style)
;;             `(invoke-with-stumpwm-formatting ,pane #',cont)))))

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
       (format pane "~A" ,string))
     ;; (if *display-as-table*
     ;;     (with-cell ()
     ;;       (format pane "~A" ,string))
     ;;     (format pane "~A" ,string))
     ))

(defun call-next-formatter (formatter-list frame pane)
  (when (eql *display-style* :text)
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
    (call-next-formatter rest frame pane)))

(define-formatter (format-with-colors :auto-call-continuation nil)
    (frame pane rest)
  (let ((*foreground-color* (mode-line-foreground-color frame))
        (*background-color* (mode-line-background-color frame)))
    (call-next-formatter rest frame pane))
  ;; (with-colors (pane (mode-line-foreground-color frame)
  ;;                    (mode-line-background-color frame))
  ;;   (call-next-formatter rest frame pane))
  )

(defun make-color-formatter (foreground-ink background-ink)
  (lambda (frame pane rest)
    (with-colors (pane foreground-ink background-ink)
      (call-next-formatter rest frame pane))
    ;; (let ((*foreground-color* foreground-ink)
    ;;       (*background-color* background-ink))
    ;;   (call-next-formatter rest frame pane))
    ;; (with-colors (pane foreground-ink background-ink)
    ;;   (call-next-formatter rest frame pane))
    ))

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
