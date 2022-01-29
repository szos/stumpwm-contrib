(in-package #:clim-mode-line)

;;; This file contains general macros that dont have a clear place where they
;;; should reside

(defmacro do-list-with-interspersed-element
    ((var list &body interspersed-forms) &body body)
  (alexandria:with-gensyms (rest)
    `(loop :for (,var ,rest) :on ,list
           :do (progn ,@body)
           :when ,rest
             :do (progn ,@interspersed-forms))))

;; (defmacro do-list-with-interspersed-element
;;     ((var list &body interspersed-forms) &body body)
;;   (alexandria:with-gensyms (initial rest hold cont tmp)
;;     `(flet ((,cont (,var)
;;               ,@body))
;;        (let* ((,hold ,list)
;;               (,initial (car ,hold))
;;               (,rest (cdr ,hold)))
;;          (when ,initial
;;            (,cont ,initial))
;;          (dolist (,tmp ,rest)
;;            ,@interspersed-forms
;;            (,cont ,tmp))))))

(defmacro with-undrawn-output-record ((stream) &body body)
  (let ((s (gensym)))
    `(let ((,s ,stream))
       (with-output-recording-options (,s :draw nil :record t)
         (with-new-output-record (,s)
           ,@body)))))

(defmacro with-output-record-bounds ((x y width height) record &body body)
  (alexandria:with-gensyms (rec)
    `(let ((,rec ,record))
       (multiple-value-bind (,x ,y) (output-record-position ,rec)
         (declare (ignorable ,x ,y))
         (multiple-value-bind (,width ,height) (bounding-rectangle-size ,rec)
           (declare (ignorable ,width ,height))
           ,@body)))))

(defmacro with-right-alignment ((frame pane) &body body)
  (alexandria:with-gensyms (stream width record cursor-x ;; top-level
                                   )
    `(let* ((,stream ,pane)
            (,cursor-x (stream-cursor-position ,stream))
            ;; (,top-level (frame-top-level-sheet frame))
            (,record
              (with-output-recording-options (,stream :draw nil :record t)
                (with-new-output-record (,stream)
                  ,@body)))
            (,width (mode-line-head-width ,frame)))
       (declare (ignorable ,cursor-x))
       (multiple-value-bind (x y) (output-record-position ,record)
         (declare (ignore x))
         (multiple-value-bind (w h) (bounding-rectangle-size ,record)
           (declare (ignorable h))
           (setf (output-record-position ,record)
                 (values (- ,width w) y))
           ;; (draw-rectangle* ,stream ,cursor-x 0 (- ,width w) h)
           ))
       (tree-recompute-extent ,record)
       (replay ,record ,stream))))

(defvar *background-color* nil)
(defvar *foreground-color* nil)

(defmacro with-background ((pane background-ink) &body body)
  (alexandria:with-gensyms (stream)
    `(let ((,stream ,pane)
           (*background-color* ,background-ink))
       (surrounding-output-with-border (,stream :ink *background-color*
                                                :filled t
                                                :move-cursor nil)
         ,@body))))

(defmacro with-foreground ((pane foreground-ink) &body body)
  (alexandria:with-gensyms (stream)
    `(let ((,stream ,pane)
           (*foreground-color* ,foreground-ink))
       (with-drawing-options (,stream :ink *foreground-color*)
         ,@body))))

(defmacro with-colors ((pane foreground-ink background-ink) &body body)
  (alexandria:with-gensyms (stream temp-fg temp-bg)
    `(let* ((,stream ,pane)
            (,temp-fg ,foreground-ink)
            (,temp-bg ,background-ink)
            (*foreground-color* ,temp-bg)
            (*background-color* ,temp-fg))
       (surrounding-output-with-border (,stream :ink *background-color*
                                                :filled t
                                                :move-cursor nil)
         (with-drawing-options (,stream :ink *foreground-color*)
           ,@body)))))

(defmacro with-inverted-ink ((pane &rest drawing-options) &body body)
  (alexandria:with-gensyms (stream ;; frame
                                   )
    `(let ((,stream ,pane)
           ;; (,frame (pane-frame ,pane))
           )
       (with-colors (,stream *background-color*
                             ;;(mode-line-background-color ,frame)
                             *foreground-color*
                             ;; (mode-line-foreground-color ,frame)
                             )
         (with-drawing-options (,stream ,@drawing-options)
           ,@body)))))

(defmacro with-table ((pane &rest options) &body body)
  ;; Set up a table, and make sure we can communicate it to functions further down
  ;; the call stack.
  `(slim:with-table (,pane ,@options)
     (let ((*display-as-table* t)
           (*display-style* :table))
       ,@body)))

(defmacro with-table-row ((&rest options) &body body)
  ;; Defined because we want a consistent syntax. Plus then we can make our own
  ;; changes without anyone needing to rewrite anything (well, unless we add
  ;; required arguments)
  (declare (ignore options))
  `(slim:row ,@body))

(defmacro with-cell ((&optional (pane 'slim:*pane*) &rest options) &body body)
  (declare (ignore options))
  (alexandria:with-gensyms (cont)
    `(flet ((,cont ()
              ,@body))
       (declare (dynamic-extent (function ,cont)))
       ;; (surrounding-output-with-border (,stream :ink *background-color*
       ;;                                          :filled t
       ;;                                          :move-cursor nil)
       ;;   (with-drawing-options (,stream :ink *foreground-color*)
       ;;     ,@body))
       (case *display-style*
         ((:text) (funcall #',cont))
         ((:table) (formatting-cell (,pane :align-x *align-x*)
                     (funcall #',cont)))))))
