(in-package :keyboard-generator)

(defparameter *svg-scale-factor* 40)
(defparameter *svg-y-shift* 500)
(defparameter *svg-x-shift* 20)

(defun xp (point)
  (+ *svg-x-shift* (* (first point) *svg-scale-factor*)))

(defun yp (point)
  (- *svg-y-shift* (* (second point) *svg-scale-factor*)))

(defun path-iterator (point-list origin scene)
  (cond ((null (rest point-list))
         (draw scene (:line :x1 (xp (first point-list))
                            :y1 (yp (first point-list))
                            :x2 (xp origin)
                            :y2 (yp origin)
                            :stroke "black")))
        (t (draw scene (:line :x1 (xp (first point-list))
                              :y1 (yp (first point-list))
                              :x2 (xp (second point-list))
                              :y2 (yp (second point-list))
                              :stroke "black"))
           (path-iterator (rest point-list) origin scene))))

(defun lookup-label-id (id)
  (cdr (assoc id *dict-key-labels*)))

(defparameter *label-range* '(nil . nil))

(defun output-svg-label (a b label-id scene)
  (let ((avg (/ (+ (xp a) (xp b)) 2)))
    (when (or (null (car *label-range*)) (< avg (car *label-range*)))
      (setf (car *label-range*) avg))
    (when (or (null (cdr *label-range*)) (> avg (cdr *label-range*)))
      (setf (cdr *label-range*) avg))
    (text scene (:x avg
                 :y (- (yp a) (- avg (xp a)) -10)
                 :stroke "black"
                 :fill "black"
                 :text-anchor "middle"
                 :font-size 20
                 :font-family "Times, serif"
                 )
      (lookup-label-id label-id))))

(defun ratio->xpos (ratio frame-interval xpos-range)
  (+ (car xpos-range)
     (* (/ (log ratio) (log frame-interval))
        (- (cdr xpos-range) (car xpos-range)))))

(defun create-hairpin (a b key-id scene)
  (let* ((x-center (/ (+ (xp a) (xp b)) 2))
         (y-center (- (yp a) (- x-center (xp a))))
         (line-center (ratio->xpos (id->ratio key-id) 16/1 *label-range*)))
    (draw scene (:circle :cx x-center
                         :cy y-center
                         :r 5
                         :fill "red"))
    (draw scene (:line :x1 x-center
                       :y1 y-center
                       :x2 line-center
                       :y2 (+ y-center 20)
                       :stroke "red"))
    (draw scene (:line :x1 line-center
                       :y1 (+ y-center 20)
                       :x2 line-center
                       :y2 500
                       :stroke "red"))))


(defun write-svg-file (path)
  (setf *svg-scale-factor* 40)
  (setf *svg-y-shift* 370)
  (setf *svg-x-shift* 20)
  (with-svg-to-file (scene 'svg-1.2-toplevel :width 2250 :height 400)
      (path :if-exists :supersede :if-does-not-exist :create)
    ;; (draw scene (:circle :cx 200 :cy 150 :r 50) :fill "red")
    (draw scene (:rect :x 0 :y 0 :width "100%" :height "100%"))

    (let ((vertices (generate-vertices))
          (*label-range* '(nil . nil)))
      (dolist (row vertices)
        (dolist (key-shape row)
          (path-iterator key-shape (first key-shape) scene)
          (output-svg-label (first key-shape)
                            (first (last key-shape))
                            (third (first key-shape))
                            scene)))
      (dolist (row vertices)
        (dolist (key-shape row)
          (create-hairpin (first key-shape)
                          (first (last key-shape))
                          (third (first key-shape))
                          scene))))))


;; copy-paste from tex-generator, as reference material

(defun output-draw-command (a b)
  (format t "~&\\draw[thick] (~s,~s) -- (~s,~s);" (first a) (second a) (first b) (second b)))

(defun output-label-command (a b label-text)
  (let ((avg (/ (+ (first a) (first b)) 2)))
    (format t "~&\\node at (~s,~s) {\\Large{~a}};"
            avg
            (+ (- avg (first a)) (second a))
            label-text)))

(defun generate-pikz-code (vertices)
  (dolist (row vertices)
    (dolist (key-shape row)
      (format t "~&% key number ~s:" (third (first key-shape)))
      (do ((origin (first key-shape))
           (point-list key-shape (rest point-list)))
          ((null (rest point-list)) (output-draw-command (first point-list) origin))
        (output-draw-command (first point-list) (second point-list)))
      (output-label-command (first key-shape)
                            (first (last key-shape))
                            (cdr (assoc (third (first key-shape)) *dict-key-labels*))))))

(defun read-string-from-file (pathname &key (buffer-size 4096) (element-type 'character))
  "Return the contents of PATHNAME as a string."
  (with-open-file (file-stream pathname)
    (with-output-to-string (datum)
      (let ((buffer (make-array buffer-size :element-type element-type)))
        (loop for bytes-read = (read-sequence buffer file-stream)
              do (write-sequence buffer datum :start 0 :end bytes-read)
              while (= bytes-read buffer-size))))))

(defun write-tex-file (path)
  (with-open-file (file path
                        :direction :output
                        :if-exists :overwrite
                        :if-does-not-exist :create)
    (let ((*standard-output* file))
      (format t "~a" (read-string-from-file "latex-template-preamble.tex"))
      (generate-pikz-code (generate-vertices))
      (format t "~a" (read-string-from-file "latex-template-closing.tex")))))
