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
                            :stroke "gray")))
        (t (draw scene (:line :x1 (xp (first point-list))
                              :y1 (yp (first point-list))
                              :x2 (xp (second point-list))
                              :y2 (yp (second point-list))
                              :stroke "gray"))
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
                 :font-size 40
                 :font-family "Times, serif")
      (lookup-label-id label-id))))

(defun ratio->xpos (ratio frame-interval xpos-range)
  (+ (car xpos-range)
     (* (/ (log ratio) (log frame-interval))
        (- (cdr xpos-range) (car xpos-range)))))

(defun create-hairpin (a b key-id scene)
  (let* ((x-center (/ (+ (xp a) (xp b)) 2))
         (y-center (- (yp a) (* 0.4 (- x-center (xp a)))))
         (line-center (ratio->xpos (id->ratio key-id) 16/1 *label-range*)))
    (draw scene (:circle :cx x-center
                         :cy y-center
                         :r 10
                         :fill "black"))
    (draw scene (:line :x1 x-center
                       :y1 y-center
                       :x2 line-center
                       :y2 (+ y-center 20)
                       :stroke "black"))
    (draw scene (:line :x1 line-center
                       :y1 (+ y-center 20)
                       :x2 line-center
                       :y2 "100%"
                       :stroke "black"))))

(defun make-path (root modifier suffix)
  (format nil "~a-~a.~a" root modifier suffix))

(defun generate-kbd (path svg-width svg-height)
  (with-svg-to-file (scene 'svg-1.2-toplevel
                           :width svg-width
                           :height svg-height)
      ((make-path path "keyboard" "svg")
       :if-exists :supersede
       :if-does-not-exist :create)
    ;; (draw scene (:circle :cx 200 :cy 150 :r 50) :fill "red")
    (draw scene (:rect :x 0 :y 0 :width "100%" :height "100%" :fill "white"))
    (let ((vertices (generate-vertices)))
      (setf *label-range* '(nil . nil))
      (dolist (row vertices)
        (dolist (key-shape row)
          (path-iterator key-shape (first key-shape) scene)
          (output-svg-label (first key-shape)
                            (first (last key-shape))
                            (third (first key-shape))
                            scene)))
      ;; needs to be separate because `output-svg-label' sets `*label-range*' first.
      (dolist (row vertices)
        (dolist (key-shape row)
          (create-hairpin (first key-shape)
                          (first (last key-shape))
                          (third (first key-shape))
                          scene))))))

(defun generate-interval-list (path path-extension svg-width svg-height interval-list)
  (with-svg-to-file (scene 'svg-1.2-toplevel :width svg-width :height svg-height)
      ((make-path path path-extension "svg")
       :if-exists :supersede
       :if-does-not-exist :create)
    (let ((padding 30))
      (draw scene (:rect :x 0 :y 0 :width "100%" :height "100%" :fill "white"))
      (loop for interval in interval-list
            for i from 1
            do (let ((center (ratio->xpos (second interval) 16/1 *label-range*))
                     (origin (ratio->xpos 1/1 16/1 *label-range*))
                     (ypos (+ (* i padding) 20)))
                 (draw scene (:line :x1 center :y1 0
                                    :x2 center :y2 "100%"
                                    :stroke (if (= i 1) "black" "gray")
                                    :stroke-width (if (= i 1) 3 1)))
                 (draw scene (:circle :cx origin
                                      :cy ypos
                                      :r 4
                                      :fill "black"))
                 (draw scene (:circle :cx center
                                      :cy ypos
                                      :r 4
                                      :fill "black"))
                 (draw scene (:line :x1 origin :y1 ypos
                                    :x2 center :y2 ypos
                                    :stroke "black"
                                    :stroke-width 3))
                 (text scene (:x (+ center 15) :y (+ ypos 6)
                              :font-size 25
                              :font-family "Times, serif"
                              :stroke "black" :fill "black")
                   (first interval)))))))

(defun write-svg-file (path)
  "Path without suffix."
  (setf *svg-scale-factor* 90)
  (setf *svg-y-shift* 900)
  (setf *svg-x-shift* 100)

  (generate-kbd path 5100 1000)
  (generate-interval-list path "lantica" 5100 1700 *intervals-lantica*)
  (generate-interval-list path "pyth" 5100 460 *intervals-pythagorean*)
  (generate-interval-list path "just" 5100 600 *intervals-just*))
