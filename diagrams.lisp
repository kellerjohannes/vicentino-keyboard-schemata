(in-package :vicentino-keyboard-generator)

(defun temper (interval fraction-of-sc)
  (* interval (expt 81/80 fraction-of-sc)))

(defun linear-system (generator-interval lowest highest)
  (loop for i from lowest to highest
        collect (cons i (reduce-octave (expt generator-interval i)))))

(defun generate-scale-diagram (path interval-list svg-width &optional (framing-interval 2/1))
  (let ((x-margin 20)
        (y-margin 20))
    (with-svg-to-file (scene 'svg-1.2-toplevel :width svg-width :height 100)
        (path :if-does-not-exist :create :if-exists :supersede)
      (draw scene (:rect :x 0 :y 0 :width "100%" :height "100%" :fill "white"))
      ;;(draw scene (:circle :cx 100 :cy 200 :r 15 :fill :red))
      (dolist (interval interval-list)
        (let ((xpos (+ x-margin (* (/ (log (cdr interval)) (log framing-interval))
                                   (- svg-width (* 2 x-margin))))))
          ;; (draw scene (:circle :cx xpos
          ;;                      :cy y-margin
          ;;                      :r 5
          ;;                      :fill "black"))
          (draw scene (:line :x1 xpos :y1 (+ y-margin 10)
                             :x2 xpos :y2 "100%"
                             :stroke "black"))
          (text scene (:x xpos :y y-margin
                       :font-size 12
                       :text-anchor "middle"
                       :stroke "black" :fill "black"
                       :font-family "Times, serif")
            (format nil "~a" (car interval))))))))


(defun edo (number-of-steps)
  (loop for i from 0 to number-of-steps
        collect (cons i (expt 2/1 (/ i number-of-steps)))))

(defun vicentino (path margin width height number-of-lines)
  (let* ((distance-between-lines (/ (- height margin margin) (1- number-of-lines)))
         (radius (* 1/5 distance-between-lines))
         (note-offset (* margin 3/2))
         (note-padding (/ (- width margin margin note-offset note-offset)
                          (- (* 4 (1- number-of-lines)) 2))))
    (with-svg-to-file (scene 'svg-1.2-toplevel :width width :height height)
        (path :if-exists :supersede :if-does-not-exist :create)
      (draw scene (:rect :x 0 :y 0 :width "100%" :height "100%" :fill "white"))
      (dotimes (line-id number-of-lines)
        (let ((ypos (+ margin (* line-id distance-between-lines))))
          (draw scene (:line :x1 margin :y1 ypos
                             :x2 (- width margin) :y2 ypos
                             :stroke "black"
                             :stroke-width 5
                             :stroke-linecap "round"))))
      (let ((level-counter 0)
            (level-delta 1)
            (origin-level (+ (* (1- number-of-lines) distance-between-lines) margin)))
        (dotimes (note-id (1- (* 4 (1- number-of-lines))))
          (draw scene (:circle :cx (+ margin note-offset (* note-id note-padding))
                               :cy (- origin-level
                                      (* level-counter 1/2 distance-between-lines))
                               :r radius
                               :fill "black"))
          (incf level-counter level-delta)
          (when (= level-counter (- (* 2 number-of-lines) 3))
            (setf level-delta (* -1 level-delta))))))))
