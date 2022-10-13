(in-package :keyboard-generator)

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
