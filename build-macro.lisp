(in-package :keyboard-generator)

(defun translation (figure x-shift y-shift key-nr)
  (mapcar #'(lambda (coord) (list (+ (first coord) x-shift) (+ (second coord) y-shift) key-nr)) figure))

(defmacro create-row (origin-x offset-x origin-y row pattern)
  (do ((cur-list pattern (rest (rest cur-list)))
       (cur-shape)
       (pos 0 (1+ pos))
       (result-vertex nil))
      ((null cur-list) `(list ,@(reverse result-vertex)))
    (unless (equal (first cur-list) 'x)
      (setf cur-shape (intern (concatenate 'string
                       (symbol-name row)
                       "-SHAPE-"
                       (symbol-name (first cur-list)))))
      (push `(translation (rest ,cur-shape) (+ ,origin-x (* ,pos ,offset-x)) ,origin-y ,(second cur-list)) result-vertex))))


(defmacro build-generator (&body body)
  `(defun generate-vertices (&key (air 0.08)
                  (white-width 1.8)
                  (black-width 1.1)
                  (tastino-width (* 0.8 black-width))
                  (tastino-depth (* 1.2 black-width))
                  (row1-depth 1.8)
                  (row2-depth black-width)
                  (row3-depth black-width)
                  (row4-depth row1-depth)
                  (row5-depth black-width)
                  (row6-depth black-width))
    (let ((back-lower-manual (+ row1-depth air air row2-depth row3-depth))
      (back-upper-manual (+ row4-depth air air row5-depth row6-depth))
      (half-black-width (+ (* 0.5 black-width) air)))
      (let ((row1-shape-a (list '(0 5 6 0 1 5 1 3 4 1 2 3)
                (list 0.0 0.0)
                (list 0.0 row1-depth)
                (list 0.0 back-lower-manual)
                (list (- (+ white-width (* 0.5 air)) half-black-width)
                      back-lower-manual)
                (list (- (+ white-width (* 0.5 air)) half-black-width)
                      row1-depth)
                (list white-width row1-depth)
                (list white-width 0.0)))
        (row1-shape-b (list '(0 5 6 0 1 5 2 4 5 2 3 4)
                (list 0.0 0.0)
                (list 0.0 row1-depth)
                (list (- half-black-width (* 0.5 air)) row1-depth)
                (list (- half-black-width (* 0.5 air)) back-lower-manual)
                (list white-width back-lower-manual)
                (list white-width row1-depth)
                (list white-width 0.0)))
        (row1-shape-c (list '(0 6 7 0 1 6 2 4 5 2 3 4)
                (list 0.0 0.0)
                (list 0.0 row1-depth)
                (list (- half-black-width (* 0.5 air)) row1-depth)
                (list (- half-black-width (* 0.5 air)) back-lower-manual)
                (list (- (+ white-width (* 0.5 air)) half-black-width)
                      back-lower-manual)
                (list (- (+ white-width (* 0.5 air)) half-black-width)
                      row1-depth)
                (list white-width row1-depth)
                (list white-width 0.0)))
        (row1-shape-d (list '(0 8 9 0 1 8 2 7 8 2 3 7 3 5 6 3 4 5)
                (list 0.0 0.0)
                (list 0.0 row1-depth)
                (list (- half-black-width (* 0.5 air))
                      row1-depth)
                (list (- half-black-width (* 0.5 air))
                      (- back-lower-manual air tastino-depth))
                (list (- half-black-width (* 0.5 air))
                      back-lower-manual)
                (list (- (+ white-width (* 0.5 air))
                     air
                     (* 0.5 tastino-width))
                      back-lower-manual)
                (list (- (+ white-width (* 0.5 air))
                     air
                     (* 0.5 tastino-width))
                      (- back-lower-manual air tastino-depth))
                (list white-width
                      (- back-lower-manual air tastino-depth))
                (list white-width row1-depth)
                (list white-width 0.0)))
        (row1-shape-e (list '(0 8 9 0 1 8 1 6 7 1 2 6 3 5 6 3 4 5)
                (list 0.0 0.0)
                (list 0.0 row1-depth)
                (list 0.0 (- back-lower-manual air tastino-depth))
                (list (- (+ (* 0.5 tastino-width) air) (* 0.5 air))
                      (- back-lower-manual air tastino-depth))
                (list (- (+ (* 0.5 tastino-width) air) (* 0.5 air))
                      back-lower-manual)
                (list (- (+ white-width (* 0.5 air)) half-black-width)
                      back-lower-manual)
                (list (- (+ white-width (* 0.5 air)) half-black-width)
                      (- back-lower-manual air tastino-depth))
                (list (- (+ white-width (* 0.5 air)) half-black-width)
                      row1-depth)
                (list white-width row1-depth)
                (list white-width 0.0)))
        (row1-shape-f (list '(0 2 3 0 1 2)
                (list 0.0 0.0)
                (list 0.0 back-lower-manual)
                (list white-width back-lower-manual)
                (list white-width 0.0)))
        (row2-shape-a (list '(0 2 3 0 1 2)
                (list 0.0 0.0)
                (list 0.0 row2-depth)
                (list black-width row2-depth)
                (list black-width 0.0)))
        (row3-shape-a (list '(0 2 3 0 1 2)
                (list 0.0 0.0)
                (list 0.0 row3-depth)
                (list black-width row3-depth)
                (list black-width 0.0)))
        (row3-shape-b (list '(0 2 3 0 1 2)
                (list (* 0.5 (- black-width tastino-width)) (- row3-depth tastino-depth))
                (list (* 0.5 (- black-width tastino-width)) row3-depth)
                (list (+ (* 0.5 (- black-width tastino-width)) tastino-width) row3-depth)
                (list (+ (* 0.5 (- black-width tastino-width)) tastino-width)
                      (- row3-depth tastino-depth))))
        (row4-shape-a (list '(0 5 6 0 1 5 1 3 4 1 2 3)
                (list 0.0 0.0)
                (list 0.0 row4-depth)
                (list 0.0 back-upper-manual)
                (list (- (+ white-width (* 0.5 air)) half-black-width)
                      back-upper-manual)
                (list (- (+ white-width (* 0.5 air)) half-black-width)
                      row4-depth)
                (list white-width row4-depth)
                (list white-width 0.0)))
        (row4-shape-b (list '(0 5 6 0 1 5 2 4 5 2 3 4)
                (list 0.0 0.0)
                (list 0.0 row4-depth)
                (list (- half-black-width (* 0.5 air)) row4-depth)
                (list (- half-black-width (* 0.5 air)) back-upper-manual)
                (list white-width back-upper-manual)
                (list white-width row4-depth)
                (list white-width 0.0)))
        (row4-shape-c (list '(0 6 7 0 1 6 2 4 5 2 3 4)
                (list 0.0 0.0)
                (list 0.0 row4-depth)
                (list (- half-black-width (* 0.5 air)) row4-depth)
                (list (- half-black-width (* 0.5 air)) back-upper-manual)
                (list (- (+ white-width (* 0.5 air)) half-black-width)
                      back-upper-manual)
                (list (- (+ white-width (* 0.5 air)) half-black-width)
                      row4-depth)
                (list white-width row4-depth)
                (list white-width 0.0)))
        (row4-shape-f (list '(0 2 3 0 1 2)
                (list 0.0 0.0)
                (list 0.0 back-upper-manual)
                (list white-width back-upper-manual)
                (list white-width 0.0)))
        (row5-shape-a (list '(0 2 3 0 1 2)
                (list 0.0 0.0)
                (list 0.0 row2-depth)
                (list black-width row2-depth)
                (list black-width 0.0)))
        (row6-shape-a (list '(0 2 3 0 1 2)
                (list 0.0 0.0)
                (list 0.0 row3-depth)
                (list black-width row3-depth)
                (list black-width 0.0))))
    (list ,@body)))))
