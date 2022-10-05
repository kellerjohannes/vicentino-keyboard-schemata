(defparameter *dict-key-labels* nil)

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

(load "build-macro.lisp")

(defun generate-identity-labels (a b)
  (loop for i from a to b collect (cons i (write-to-string i))))

(defun generate-empty-labels (a b)
  (loop for i from a to b collect ""))

;; numbering
(setf *dict-key-labels* (generate-identity-labels 1 146))
;; no labels
(setf *dict-key-labels* (generate-empty-labels 1 146))
;; only sharpd and flats
(setf *dict-key-labels* '((1 . "") (2 . "") (3 . "$\\sharp$") (4 . "$\\sharp$") (5 . "$\\flat$") (6 . "$\\flat$") (7 . "") (8 . "") (9 . "$\\sharp$") (10 . "$\\sharp$") (11 . "$\\flat$") (12 . "$\\flat$") (13 . "") (14 . "") (15 . "$\\sharp$") (16 . "") (17 . "") (18 . "$\\sharp$") (19 . "$\\sharp$") (20 . "$\\flat$") (21 . "$\\flat$") (22 . "") (23 . "") (24 . "$\\sharp$") (25 . "$\\sharp$") (26 . "$\\flat$") (27 . "$\\flat$") (28 . "") (29 . "") (30 . "$\\sharp$") (31 . "$\\sharp$") (32 . "$\\flat$") (33 . "$\\flat$") (34 . "") (35 . "") (36 . "$\\sharp$") (37 . "") (38 . "") (39 . "$\\sharp$") (40 . "$\\sharp$") (41 . "$\\flat$") (42 . "$\\flat$") (43 . "") (44 . "") (45 . "$\\sharp$") (46 . "$\\sharp$") (47 . "$\\flat$") (48 . "$\\flat$") (49 . "") (50 . "") (51 . "$\\sharp$") (52 . "") (53 . "") (54 . "$\\sharp$") (55 . "$\\sharp$") (56 . "$\\flat$") (57 . "$\\flat$") (58 . "") (59 . "") (60 . "$\\sharp$") (61 . "$\\sharp$") (62 . "$\\flat$") (63 . "$\\flat$") (64 . "") (65 . "") (66 . "$\\sharp$") (67 . "$\\sharp$") (68 . "$\\flat$") (69 . "$\\flat$") (70 . "") (71 . "") (72 . "$\\sharp$") (73 . "") (74 . "") (75 . "$\\sharp$") (76 . "$\\sharp$") (77 . "$\\flat$") (78 . "$\\flat$") (79 . "") (80 . "") (81 . "$\\sharp$") (82 . "$\\sharp$") (83 . "$\\flat$") (84 . "$\\flat$") (85 . "") (86 . "") (87 . "$\\sharp$") (88 . "") (89 . "") (90 . "$\\sharp$") (91 . "$\\sharp$") (92 . "$\\flat$") (93 . "$\\flat$") (94 . "") (95 . "") (96 . "$\\sharp$") (97 . "$\\sharp$") (98 . "$\\flat$") (99 . "$\\flat$") (100 . "") (101 . "") (102 . "$\\sharp$") (103 . "$\\sharp$") (104 . "$\\flat$") (105 . "$\\flat$") (106 . "") (107 . "") (108 . "$\\sharp$") (109 . "") (110 . "") (111 . "$\\sharp$") (112 . "$\\sharp$") (113 . "$\\flat$") (114 . "$\\flat$") (115 . "") (116 . "") (117 . "$\\sharp$") (118 . "$\\sharp$") (119 . "$\\flat$") (120 . "$\\flat$") (121 . "") (122 . "") (123 . "$\\sharp$") (124 . "") (125 . "") (126 . "$\\sharp$") (127 . "$\\sharp$") (128 . "$\\flat$") (129 . "$\\flat$") (130 . "") (131 . "") (132 . "$\\sharp$") (133 . "$\\sharp$") (134 . "$\\flat$") (135 . "$\\flat$") (136 . "") (137 . "") (138 . "$\\sharp$") (139 . "$\\sharp$") (140 . "$\\flat$") (141 . "$\\flat$") (142 . "") (143 . "") (144 . "$\\sharp$") (145 . "") (146 . "")))
;; vicentino numbering
(setf *dict-key-labels* '((1 . "C1") (2 . "C4") (3 . "D2") (4 . "D5") (5 . "D3") (6 . "D6") (7 . "D1") (8 . "D4") (9 . "E3") (10 . "E6") (11 . "E2") (12 . "E5") (13 . "E1") (14 . "E4") (15 . "F3") (16 . "F1") (17 . "F4") (18 . "G2") (19 . "G5") (20 . "G3") (21 . "G6") (22 . "G1") (23 . "G4") (24 . "A2") (25 . "A5") (26 . "A3") (27 . "A6") (28 . "A1") (29 . "A4") (30 . "B3") (31 . "B6") (32 . "B2") (33 . "B5") (34 . "B1") (35 . "B4") (36 . "C3")))

;; arciorgano, 1 octave, c-c
(build-generator ()
  (create-row 0.0 (+ white-width air) 0.0
          row1 (e 1 c 7 d 13 e 16 c 22 c 28 d 34))
  (create-row (- (+ (* 0.5 air) white-width) (* 0.5 black-width))
          (+ white-width air)
          (+ row1-depth air)
          row2 (a 3 a 11 x 0 a 18 a 24 a 32))
  (create-row (- (+ (* 0.5 air) white-width) (* 0.5 black-width))
          (+ white-width air)
          (+ row1-depth air row2-depth air)
          row3 (a 5 a 9 b 15 a 20 a 26 a 30 b 36))
  (create-row 0.0
          (+ white-width air) (+ back-lower-manual air)
          row4 (a 2 c 8 b 14 a 17 c 23 c 29 b 35))
  (create-row (- (+ (* 0.5 air) white-width) (* 0.5 black-width))
          (+ white-width air)
          (+ back-lower-manual air row4-depth air)
          row5 (a 4 a 12 x 0 a 19 a 25 a 33 x 0))
  (create-row (- (+ (* 0.5 air) white-width) (* 0.5 black-width))
          (+ white-width air)
          (+ back-lower-manual air row4-depth air row5-depth air)
          row6 (a 6 a 10 x 0 a 21 a 27 a 31 x 0)))

;; arciorgano, 2 octaves, c-c
(build-generator ()
  (create-row 0.0 (+ white-width air) 0.0
          row1 (e 1 c 7 d 13 e 16 c 22 c 28 d 34 e 37 c 43 d 49 e 52 c 58 c 64 d 70))
  (create-row (- (+ (* 0.5 air) white-width) (* 0.5 black-width))
          (+ white-width air)
          (+ row1-depth air)
          row2 (a 3 a 11 x 0 a 18 a 24 a 32 x 0 a 39 a 47 x 0 a 54 a 60 a 68 x 0))
  (create-row (- (+ (* 0.5 air) white-width) (* 0.5 black-width))
          (+ white-width air)
          (+ row1-depth air row2-depth air)
          row3 (a 5 a 9 b 15 a 20 a 26 a 30 b 36 a 41 a 45 b 51 a 56 a 62 a 66 b 72))
  (create-row 0.0
          (+ white-width air) (+ back-lower-manual air)
          row4 (a 2 c 8 b 14 a 17 c 23 c 29 b 35 a 38 c 44 b 50 a 53 c 59 c 65 b 71))
  (create-row (- (+ (* 0.5 air) white-width) (* 0.5 black-width))
          (+ white-width air)
          (+ back-lower-manual air row4-depth air)
          row5 (a 4 a 12 x 0 a 19 a 25 a 33 x 0 a 40 a 48 x 0 a 55 a 61 a 69))
  (create-row (- (+ (* 0.5 air) white-width) (* 0.5 black-width))
          (+ white-width air)
          (+ back-lower-manual air row4-depth air row5-depth air)
          row6 (a 6 a 10 x 0 a 21 a 27 a 31 x 0 a 42 a 46 x 0 a 57 a 63 a 67 x 0)))

;; arciorgano, full keyboard
(build-generator ()
  (create-row 0.0 (+ white-width air) 0.0 row1 (a 1 c 7 d 13 e 16 c 22 c 28 d 34 e 37 c 43 d 49 e 52 c 58 c 64 d 70 e 73 c 79 d 85 e 88 c 94 c 100 d 106 e 109 c 115 d 121 e 124 c 130 c 136 b 142 f 145))
  (create-row (- (+ (* 0.5 air) white-width) (* 0.5 black-width))
          (+ white-width air)
          (+ row1-depth air)
          row2 (a 3 a 11 x 0 a 18 a 24 a 32 x 0 a 39 a 47 x 0 a 54 a 60 a 68 x 0 a 75 a 83 x 0 a 90 a 96 a 104 x 0 a 111 a 119 x 0 a 126 a 132 a 140))
  (create-row (- (+ (* 0.5 air) white-width) (* 0.5 black-width))
          (+ white-width air)
          (+ row1-depth air row2-depth air)
          row3 (a 5 a 9 b 15 a 20 a 26 a 30 b 36 a 41 a 45 b 51 a 56 a 62 a 66 b 72 a 77 a 81 b 87 a 92 a 98 a 102 b 108 a 113 a 117 b 123 a 128 a 134 a 138))
  (create-row (+ (* 3 white-width) (* 3 air))
          (+ white-width air) (+ back-lower-manual air)
          row4 (a 17 c 23 c 29 b 35 a 38 c 44 b 50 a 53 c 59 c 65 b 71 a 74 c 80 b 86 a 89 c 95 c 101 b 107 a 110 c 116 b 122 a 125 c 131 c 137 b 143 f 146))
  (create-row (+ (* 3 white-width) (* 3 air) (- (+ (* 0.5 air) white-width) (* 0.5 black-width)))
          (+ white-width air)
          (+ back-lower-manual air row4-depth air)
          row5 (a 19 a 25 a 33 x 0 a 40 a 48 x 0 a 55 a 61 a 69 x 0 a 76 a 84 x 0 a 91 a 97 a 105 x 0 a 112 a 118 x 0 a 127 a 133 a 141))
  (create-row (+ (* 3 white-width) (* 3 air) (- (+ (* 0.5 air) white-width) (* 0.5 black-width)))
          (+ white-width air)
          (+ back-lower-manual air row4-depth air row5-depth air)
          row6 (a 21 a 27 a 31 x 0 a 42 a 46 x 0 a 57 a 63 a 67 x 0 a 78 a 82 x 0 a 93 a 99 a 103 x 0 a 114 a 120 x 0 a 129 a 135 a 139)))


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

(defun write-tex-file ()
  (with-open-file (file "tex-output.tex"
                        :direction :output
                        :if-exists :overwrite
                        :if-does-not-exist :create)
    (let ((*standard-output* file))
      (format t "~a" (read-string-from-file "latex-template-preamble.tex"))
      (generate-pikz-code (generate-vertices))
      (format t "~a" (read-string-from-file "latex-template-closing.tex")))))
