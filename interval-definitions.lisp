(in-package :keyboard-generator)



'("D2" "D5" "D3" "D6" "D1" "D4" "E3" "E6" "E2" "E5" "E1" "E4" "F3" "F1" "F4" "G2" "G5" "G3" "G6" "G1" "G4" "A2" "A5" "A3" "A6" "A1" "A4" "B3" "B6" "B2" "B5" "B1" "B4")

(defun get-31ed2 (degree)
  (expt 2 (/ (1- degree) 31)))

(defparameter *dict-key-31-pitchclass*
  `((1 . ,(get-31ed2 1)) ; c
    (2 . ,(get-31ed2 2)) ; c dot
    (3 . ,(get-31ed2 3)) ; c sharp
    (4 . ,(get-31ed2 5)) ; d flat dot
    (5 . ,(get-31ed2 4)) ; d flat
    (6 . ,(* 3/2 1/2 (get-31ed2 19))) ; d comma
    (7 . ,(get-31ed2 6)) ; d
    (8 . ,(get-31ed2 7)) ; d dot
    (9 . ,(get-31ed2 8)) ; d sharp
    (10 . ,(* 3/2 1/2 (get-31ed2 24))) ; e comma
    (11 . ,(get-31ed2 9)) ; e flat
    (12 . ,(get-31ed2 10)) ; e flat dot
    (13 . ,(get-31ed2 11)) ; e
    (14 . ,(get-31ed2 12)) ; e dot
    (15 . ,(get-31ed2 13)) ; e sharp
    (16 . ,(get-31ed2 14)) ; f
    (17 . ,(get-31ed2 15)) ; f dot
    (18 . ,(get-31ed2 16)) ; f sharp
    (19 . ,(get-31ed2 18)) ; g flat dot
    (20 . ,(get-31ed2 17)) ; g flat
    (21 . ,(* 3/2 (get-31ed2 1))) ; g comma
    (22 . ,(get-31ed2 19)) ; g
    (23 . ,(get-31ed2 20)) ; g dot
    (24 . ,(get-31ed2 21)) ; g sharp
    (25 . ,(get-31ed2 23)) ; a flat dot
    (26 . ,(get-31ed2 22)) ; a flat
    (27 . ,(* 3/2 (get-31ed2 6))) ; a comma
    (28 . ,(get-31ed2 24)) ; a
    (29 . ,(get-31ed2 25)) ; a dot
    (30 . ,(get-31ed2 26)) ; a sharp
    (31 . ,(* 3/2 (get-31ed2 11))) ; b comma
    (32 . ,(get-31ed2 27)) ; b flat
    (33 . ,(get-31ed2 28)) ; b flat dot
    (34 . ,(get-31ed2 29)) ; b
    (35 . ,(get-31ed2 30)) ; b dot
    (0 . ,(get-31ed2 0)) ; b sharp
    ))


(defun id->ratio (id)
  (multiple-value-bind (octaves id-class)
      (floor id 36)
    (* (expt 2 octaves) (cdr (assoc id-class *dict-key-31-pitchclass*)))))



(defparameter *intervals-lantica*
  `(("comma [¼ SC]"                          ,(expt 81/80 1/4) "" "")
    ("comma [1 SC]"                          81/80 "" "")
    ("diesis enarmonico minore"              ,(get-31ed2 1))
    ("diesis enarmonico maggiore"            ,(get-31ed2 2))
    ("semitono minore"                       ,(get-31ed2 2))
    ("semitono maggiore naturale"            ,(get-31ed2 3) "molle" "incitato")
    ("semitono maggiore accidentale"         ,(get-31ed2 3) "molle" "incitato")
    ("tono minore accidentale"               ,(get-31ed2 4))
    ("tono naturale"                         ,(get-31ed2 5) "incitato" "molle")
    ("tono accidentale"                      ,(get-31ed2 5) "incitato" "molle")
    ("tono maggiore accidentale"             ,(get-31ed2 6))
    ("tono maggiore accidentale propinqua"   ,(get-31ed2 7))
    ("terza minima"                          ,(get-31ed2 7))
    ("terza manca del minore"                ,(get-31ed2 7))
    ("terza minore naturale"                 ,(get-31ed2 8))
    ("terza minore accidentale"              ,(get-31ed2 8))
    ("terza minore propinquissima"           6/5)
    ("terza minore propinqua"                ,(get-31ed2 9))
    ("terza più che minore"                  ,(get-31ed2 9))
    ("terza maggiore naturale"               ,(get-31ed2 10))
    ("terza maggiore accidentale"            ,(get-31ed2 10))
    ("terza maggiore propinquissima"         ,(* 5/4 (expt 81/80 1/4)))
    ("terza maggiore propinqua"              ,(get-31ed2 11))
    ("terza più che maggiore"                ,(get-31ed2 11))
    ("[quarta minima]"                       ,(get-31ed2 12))
    ("quarta naturale"                       ,(get-31ed2 13))
    ("quarta accidentale"                    ,(get-31ed2 13))
    ("quarta propinquissima"                 ,(* 4/3 (expt 81/80 1/2)))
    ("salto più che di quarta"               ,(get-31ed2 14))))
