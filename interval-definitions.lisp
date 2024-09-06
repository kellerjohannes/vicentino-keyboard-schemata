(in-package :vicentino-keyboard-generator)


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

(defun reduce-octave (interval)
  (cond ((< interval 1/1) (reduce-octave (* 2/1 interval)))
        ((>= interval 2/1) (reduce-octave (* 1/2 interval)))
        (t interval)))

(defun get-mt (fifth)
  (reduce-octave (expt (* 3/2 (expt 81/80 -1/4)) fifth)))

(defparameter *dict-key-31-mt-pitchclass*
  `((19 . ,(get-mt -18)) ; g flat dot
    (4 .  ,(get-mt -17)) ; d flat dot
    (25 . ,(get-mt -16)) ; a flat dot
    (12 . ,(get-mt -15)) ; e flat dot
    (33 . ,(get-mt -14)) ; b flat dot
    (17 . ,(get-mt -13)) ; f dot
    (2 .  ,(get-mt -12)) ; c dot
    (23 . ,(get-mt -11)) ; g dot
    (8 .  ,(get-mt -10)) ; d dot
    (29 . ,(get-mt -9)) ; a dot
    (14 . ,(get-mt -8)) ; e dot
    (35 . ,(get-mt -7)) ; b dot
    (20 . ,(get-mt -6)) ; g flat
    (5 .  ,(get-mt -5)) ; d flat
    (26 . ,(get-mt -4)) ; a flat
    (11 . ,(get-mt -3)) ; e flat
    (32 . ,(get-mt -2)) ; b flat
    (16 . ,(get-mt -1)) ; f
    (1 .  ,(get-mt 0)) ; c
    (22 . ,(get-mt 1)) ; g
    (7 .  ,(get-mt 2)) ; d
    (28 . ,(get-mt 3)) ; a
    (13 . ,(get-mt 4)) ; e
    (34 . ,(get-mt 5)) ; b
    (18 . ,(get-mt 6)) ; f sharp
    (3 .  ,(get-mt 7)) ; c sharp
    (24 . ,(get-mt 8)) ; g sharp
    (9 .  ,(get-mt 9)) ; d sharp
    (30 . ,(get-mt 10)) ; a sharp
    (15 . ,(get-mt 11)) ; e sharp
    (0 .  ,(* 1/2 (get-mt 12))) ; b sharp

    (21 . ,(* 3/2 (get-mt 0))) ; g comma
    (6 . ,(* 3/2 1/2 (get-mt 1))) ; d comma
    (27 . ,(* 3/2 (get-mt 2))) ; a comma
    (10 . ,(* 3/2 1/2 (get-mt 3))) ; e comma
    (31 . ,(* 3/2 (get-mt 4))) ; b comma
    ))

(defparameter *dict-key-adaptive-just-pitchclass*
  `(
    (20 . ,(get-mt -6)) ; g flat
    (5 .  ,(get-mt -5)) ; d flat
    (26 . ,(get-mt -4)) ; a flat
    (11 . ,(get-mt -3)) ; e flat
    (32 . ,(get-mt -2)) ; b flat
    (16 . ,(get-mt -1)) ; f
    (1 .  ,(get-mt 0)) ; c
    (22 . ,(get-mt 1)) ; g
    (7 .  ,(get-mt 2)) ; d
    (28 . ,(get-mt 3)) ; a
    (13 . ,(get-mt 4)) ; e
    (34 . ,(get-mt 5)) ; b
    (18 . ,(get-mt 6)) ; f sharp
    (3 .  ,(get-mt 7)) ; c sharp
    (24 . ,(get-mt 8)) ; g sharp
    (9 .  ,(get-mt 9)) ; d sharp
    (30 . ,(get-mt 10)) ; a sharp
    (15 . ,(get-mt 11)) ; e sharp
    (0 .  ,(* 1/2 (get-mt 12))) ; b sharp

    (21 . ,(reduce-octave (* 3/2 (get-mt -7)))) ; g flat comma
    (6 .  ,(reduce-octave (* 3/2 (get-mt -6)))) ; d flat comma
    (27 . ,(reduce-octave (* 3/2 (get-mt -5)))) ; a flat comma
    (12 . ,(reduce-octave (* 3/2 (get-mt -4)))) ; e flat comma
    (33 . ,(reduce-octave (* 3/2 (get-mt -3)))) ; b flat comma
    (17 . ,(reduce-octave (* 3/2 (get-mt -2)))) ; f comma
    (2 .  ,(reduce-octave (* 3/2 (get-mt -1)))) ; c comma
    (23 . ,(reduce-octave (* 3/2 (get-mt 0)))) ; g comma
    (8 .  ,(reduce-octave (* 3/2 (get-mt 1)))) ; d comma
    (29 . ,(reduce-octave (* 3/2 (get-mt 2)))) ; a comma
    (14 . ,(reduce-octave (* 3/2 (get-mt 3)))) ; e comma
    (35 . ,(reduce-octave (* 3/2 (get-mt 4)))) ; b comma
    (19 . ,(reduce-octave (* 3/2 (get-mt 5)))) ; f sharp comma
    (4 .  ,(reduce-octave (* 3/2 (get-mt 6)))) ; c sharp comma
    (25 . ,(reduce-octave (* 3/2 (get-mt 7)))) ; g sharp comma
    (10 . ,(reduce-octave (* 3/2 (get-mt 8)))) ; d sharp comma
    (31 . ,(reduce-octave (* 3/2 (get-mt 9)))) ; a sharp comma
    ))


(defun id->ratio (id tuning)
  (multiple-value-bind (octaves id-class)
      (floor id 36)
    (* (expt 2 octaves) (cdr (assoc id-class tuning)))))



(defparameter *intervals-lantica-31ed2*
  `(("unisono, unisonanza"                   1/1 "" "")
    ("comma [¼ syntonisches Komma]"          ,(expt 81/80 1/4) "" "")
    ("comma [1 syntonisches Komma]"          81/80 "" "")
    ("diesis enarmonico minore"              ,(get-31ed2 2))
    ("diesis enarmonico maggiore"            ,(get-31ed2 3))
    ("semitono minore"                       ,(get-31ed2 3))
    ("semitono maggiore naturale"            ,(get-31ed2 4) "molle" "incitato")
    ("semitono maggiore accidentale"         ,(get-31ed2 4) "molle" "incitato")
    ("tono minore accidentale"               ,(get-31ed2 5))
    ("tono naturale"                         ,(get-31ed2 6) "incitato" "molle")
    ("tono accidentale"                      ,(get-31ed2 6) "incitato" "molle")
    ("tono maggiore accidentale"             ,(get-31ed2 7))
    ("tono maggiore accidentale propinqua"   ,(get-31ed2 8))
    ("terza minima"                          ,(get-31ed2 8))
    ("terza manca del minore"                ,(get-31ed2 8))
    ("terza minore naturale"                 ,(get-31ed2 9))
    ("terza minore accidentale"              ,(get-31ed2 9))
    ("terza minore propinquissima"           6/5)
    ("terza minore propinqua"                ,(get-31ed2 10))
    ("terza più che minore"                  ,(get-31ed2 10))
    ("terza maggiore naturale"               ,(get-31ed2 11))
    ("terza maggiore accidentale"            ,(get-31ed2 11))
    ("terza maggiore propinquissima"         ,(* 5/4 (expt 81/80 1/4)))
    ("terza maggiore propinqua"              ,(get-31ed2 12))
    ("terza più che maggiore"                ,(get-31ed2 12))
    ("[quarta minima]"                       ,(get-31ed2 13))
    ("quarta naturale"                       ,(get-31ed2 14))
    ("quarta accidentale"                    ,(get-31ed2 14))
    ("quarta propinquissima"                 ,(* 4/3 (expt 81/80 1/2)))
    ("quarta propinqua"                      ,(get-31ed2 15))
    ("salto più che di quarta"               ,(get-31ed2 15))
    ("tritono naturale"                      ,(get-31ed2 16))
    ("tritono accidentale"                   ,(get-31ed2 16))
    ("quinta imperfetta"                     ,(get-31ed2 17))
    ("quinta imperfetta propinqua"           ,(get-31ed2 18))
    ("quinta più che imperfetta"             ,(get-31ed2 18))
    ("quinta naturale"                       ,(get-31ed2 19))
    ("quinta accidentale"                    ,(get-31ed2 19))
    ("quinta propinquissima"                 3/2)
    ("quinta propinqua"                      ,(get-31ed2 20))
    ("salto più che di quinta"               ,(get-31ed2 20))
    ("ottava"                                2/1)))

(defparameter *intervals-lantica-mt*
  `(("unisono, unisonanza"                              1/1 "" "")
    ("comma [¼ syntonisches Komma]"                     ,(expt 81/80 1/4) "" "")
    ("Unterschied zwischen 31 ¼-Komma-Quinten und 1:1." ,(get-mt -31))
    ("comma [1 syntonisches Komma]"                     81/80 "" "")
    ("diesis enarmonico minore [temperiert]"            ,(get-mt (+ -12 31)))
    ("diesis enarmonico minore [mitteltönig, 128:125]"  ,(get-mt -12))
    ("diesis enarmonico maggiore [mitteltönig]"         ,(get-mt 7))
    ("semitono minore [mitteltönig]"                       ,(get-mt 7))
    ("diesis enarmonico maggiore [abweichend]"          ,(get-mt (- 7 31)))
    ("semitono minore [abweichend]"                       ,(get-mt (- 7 31)))
    ("semitono maggiore accidentale [abweichend]"         ,(get-mt (+ -5 31)) "molle" "incitato")
    ("semitono maggiore naturale [mitteltönig]"            ,(get-mt -5) "molle" "incitato")
    ("semitono maggiore accidentale [mitteltönig]"         ,(get-mt -5) "molle" "incitato")
    ("tono minore accidentale [abweichend]"               ,(get-mt (+ -17 31)))
    ("tono minore accidentale [mitteltönig]"               ,(get-mt -17))
    ("tono naturale [mitteltönig]"                         ,(get-mt 2) "incitato" "molle")
    ("tono accidentale [mitteltönig]"                      ,(get-mt 2) "incitato" "molle")
    ("tono accidentale [abweichend]"                      ,(get-mt (- 2 31)) "incitato" "molle")
    ("tono maggiore accidentale [abweichend]"             ,(get-mt (+ -10 31)))
    ("tono maggiore accidentale [mitteltönig]"             ,(get-mt -10))
    ("tono maggiore accidentale propinqua [mitteltönig]"   ,(get-mt 9))
    ("terza minima [mitteltönig]"                          ,(get-mt 9))
    ("terza manca del minore [mitteltönig]"                ,(get-mt 9))
    ("tono maggiore accidentale propinqua [abweichend]"   ,(get-mt (- 9 31)))
    ("terza minima [abweichend]"                          ,(get-mt (- 9 31)))
    ("terza manca del minore [abweichend]"                ,(get-mt (- 9 31)))
    ("terza minore accidentale [abweichend]"              ,(get-mt (+ -3 31)))
    ("terza minore naturale [mitteltönig]"                 ,(get-mt -3))
    ("terza minore accidentale [mitteltönig]"              ,(get-mt -3))
    ("terza minore propinquissima [6:5]"                   6/5)
    ("terza minore propinqua [abweichend]"                ,(get-mt (+ -15 31)))
    ("terza più che minore [abweichend]"                  ,(get-mt (+ -15 31)))
    ("terza minore propinqua [mitteltönig]"                ,(get-mt -15))
    ("terza più che minore [mitteltönig]"                  ,(get-mt -15))
    ("terza maggiore naturale [mitteltönig, 5:4]"               ,(get-mt 4))
    ("terza maggiore accidentale [mitteltönig, 5:4]"            ,(get-mt 4))
    ("terza maggiore propinquissima"         ,(* 5/4 (expt 81/80 1/4)))
    ("terza maggiore accidentale [abweichend]"            ,(get-mt (- 4 31)))
    ("terza maggiore propinqua [abweichend]"              ,(get-mt (+ -8 31)))
    ("terza più che maggiore [abweichend]"                ,(get-mt (+ -8 31)))
    ("terza maggiore propinqua [mitteltönig]"              ,(get-mt -8))
    ("terza più che maggiore [mitteltönig]"                ,(get-mt -8))
    ("[quarta minima, mitteltönig]"                       ,(get-mt 11))
    ("[quarta minima, abweichend]"                       ,(get-mt (- 11 31)))
    ("quarta accidentale [abweichend]"                    ,(get-mt (+ -1 31)))
    ("quarta naturale [mitteltönig]"                       ,(get-mt -1))
    ("quarta accidentale [mitteltönig]"                    ,(get-mt -1))
    ("quarta propinquissima"                 ,(* (get-mt -1) (expt 81/80 1/4)))
    ("quarta propinqua [abweichend]"                      ,(get-mt (+ -13 31)))
    ("salto più che di quarta [abweichend]"               ,(get-mt (+ -13 31)))
    ("quarta propinqua [mitteltönig]"                      ,(get-mt -13))
    ("salto più che di quarta [mitteltönig]"               ,(get-mt -13))
    ("tritono naturale [mitteltönig]"                      ,(get-mt 6))
    ("tritono accidentale [mitteltönig]"                   ,(get-mt 6))
    ("tritono accidentale [abweichend]"                   ,(get-mt (- 6 31)))
    ("quinta imperfetta accidentale [abweichend]"                     ,(get-mt (+ -6 31)))
    ("quinta imperfetta naturale [mitteltönig]"                     ,(get-mt -6))
    ("quinta imperfetta accidentale [mitteltönig]"                     ,(get-mt -6))
    ("quinta imperfetta propinqua [abweichend]"           ,(get-mt (+ -18 31)))
    ("quinta più che imperfetta [abweichend]"             ,(get-mt (+ -18 31)))
    ("quinta imperfetta propinqua [mitteltönig]"           ,(get-mt -18))
    ("quinta più che imperfetta [mitteltönig]"             ,(get-mt -18))
    ("quinta naturale [mitteltönig]"                       ,(get-mt 1))
    ("quinta accidentale [mitteltönig]"                    ,(get-mt 1))
    ("quinta accidentale [abweichend]"                    ,(get-mt (- 1 31)))
    ("quinta propinquissima [abweichend]"                 3/2)
    ("quinta propinqua [abweichend]"                      ,(get-mt (+ -11 31)))
    ("salto più che di quinta [abweichend]"               ,(get-mt (+ -11 31)))
    ("quinta propinqua [mitteltönig]"                      ,(get-mt -11))
    ("salto più che di quinta [mitteltönig]"               ,(get-mt -11))
    ("ottava"                                2/1)))

(defparameter *intervals-pythagorean*
  `(("Unisono (1:1)" 1/1)
    ("Komma (531441:524288)" 531441/524288)
    ("Limma, semitonus minor (256:243)" 256/243)
    ("Apotome, semitonus major (2187:2048)" 2187/2048)
    ("Tonus (9:8, sesquiottava)" 9/8)
    ("Semiditonus (32:27)" 32/27)
    ("Ditonus (81:64)" 81/64)
    ("Diatessaron (4:3, sesquitertia)" 4/3)
    ("Diapente (3:2, sesquialtera)" 3/2)
    ("Hexachordum minor (128:81)" 128/81)
    ("Hexachordum minor (27:16)" 27/16)
    ("Diapason (2:1, dupla)" 2/1)))

(defparameter *intervals-just*
  `(("1:1 [Unisono]" 1/1)
    ("81:80 [syntonisches Komma]" 81/80)
    ("25:24 [chromatischer Halbton, übermässige Prime]" 25/24)
    ("16:15 [diatonischer Halbton, kleine Sekunde]" 16/15)
    ("10:9 [kleiner Ganzton, grosse Sekunde]" 10/9)
    ("9:8 [grosser Ganzton, grosse Sekunde]" 9/8)
    ("8:7 [septimaler Ganzton]" 8/7)
    ("7:6 [septimale Terz]" 7/6)
    ("6:5 [kleine Terz]" 6/5)
    ("5:4 [grosse Terz]" 5/4)
    ("4:3 [Quarte]" 4/3)
    ("3:2 [Quinte]" 3/2)
    ("8:5 [kleine Sexte]" 8/5)
    ("5:3 [grosse Sexte]" 5/3)
    ("7:4 [natürliche Septime]" 7/4)
    ("2:1 [Oktave]" 2/1)))
