(in-package :keyboard-generator)


;;;; tex / pikz output generation

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
