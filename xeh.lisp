;;; xeh
;;; A command-line Canonical HEX+ASCII display tool written in ~100 lines of Common Lisp.
;;; https://github.com/obsfx/xeh

(defun dump (output) 
  (format t "~A" output))

(defun dump-n (output n) 
  ;; https://stackoverflow.com/a/24758778/13615958
  (format t "~v@{~A~:*~}" n output))

(defun repeat-n (output n) 
  ;; https://stackoverflow.com/a/24758778/13615958
  (format nil "~v@{~A~:*~}" n output))

(defun newline () 
  (format t "~%"))

(defun slice (l start len)
  (if (< start (length l))
      (let ((end (+ start len)))
        (subseq l start (if (< end (length l)) end (length l))))))

(defun fixed-slice (l start len)
  (let ((base-list (make-list len :initial-element "  ")))
    (loop for i from 0 to (- len 1)
          do (let* ((hex (nth (+ start i) l))
                    (placed-val (if hex hex "  ")))
               (setf (nth i base-list) placed-val)))
    (values base-list)))

(defun join (str-list &optional char)
  (reduce 
    #'(lambda (a b)
        (format nil "~A~A~A" a (if char char "") b))
    str-list))

(defun format-single-char-hex-values (hex-list)
  (mapcar 
    #'(lambda (hex)
        (format nil "~A~A" (repeat-n "0" (- 2 (length hex))) hex))
    hex-list))

(defun get-row-address (byte-pos)
  (let ((hex-byte-pos (write-to-string byte-pos :base 16)))
    (format nil "~A~A" (repeat-n "0" (- 8 (length hex-byte-pos))) hex-byte-pos)))

(defun hex-list-to-str-list (str-list)
  (mapcar 
    #'(lambda (str) 
        (let* ((ascii (parse-integer str :radix 16))
               (c (code-char ascii)))
          (if (and c (> ascii 31) (< ascii 127)) c (format nil "."))))
    str-list))

(defun read-file-bytes (file-path)
  (with-open-file 
      (stream 
        file-path
        :element-type '(unsigned-byte 8)
        :if-does-not-exist nil)
    ;; https://stackoverflow.com/a/3814098/13615958
    ;; https://lispcookbook.github.io/cl-cookbook/iteration.html
    (if stream
        (loop for byte = (read-byte stream nil nil)
          while byte
          collect (write-to-string byte :base 16)))))

(defun dump-hex-row (buffer byte-pos)
  (if (< byte-pos (length buffer))
      (let* ((row (fixed-slice buffer byte-pos 16))
             (left (subseq row 0 8))
             (right (subseq row 8 16)))
        (progn
          (dump (string-downcase (get-row-address byte-pos))) (dump "  ")
          (dump (string-downcase (join left " "))) (dump "  ")
          (dump (string-downcase (join right " "))) (dump "  ")
          (dump "|")
          (dump (join (hex-list-to-str-list (slice buffer byte-pos 16))))
          (dump "|")
          (newline)
          (dump-hex-row buffer (+ byte-pos 16))))
      (progn
        (dump (string-downcase (get-row-address (length buffer))))
        (newline))))

(defparameter buffer nil)
(defparameter file-path nil)
(defun main()
    (progn
      (setf file-path (second *posix-argv*))
      (if file-path 
          (setf buffer (read-file-bytes file-path)))
      (if (not buffer)
          (progn 
            (format t "xeh: ~A: File couldn't found" file-path)
            (quit)))
      (setf buffer (format-single-char-hex-values buffer))
      (dump-hex-row buffer 0)))
