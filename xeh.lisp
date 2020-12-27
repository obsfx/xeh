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
                    (val (nth i base-list))
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
          (dump (get-row-address byte-pos)) (dump "  ")
          (dump (join left " ")) (dump "  ")
          (dump (join right " ")) (dump "  ")
          (dump "|")
          (dump (join (hex-list-to-str-list (slice buffer byte-pos 16))))
          (dump "|")
          (newline)
          (dump-hex-row buffer (+ byte-pos 16))))))

(defparameter buffer nil)
;; https://stackoverflow.com/a/20048719/13615958
(defparameter file-path (nth 1 *posix-argv*))

(if file-path 
    (setf buffer (read-file-bytes file-path)))

(if (not buffer)
    (progn 
      (format t "xeh: ~A: File couldn't found" file-path)
      (quit)))

(dump buffer)
(newline)
(setf buffer (format-single-char-hex-values buffer))

(newline)

(dump-hex-row buffer 0)
(dump (get-row-address (length buffer)))
;(dump (fixed-slice buffer 20 (length buffer)))

;(loop for i from 0 to ( - (length buffer) 1)
;      do (let ((hex (nth i buffer)))
;           (dump hex)))

;(loop for x in buffer
;      do (dump x))
; (format nil "~&~S~&" *posix-argv*)

;(defun read-to-buffer (file-path)
;  (with-open-file 
;      (stream 
;        file-path
;        :element-type '(unsigned-byte 8)
;        :if-does-not-exist nil)
;    (defun read-stream ()
;      (progn
;        (setf data (read-byte stream nil nil))
;        (if data
;            (progn
;              (append-to-buffer (write-to-string data :base 16))
;              (read-stream)))))
;    (if stream 
;        (read-stream)
;        (progn 
;          (p "file couldn't found") 
;          (nl)))))
;
;(defun join (str-list)
;  (reduce 
;    #'(lambda (a b)
;        (format nil "~A ~A" a b))
;    (mapcar 
;      #'(lambda (str)
;          (format nil "~A~A"
;                  (sn "0" (- 2 (length str)))
;                  str))
;      str-list)))
;
;
;
;(defun dump () 
;  (let* ((current-row (floor byte-counter 16))
;         (row-buffer (slice buffer (* current-row 16) (+ (* current-row 16) 16)))
;         (row-l (slice row-buffer 0 8))
;         (row-r (slice row-buffer 8 16)))
;    (if (< byte-counter (length buffer))
;        (progn
;          (dump-row-address byte-counter)
;          (p (join row-l))
;          (p "  ")
;          (if row-r (p (join row-r)))
;          (if (< (length row-buffer) 16)
;              (progn
;                (pn "  " (- 16 (length row-buffer)))
;                (pn " " (- 15 (length row-buffer)))
;                (if (< (- 16 (length row-buffer)) 2)
;                    (p " "))))
;          (p "   |")
;          (p (buffer-to-string row-buffer))
;          (p "|")
;          (nl)
;          (setf byte-counter (+ byte-counter 16))
;          (dump)))))
;
;(read-to-buffer "./testfile")
;(nl)
;
;(mapcar #'(lambda (byte) 
;            (p (length byte))) 
;        buffer)
;(nl)
;(p (subseq buffer 0 1))
;(nl)
;(dump-row-address 16)
;(nl)
;(dump)

;(if (not (boundp 'buffer)) (p "var") (p "yok"))

;(with-open-file 
;    (stream 
;      "./testfile" 
;        :element-type '(unsigned-byte 8) 
;        :if-does-not-exist nil)
;  (defun through-stream () 
;    (progn 
;      (setf data (read-byte stream nil nil))
;      (if data 
;          (progn 
;            (format t "~a~%" (write-to-string data :base 16))
;            (format t "~a~%" (code-char data))
;            (through-stream)))))
;  (if stream 
;      (through-stream)
;      (format t "file couldn't found~%")))

; (defparameter data 5)
; (defun through-stream (data)
;   (if (> data 0)
;       (format t "~a " data)
;       (progn (setf data (- data 1))
;        (through-stream data))))
; 
; (through-stream data)
