(defparameter buffer ())
(defparameter data nil)
(defparameter byte-counter 0)

(defun p (output) 
  (format t "~A" output))

(defun pn (output n) 
  ;; https://stackoverflow.com/a/24758778/13615958
  (format t "~v@{~A~:*~}" n output))

(defun sn (output n) 
  ;; https://stackoverflow.com/a/24758778/13615958
  (format nil "~v@{~A~:*~}" n output))

(defun nl () 
  (format t "~%"))

(defun append-buffer (byte) 
  (setf buffer (append buffer (list byte))))

(defun read-to-buffer (file-path)
  (with-open-file 
      (stream 
        file-path
        :element-type '(unsigned-byte 8)
        :if-does-not-exist nil)
    (defun read-stream ()
      (progn
        (setf data (read-byte stream nil nil))
        (if data
            (progn
              (append-buffer (write-to-string data :base 16))
              (read-stream)))))
    (if stream 
        (read-stream)
        (progn 
          (p "file couldn't found") 
          (nl)))))

(defun join (str-list)
  (reduce 
    #'(lambda (a b)
        (format nil "~A ~A" a b))
    (mapcar 
      #'(lambda (str)
          (format nil "~A~A"
                  (sn "0" (- 2 (length str)))
                  str))
      str-list)))

(defun slice (l start end)
  (if (< start (length l))
      (subseq l start (if (< end (length l))
                          end
                          (length l)))))

(defun buffer-to-string (str-list)
  (reduce 
    #'(lambda (a b)
        (format nil "~A~A" a b))
    (mapcar 
      #'(lambda (str) 
          (let* ((ascii (parse-integer str :radix 16))
                (c (code-char ascii)))
            (if (and c (not (= ascii 10)) (not (= ascii 13))) 
                (format nil "~A" c)
                (format nil "."))))
      str-list)))

(defun dump-row-address (byte-count)
  (let ((hex-byte-count (write-to-string byte-count :base 16)))
    (progn 
      (pn "0" (- 8 (length hex-byte-count)))
      (p hex-byte-count)
      (p "  "))))

(defun dump () 
  (let* ((current-row (floor byte-counter 16))
         (row-buffer (slice buffer (* current-row 16) (+ (* current-row 16) 16)))
         (row-l (slice row-buffer 0 8))
         (row-r (slice row-buffer 8 16)))
    (if (< byte-counter (length buffer))
        (progn
          (dump-row-address byte-counter)
          (p (join row-l))
          (p "  ")
          (if row-r (p (join row-r)))
          (if (< (length row-buffer) 16)
              (progn
                (pn "  " (- 16 (length row-buffer)))
                (pn " " (- 15 (length row-buffer)))
                (if (< (- 16 (length row-buffer)) 2)
                    (p " "))))
          (p "   |")
          (p (buffer-to-string row-buffer))
          (p "|")
          (nl)
          (setf byte-counter (+ byte-counter 16))
          (dump)))))

(read-to-buffer "./testfile")
(nl)

(mapcar #'(lambda (byte) 
            (p (length byte))) 
        buffer)
(nl)
(p (subseq buffer 0 1))
(nl)
(dump-row-address 16)
(nl)
(dump)

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
