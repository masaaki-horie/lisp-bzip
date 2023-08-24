(defpackage my-compress
  (:use :cl)
  (:export :compress)
  (:export :decompress))
(in-package :my-compress)

(defun compress (filename)
  (write-line (concatenate 'string "my-compress/src/main.lisp !" filename)))

(defun rotate (in)
  (let ((in-list (coerce in 'list)))
    (concatenate 'string
                 (coerce (cdr in-list) 'string)
                 (string (car in-list)))))

(defun generate-rotated-strings (in)
  (labels ((rec (in aux count lim)
             (if (eq count lim)
                 (nreverse aux)
                 (rec (rotate in)
                      (cons (cons in count) aux)
                      (1+ count)
                      lim))))
    (rec in nil 0 (length in))))

(defun generate-sorted-strings (in)
  (stable-sort
   (generate-rotated-strings in)
   #'string<
   :key #'car))

(defun extract-order (list)
  (mapcar #'cdr list))

(defun find-zero-index (list)
  (labels ((rec (list aux)
             (if (zerop (car list)) aux
                 (rec (cdr list) (1+ aux)))))
    (rec list 0)))

(defun generate-bwt (list)
  (coerce
   (map 'list
        (lambda (elem)
          (car (get-last-character (car elem))))
        list) 'string))

(defun get-last-character (str)
  (last (coerce str 'list)))

(defun bwt-block (str)
  (let ((sorted-strs (generate-sorted-strings str)))
    (cons (generate-bwt sorted-strs)
          (find-zero-index (extract-order sorted-strs)))))

(defun RLE-output (list)
  (if
   (null list) nil
   (let ((elem (car list)))
     (progn 
       (cond
         ((and (numberp (car elem)) (null (cdr elem)))
          (format t "~A " (* -1 (car elem))))
         ((and (null (car elem)) (characterp (cdr elem)))
          (format t "~A " (cdr elem)))
         ((and (numberp (car elem)) (characterp (cdr elem)))
          (format t "~A,~A " (princ-to-string (car elem)) (string (cdr elem))))
         (t nil))
       (RLE-output (cdr list))))))

(defun RLE (in)
  (labels
      ((RLE-main (in aux char shift length-function)
         (if (null in) (nreverse aux)
             (if (zerop shift)
                 (let* ((next-func
                          (if (eq length-function #'same-length)
                              #'diff-length
                              #'same-length))
                        (run-length (funcall next-func in)))
                   (RLE-main
                    in
                    (if (zerop run-length)
                        aux
                        (if (eq next-func #'same-length)
                            (cons (cons run-length (car in)) aux)
                            (cons (cons run-length nil) aux)))
                    (car in)
                    run-length
                    next-func))
                 (RLE-main
                  (cdr in)
                  (if (eq length-function #'diff-length)
                      (cons (cons nil (car in)) aux)
                      aux)
                  char
                  (1- shift)
                  length-function)))))
    (RLE-main in nil nil 0 #'same-length)))

(defun diff-length (in)
  (labels
      ((diff-length-rec (in aux cnt)
         (if (or (eq (car in) aux) (null in) (> cnt 127))
             (1- cnt)
             (diff-length-rec (cdr in) (car in) (1+ cnt)))))
    (diff-length-rec in nil 0)))

(defun same-length (in)
  (labels
      ((same-length-rec (in aux cnt)
         (if (or (not (eq (car in) aux)) (null in) (> cnt 126))
             cnt
             (same-length-rec (cdr in) aux (1+ cnt)))))
    (same-length-rec in (car in) 0)))

(defun RLE-decode (list)
  (labels
      ((decode-rec (list aux)
         (if (null list) (nreverse aux)
             (cond
               ((and (numberp (caar list)) (characterp (cdar list)))
                (decode-rec (cdr list)
                            (concatenate
                             'string
                             (repeat-string
                              (caar list)
                              (string (cdar list)))
                             aux)))
               ((and (null (caar list)) (characterp (cdar list)))
                (decode-rec (cdr list)
                            (concatenate
                             'string
                             (string (cdar list))
                             aux)))
               ((and (numberp (caar list)) (null (cdar list)))
                (decode-rec (cdr list) aux))
               (t nil)))))
    (decode-rec list "")))

(defun repeat-string (count char)
  (labels
      ((main (count char aux)
         (if (zerop count) aux
             (main (1- count) char (concatenate 'string aux char)))))
    (main count char "")))

(defun decompress (filename)
  ())

