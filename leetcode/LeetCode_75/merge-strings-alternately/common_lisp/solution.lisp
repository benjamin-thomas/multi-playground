; echo ./solution.lisp | entr -c sbcl --script /_

(defun red (str)
  (format nil "~C[31m~a~C[m" #\Esc str #\Esc))

(defun green (str)
  (format nil "~C[32m~a~C[m" #\Esc str #\Esc))

(defun assert-equal (a b)
    (if (equal a b)
        (format t (green "🟢 PASS: ~a~%") a)
        (format t (red   "🔴 FAIL: ~a != ~a~%") a b)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun merge-str (str-a str-b)
    (let* ((len-a (length str-a))
            (len-b (length str-b))
            (*buf* (make-string (+ len-a len-b) :initial-element #\♥))
            (*i* 0)
            (*j* 0)
            )

            (loop while (< *j* (max len-a len-b)) do
                (when (< *j* len-a)
                    (setf (char *buf* *i*) (char str-a *j*))
                    (incf *i*)
                )
                (when (< *j* len-b)
                    (setf (char *buf* *i*) (char str-b *j*))
                    (incf *i*)
                )
                (incf *j*))

            *buf*))


(assert-equal "AaBbCc" (merge-str"ABC" "abc"))
(assert-equal "AaBb_CDE" (merge-str"AB_CDE" "ab"))
(assert-equal "AaBb_cde" (merge-str"AB" "ab_cde"))
(assert-equal "ABC" (merge-str"ABC" ""))
(assert-equal "abc" (merge-str"" "abc"))
(assert-equal "" (merge-str "" ""))

(format t "~%DONE!~%")


