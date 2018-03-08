

;; 将字符串转移为tex可识别的字符串。
(defmacro escape-to-tex (string)
  `(concat ,@(mapcar #'tex-underline-escape-decorator string)))

;; 将"_"转换为"\\_"以便在tex中使用。
(defun tex-underline-escape-decorator (ch)
  (if (char-equal ch ?_)
      "\\_"
    (char-to-string ch)))










