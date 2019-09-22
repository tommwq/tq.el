(defun tq-has-prefix (str prefix)
  "判断str是否包含前缀prefix。"
  (string= prefix (substring str 0 (min (length prefix) (length str)))))

(defun tq-has-suffix (str suffix)
  "判断str是否包含后缀suffix。"
  (let ((ls (length str))
        (lx (length suffix)))
    (and (<= lx ls)
         (string= suffix (substring str (- ls lx))))))

(defun tq-first-char (s)
  "返回字符串首字母。"
  (if (zerop (length s))
      ""
    (string (nth 0 (string-to-list s)))))

(defun tq-stringify (symbol)
  "将对象或符号转换为字符串。"
  (if (stringp symbol)
      symbol
    (prin1-to-string symbol)))

(defun tq-upcase-first-letter (word)
  "将字符串首字母改为大写。"
  (if (zerop (length word))
      word
    (concat (upcase (substring word 0 1))
            (substring word 1))))

(defun tq-downcase-first-letter (word)
  "将字符串首字母改为小写。"
  (if (zerop (length word))
      word
    (concat (downcase (substring word 0 1))
            (substring word 1))))

(defun tq-snake-to-camel (snake)
  "将字符串button_width转换为buttonWidth。"
  (let* ((pieces (split-string snake "_")))
    (concat (downcase (car pieces))
            (mapconcat #'capitalize (cdr pieces) ""))))

(defun tq-snake-to-pascal (snake)
  "将字符串button_width转换为ButtonWidth。"
  (mapconcat #'capitalize (split-string snake "_") ""))

;; 将字符串转移为tex可识别的字符串。
(defmacro escape-to-tex (string)
  `(concat ,@(mapcar #'tex-underline-escape-decorator string)))

;; 将"_"转换为"\\_"以便在tex中使用。
(defun tex-underline-escape-decorator (ch)
  (if (char-equal ch ?_)
      "\\_"
    (char-to-string ch)))

(defun tq-escape-xml (text)
  "对text中的特殊字符按照XML转义规则进行转义。"
  (dolist (pair '(("&" "&amp;")
                  ("\"" "&quot;")
                  ("'" "&apos;")
                  ("<" "&lt;")
                  (">" "&gt;" )))
    (setf text (replace-regexp-in-string (nth 0 pair) (nth 1 pair) text)))
  text)

(defun tq-render-template (template hash)
  "渲染模板。将template中的字符串${foo}更换为hash[foo]的值。"
  (let ((holder "")
        (value "")
        (result template))
    (dolist (key (hash-table-keys hash))
      (setf holder (format "${%s}" key))
      (setf value (gethash key hash))
      (setf result (replace-regexp-in-string holder value result)))
    result))

(defun tq-render-template-from-sequence (template &rest sequence)
  "渲染模板。从sequence建立散列表进行渲染。"
  (tq-render-template template (apply #'tq-make-string-hash sequence)))
