(require 'subr-x)

(defun tq-str-ascii-in-range-p (char first last)
  "检测字符ASCII编码是否在区间[first,last]内。"
  (let ((ascii (string-to-char char)))
    (and (<= first ascii) (<= ascii last))))

(defun tq-str-upcase-p (char)
  "检测大写字母。"
  (tq-str-ascii-in-range-p char #x41 #x5a))

(defun tq-str-downcase-p (char)
  "检测小写字母。"
  (tq-str-ascii-in-range-p char #x61 #x7a))

(defun tq-str-digit-p (char)
  "检测数字。"
  (tq-str-ascii-in-range-p char #x30 #x39))

(defun tq-str-remove-last (string n)
  "去除字符串尾部n个字符。"
  (let ((position (- (length string) n)))
    (if (< position 0)
        ""
      (substring string 0 position))))

(defun tq-str-get-last (string n)
  "返回字符串尾部n个字符。"
  (let ((prefix-length (- (length string) n)))
    (if (< prefix-length 0)
        string
      (substring string prefix-length))))

(defun tq-str-plural (singular)
  "返回复数。

如果singular以es结尾，返回singular。如果以y结尾，返回-ies。如果以s结尾，返回-es。"
  (cond ((string-suffix-p singular "y") (concat (tq-str-remove-last singular 1) "ies"))
        ((string-suffix-p singular "es") singular)
        ((string-suffix-p singular "s") (concat singular "es"))
        (t (concat singular "s"))))

(defun tq-str-split-pascal-case (pascal-case)
  "分解PascalCase格式字符串，返回小写单词列表。"
  (let ((words nil)
        (begin 0))
    (dotimes (index (length pascal-case))
      (when (tq-str-upcase-p (substring pascal-case index (1+ index)))
        (if (not (= begin index))
            (push (downcase (substring pascal-case begin index)) words))
        (setf begin index)))
    (push (downcase (substring pascal-case begin)) words)
    (nreverse words)))

(defun tq-str-split-camel-case (camel-case)
  "分解cascalCase格式字符串，返回小写单词列表。"
  (let ((words nil)
        (begin 0))
    (dotimes (index (length camel-case))
      (when (tq-str-upcase-p (substring camel-case index (1+ index)))
        (if (not (= begin index))
            (push (downcase (substring camel-case begin index)) words))
        (setf begin index)))
    (push (downcase (substring camel-case begin)) words)
    (nreverse words)))

(defun tq-str-split-kebab-case (kebab-case)
  "分解kebab-case格式字符串，返回小写单词列表。"
  (let ((words nil))
    (dolist (word (split-string kebab-case "-"))
      (push (downcase word) words))
    (nreverse words)))

(defun tq-str-split-snake-case (snake-case)
  "分解snake_case格式字符串，返回小写单词列表。"
  (let ((words nil))
    (dolist (word (split-string snake-case "_"))
      (push (downcase word) words))
    (nreverse words)))

(defun tq-str-split-slash (slash-string)
  "分解由斜杠分隔的字符串，返回单词列表。"
  (let ((words nil)
        (begin 0))
    (dotimes (index (length slash-string))
      (when (string-equal "/" (substring slash-string index (1+ index)))
        (if (not (= begin index))
            (push (substring slash-string begin index) words))
        (setf begin (1+ index))))
    (push (substring slash-string begin) words)
    (nreverse words)))

(defun tq-str-join-kebab-case (downcase-list)
  (string-join downcase-list "-"))

(defun tq-str-join-camel-case (downcase-list)
  (let ((first (pop downcase-list)))
    (concat first (string-join (mapcar (lambda (s) (capitalize s)) downcase-list) ""))))

(defun tq-str-join-pascal-case (downcase-list)
  (string-join (mapcar (lambda (s) (capitalize s)) downcase-list) ""))

(defun tq-str-join-snake-case (downcase-list)
  (string-join downcase-list "_"))

(defun tq-str-pascal-to-camel (pascal-case)
  (tq-str-join-camel-case (mapcar #'downcase (tq-str-split-pascal-case pascal-case))))

(defun tq-str-pascal-to-kebab (pascal-case)
  (tq-str-join-kebab-case (mapcar #'downcase (tq-str-split-pascal-case pascal-case))))

(defun tq-str-pascal-to-snake (pascal-case)
  (tq-str-join-snake-case (mapcar #'downcase (tq-str-split-pascal-case pascal-case))))

(defun tq-str-kebab-to-camel (kebab-case)
  (tq-str-join-camel-case (mapcar #'downcase (tq-str-split-kebab-case kebab-case))))

(defun tq-str-kebab-to-pascal (kebab-case)
  (tq-str-join-pascal-case (mapcar #'downcase (tq-str-split-kebab-case kebab-case))))

(defun tq-str-kebab-to-snake (kebab-case)
  (tq-str-join-snake-case (mapcar #'downcase (tq-str-split-kebab-case kebab-case))))

(defun tq-str-camel-to-kebab (camel-case)
  (tq-str-join-kebab-case (mapcar #'downcase (tq-str-split-camel-case camel-case))))

(defun tq-str-camel-to-pascal (camel-case)
  (tq-str-join-pascal-case (mapcar #'downcase (tq-str-split-camel-case camel-case))))

(defun tq-str-camel-to-snake (camel-case)
  (tq-str-join-snake-case (mapcar #'downcase (tq-str-split-camel-case camel-case))))

(defun tq-str-snake-to-kebab (snake-case)
  (tq-str-join-kebab-case (mapcar #'downcase (tq-str-split-snake-case snake-case))))

(defun tq-str-snake-to-pascal (snake-case)
  (tq-str-join-pascal-case (mapcar #'downcase (tq-str-split-snake-case snake-case))))

(defun tq-str-snake-to-camel (snake-case)
  (tq-str-join-camel-case (mapcar #'downcase (tq-str-split-snake-case snake-case))))

(defun tq-str-first-char (s)
  "返回字符串首字母。"
  (if (zerop (length s))
      ""
    (string (nth 0 (string-to-list s)))))

(defun tq-str-stringify (symbol)
  "将对象或符号转换为字符串。"
  (if (stringp symbol)
      symbol
    (prin1-to-string symbol)))

(defun tq-str-upcase-first-char (word)
  "将字符串首字母改为大写。"
  (if (zerop (length word))
      word
    (concat (upcase (substring word 0 1))
            (substring word 1))))

(defun tq-str-downcase-first-letter (word)
  "将字符串首字母改为小写。"
  (if (zerop (length word))
      word
    (concat (downcase (substring word 0 1))
            (substring word 1))))

(defun tq-str-escape-xml (text)
  "对text中的特殊字符按照XML转义规则进行转义。"
  (dolist (replacement '(("&" "&amp;")
                         ("\"" "&quot;")
                         ("'" "&apos;")
                         ("<" "&lt;")
                         (">" "&gt;" )))
    (setf text (replace-regexp-in-string (nth 0 replacement) (nth 1 replacement) text)))
  text)

(defun tq-str-path-join (root &rest path-parts)
  "拼接文件路径。"
  (dolist (part path-parts)
    (setf root (expand-file-name part root)))
  root)

(provide 'tq-str)
