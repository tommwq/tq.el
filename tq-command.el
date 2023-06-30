(defun tq-open (buffer-type)
  "打开新的缓冲区，并设置对应的模式。"
  (interactive "sbuffer type: ")
  (let* ((buffer-name (format "*temporary-%s*" buffer-type))
         (mode-table (tq-util-make-hash "java" #'java-mode
                                        "go" #'go-mode
                                        "xml" #'xml-mode
                                        "c" #'c-mode
			                                  "c++" #'c++-mode
                                        "powershell" #'powershell-mode
                                        "shell" #'shell-mode
                                        "lisp" #'lisp-interaction-mode
                                        "python" #'python-mode
                                        "r" #'r-mode
                                        "org" #'org-mode
                                        "javascript" #'javascript-mode
                                        "css" #'css-mode
                                        "sql" #'sql-mode
                                        "gradle" #'groovy-mode
			                                  "kotlin" #'kotlin-mode
			                                  "dockerfile" #'dockerfile-mode
			                                  "typescript" #'typescript-mode
                                        "elisp" #'lisp-interaction-mode
                                        "php" #'php-mode
                                        "plantuml" #'plantuml-mode
                                        "" #'text-mode))
         (mode-setter (gethash buffer-type mode-table)))
    (unless mode-setter (setf mode-setter #'text-mode))
    (switch-to-buffer buffer-name)
    (funcall mode-setter)))

(defun tq-set-font (&optional font-size)
  "设置字体大小。"
  (interactive "n字体大小: ")
  (if (and
       (boundp 'tq-font-size)
       (boundp 'tq-latin-font)
       (boundp 'tq-chinese-font))
      (let* ((size (or font-size tq-font-size))
             (latin-font (format "%s-%d" tq-latin-font size))
             (chinese-font (format "%s-%d" tq-chinese-font size)))
        (set-frame-font latin-font)
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font t charset chinese-font)))))

(defun tq-reformat-cpp ()
  (interactive)
  (untabify (point-min) (point-max))
  (beginning-of-buffer)
  (replace-regexp " +" " ")
  (beginning-of-buffer)
  (replace-string "\n{\n" " {\n")
  (beginning-of-buffer)
  (replace-string "( " "(")
  (beginning-of-buffer)
  (replace-string " )" ")")
  (beginning-of-buffer)
  (replace-string " ;" ";")
  (beginning-of-buffer)
  (replace-regexp ")[ \n\r]+{" ") {")
  (indent-region (point-min) (point-max)))


(provide 'tq-command)
