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

(defun tq-format-cpp ()
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

(defun tq-record ()
  "打开日记录文件。如果文件不存在，创建文件。

日记录文件位于周记录目录中，周记录目录是tq-record-directory目录下的一个子目录。
周记录目录名字类似2022-q3-w35，日记录文件名字类似2022-q3-w35-20220829.org。
"
  (interactive)
  (let* ((year (format-time-string "%Y"))
         (season (number-to-string (tq-season-number)))
         (date (format-time-string "%Y年%m月%d日"))
         (week-record-directory-name 
          (format-time-string (concat "%Y-q" season  "-w%V")))
         (day-record-file-name
          (format-time-string (concat "%Y-q" season  "-w%V-%Y%m%d.org")))
         (root-path nil))
    ;; 保证根目录存在。
    (dolist (sub (list tq-record-directory year (concat year "-q" season) week-record-directory-name))
      (setf root-path (expand-file-name sub root-path)))
    (if (not (file-exists-p root-path))
        (make-directory root-path t))
    (setf day-record-file-name (expand-file-name day-record-file-name root-path))
    (if (file-exists-p day-record-file-name)
        (find-file day-record-file-name)
      (tq-file-write-and-open day-record-file-name
                              (tq-template-render-sequence "# -*- mode: org -*-
#+options: ^:nil
#+todo: todo(t) delay(y@/!) | done(d@/!) cancel(c@/!)
#+title: ${date}
#+date: ${date}
* 工作
* 后续
* 记录
" "date" date)))))

(provide 'tq-command)
