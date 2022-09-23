;; tq.el
;; 常用功能库
;; 2019年09月22日

;; usage:
;; (add-to-list 'load-path "/path/to//tq.el/")
;; (require 'tq)

;; 加载本目录下tq-*.el文件。
(dolist (part '(tq-local
                tq-util
                tq-string
                tq-java
                tq-file
                tq-math
                tq-command
                tq-psp
                tq-tmp
                tq-settings))
  (let ((file-name (format "%s.el" (prin1-to-string part))))
    (message (expand-file-name file-name (file-name-directory load-file-name)))
    (load (expand-file-name file-name (file-name-directory load-file-name)))))

(provide 'tq)

