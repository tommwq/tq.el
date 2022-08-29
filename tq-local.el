(setq backup-directory-alist (quote (("." . "~/.backups"))))
;; 设置工作目录
(setf tq-working-directory "D:/workspace/project/")
(setf default-directory tq-working-directory)
(setf tq-note-directory (concat tq-working-directory "notes"))
(setf tq-record-directory "d:/workspace/project/note/日志和计划/");
