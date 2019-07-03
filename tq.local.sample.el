
(defun tq-set-font ()
  (set-frame-font "Times New Roman-16")
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font t charset
		              (font-spec :family "华文中宋" :size 20))))


  (setq backup-directory-alist (quote (("." . "D:/workspace/temporary/emacsbackup"))))
  (tq-set-font)

  ;; 设置工作目录
  (setf default-directory "D:/Workspace/")


