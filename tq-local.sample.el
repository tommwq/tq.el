
(defun tq-set-font ()
  (set-frame-font "Times New Roman-14")
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font t charset
		              (font-spec :family "新宋体" :size 18))))


  (setq backup-directory-alist (quote (("." . "D:/workspace/temporary/emacsbackup"))))
  (tq-set-font)

  ;; 设置工作目录
  (setf default-directory "D:/Workspace/")

