
(defun tq-set-font ()
  (set-frame-font "Times New Roman-16")
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font t charset
		              (font-spec :family "华文中宋" :size 20))))

(progn
  (setq backup-directory-alist (quote (("." . "C:/Users/WangQian/Workspace/AutoBackup"))))
  (tq-set-font))




