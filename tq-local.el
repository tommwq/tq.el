;; tq-local.el
;; 本地配置。

;; (defcustom tq-chinese-font "微软雅黑"
;;   "中文字体。"
;;   :type 'string
;;   :group 'tq
;;   :set #'tq-update-chinese-font)

;; (defcustom tq-chinese-font-size 18
;;   "中文字体尺寸。"
;;   :type 'number
;;   :group 'tq
;;   :set #'tq-update-chinese-font)

;; (defcustom tq-latin-font "Source Code Pro"
;;   "拉丁字母字体。"
;;   :type 'string
;;   :group 'tq
;;   :set #'tq-update-latin-font)

;; (defcustom tq-latin-font-size 14
;;   "拉丁字母字体尺寸。"
;;   :type 'number
;;   :group 'tq
;;   :set #'tq-update-latin-font)

;; (defcustom tq-working-directory "."
;;   "工作目录。"
;;   :type 'directory
;;   :set #'(lambda (symbol value)
;;            (set-default symbol value)
;;            (setf default-directory tq-working-directory))
;;   :group 'tq)

;; (defcustom tq-git-program "git"
;;   "git程序路径（含文件名）。会影响magit-git-executable的值。"
;;   :type 'file
;;   :group 'tq)

;; (defcustom tq-python-program "python"
;;   "python程序路径（含文件名）。会影响python-shell-interpreter的值。"
;;   :type 'file
;;   :group 'tq)

;; (defcustom tq-system-path
;;   ""
;;   "系统路径。"
;;   :type 'string
;;   :group 'tq)

;; (defcustom tq-system-variables nil
;;   "系统变量。"
;;   :type 'plist
;;   :group 'tq
;;   :set #'(lambda (symbol value)
;;            (set-default symbol value)
;;            (tq-set-cmd-variables tq-system-variables)
;;            (tq-set-powershell-variables tq-system-variables)))


(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (plantuml . t)))

(setq org-plantuml-jar-path "C:/Users/WangQian/Workspace/Kit/plantuml.1.2019.10.jar")

(defun tq-set-font ()
  (let (
        (latin-font "M+ 1mn-12")
        (chinese-font "微软雅黑-12"))
    (set-frame-font latin-font)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font t charset chinese-font))))

(setq backup-directory-alist (quote (("." . "~/.backups"))))
(tq-set-font)

;; 设置工作目录
(setf default-directory "c:/Users/WangQian/Workspace/")


