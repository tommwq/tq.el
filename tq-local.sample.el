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

(setq org-plantuml-jar-path "C:/Users/guosen/.IdeaIC2019.1/config/plugins/plantuml4idea/lib/plantuml.1.2018.11.jar")

(defun tq-set-font ()
  (let (
        (latin-font "M+ 1mn regular-11")
        (chinese-font "微软雅黑-11"))
    (set-frame-font latin-font)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font t charset chinese-font))))

(setq backup-directory-alist (quote (("." . "~/.backups"))))
(tq-set-font)

;; 设置工作目录
(setf default-directory "D:/workspace/project/")

;; 光标样式
(setq cursor-type 'hbar)

(setq-default indent-tabs-mode nil)
;;  (setq-default indent-tables-mode nil)


;; 隐藏菜单栏
(menu-bar-mode -1)

;; 隐藏工具栏
(tool-bar-mode -1)

;; 隐藏滚动条
(scroll-bar-mode -1)

(setq display-time-day-and-date 1)
(display-time-mode 1)
(setq display-time-24hr-format t)

;; 启用缩写
(fset 'yes-or-no-p 'y-or-n-p)

;; 关闭启动界面
(setq inhibit-startup-message t)
(setq gnus-inhibit-startup-message t)

;; 显示行号
(global-linum-mode t)

;; 显示列号
(setq column-number-mode t)

;; (setf magit-git-executable tq-git-program)
;; (setf python-shell-interpreter tq-python-program)


;; 设置命令搜索路径
;; (add-to-list 'exec-path "C:\\Program Files\\Git\\bin")

;; 包管理系统
(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives
        '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
          ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
  (package-initialize))

;; 高亮当前行
(global-hl-line-mode t)

;;  (set-cursor-color "white")

;;(set-cursor-color "white")

;;(set-face-attribute hl-line-face nil :underline t)

;; 启用自动保存
(setq auto-save-mode t)

;; 关闭自动备份
;; (setq make-backup-files nil)
;;  (setq backup-directory-alist (quote (("." . "~/.backups"))))

;; 鼠标指针规避光标
(mouse-avoidance-mode 'animate)

;; 全屏显示
;; (w32-maximize-window)

;;(desktop-save-mode 1)
;;(setq desktop-dirname "~/")

;; 设置快捷键
(global-set-key [f2] 'clipboard-kill-ring-save)
(global-set-key [f3] 'isearch-forward)

(set-cursor-color "orange")

;;(set-cursor-color "orange")

;; 
(setq visible-bell t)

;; 缩进
(setq tab-stop-list nil)

;; 关联文件
;; Java
;; (add-to-list 'auto-mode-alist '("\\.aidl\\'" . java-mode))
;; Lisp
;; (add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))


;; org mode
;; (custom-set-variables
;;  '(org-agenda-files (list "C:/Users/WangQian/Workspace/Notes/Agenda/")))


;; nxml-mode
(setf nxml-child-indent 8)
(setf nxml-attribute-indent 8)

;; 编码
(set-encodings)

(setq c-default-style
      '((java-mode . "tq-c-style")
        (c-mode . "tq-c-style")
        (c++-mode . "tq-c-style")
        (other . "tq-c-style")))

;; 设置钩子。
(add-hook 'shell-mode-hook 'tq-initialize-shell-mode)
(add-hook 'c-mode-common-hook 'tq-c-mode-hook)
(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'nxml-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook 'tq-c-mode-hook)
(add-hook 'powershell-mode-hook #'(lambda ()
                                    (tq-initialize-powershell-mode)
                                    (tq-add-powershell-path tq-system-path)))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; (add-hook 'c-mode-common-hook 'google-set-c-style)
;; (add-hook 'c-mode-common-hook 'google-make-newline-indent)


(global-auto-revert-mode t)

;; 避免C-p失效
(global-set-key (kbd "M-p") 'previous-line)
(global-set-key (kbd "M-n") 'next-line)

;; 显示时间
(setq display-time-mode t)

(setq tab-width 8
      c-basic-offset 8
	  indent-tabs-mode nil)
(set-org-todo-keywords)
(switch-to-buffer "*scratch*")
;; (delete-other-windows)
;; (delete-region (point-min) (point-max))
(tq-set-font)

