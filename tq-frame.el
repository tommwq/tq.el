(provide 'tq)

(defun tq-init-frame ()
  "初始化窗口。"

  ;; 隐藏菜单栏
  (menu-bar-mode -1)

  ;; 隐藏工具栏
  (tool-bar-mode -1)
  
  ;; 隐藏滚动条
  (scroll-bar-mode -1)
  
  ;; 启用缩写
  (fset 'yes-or-no-p 'y-or-n-p)
  
  ;; 设置字体
  (let ((font (if
		  (boundp 'tq-font)
		  tq-font
		"Consolas-10.5"))
	(chinese-font (if
			  (boundp 'tq-chinese-font)
			  tq-chinese-font
			"微软雅黑"))
	)
    (set-frame-font font)

    ;; 设置中文字体
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font t
			charset
			(font-spec :family chinese-font :size 14)
			)
      )
    )

  ;; 关闭启动界面
  (setq inhibit-startup-message t)
  (setq gnus-inhibit-startup-message t)
  
  ;; 显示行号
  (global-linum-mode t)

  ;; 设置工作目录
  (setq default-directory tq-work-directory)
  
  ;; 设置备份目录
  (setq backup-directory-alist (quote (("." . "C:/Users/WangQian/Workspace/AutoBackup"))))
  
  ;; 设置日记目录
  (setq diary-file "C:\\Users\\WangQian\\Workspace\\Diary")
  
  ;; 设置命令搜索路径
  (add-to-list 'exec-path "C:\\Program Files\\Git\\bin")

  ;; 包管理系统
  (when (>= emacs-major-version 24)
    (require 'package)
    (setq package-archives
	  '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
	    ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
    (package-initialize))
  
  ;; 高亮当前行
  (global-hl-line-mode t)
  ;;(set-cursor-color "white")
  
  ;; 启用自动保存
  (setq auto-save-mode t)

  ;; 关闭自动备份
  ;; (setq make-backup-files nil)

  ;; 鼠标指针规避光标
  (mouse-avoidance-mode 'animate)

  ;; 全屏显示
  ;; (w32-maximize-window)

  ;;(desktop-save-mode 1)
  ;;(setq desktop-dirname "~/")

  ;; 设置快捷键
  (global-set-key [f2] 'clipboard-kill-ring-save)
  (global-set-key [f3] 'isearch-forward)

  ;; 光标样式
  (setq default-cursor-type 'hbar)

  ;; 
  (setq visible-bell t)

  ;; 缩进
  (setq tab-stop-list nil)

  ;; 设置钩子
  (add-hook 'c-mode-hook 'hs-minor-mode)
  (add-hook 'nxml-mode-hook 'hs-minor-mode)
  (add-hook 'java-mode-hook 'hs-minor-mode)

  (defun tq-c-mode-hook ()
    (c-set-style "tq-c-style")
    (setq tab-width 8
	  indent-tabs-mode nil)
    (c-toggle-auto-newline t))
  
  (add-hook 'c-mode-common-hook 'tq-c-mode-hook)

  ;; 关联文件
  ;; Java
  (add-to-list 'auto-mode-alist '("\\.aidl\\'" . java-mode))
  ;; Lisp
  (add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

  ;; org mode
  (custom-set-variables
   '(org-agenda-files (list "C:/Users/WangQian/Workspace/Notes/Agenda/")))

  ;; nxml-mode
  (setf nxml-child-indent 8)
  (setf nxml-attribute-indent 8)

  ;; 编码
  (set-encodings))

(defun set-encodings ()
  "设置字符编码。"
  (setf file-name-coding-system 'utf-8
        default-file-name-coding-system 'utf-8-unix
        default-buffer-file-coding-system 'utf-8-unix)
  (set-language-environment "UTF-8")
  ;; 设置文件缓冲区默认保存编码。
  (set-buffer-file-coding-system 'utf-8-unix)
  (prefer-coding-system 'chinese-gbk-dos)
  (prefer-coding-system 'chinese-gbk-unix)
  (prefer-coding-system 'utf-8-dos)
  (prefer-coding-system 'utf-8-unix))



