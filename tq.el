(provide 'tq)

(require 'tq-util)
(require 'tq-devutil)
(require 'tq-style)

(defun init-frame ()
  "初始化窗口。"
  ;; 菜单栏
  (menu-bar-mode -1)
  ;; 工具栏
  (tool-bar-mode -1)
  ;; 滚动条
  (scroll-bar-mode -1)
  ;; 缩写
  (fset 'yes-or-no-p 'y-or-n-p)
  ;; 字体
  (set-frame-font "Roboto Mono-10.5")
  ;;(set-frame-font "PT Mono-12")
  ;; 中文字体
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font t charset
		      (font-spec :family "宋体" :size 14)))
  ;; 启动界面
  (setq inhibit-startup-message t)
  (setq gnus-inhibit-startup-message t)
  ;; 显示行号
  (global-linum-mode t)
  ;; 工作目录
  (setq default-directory "C:\\Users\\WangQian\\Workspace")
  ;; 备份目录
  (setq backup-directory-alist (quote (("." . "C:/Users/WangQian/Workspace/AutoBackup"))))
  ;; 日记目录
  (setq diary-file "C:\\Users\\WangQian\\Workspace\\Diary")
  ;; 包管理系统
  (when (>= emacs-major-version 24)
    (require 'package)
    (add-to-list
     'package-archives
     '("melpa" . "http://melpa.org/packages/")
     t)
    (package-initialize))
  ;; 高亮当前行
  (global-hl-line-mode t)
  (set-cursor-color "white")
  ;; 启用自动保存
  (setq auto-save-mode t)
  ;; 关闭自动备份
  ;; (setq make-backup-files nil)
  ;; 鼠标指针规避光标
  (mouse-avoidance-mode 'animate)
  ;; 全屏显示
  (w32-maximize-window)

  ;;(desktop-save-mode 1)
  ;;(setq desktop-dirname "~/")

  ;; 设置快捷键
  (global-set-key [f2] 'clipboard-kill-ring-save)
  (global-set-key [f3] 'isearch-forward)
  ;; 光标样式
  (setq default-cursor-type 'bar)
  ;; 
  (setq visible-bell t)

  ;; 缩进
  (setq tab-stop-list nil)

  ;; 钩子
  (add-hook 'c-mode-hook 'hs-minor-mode)
  (add-hook 'nxml-mode-hook 'hs-minor-mode)
  (add-hook 'java-mode-hook 'hs-minor-mode)

  (defun tq-c-mode-hook ()
    (c-set-style "tq-c-style")
    (setq tab-width 8
	  indent-tabs-mode nil)
    (c-toggle-auto-newline t))
  (add-hook 'c-mode-common-hook 'tq-c-mode-hook)

  ;; Java
  (add-to-list 'auto-mode-alist '("\\.aidl\\'" . java-mode))

  ;; Lisp
  (add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

  ;; org mode
  (custom-set-variables
   '(org-agenda-files (list "C:/Users/WangQian/Workspace/Notes/Agenda/")))

  ;; 编码
  (set-encodings))

(defun set-encodings ()
  "设置字符编码。"
  (setf file-name-coding-system 'utf-8
        default-file-name-coding-system 'utf-8-unix
        default-buffer-file-coding-system 'utf-8-unix)
  (set-language-environment "UTF-8")
  (set-buffer-file-coding-system 'utf-8)
  (prefer-coding-system 'chinese-gbk-dos)
  (prefer-coding-system 'chinese-gbk-unix)
  (prefer-coding-system 'utf-8-dos)
  (prefer-coding-system 'utf-8-unix))
