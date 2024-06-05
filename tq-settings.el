(provide 'tq-settings)

(require 'tq-command)

(when (eq system-type 'windows-nt)
  (setq gc-cons-threshold (* 512 1024 1024))
  (setq gc-cons-percentage 0.5)
  (run-with-idle-timer 5 t #'garbage-collect))

(setq inhibit-compacting-font-caches t) 

;; 光标样式
(setq cursor-type 'box)
(set-default 'cursor-type 'box)
(setq cursor-in-non-selected-windows 'box)
;; c-basic-offset 2
;; tab-width 2
;; inhibit-startup-screen nil
;; (eww-search-prefix "https://cn.bing.com/search?q=")

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

;; 高亮当前行（已通过配置实现，不再需要）
;;(global-hl-line-mode 1)
;;(set-face-attribute hl-line-face nil :underline nil)
;;(set-face-background 'hl-line "bisque")
;;(set-face-attribute hl-line-face nil :underline nil)
;;(set-face-background 'hl-line "white")
;;(set-cursor-color "yellow")


;; 启用自动保存
(setq auto-save-mode nil)
(setq make-backup-files nil)
(setq backup-directory-alist (quote (("." . "~/.backups"))))

;; 鼠标指针规避光标
(mouse-avoidance-mode 'animate)

;; 全屏显示
;; (w32-maximize-window)

;;(desktop-save-mode 1)
;;(setq desktop-dirname "~/")

;; 设置快捷键
(global-set-key [f2] 'clipboard-kill-ring-save)
(global-set-key [f3] 'isearch-forward)


;; use customize
;;(set-cursor-color "black")


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
;; (setf nxml-child-indent 4)
;; (setf nxml-attribute-indent 4)


(setq c-default-style
      '((java-mode . "tq-c-style")
        (c-mode . "tq-c-style")
        (c++-mode . "tq-c-style")
        (other . "tq-c-style")))

;; 设置钩子。
(add-hook 'shell-mode-hook #'(lambda ()
                               (set-buffer-process-coding-system 'gbk 'gbk)))

(add-hook 'c-mode-common-hook 'tq-c-mode-hook)
(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'nxml-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook 'tq-c-mode-hook)
(add-hook 'powershell-mode-hook #'(lambda ()
                                    (set-buffer-process-coding-system 'utf-8 'utf-8)))

(add-hook 'html-mode-hook #'(lambda ()
                              (electric-indent-mode -1)))
;; (add-hook 'c-mode-common-hook 'google-set-c-style)
;; (add-hook 'c-mode-common-hook 'google-make-newline-indent)


(global-auto-revert-mode t)

;; 避免C-p失效
(global-set-key (kbd "M-p") 'previous-line)
(global-set-key (kbd "M-n") 'next-line)

;; 显示时间
(setq display-time-mode t)

;; (setq tab-width 4
;;       c-basic-offset 4
;;       indent-tabs-mode nil)
(electric-indent-mode -1)

(setq org-todo-keywords '((sequence "todo(t)" "delay(y)" "|"
                                    "done(d)" "cancel(c)")))

;; 设置窗口半透明
(set-frame-parameter (selected-frame) 'alpha 100)

(setf org-export-in-background nil)
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(global-set-key (kbd "<insert>") nil)



(c-add-style "tq-c-style" tq-c-style)



"设置字符编码。"
;; (setf file-name-coding-system 'utf-8
;;       default-file-name-coding-system 'utf-8-unix
;;       default-buffer-file-coding-system 'utf-8-unix)


(set-language-environment 'Chinese-GBK)


;; 编码设置 begin
(set-language-environment 'Chinese-GB)
;; default-buffer-file-coding-system变量在emacs23.2之后已被废弃，使用buffer-file-coding-system代替
(set-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(setq-default pathname-coding-system 'euc-cn)
(setq file-name-coding-system 'euc-cn)
;; 另外建议按下面的先后顺序来设置中文编码识别方式。
;; 重要提示:写在最后一行的，实际上最优先使用; 最前面一行，反而放到最后才识别。
;; utf-16le-with-signature 相当于 Windows 下的 Unicode 编码，这里也可写成
;; utf-16 (utf-16 实际上还细分为 utf-16le, utf-16be, utf-16le-with-signature等多种)
(prefer-coding-system 'cp950)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'cp936)
;;(prefer-coding-system 'gb18030)
;(prefer-coding-system 'utf-16le-with-signature)
(prefer-coding-system 'utf-16)
;; 新建文件使用utf-8-unix方式
;; 如果不写下面两句，只写
;; (prefer-coding-system 'utf-8)
;; 这一句的话，新建文件以utf-8编码，行末结束符平台相关
(prefer-coding-system 'utf-8-dos)
(prefer-coding-system 'utf-8-unix)
;; 编码设置 end




;; 设置文件缓冲区默认保存编码。


;; (set-buffer-file-coding-system 'utf-8-unix)
;; (prefer-coding-system 'chinese-gbk-dos)
;; (prefer-coding-system 'chinese-gbk-unix)
;; (prefer-coding-system 'utf-8-dos)
;; (prefer-coding-system 'utf-8-unix)

(tq-util-maximize-window)
