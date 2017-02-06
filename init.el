;; 字体测试
;; oO0 
;; l1I
;; WWW

;; 字体
;;(set-frame-font "M+ 1mn regular-12")
;;(set-frame-font "Envy Code R-12")
;;(set-frame-font "M+ 1mn regular-12")
;;(set-frame-font "M+ 2m regular-11")
;;(set-frame-font "Verily Serif Mono-16")
;;(set-frame-font "Monaco-10.5")
;;(set-frame-font "Consolas-10")
;;(set-frame-font "仿宋-11")
;;(set-frame-font "PT Mono-12")
;;(set-frame-font "Cutive Mono-10.5")
;;(set-frame-font "Courier New-12")
;;(set-frame-font "Roboto Mono-11")
(set-frame-font "Source Code Pro-10")
;;(set-fontset-font (frame-parameter nil 'font) 'han (font-spec :family "仿宋" :size 18))
(set-fontset-font (frame-parameter nil 'font) 'han (font-spec :family "新宋体" :size 14))
;;(set-fontset-font (frame-parameter nil 'font) 'han (font-spec :family "华文中宋" :size 17))
;;(set-fontset-font (frame-parameter nil 'font) 'han (font-spec :family "微软雅黑" :size 16))
;;(set-fontset-font (frame-parameter nil 'font) 'han (font-spec :family "方正小标宋" :size 18))

;; 语法高亮
(global-font-lock-mode t)

;; 以y/n代替yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; 显示括号匹配
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;; 自动换行
(global-visual-line-mode 1)

;; 显示时间
(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)

;; 去掉工具栏
(tool-bar-mode 0)

;; 去掉菜单栏
(menu-bar-mode 0)

;; 去掉滚动条
(scroll-bar-mode 0)

;; 显示行号
(linum-mode t)
(global-linum-mode t)
(column-number-mode t)

;; 取消启动画面
(setq gnus-inhibit-startup-message t)
(setq inhibit-startup-message t)

;; 前景颜色
;; (set-foreground-color "white")

;; 背景颜色
;; (set-background-color "black")

;; 高亮当前行
(global-hl-line-mode 1)
;;(set-face-background 'hl-line "#242424")

;; 工作目录
(setq tq-work-directory "D:/workspace")
(setq default-directory tq-work-directory)

;; 自动保存
(setq auto-save-mode t)

;; 关闭自动备份
(setq make-backup-files nil)

;; 字符编码
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq file-name-coding-system 'gbk)

;; 鼠标指针规避光标
(mouse-avoidance-mode 'animate)

;; 全屏
(defun x-full-screen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NEW_WM_STATE_FULLSCREEN" 0)))

(defun win32-full-screen ()
  (w32-send-sys-command 61488))

(win32-full-screen)

;; 使用空格缩进
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq-default c-basic-offset 2)

;; (global-visual-line-mode t)

;; org-mode
(setq org-src-fontify-natively t)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; (desktop-save-mode t)
;; (setq desktop-dirname "C:/Users/guosen/emacs-desktop")

(global-set-key [f2] 'clipboard-kill-ring-save)
(global-set-key [f3] 'isearch-forward)
(setq cursor-type 'box)

;; SLIME
(add-to-list 'load-path "D:/Program Files/emacs-24.3/site-lisp/slime-2.8")
(require 'slime)
(slime-setup)
(require 'slime-autoloads)
(slime-setup '(slime-fancy))
(setq inferior-lisp-program 
"D:/sbcl/sbcl.exe"
;"C:\\Program Files\\Steel Bank Common Lisp\\1.2.7\\sbcl.exe"
;sbcl.exe --core C:/Program Files/Steel Bank Common Lisp/1.2.7/sbcl.core")
)


;; php-mode
;;(add-to-list 'load-path "~/.emacs.d")
;;(require 'php-mode)
;;(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

;; lua-mode
;;(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
;;(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))


(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; TQ
(add-to-list 'load-path "~/")
(require 'tq)

(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                           ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                           ("marmalade" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/marmalade/")
                           ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))
  (package-initialize))

(setq diary-file "~/diary")
(setq view-diary-entries-initially t
      mark-diary-entries-in-calendar t
      number-of-diary-entries 7)

(defun tq-c-mode-hook ()
  (c-set-style "tq")
  (setq tab-width 2
        indent-tabs-mode nil)
  (c-toggle-auto-newline 1))

(add-hook 'c-mode-common-hook 'tq-c-mode-hook)

;;(desktop-save-mode 1)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default default default italic underline success warning error])
 '(ansi-color-names-vector ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(ansi-term-color-vector [unspecified "#FAFAFA" "#FF1744" "#66BB6A" "#F57F17" "#42A5F5" "#7E57C2" "#0097A7" "#546E7A"])
 '(beacon-color "#F8BBD0")
 '(custom-enabled-themes (quote (tsdh-light)))
 '(custom-safe-themes (quote ("d5b121d69e48e0f2a84c8e4580f0ba230423391a78fcb4001ccb35d02494d79e" "6e4f8aba68e6934ad0e243f2fc7e6778d87f7d9b16e069cb9fec0cfa7f2f845a" "fa80190f587f2fab395b878d78d4db0aab0fac9844e1345d55f2f7558eff221f" "4486ade2acbf630e78658cd6235a5c6801090c2694469a2a2b4b0e12227a64b9" "8bb8a5b27776c39b3c7bf9da1e711ac794e4dc9d43e32a075d8aa72d6b5b3f59" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "55d31108a7dc4a268a1432cd60a7558824223684afecefa6fae327212c40f8d3" "e3d194a55a73cb87b602d902bb21210f3970d4c07303276c073f4bdb73ba2a3b" "29af7993f426d7871a605776a48e38ecc22bc80dd9bff0bc65303dfe5798ac64" "e5c6caa4860b1ba51dc5ad335c0c2734ea650a6098dd9652a1ab3d9aa702e185" "78559045fb299f3542c232166ad635c59cf0c6578d80a58b885deafe98a36c66" "5dd70fe6b64f3278d5b9ad3ff8f709b5e15cd153b0377d840c5281c352e8ccce" default)))
 '(evil-emacs-state-cursor (quote ("#D50000" bar)))
 '(evil-insert-state-cursor (quote ("#D50000" hbar)))
 '(evil-normal-state-cursor (quote ("#F57F17" box)))
 '(evil-visual-state-cursor (quote ("#66BB6A" box)))
 '(highlight-symbol-colors (quote ("#F57F17" "#66BB6A" "#0097A7" "#42A5F5" "#7E57C2" "#D84315")))
 '(highlight-symbol-foreground-color "#546E7A")
 '(highlight-tail-colors (quote (("#F8BBD0" . 0) ("#FAFAFA" . 100))))
 '(hl-sexp-background-color "#efebe9")
 '(org-agenda-files (list "D:/Workspace/Agenda/"))
 '(pos-tip-background-color "#ffffff")
 '(pos-tip-foreground-color "#78909C")
 '(tabbar-background-color "#ffffff")
 '(vc-annotate-background "#ffffff")
 '(vc-annotate-color-map (quote ((20 . "#ab4642") (50 . "#dc9656") (80 . "#f7ca88") (110 . "#a1b56c") (140 . "#86c1b9") (170 . "#7cafc2") (200 . "#ba8baf") (230 . "#a16046") (260 . "#181818") (290 . "#282828") (320 . "#383838") (350 . "#585858"))))
 '(vc-annotate-very-old-color "#585858"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(defun insert-org-header ()
  "insert org mode header

TODO switch on org mode 
"
  (interactive)
  (beginning-of-buffer)
  (insert "# -*- mode: org -*-\n")
  (insert "#+Title: \n")
  (insert "#+Author: Wang Qian\n")
  (insert (format "#+Date: %s\n"
                  (format-time-string "%Y-%m-%d")))
  (insert "#+Startup: showall\n")
  (org-mode)
  (insert "\n"))

(setf nxml-child-indent 8)
(setf nxml-attribute-indent 8)

(defun init-java-file (class-name)
  (interactive "sEnter the class name:")
  (beginning-of-buffer)
  (let ((date (format-time-string "%Y-%m-%d"))
        (header-format
"/**
 * File: %s.java
 * Description: Convert celsius to fahrenheit.
 *              This program is an exercise from book Introduction to Java Programming, 6th.
 * Author: Wang Qian
 * Create Date: %s
 * Last Modified: %s
 */

public class %s {
  public static void main(String[] args) {

  }
}
"))
    (insert (format header-format class-name date date class-name))))

(defconst tq-c-style
  '((c-basic-offset . 8)
    (c-comment-only-line-offset . 0)
    (indent-tabs-mode . nil)
    (fill-column . 80)
    (c-indent-comments-syntactically-p . t)
    (c-hanging-braces-alist . ((class-open after)
                               (class-close after)
                               (defun-open after)
                               (defun-close after)
                               (inline-open after)
                               (inline-close after)
                               (block-open after)
                               (block-close after)
                               (brace-list-open after)
                               (brace-list-close after)
                               (brace-entry-open after)
                               (statement-case-open after)
                               (case-lable after)
                               (access-label after)
                               (do-while-closure after)
                               (else-clause after)
                               (catch-clause after)
                               (substatement-open after)
                               (substatement-close after)
                               (namespace-open after)
                               (namespace-close after)))
    (c-cleanup-list . nil)
    (c-hanging-colons-alist . ((case-label after)))
    (c-offsets-alist . ((substatement . 0)
                        (case-label . 0))))
  "tq-c-style")
(c-add-style "tq" tq-c-style)

