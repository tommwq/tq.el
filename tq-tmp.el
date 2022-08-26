(defcustom tq-indent-offset 2
  "缩进。"
  :type 'integer
  :group 'tq)


(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "done" "todo"))))

(defun tq-replace-regexp-pairs (pairs text)
  "Replace regexp pairs in text. 
pairs is a list of string with even number, the odd ones are patterns, 
the even ones are replacements."
  (let ((pattern "")
        (replace ""))
    (while (< 0 (length pairs))
      (setf pattern (pop pairs))
      (setf replace (pop pairs))
      (setf text (replace-regexp-in-string pattern replace text)))
    text))


(defun tq-execute-template (variable-pairs template)
  "渲染模板。将模板中的${foo}替换为variable-pairs中foo对应的值。"
  (let ((pairs nil)
        (pattern "")
        (replace ""))
    (while variable-pairs
      (setf pattern (format "${%s}" (pop variable-pairs)))
      (setf replace (pop variable-pairs))
      (push replace pairs)
      (push pattern pairs))
    (tq-replace-regexp-pairs pairs template)))



(defun tq-java-package-to-directory (package)
  (replace-regexp-in-string "\\." "/" package))


(defun gen-latex-code-sample (code)
  "生成LaTeX代码示例。"
  (let ((fmt "
\\begin{tabular}{@{} l @{} l @{}}
\\begin{minipage}{3in}
\\begin{verbatim}
%s
\\end{verbatim}
\\end{minipage}
&
\\begin{minipage}{3in}
%s
\\end{minipage}
\\end{tabular}
"))
    (format fmt code code)))


(defconst tq-c-style
  '((c-tab-always-indent . t)
    (c-basic-offset . 4)
    (c-comment-only-line-offset . 0)
    (c-echo-syntactic-information-p . t)
    (c-cleanup-list . (
                       ;; brace-else-brace
                       ;; brace-elseif-brace
                       ;; brace-catch-brace
                       ;; empty-defun-braces
                       ;; one-liner-defun
                       ;; defun-close-semi
                       ;; list-close-comma
                       ;; scope-operator
                       ;; space-before-funcall
                       ;; compact-empty-funcall
                       ;; comment-close-slash
                       ))
    (c-hanging-braces-alist . (;; (substatement-open after)
                               ;; (inline-open after)
                               ;; (class-open after)
                               ;; (class-close nil)
                               ;; (defun-open after)
			                   ;; (defun-close nil)
                               ;; (brace-entry-open after)
                               ;; (statement after)
                               ;; (case-label after)
                               ;; (else-case)
                               ;; (block-close before)
                               ;; (access-label after)
                               ;; (do-while-closure after)
                               ;; (catch-clause after)
                               ;; (member-init-intro after)
                               ;; (brace-list-open after)
                               ;; (substatement-open nil)
                               ;; (inline-open nil)
                               ;; (class-open nil)
                               ;; (class-close nil)
                               ;; (defun-open nil)
                               ;; (defun-close nil)
                               ;; (brace-entry-open nil)
                               ;; (statement nil)
                               ;; (case-label nil)
                               ;; (else-case)
                               ;; (block-close nil)
                               ;; (access-label nil)
                               ;; (do-while-closure nil)
                               ;; (catch-clause nil)
                               ;; (member-init-intro nil)
                               ;; (brace-list-open nil)
                               ))
    (c-hanging-colons-alist .  (
                                ;;(member-init-intro before)
                                ;;(inher-intro)
                                ;;(case-label after)
                                ;;(access-label after)
                                ))
    (c-offsets-alist . ((substatement-open . 0)
			            (statement-case-open . +)
                        (label . 0)
			            (inline-open . 0)
                        (case-label . 0)
                        (block-open . 0))))
  "tq c style")

(c-add-style "tq-c-style" tq-c-style)



(defun tq-initialize-shell-mode ()
  "避免使用shell模式启动PowerShell时中文文件名出现乱码。"
  (set-buffer-process-coding-system 'gbk 'gbk))

(defun tq-initialize-powershell-mode ()
  "避免使用PowerShell模式启动PowerShell时中文文件名出现乱码。"
  (set-buffer-process-coding-system 'utf-8 'utf-8))

;; (defun set-org-todo-keywords ()
;;   "设置org-mode中的todo阶段。"
;;   (setq org-todo-keywords
;;         '((sequence "todo(t)" "in-action(i!)" "delegate(e/!)" "delay(y/!)"
;;                     "|"
;;                     "done(d/!)" "canceled(c/!)"))))
(defun set-org-todo-keywords ()
  "设置org-mode中的todo阶段。"
  (setq org-todo-keywords
        '((sequence "todo(t)" "in-action(i!)" "delegate(e!)" "delay(y!)"
                    "|"
                    "done(d!)" "canceled(c!)"))))

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

;; (defvar tq-note-path "c:/Users/WangQian/Workspace/Notes/")

;; (setq org-publish-project-alist
;;       `(
;;         ("org-notes"
;;          :base-directory ,tq-note-path
;;          :base-extension "txt"
;;          :publishing-directory ,tq-note-path
;;          :recursive t
;;          :publishing-function org-html-publish-to-html
;;          :headline-levels 4          
;;          :auto-preamble nil
;;          :auto-sitemap t
;;          :sitemap-filename "sitemap.txt"
;;          :sitemap-title "sitemap"
;;          :section-numbers nil
;;          :table-of-contents t
;;          :style "<link rel='stylesheet' type='text/css' href='css/org-manual.css' />"
;;          :style-include-default nil
;;          )
;;         ("org"
;;          :components ("org-notes" "org-static")
;;          )
;;         )
;;       )


(defun tq-add-cmd-path (path)
  (setenv "PATH" (concat path ";" (getenv "PATH")))
  nil)

(defun tq-add-powershell-path (path)
  "在*PowerShell*中增加系统路径。"
  (let ((buffer (get-buffer "*PowerShell*")))
    (when buffer (powershell-invoke-command-silently
                  (get-buffer-process buffer)
                  (format "$env:PATH+=';%s'" path)))))


(defun tq-new-gitignore (&optional directory)
  "建立gitignore文件"
  (interactive "sdirectory: ")
  (if (string-equal directory "")
      (setf path default-directory))
  (tq-write-file (concat directory "/.gitignore") "
.gradle
.vs/
build/
gradle/
Debug/
Release/
gradlew
gradlew.bat
*~
\#*
*.exe
*.idb
*.ilk
*.htm
*.log
*.obj
*.pch
*.pdb
*.swp
*.tli
*.tlh
*.tlog
*.user
"))


(defun tq-c-mode-hook ()
  ;;  (message "tq-c-mode-hook")
  ;;  (c-set-style "linux")
  ;; (c-set-style "")
  ;; (setq tab-width 4
	;;     indent-tabs-mode nil)
  ;;  (c-toggle-auto-newline t)
  (tq-set-indent tq-indent-offset))


(defun print-api-gateway-configuration (path-prefix interface-list)
  "打印 API 网关配置。

path-prefix 路径前缀，如 bp、openacct 等。
interface-list 接口名字列表。
"
  (dolist (x interface-list)
    (princ (format "/%s/%s/1.0->%s
" path-prefix x x))))


(defun print-test-url (address path-prefix interface-list)
  "打印测试 URL。

address 服务器地址，如 127.0.0.1:443
path-prefix 路径前缀，如 bp、openacct 等。
interface-list 接口名字列表。
"
  (dolist (x interface-list)
    (princ (format "http://%s/gsbp/%s/%s/1.0?
" address path-prefix x))))




;; https://stackoverflow.com/questions/9547912/emacs-calendar-show-more-than-3-months
;; 12-MONTH CALENDAR -- SCROLLS BY MONTH (FORWARDS / BACKWARDS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                            ;;;
;;; Scroll a yearly calendar by month -- in a forwards or backwards direction. ;;;
;;;                                                                            ;;;
;;; To try out this example, evaluate the entire code snippet and type:        ;;;
;;;                                                                            ;;;
;;;     M-x year-calendar                                                      ;;;
;;;                                                                            ;;;
;;; To scroll forward by month, type the key:  >                               ;;;
;;;                                                                            ;;;
;;; To scroll backward by month, type the key:  <                              ;;;
;;;                                                                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load "calendar" '(progn
  (define-key calendar-mode-map "<" 'lawlist-scroll-year-calendar-backward)
  (define-key calendar-mode-map ">" 'lawlist-scroll-year-calendar-forward) ))

(defmacro lawlist-calendar-for-loop (var from init to final do &rest body)
  "Execute a for loop.
Evaluate BODY with VAR bound to successive integers from INIT to FINAL,
inclusive.  The standard macro `dotimes' is preferable in most cases."
  `(let ((,var (1- ,init)))
    (while (>= ,final (setq ,var (1+ ,var)))
      ,@body)))

(defun year-calendar (&optional month year)
  "Generate a one (1) year calendar that can be scrolled by month in each direction.
This is a modification of:  http://homepage3.nifty.com/oatu/emacs/calendar.html
See also:  http://ivan.kanis.fr/caly.el"
(interactive)
  (require 'calendar)
  (let* ((current-year (number-to-string (nth 5 (decode-time (current-time)))))
         (month (if month month
           (string-to-number
             (read-string "Please enter a month number (e.g., 1):  " nil nil "1"))))
         (year (if year year
           (string-to-number
             (read-string "Please enter a year (e.g., 2014):  "
               nil nil current-year)))))
    (switch-to-buffer (get-buffer-create calendar-buffer))
    (when (not (eq major-mode 'calendar-mode))
      (calendar-mode))
    (setq displayed-month month)
    (setq displayed-year year)
    (setq buffer-read-only nil)
    (erase-buffer)
    ;; horizontal rows
    (lawlist-calendar-for-loop j from 0 to 3 do
      ;; vertical columns
      (lawlist-calendar-for-loop i from 0 to 2 do
        (calendar-generate-month
          ;; month
          (cond
            ((> (+ (* j 3) i month) 12)
              (- (+ (* j 3) i month) 12))
            (t
              (+ (* j 3) i month)))
          ;; year
          (cond
            ((> (+ (* j 3) i month) 12)
             (+ year 1))
            (t
              year))
          ;; indentation / spacing between months
          (+ 5 (* 25 i))))
      (goto-char (point-max))
      (insert (make-string (- 10 (count-lines (point-min) (point-max))) ?\n))
      (widen)
      (goto-char (point-max))
      (narrow-to-region (point-max) (point-max)))
    (widen)
    (goto-char (point-min))
    (setq buffer-read-only t)))

(defun lawlist-scroll-year-calendar-forward (&optional arg event)
  "Scroll the yearly calendar by month in a forward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (unless arg (setq arg 1))
  (save-selected-window
    (if (setq event (event-start event)) (select-window (posn-window event)))
    (unless (zerop arg)
      (let ((month displayed-month)
            (year displayed-year))
        (calendar-increment-month month year arg)
        (year-calendar month year)))
    (goto-char (point-min))
    (run-hooks 'calendar-move-hook)))

(defun lawlist-scroll-year-calendar-backward (&optional arg event)
  "Scroll the yearly calendar by month in a backward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (lawlist-scroll-year-calendar-forward (- (or arg 1)) event))
