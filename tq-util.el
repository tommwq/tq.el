(defun tq-util-find-definition-file (fn)
  "返回包含fn定义的文件名。"
  (let* ((buffer (car (find-definition-noselect fn nil)))
         (file-name (buffer-file-name buffer)))
    file-name))

(defun tq-util-remove-last (sequence)
  "移除列表中最后一个元素。"
  (let ((n-seq (nreverse sequence)))
    (if (= 0 (length n-seq))
        sequence
      (progn
        (pop n-seq)
        (nreverse n-seq)))))

(defun tq-util-insert-time ()
  "在缓冲区中插入时间字符串。"
  (interactive)
  (insert (format-time-string "%H时%M分%S秒")))

(defun tq-util-insert-date ()
  "在缓冲区中插入日期字符串。"
  (interactive)
  (insert (format-time-string "%Y年%m月%d日")))

(defun tq-util-insert-datetime ()
  "在buffer中插入日期时间字符串。"
  (interactive)
  (insert (format-time-string "%Y年%m月%d日%H时%M分%S秒")))

(defun tq-util-insert-datetime-short ()
  "在buffer中插入日期时间字符串。"
  (interactive)
  (insert (format-time-string "%d/%m %H:%M")))

(defun tq-util-maximize-window-windows ()
  "w32全屏显示。"
  (interactive)
  (let ((sc-maximize 61488))
    (w32-send-sys-command sc-maximize)))

(defun tq-util-maximize-window-linux ()
  "linux全屏显示。"
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NEW_WM_STATE_FULLSCREEN" 0)))

(defun tq-util-maximize-window ()
  "让窗口全屏显示。"
  (interactive)
  (if (string-equal system-type "windows-nt")
      (tq-util-maximize-window-windows)
    (tq-util-maximize-window-linux)))

(defun tq-util-make-string-hash (&rest elements)
  "从序列生成散列表。散列表的键和值都是字符串类型。序列a b c d生成的散列表是{a=>b, c=>d}。"
  (let ((key "")
        (value "")
        (table (make-hash-table :test #'equal)))
    (while (< 0 (length elements))
      (setf key (tq-str-stringify (pop elements)))
      (setf value (if (< 0 (length elements))
                      (tq-str-stringify (pop elements))
                    ""))
      (setf (gethash key table) value))
    table))

(defun tq-util-make-hash (&rest elements)
  "从序列生成散列表。序列a b c d生成的散列表是{a=>b, c=>d}。"
  (let ((key nil)
        (value nil)
        (table (make-hash-table :test #'equal)))
    (while (< 0 (length elements))
      (setf key (pop elements))
      (setf value (if (< 0 (length elements))
                      (pop elements)
                    nil))
      (setf (gethash key table) value))
    table))

(defun tq-util-season-number ()
  "获取季度序号。"
  (+ 1 (/ (string-to-number (format-time-string "%m")) 4)))

(defun tq-upcase-first-char (field)
  "将首字母改成大写字母。与capitalize不同，不会将其他字母改成小写。"
  (if (= 0 (length field))
      field
    (concat (upcase (substring field 0 1))
            (substring field 1))))



(defun tq-path-join (root &rest path-parts)
  "拼接文件路径。"
  (dolist (part path-parts)
    (setf root (expand-file-name part root)))
  root)

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

(defconst tq-c-style
  '((c-tab-always-indent . t)
    (c-basic-offset . 2)
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

(defun tq-c-mode-hook ()
  ;;  (message "tq-c-mode-hook")
  ;;  (c-set-style "linux")
  ;; (c-set-style "")
  ;; (setq tab-width 4
	;;     indent-tabs-mode nil)
  ;;  (c-toggle-auto-newline t)
  (tq-set-indent tq-indent-offset))

(provide 'tq-util)
