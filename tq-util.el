(defun tq-util-verify-china-identification-card (id-card-number)
  "校验身份证。
(tq-util-verify-china-identification-card "11010519491231002X")
(tq-util-verify-china-identification-card "440524188001010014")
"
  (let ((A nil)
        (B (list 1 2 4 8 5 10 9 7 3 6 1 2 4 8 5 10 9 7))
        (ch nil)
        (check-sum 0))
    (if (not (eq (length id-card-number) 18))
        nil)
    (dotimes (i 18)
      (setf ch (substring id-card-number i (1+ i)))
      (push (if (string-equal "X" ch) 10 (string-to-number ch)) A))
    (dotimes (i 18)
      (setf check-sum (+ check-sum (* (nth i A) (nth i B)))))
    (equal 1 (mod check-sum 11))))

(defun tq-util-calculate-china-identification-card-check-sum (id-card-number)
  "计算身份证最后一位。
(tq-util-calculate-china-identification-card-check-sum "11010519491231002") 
(tq-util-calculate-china-identification-card-check-sum "44052418800101001") 
"
  (let ((A nil)
        (B (list 2 4 8 5 10 9 7 3 6 1 2 4 8 5 10 9 7))
        (ch nil)
        (check-sum 0))
    (if (not (eq (length id-card-number) 17))
        nil)
    (dotimes (i 17)
      (setf ch (substring id-card-number i (1+ i)))
      (push (if (string-equal "X" ch) 10 (string-to-number ch)) A))
    (dotimes (i 17)
      (setf check-sum (+ check-sum (* (nth i A) (nth i B)))))
    (setf check-sum (mod check-sum 11))
    (cond 
     ((eq 0 check-sum) "1")
     ((eq 1 check-sum) "0")
     ((eq 2 check-sum) "X")
     (t (number-to-string (- 12 check-sum))))))

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

(defun tq-util-season-number-by-month (month-number)
  "获取月份所在季度。"
  (1+ (/ (- month-number 1) 3)))

(defun tq-util-season-number ()
  "获取季度序号。"
  (tq-util-season-number-by-month (string-to-number (format-time-string "%m"))))

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

(defun tq-util-ralative-luminance (rgb)
  "计算相对流明。"
  (let* ((r (floor (/ rgb #x10000)))
         (g (mod (floor (/ rgb #x100)) #x100))
         (b (mod rgb #x100))
         (rs (/ r 255.0))
         (gs (/ g 255.0))
         (bs (/ b 255.0))
         (rr (if (<= rs 0.03928) 
                 (/ rs 12.92)
               (expt (/ (+ rs 0.055) 1.055) 2.4)))
         (gg (if (<= gs 0.03928) 
                 (/ gs 12.92)
               (expt (/ (+ gs 0.055) 1.055) 2.4)))
         (bb (if (<= bs 0.03928) 
                 (/ bs 12.92)
               (expt (/ (+ bs 0.055) 1.055) 2.4))))
    (+ (* 0.2126 rr) (* 0.7152 gg) (* 0.0722 bb))))


(defun tq-util-color-contrast (rgb1 rgb2)
  (let* ((l1 (tq-util-ralative-luminance rgb1))
         (l2 (tq-util-ralative-luminance rgb2))
         (r (/ (+ l1 0.05) (+ l2 0.05))))
    (if (< r 1)
        (/ 1 r)
      r)))

(defun tq-util-insert-function-parameter-table (function-name)
  "在缓冲区当前位置插入函数参数表。"
  (interactive "s函数名字: ")
  (insert (format
"#+caption: %s
|------+------+------+----------+----------+------|
| 方向 | 参数 | 含义 | 数据类型 | 取值范围 | 必要 |
|------+------+------+----------+----------+------|
| 传入 |      |      |          |          |      |
|------+------+------+----------+----------+------|
| 返回 |      |      |          |          |      |
|------+------+------+----------+----------+------|
"
function-name)))

(defun tq-util-insert-user-case (use-case-name)
    "插入用例。"
  (interactive "s用例名字：")
  (insert (format
"用例：%s
- *主执行者* 
- *范围* 
- *级别* 子功能级/用户目标级/概要级
- *利益相关方及利益*
  - *利益相关方1*
  - *利益相关方2*
- *前置条件* 
- *最小保证* 
- *成功保证* 
- *主成功场景*
   1. 
   2. 
   3. 
   4. 
- *扩展*
"
use-case-name)))

(provide 'tq-util)
