;; File: utils.el
;; Description: Utility functions.
;; Author: Wang Qian
;; Create: 2016-09-10
;; Modify: 2017-02-03

(provide 'tq)

(defun w32-maximize-window ()
  "全屏显示。"
  (interactive)
  (let ((sc-maximize 61488))
    (w32-send-sys-command sc-maximize)))

(defun x-full-screen ()
  "全屏显示。"
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
			 '(2 "_NEW_WM_STATE_FULLSCREEN" 0)))

(defun tq-insert-date ()
  "在buffer中插入日期字符串。"
  (interactive)
  (insert
   (format-time-string "%Y年%m月%d日")))

