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

(defun inch-to-centimeter (inch)
  "将英寸转换为厘米。"
  (* inch 2.54))

(defun centimeter-to-inch (cm)
  "将厘米转换为英寸。"
  (/ cm 2.54))

(defun calculate-mobilephone-screen-width (diagonal-length)
  "计算手机屏幕宽度。
diagonal-length是屏幕尺寸，如5.2寸。
返回以厘米为单位的屏幕宽度。
计算使用的长宽比为1.78:1。"
  (let ((dl (inch-to-centimeter diagonal-length)))
    (sqrt (/ (* dl dl) (+ 1 (* 1.78 1.78))))))

(defun calculate-mobilephone-screen-height (diagonal-length)
  (* 1.78 (calculate-mobilephone-screen-width diagonal-length)))


(defun print-mobilephone-width-and-height (x)
  "打印手机屏幕长、宽。
x是屏幕寸数，如5.2寸。
"
  (let ((w (calculate-mobilephone-screen-width x))
	(h (calculate-mobilephone-screen-height x)))
    (princ (format "%.1f寸屏幕 长 %.2f 厘米, 宽 %.2f 厘米\n" x h w)))
  nil)

(defun calculate-mobilephone-dpi (dl height-pixels)
  "计算手机屏幕dpi。
dl是英尺数。
height-pixels是长边像素数。
"
  (let ((h (calculate-mobilephone-screen-height dl)))
    (/ height-pixels (centimeter-to-inch h))))


(defun calculate-mobilephone-ppi (diagonal-length width-pixels height-pixels)
  "计算手机屏幕PPI。
diagonal-length 屏幕尺寸。
width-pixels 屏幕宽像素数。
height-pixels 屏幕长像素数。
"
  (/ (sqrt (+ (* width-pixels width-pixels)
	      (* height-pixels height-pixels)))
     diagonal-length))


(defun first-char (s)
  "返回字符串首字母。"
  (if (zerop (length s))
      ""
    (string (nth 0 (string-to-list s)))))

