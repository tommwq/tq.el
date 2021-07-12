;; tq-math.el
;; 文件功能。
;; 建立日期：2019年09月22日
;; 修改日期：2019年09月23日

(defun tq-round (value precision)
  "对值进行舍入，保留precision位小数。"
  (let ((base (expt 10 precision)))
    (/ (round (* value base)) (* 1.0 base))))

(defun tq-sum (&rest numbers)
  "求和。"
  (apply '+ numbers))

(defun tq-average (&rest numbers)
  "计算算数平均数。"
  (let ((count (length numbers)))
    (if (= 0 count)
        0
      (/ (apply '+ numbers) (* 1.0 count)))))

(defun tq-increase-percent (value percent)
  (* value (+ 1 (/ percent 100.0))))

(defun tq-decrease-percent (value percent)
  (* value (- 1 (/ percent 100.0))))

(defun tq-in-bound (value lower upper &optional include)
  "计算值是否在范围内。"
  (let ((less (if include #'<= #'<))
        (greater (if include #'>= #'>)))
    (and (funcall less value upper)
         (funcall greater value lower))))

(defun tq-in-bound-percent (value target lower-percent upper-percent &optional include)
  "计算值是否在范围内（百分比）。"
  (tq-in-bound value
               (tq-decrease-percent target lower-percent)
               (tq-increase-percent target upper-percent)
               include))

(defun approximate-sigma-square (&rest numbers)
  "计算sigma^2的近似值。
计算公式为 S^2 = \frac{\sum_{i=1}^n(x_i-\overline{x})^2}{n-1}
"
  (let ((len (length numbers))
        (avg (apply 'average numbers)))
    (if (<= len 1)
        0.0
      (/ (apply '+ (mapcar #'(lambda (x)(* (- x avg) (- x avg))) numbers))
         (- len 1)))))

(defun harmonic-number (n)
  "调和级数。1 + 1/2 + 1/3 + ... + 1/n"
  (let ((sum 0))
    (dotimes (x n)
      (setf sum (+ sum (/ 1.0 (1+ x)))))
    sum))
