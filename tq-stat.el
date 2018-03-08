;;;; 统计相关函数。

(defun sum (&rest numbers)
  "求和。"
  (apply '+ numbers))

(defun average (&rest numbers)
  "计算算数平均数。"
  (let ((count (length numbers)))
    (if (= 0 count)
        0
    (/ (apply '+ numbers) (* 1.0 count)))))

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

