;; tq-math.el
;; 文件功能。
;; 建立日期：2019年09月22日
;; 修改日期：2019年09月23日

(defun tq-round (value precision)
  "对值进行舍入，保留precision位小数。"
  (let ((base (expt 10 precision)))
    (/ (round (* value base)) (* 1.0 base))))

(defun tq-sum (vector)
  "求和。"
  (apply '+ vector))

(defun tq-average (vector)
  "计算算数平均数。"
  (let ((count (length vector)))
    (if (= 0 count)
        0
      (/ (tq-sum vector) (* 1.0 count)))))

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

(defun tq-approximate-sigma-square (&rest numbers)
  "计算sigma^2的近似值。
计算公式为 S^2 = \frac{\sum_{i=1}^n(x_i-\overline{x})^2}{n-1}
"
  (let ((len (length numbers))
        (avg (apply 'average numbers)))
    (if (<= len 1)
        0.0
      (/ (apply '+ (mapcar #'(lambda (x)(* (- x avg) (- x avg))) numbers))
         (- len 1)))))

(defun tq-harmonic-number (n)
  "调和级数。1 + 1/2 + 1/3 + ... + 1/n"
  (let ((sum 0))
    (dotimes (x n)
      (setf sum (+ sum (/ 1.0 (1+ x)))))
    sum))

(defun tq-inner-product (vector1 vector2)
  "计算两个向量的内积。"
  (let ((size (length vector1))
        (result 0.0))
    (if (not (= size (length vector2)))
        (error "长度不同的两个向量不能计算内积。"))
    (dotimes (index size)
      (setf result (+ result (* (nth index vector1) (nth index vector2)))))
    result))

(defun tq-square (x)
  "计算平方。"
  (* x x))

(defun tq-square-sum (vector)
  "计算平方和。"
  (tq-sum (mapcar 'tq-square vector)))

(defun tq-ordinary-least-square (vector-x vector-y)
  "采用最小二乘法计算 y = a + bx。返回 (list a b)。"
  (let* ((a 0)
         (b 0)
         (size (length vector-x))
         (avg-x (tq-average vector-x))
         (avg-y (tq-average vector-y))
         (denominator (- (tq-square-sum vector-x) (* size avg-x avg-x))))
    (if (= denominator 0)
        (error "无法应用最小二乘法。"))
    (setf b (/ (- (tq-inner-product vector-x vector-y) (* size avg-x avg-y)) denominator))
    (setf a (- avg-y (* b avg-x)))
    (list a b)))

(defun tq-varience (vector)
  "计算方差。"
  (let ((n (length vector))
        (e (tq-average vector))
        (result 0.0))
    (dolist (x vector)
      (setf result (+ result (expt (- x e) 2))))
    (if (= 0 n)
        0.0
      (/ result n))))

(defun tq-covarience (vector1 vector2)
  "计算协方差。"
  (let ((n1 (length vector1))
        (n2 (length vector2))
        (e1 (expect vector1))
        (e2 (expect vector2))
        (m nil))
    (if (not (= n1 n2))
        (error "无法计算协方差。"))
    (dotimes (i n1)
      (push (* (nth i vector1) (nth i vector2)) m))
    (- (tq-average m) (* e1 e2))))

(defun tq-correlation-coefficient (vector1 vector2)
  "计算相关系数。"
  (let ((var-product (* (varience vector1)
                     (varience vector2)))
        (cov (covarience vector1 vector2)))
    (if (= 0.0 var-product)
        (error "无法计算相关系数。")
      (/ cov (sqrt var-product)))))

(defun tq-r-square (vector1 vector2)
  "计算相关系数平方（R 平方）。"
  (expt (correlation-coefficient vector1 vector2) 2))

(provide 'tq-math)
