(defun fail-probability (mttf period)
  "预测在接下来的period时间里，模块发生故障的概率。mttf是模块的平均故障发生时间。"
  (let ((e 2.71828))
    (- 1 (expt e (* -1 (/ period mttf))))))
