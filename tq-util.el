(defun tq-amdahl-speedup (parallel-ratio parallel-node-count)
  "计算按照Amdahl法则可以获得的优化效果"
  (tq-round (/ 1.0 (+ (- 1.0 parallel-ratio) (/ parallel-ratio parallel-node-count))) 2))

(defun tq-amdahl-speedup-limit (parallel-ratio)
  "计算按照Amdahl法则可以获得的最大优化效果"
  (tq-round (/ 1.0 (- 1.0 parallel-ratio)) 2))

(defun tq-fail-probability (mttf period)
  "预测在接下来的period时间里，模块发生故障的概率。mttf是模块的平均故障发生时间。"
  (let ((e 2.71828))
    (- 1 (expt e (* -1 (/ period mttf))))))

(defun tq-reload ()
  "重新加载tq库。"
  (interactive)
  (let* ((install-directory (eval-when-compile default-directory))
         (tq-file-name (expand-file-name "tq.el" install-directory)))
    (message "reload tq.el")
    (load tq-file-name)))

(defun tq-inch-to-cm (inch)
  "将英寸转换为厘米。"
  (* inch 2.54))

(defun tq-cm-to-inch (cm)
  "将厘米转换为英寸。"
  (/ cm 2.54))

(defun tq-insert-time ()
  "在缓冲区中插入时间字符串。"
  (interactive)
  (insert (format-time-string "%H时%M分%S秒")))

(defun tq-insert-date ()
  "在缓冲区中插入日期字符串。"
  (interactive)
  (insert (format-time-string "%Y年%m月%d日")))

(defun tq-insert-datetime ()
  "在buffer中插入日期时间字符串。"
  (interactive)
  (insert (format-time-string "%Y年%m月%d日 %H时%M分%S秒")))

(defun tq-execute-shell (command &optional work-directory)
  "execute shell command in work directory."
  (let ((stack nil))
    (when work-directory
      (push default-directory stack)
      (cd work-directory))
    (shell-command command)
    (when work-directory
      (cd (pop stack)))))

(defun tq-join-path (root &rest path-list)
  "Join path. "
  (expand-file-name (seq-reduce
                     (lambda (base path)
                       (if base
                           (concat base "/" path)
                         path))
                     path-list
                     nil)
                    root))

(defun max-window-windows ()
  "w32全屏显示。"
  (interactive)
  (let ((sc-maximize 61488))
    (w32-send-sys-command sc-maximize)))

(defun max-window-linux ()
  "linux全屏显示。"
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NEW_WM_STATE_FULLSCREEN" 0)))

(defun max-window ()
  "让窗口全屏显示。"
  (interactive)
  (if (string-equal system-type "windows-nt")
      (max-window-windows)
    (max-window-linux)))

(defun tq-make-string-hash (&rest elements)
  "从序列生成散列表。散列表的键和值都是字符串类型。序列a b c d生成的散列表是{a=>b, c=>d}。"
  (let ((key "")
        (value "")
        (table (make-hash-table :test #'equal)))
    (while (< 0 (length elements))
      (setf key (tq-stringify (pop elements)))
      (setf value (if (< 0 (length elements))
                      (tq-stringify (pop elements))
                    ""))
      (setf (gethash key table) value))
    table))

(defun tq-make-hash (&rest elements)
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

(defun tq-open-buffer (buffer-type)
  "打开新的缓冲区，并设置对应的模式。"
  (interactive "sbuffer type: ")
  (let* ((buffer-name (format "*temporary-%s*" buffer-type))
         (mode-table (tq-make-hash "java" #'java-mode
                                   "go" #'go-mode
                                   "xml" #'xml-mode
                                   "c" #'c-mode
			           "c++" #'c++-mode
                                   "powershell" #'powershell-mode
                                   "shell" #'shell-mode
                                   "lisp" #'lisp-interaction-mode
                                   "python" #'python-mode
                                   "r" #'r-mode
                                   "org" #'org-mode
                                   "javascript" #'javascript-mode
                                   "css" #'css-mode
                                   "sql" #'sql-mode
                                   "gradle" #'groovy-mode
			           "kotlin" #'kotlin-mode
			           "dockerfile" #'dockerfile-mode
			           "typescript" #'typescript-mode
                                   "elisp" #'lisp-interaction-mode
                                   "php" #'php-mode
                                   "" #'text-mode))
         (mode-setter (gethash buffer-type mode-table)))
    (unless mode-setter (setf mode-setter #'text-mode))
    (switch-to-buffer buffer-name)
    (funcall mode-setter)))



(defun amdahl-accelerate-ratio (processor-number parallel-ratio)
  "根据Amdahl定律计算加速比。"
  (let ((n processor-number)
        (p parallel-ratio))
    (/ 1 (+ (- 1 p) (/ p n)))))

