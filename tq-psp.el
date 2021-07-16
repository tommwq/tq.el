;; tq-psp-set-timerecord-file 输入路径，设置变量tq-psp-timerecord-file，指向一个文件。取消，使用defcustom。
;; tq-psp-begin 输入任务名，在记录日志中插入一条任务开始 任务 时间
;; tq-psp-end 输入任务么，在记录日志中插入一条任务结束 任务 时间
;; tq-psp-interrupt 输入时间和原因，在记录中插入一条任务中断 时间 原因


(defcustom tq-psp-timerecord-filename "~/.timerecord"
  "PSP时间日志记录文件。"
  :type 'string
  :group 'tq)

(defvar tq-psp-current nil)

(defun tq-psp-get-timerecord-filename ()
  "如果tq-psp-timerecord-filename是目录，选择该目录下的.timerecord作为文件名。"
  (if (directory-name-p tq-psp-timerecord-filename)
      (expand-file-name ".timerecord" tq-psp-timerecord-filename)
    tq-psp-timerecord-filename))

(defun tq-psp-create-timerecord-file-in-need ()
  "建立timerecord文件。"
  (let* ((filename (tq-psp-get-timerecord-filename))
	 (directory (file-name-directory filename)))
    (unless (file-exists-p directory)
      (make-directory directory t))
    (unless (file-exists-p filename)
      (append-to-file "" 'ignored filename))))

(defun tq-psp-append-timerecord-file (line)
  "追加写时间记录文件"
  (message line)
  (tq-psp-create-timerecord-file-in-need)
  (append-to-file (format "%s\n" line) 'ignored (tq-psp-get-timerecord-filename)))

(defun tq-psp-begin (job note)
  "开始任务。

在时间记录日志中添加一行：

2021年1月1日 10时0分 开始 读书 Java编程指南
"
  (interactive "s任务：
s备注：")
  (let ((date (format-time-string "%Y年%m月%d日"))
        (time (format-time-string "%H时%M分"))
        (line-format "%s %s 开始 %s %s"))
    (tq-psp-append-timerecord-file (format line-format date time job note))
    (setf tq-psp-current job)))

(defun tq-psp-end (complete unit)
  "结束任务。

在时间记录日志中添加一行：

2021年1月1日 12时0分 结束 读书 完成 是 单元 30
"
  (interactive "X是否完成：
n单元：")
  (let ((date (format-time-string "%Y年%m月%d日"))
        (time (format-time-string "%H时%M分"))
        (job tq-psp-current)
        (line-format "%s %s 结束 %s 完成 %s 单元 %d"))
    (tq-psp-append-timerecord-file
     (format line-format date time job (if complete "是" "否") unit)))
  (setf tq-psp-current nil))

(defun tq-psp-interrupt (reason minutes)
  "输入时间和原因。

在时间记录日志中添加一行：

2021年1月1日 12时0分 中断 时间 5 原因 电话
"
  (interactive "s原因：
n分钟：")
  (let ((date (format-time-string "%Y年%m月%d日"))
        (time (format-time-string "%H时%M分")))
    (tq-psp-append-timerecord-file
     (format "%s %s 中断 分钟 %d 原因 %s"
             date
             time
	         minutes
	         reason))))

(defun tq-psp-view-timerecord ()
  (interactive)
  (find-file (tq-psp-get-timerecord-filename))
  (read-only-mode))
