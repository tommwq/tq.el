;; tq-psp-set-timerecord-file 输入路径，设置变量tq-psp-timerecord-file，指向一个文件。取消，使用defcustom。
;; tq-psp-start-job 输入任务名，在记录日志中插入一条任务开始 任务 时间
;; tq-psp-stop-job 输入任务么，在记录日志中插入一条任务结束 任务 时间
;; tq-psp-get-current-job 输出当前任务。
;; tq-psp-interrupt-job 输入时间和原因，在记录中插入一条任务中断 时间 原因


(defcustom tq-psp-timerecord-filename "~/.timerecord"
  "PSP时间日志记录文件。"
  :type 'string
  :group 'tq)

(defvar tq-psp-current-job nil)

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

(defun tq-psp-start-job (job-name)
  "开始任务。"
  (interactive "sjob name: ")
  (tq-psp-append-timerecord-file
   (format "任务开始 %s %s"
	   job-name
	   (format-time-string " %Y年%m月%d日%H时%M分")))
  (setf tq-psp-current-job job-name))

(defun tq-psp-stop-job (unit)
  "结束任务。"
  (interactive "nunit: ")
  (tq-psp-append-timerecord-file
   (format "任务结束 %s %s 单位 %d"
	   tq-psp-current-job
	   (format-time-string " %Y年%m月%d日%H时%M分")
	   unit))
  (setf tq-psp-current-job nil))

(defun tq-psp-get-current-job ()
  (interactive)
  (message
   (format "current job: [%s]"
	   (if tq-psp-current-job
	       tq-psp-current-job
	     "none"))))

(defun tq-psp-interrupt-job (reason minutes)
  "输入时间和原因，在记录中插入一条任务中断 时间 原因"
  (interactive "sreason: 
nminutes: ")
  (tq-psp-append-timerecord-file
   (format "任务中断 %d分 原因：%s"
	   minutes
	   reason)))

(defun tq-psp-view-timerecord ()
  (interactive)
  (find-file (tq-psp-get-timerecord-filename))
  (read-only-mode))
