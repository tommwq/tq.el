;; tq-psp-set-timerecord-file 输入路径，设置变量tq-psp-timerecord-file，指向一个文件。取消，使用defcustom。
;; tq-psp-begin 输入任务名，在记录日志中插入一条任务开始 任务 时间
;; tq-psp-end 输入任务么，在记录日志中插入一条任务结束 任务 时间
;; tq-psp-interrupt 输入时间和原因，在记录中插入一条任务中断 时间 原因

;; TODO 使用类保存当前任务的名字、开始时间、中断时间、中断次数等。
;; TODO 学习eieio。

(require 'eieio)

;; (defclass tq-psp-interrupt
;;   ((minute)
;;    (reason)))

;; (defclass tq-psp-timerecord
;;   ((job :initarg :job
;;         :initform ""
;;         :type string
;;         :documentation "任务")
;;    (start-time :initarg :start-time
;;                :initform ""
;;                :documentation "开始时间")
;;    (stop-time :initarg :stop-time
;;               :initform ""
;;               :documentation "结束时间")
;;    (interrupt-list)
;;    (note)))
;; (cl-defmethod tq-psp-timerecord-interrupt)
;; (cl-defmethod tq-psp-timerecord-stop)


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

2021-01-01 10:00:00 BEGIN read Java
"
  (interactive "s任务：
s备注：")
  (let ((date (format-time-string "%Y-%m-%d"))
        (time (format-time-string "%H:%M:%S"))
        (line-format "%s %s Begin %s %s"))
    (tq-psp-append-timerecord-file (format line-format date time job note))
    (setf tq-psp-current job)))

(defun tq-psp-end (complete unit)
  "结束任务。

在时间记录日志中添加一行：

2021-01-01 10:00:00 END read COMPLETE Y UNIT 30
"
  (interactive "X是否完成：
n单元：")
  (let ((date (format-time-string "%Y-%m-%d"))
        (time (format-time-string "%H:%M:%S"))
        (job tq-psp-current)
        (line-format "%s %s End %s Complete %s Unit %d"))
    (tq-psp-append-timerecord-file
     (format line-format date time job (if complete "Y" "N") unit)))
  (setf tq-psp-current nil))

(defun tq-psp-interrupt (reason minutes)
  "输入时间和原因。

在时间记录日志中添加一行：

2021-01-01 10:00:00 INTERRUPT MIN 5 READON PHONE
"
  (interactive "s原因：
n分钟：")
  (let ((date (format-time-string "%Y-%m-%d"))
        (time (format-time-string "%H:%M:%S"))
        (tq-psp-append-timerecord-file
         (format "%s %s Interrupt Minute %d Reason %s"
                 date
                 time
	         minutes
	         reason)))))

(defun tq-psp-view-timerecord ()
  (interactive)
  (find-file (tq-psp-get-timerecord-filename))
  (read-only-mode))
