(require 'eieio)

(defclass tq-workflow-step-run-shell-command
  ()
  ((command :initform ""
            :type string
            :initarg :command))
  "执行一个shell命令。")

(cl-defmethod execute ((step tq-workflow-step-run-shell-command))
  (call-process-shell-command (oref step :command)))

(defclass tq-workflow-step-message
  ()
  ((text :initform ""
         :type string
         :initarg :text))
  "显示消息。")

(cl-defmethod execute ((step tq-workflow-step-message))
  (message (oref step :text)))

(defclass tq-workflow-step-open-file
  ()
  ((file-name :initform ""
              :type string
              :initarg :file-name))
  "在缓冲区中打开文件。")

(cl-defmethod execute ((step tq-workflow-step-open-file))
  (find-file (oref step :file-name)))

(cl-defmethod execute ((step tq-workflow-step-message))
  (message (oref step :text)))

(defclass tq-workflow-step-render-file
  ()
  ((file-name :initform ""
              :type string
              :initarg :file-name)
   (template :initform ""
             :type string
             :initarg :template)
   (environment :initform nil
                :initarg :environment))
  "根据template和execute-environment中渲染文件。")

(cl-defmethod execute ((step tq-workflow-step-render-file))
  (tq-write-file (oref step :file-name)
                 (tq-render-template (oref step :template) (oref step :environment))))

(defclass tq-workflow
  ()
  ((steps :initform nil
          :initarg :steps)
   (envrionment :initform nil
                :initarg :environment
                :documentation "envrionemtn是一个散列表，键和值都是字符串。"))
  "工作流对象。")

(defun tq-workflow-add-step (workflow step)
  (let ((steps (oref workflow :steps)))
    (push step steps)
    (message (prin1-to-string steps))
    (oset workflow :steps steps)
    workflow))

(defun tq-workflow-run-shell-command (command workflow)
  (tq-workflow-add-step workflow (tq-workflow-step-run-shell-command :command command)))

(defun tq-workflow-message (message workflow)
  (tq-workflow-add-step workflow (tq-workflow-step-message :text message)))

(defun tq-workflow-open-file (file-name workflow)
  (tq-workflow-add-step workflow (tq-workflow-step-open-file :file-name file-name)))

(defun tq-workflow-render-file (file-name template workflow)
  (tq-workflow-add-step workflow (tq-workflow-step-render-file :file-name file-name
                                                               :template template
                                                               :environment (oref workflow :environment))))

(defun tq-workflow-execute(workflow)
  (dolist (step (oref workflow :steps))
    (execute step)))


