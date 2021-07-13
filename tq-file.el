;; tq-file.el
;; 文件功能。
;; 建立日期：2019年09月22日
;; 修改日期：2019年09月22日

(defun tq-write-file (file-name content &optional overwrite)
  "写文件。"
  (let* ((absolute-file-name
          (if (file-name-absolute-p file-name)
              file-name
            (expand-file-name file-name default-directory)))	   
         (path (file-name-directory absolute-file-name)))
    (if (and (not overwrite)
             (file-exists-p absolute-file-name))
        (error "File existed. path: %s." absolute-file-name))
    (if (not (file-exists-p path))
        (make-directory path t))
    (write-region content nil absolute-file-name)))

(defun tq-write-file-then-open (file-name content &optional overwrite)
  "写文件并打开文件。"
  (tq-write-file file-name content overwrite)
  (find-file file-name))

(defun tq-generate-org-file-content (title)
  "生成org文件内容。"
  (let* ((template "# -*- mode: org -*-
#+options: ^:nil
#+todo: todo(t) in-action(i@/!) delegate(e@/!) delay(y@/!) | done(d!) canceled(c@/!)
#+title: ${title}
#+date: ${date}
"))
    (tq-render-template-from-sequence template
                                      "title" title
                                      "date" (format-time-string "%Y年%m月%d日"))))

(defun tq-insert-org-file (title)
  "生成org文件，插入到缓冲区。"
  (interactive "sTitle: ")
  (beginning-of-buffer)
  (insert (tq-generate-org-file-content title))
  (end-of-buffer))

(defun tq-new-org-file (file-name title)
  "建立并初始化org文件。"
  (interactive "sFileName: 
sTitle: ")
  (tq-write-file-then-open file-name (tq-generate-org-file-content title))
  (end-of-buffer))
