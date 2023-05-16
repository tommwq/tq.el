;; tq-file.el
;; 文件功能。
;; 建立日期：2019年09月22日
;; 修改日期：2019年09月22日

(defun tq-read-file (filename)
  "读取文件，返回内容字符串。"
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

;; TODO 重命名为tq-file-write
(defun tq-write-file (file-name content &optional overwrite)
  "写文件。"
  (let* ((absolute-file-name (if (file-name-absolute-p file-name)
                                 file-name
                               (expand-file-name file-name default-directory)))	   
         (path (file-name-directory absolute-file-name)))
    (if (and (not overwrite)
             (file-exists-p absolute-file-name))
        (error "文件 [%s] 已存在" absolute-file-name))
    (if (not (file-exists-p path))
        (make-directory path t))
    (write-region content nil absolute-file-name)))

(defun tq-write-file-then-open (file-name content &optional overwrite)
  "写文件并打开文件。"
  (tq-write-file file-name content overwrite)
  (find-file file-name))

(defun tq-file-read (filename)
  "读取文件，返回内容字符串。"
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun tq-file-write (file-name content &optional overwrite)
  "写文件。"
  (let* ((absolute-file-name (if (file-name-absolute-p file-name)
                                 file-name
                               (expand-file-name file-name default-directory)))	   
         (path (file-name-directory absolute-file-name)))
    (if (and (not overwrite)
             (file-exists-p absolute-file-name))
        (error "文件 [%s] 已存在" absolute-file-name))
    (if (not (file-exists-p path))
        (make-directory path t))
    (write-region content nil absolute-file-name)))

(defun tq-file-write-and-open (file-name content &optional overwrite)
  "写文件并打开文件。"
  (tq-write-file file-name content overwrite)
  (find-file file-name))

(defun tq-generate-org-file-content (title)
  "生成org文件内容。"
  (let ((template "# -*- mode: org -*-
#+options: ^:nil
#+todo: todo(t) | done(d/!) cancel(c/!)
#+HTML_HEAD: <style>body {font-size:xx-large; font-family:monospace;/*background-color:#000000;color:#f5deb3;*/}</style>
#+title: ${title}
#+date: ${date}
"))
    (tq-render-template-from-sequence template
                                      "title" title
                                      "date" (format-time-string "%Y年%m月%d日"))))

(defun tq-insert-org-file (title)
  "生成org文件，插入到缓冲区。"
  (interactive "s标题：")
  (beginning-of-buffer)
  (insert (tq-generate-org-file-content title))
  (end-of-buffer))

(defun tq-create-org (title)
  "建立并初始化org文件。"
  (interactive "s标题：")
  (let ((file-name (concat title ".org")))
  (let ((file-name (format "%s.org" title)))
    (tq-write-file-then-open file-name (tq-generate-org-file-content title))
    (end-of-buffer))))
