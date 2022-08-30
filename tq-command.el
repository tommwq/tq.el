;; (defun tq-create-day-record ()
;;   "创建工作日志文件。

;; 工作日志文件名字是格式为yyyy-qX-wY-yyyyMMdd.org的文本文件。
;; "
;;   (interactive)
;;   (let* ((season (number-to-string
;;                   (+ 1 (/ (string-to-number (format-time-string "%m")) 4))))
;;          (file-path-format (concat "%Y-q" season  "-w%V-%Y%m%d.org"))
;;          (file-path (format-time-string file-path-format))
;;          (date (format-time-string "%Y年%m月%d日")))
;;     (tq-write-file-then-open file-path
;;                              (tq-render-template-from-sequence "# -*- mode: org -*-
;; #+options: ^:nil
;; #+todo: todo(t) | done(d@/!) canceled(c@/!)
;; #+title: ${date}
;; #+date: ${date}
;; * todo 今日工作 [%]
;; * 记录
;; " "date" date))))

;; (defun tq-create-week-record ()
;;   "创建周工作日志文件。

;; 周工作日志文件名字是格式为yyyy-qX-wY.org的文本文件。
;; "
;;   (interactive)
;;   (let* ((season (number-to-string
;;                   (+ 1 (/ (string-to-number (format-time-string "%m")) 4))))
;;          (file-path-format (concat "%Y-q" season  "-w%V.org"))
;;          (file-path (format-time-string file-path-format))
;;          (date (format-time-string "%Y年%V周")))
;;     (tq-write-file-then-open file-path
;;                              (tq-render-template-from-sequence "# -*- mode: org -*-
;; #+options: ^:nil
;; #+todo: todo(t) | done(d@/!) canceled(c@/!)
;; #+title: ${date}
;; #+date: ${date}
;; * todo 本周工作 [%]
;; * 记录
;; " "date" date))))

;; (defun tq-create-problem-record (title)
;;   "创建问题记录文件。

;; 问题记录文件名字是格式为yyyyMMdd_<title>.org的文本文件。title中不得包含空格、和其他文件名禁止使用的字符。
;; "
;;   (interactive "s标题：")
;;   (let ((file-path (concat (format-time-string "%Y%m%d_") title ".org"))
;;         (date (format-time-string "%Y年%m月%d日")))
;;     (tq-write-file-then-open file-path
;;                              (tq-render-template-from-sequence "# -*- mode: org -*-
;; #+options: ^:nil
;; #+todo: todo(t) in-action(i/!) delegate(e/!) delay(y/!) | done(d/!) canceled(c/!)
;; #+HTML_HEAD: <style type=\"text/css\">body { font-size: large; }</style>
;; #+title: ${title}
;; #+date: ${date}
;; * 问题表现
;; * 排查过程
;; * 处理方案
;; * 原因分析
;; * 改善建议
;; " 
;;                                                                "date" date
;;                                                                "title" title))))
