(defun tq-create-work-record ()
  "创建工作日志文件。

工作日志文件名字是格式为yyyyMMdd.org的文本文件。
"
  (interactive)
  (let ((file-path (format-time-string "%Y%m%d.org"))
        (date (format-time-string "%Y年%m月%d日")))
    (tq-write-file-then-open file-path
                             (tq-render-template-from-sequence "# -*- mode: org -*-
#+options: ^:nil
#+todo: todo(t) in-action(i@/!) delegate(e@/!) delay(y@/!) | done(d@/!) canceled(c@/!)
#+HTML_HEAD: <style type=\"text/css\">body { font-size: large; }</style>
#+title: ${date}
#+date: ${date}
* 事项
* 记录
" "date" date))))

(defun tq-create-problem-record (title)
  "创建问题记录文件。

问题记录文件名字是格式为yyyyMMdd_<title>.org的文本文件。title中不得包含空格、和其他文件名禁止使用的字符。
"
  (interactive "s标题：")
  (let ((file-path (concat (format-time-string "%Y%m%d_") title ".org"))
        (date (format-time-string "%Y年%m月%d日")))
    (tq-write-file-then-open file-path
                             (tq-render-template-from-sequence "# -*- mode: org -*-
#+options: ^:nil
#+todo: todo(t) in-action(i@/!) delegate(e@/!) delay(y@/!) | done(d@/!) canceled(c@/!)
#+HTML_HEAD: <style type=\"text/css\">body { font-size: large; }</style>
#+title: ${title}
#+date: ${date}
* 问题表现
* 排查过程
* 处理方案
* 原因分析
* 改善建议
" 
                                                               "date" date
                                                               "title" title))))
