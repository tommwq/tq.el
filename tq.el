;; tq.el
;; 常用功能库
;; 2019年09月22日

;; usage:
;; (add-to-list 'load-path "/path/to//tq.el/")
;; (require 'tq)

(provide 'tq)

(require 'tq-command)
(require 'tq-util)
(require 'tq-str)
(require 'tq-file)
(require 'tq-template)
(require 'tq-math)
(require 'tq-java)
(require 'tq-quarkus)
(require 'tq-android)
(require 'tq-settings)

(defgroup tq nil
  ""
  :tag "tq")

(defcustom tq-indent-offset 2
  "缩进。"
  :type 'integer
  :group 'tq)

(defcustom tq-record-directory "~/record"
  "日志目录"
  :type 'string
  :group 'tq)

(defcustom tq-latin-font-size 12
  "字体大小"
  :type 'integer
  :set (lambda (symbol value)
         (progn
           (set-default symbol value)
           (tq-set-font)))
  :group 'tq)

(defcustom tq-chinese-font-size 18
  "字体大小"
  :type 'integer
  :set (lambda (symbol value)
         (progn
           (set-default symbol value)
           (tq-set-font)))
  :group 'tq)

(defcustom tq-font-size 10
  "字体大小"
  :type 'integer
  :set (lambda (symbol value)
         (progn
           (set-default symbol value)
           (set-default 'tq-latin-font-size value)
           (set-default 'tq-chinese-font-size value)
           (tq-set-font)))
  :group 'tq)

(defcustom tq-latin-font "PT Mono"
  "英文字体"
  :type 'string
  :set (lambda (symbol value)
         (progn
           (set-default symbol value)
           (tq-set-font)))
  :group 'tq)

(defcustom tq-chinese-font "方正风雅宋 简"
  "汉字字体"
  :type 'string
  :set (lambda (symbol value)
         (progn
           (set-default symbol value)
           (tq-set-font)))
  :group 'tq)

(defcustom tq-resource-directory "~/"
  "资源目录"
  :type 'string
  :group 'tq)

(defcustom tq-template-directory "~/template"
  "模板目录"
  :type 'string
  :group 'tq)


