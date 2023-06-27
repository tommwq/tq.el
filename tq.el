;; tq.el
;; 常用功能库
;; 2019年09月22日

;; usage:
;; (add-to-list 'load-path "/path/to//tq.el/")
;; (require 'tq)

(let ((file-names (list "common/tq-common.el"
                        "common/tq-str.el"
                        "common/tq-file.el"
                        "common/tq-util.el"
                        "common/tq-template.el"
                        "math/tq-math.el"
                        "java/tq-java.el"
                        "java/tq-quarkus.el"
                        "android/tq-android.el"
                        "tq-local.el"
                        "tq-tmp.el"
                        "tq-settings.el"))
      (file-full-name nil))
  (dolist (file-name file-names)
    (setf file-full-name (expand-file-name file-name (file-name-directory load-file-name)))
    (message (format "加载%s" file-full-name))
    (load file-full-name)))

(provide 'tq)


