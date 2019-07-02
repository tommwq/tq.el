
;; Android项目生成程序处理逻辑
;; 输入项目信息。
;; - 包名。
;; - 根目录。
;; 创建目录。
;; 读取目录模板。
;; 生成目录名。
;; 逐一建立目录。
;; 文件模板：文件名模板+文件内容模板。
;; ROOT
;; ROOT/app/src/androidTest/java/PACKAGE
;; ROOT/app/src/main/aidl/PACKAGE
;; ROOT/app/src/main/java/PACKAGE
;; ROOT/app/src/main/res/PACKAGE
;; ROOT/app/src/main/res/drawable
;; ROOT/app/src/main/res/layout
;; ROOT/app/src/main/res/values
;; ROOT/app/src/test/java/PACKAGE
;; 使用navigation/room/kotlin/
;; 创建gradle文件。
;; 初始化gradle。
;; 创建kotlin文件。
;; 初始化git仓库。

(load-file "tq-android-template.el")

(progn
  (defconst tq-android-placeholder-pattern "\\($[A-Z_]*\\)")
  (defun tq-android-make-file-template (name-template content-template)
    (list name-template content-template))

  (defun tq-android-render-file-template(file-template)
    (list tq-android-render-template (first file-template)
          tq-android-render-template (second file-template)))

  (defun tq-android-find-placeholders(template)
    (let ((start (setf start (string-match tq-android-placeholder-pattern template)))
          (begin 0)
          (end 0)
          (result (make-hash-table)))
      (while start
        (setf begin (match-beginning 1))
        (setf end (match-end 1))
        (puthash (substring template begin end) t result)
        (setf start (string-match tq-android-placeholder-pattern template end)))
      result))
  
  (defun tq-android-render-template(template value-table)
    (let ((symbol-table (tq-android-find-placeholders template))
          (result template)
          (replacement ""))
      (maphash
       (lambda (key value)
         (setf replacement (gethash (substring key 1) value-table ""))
         (setf result (replace-regexp-in-string key replacement result t)))
       symbol-table)
      result))

  (defun tq-android-create-file-from-template(file-template value-table)
    (let ((filename (tq-android-render-template (car file-template) value-table))
          (content (tq-android-render-template (cadr file-template) value-table)))
      ;; 建立父目录
      (if (file-name-directory filename)
          (make-directory (file-name-directory filename) t))
      ;; 生成文件
      (append-to-file content nil filename)))

  (defun test ()
    (setf vtbl (make-hash-table :test 'equal))
    (puthash "SDK_ROOT" "C/:/Users/guosen/AppData/Local/Android/Sdk" vtbl)
    (puthash "ROOT" "d:/workspace/project/tq.el/test/" vtbl)

    (dolist (tpl (list tq-android-template-settings-gradle
                       tq-android-template-local-properties
                       tq-android-template-gradle-properties
                       tq-android-template-build-gradle))
      (tq-android-create-file-from-template
       (tq-android-make-file-template (car tpl) (cadr tpl))
       vtbl)))
  (test)
  )
