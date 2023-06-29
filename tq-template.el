

(defun tq-template-render (template hash)
  "渲染模板。将template中的字符串${foo}更换为hash[foo]的值。"
  (let ((holder "")
        (value "")
        (result template))
    (dolist (key (hash-table-keys hash))
      (setf holder (format "${%s}" key))
      (setf value (gethash key hash))
      (setf result (replace-regexp-in-string holder value result)))
    result))

(defun tq-template-render-sequence (template &rest sequence)
  "渲染模板。从sequence建立散列表进行渲染。"
  (tq-template-render template (apply #'tq-util-make-string-hash sequence)))

(defun tq-template-get-template-file-name (template-name)
  (expand-file-name template-name
                    (expand-file-name "resource/template" 
                                      (file-name-directory 
                                       (directory-file-name
                                        (file-name-directory tq-template-directory))))))


;; (progn
;;   (defun tq-template-create-file (template-file-name output-directory value-hash-table)
;;     ;; template-file-name
;;     "1"
;;     )

;;   (defun tq-template-create-folder (template-file-name output-directory value-hash-table)
;;     (let ((relative-file-name nil)
;;           (output-file-name nil)
;;           (output-file-content nil))
;;       (dolist (file-name (directory-files-recursively template-file-name ".*"))
;;         (setf relative-file-name (string-remove-prefix (concat template-file-name "/") file-name))
;;         (setf output-file-name (expand-file-name (tq-template-render relative-file-name value-hash-table) output-directory))
;;         ;; TODO 设置output-file-content
;;         (prin1 output-file-name))))

;;   (defun tq-template-create (template-name output-directory value-hash-table)
;;     (let ((template-file-name (tq-template-get-template-file-name template-name)))
;;       (if (file-directory-p template-file-name)
;;           (tq-template-create-folder template-file-name output-directory value-hash-table)
;;         (tq-template-create-file template-file-name output-directory value-hash-table))))

;;   (tq-template-create "AndroidApp" "d:/test/template/" 
;;                       (tq-make-string-hash "PackagePath" "com/foo/bar"))
;;   )

(provide 'tq-template)
