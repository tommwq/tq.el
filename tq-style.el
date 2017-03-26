(provide 'tq)

(defconst tq-c-style
  '((c-tab-always-indent . t)
    (c-basic-offset . 8)
    (c-comment-only-line-offset . 0)
    (c-echo-syntactic-information-p . t)
    (c-cleanup-list . (brace-else-brace
		       brace-elseif-brace
		       brace-catch-brace
		       empty-defun-braces
		       one-liner-defun
		       defun-close-semi
		       list-close-comma
		       scope-operator
		       space-before-funcall
		       compact-empty-funcall
		       comment-close-slash))
    (c-hanging-braces-alist . (;; (substatement-open after)
			       ;; (inline-open after)
			       ;; (class-open after)
			       ;; (class-close after)
			       ;; (defun-open after)
			       ;; (defun-close after)
			       ;; (brace-entry-open after)
			       ;; (statement after)
			       ;; (case-label after)
			       ;; (else-case)
			       ;; (block-close before)
			       ;; (access-label after)
			       ;; (do-while-closure after)
			       ;; (catch-clause after)
			       ;; (member-init-intro after)
			       ;; (brace-list-open after)
			       (substatement-open nil)
			       (inline-open nil)
			       (class-open nil)
			       (class-close nil)
			       (defun-open nil)
			       (defun-close nil)
			       (brace-entry-open nil)
			       (statement nil)
			       (case-label nil)
			       (else-case)
			       (block-close nil)
			       (access-label nil)
			       (do-while-closure nil)
			       (catch-clause nil)
			       (member-init-intro nil)
			       (brace-list-open nil)))
    (c-hanging-colons-alist .  (
				;;(member-init-intro before)
				;;(inher-intro)
				;;(case-label after)
				;;(access-label after)
				))
    (c-cleanup-list . nil)
    (c-offsets-alist . ((substatement-open . 0)
			(label . 0)
			(case-label . 0)
			(block-open . 0))))
  "tq c style")

(c-add-style "tq-c-style" tq-c-style)
