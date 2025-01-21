(provide 'tq-ai)

(defcustom tq-deepseek-api-key ""
  "DeepSeek apk_key"
  :type 'string
  :group 'tq)


(defun tq-deepseek-extract-content (response)
  "Extract the content from the first choice in the RESPONSE alist."
  (let* ((choices (cdr (assoc 'choices response)))
         (first-choice (aref choices 0))          
         (message (cdr (assoc 'message first-choice)))
         (content (cdr (assoc 'content message))))
    (decode-coding-string (encode-coding-string content 'utf-8) 'utf-8)))

(defun tq-deepseek-chat-async (system-content user-content)
  "Send a request to DeepSeek API asynchronously.
SYSTEM-CONTENT is the system message content.
USER-CONTENT is the user message content.
On success, append the result to the *deepseek* buffer and show a message."
  (let* ((url "https://api.deepseek.com/chat/completions")
         (data (encode-coding-string
                (json-encode
                 `((model . "deepseek-chat")
                   (messages . [((role . "system") (content . ,system-content))
                                ((role . "user") (content . ,user-content))])
                   (stream . :json-false))) 'utf-8))
         (url-request-data data)
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " tq-deepseek-api-key))))
         (url-request-method "POST"))
    (url-retrieve
     url
     (lambda (status)
       (if (plist-get status :error)
           (message "DeepSeek request failed: %s" (plist-get status :error))
         (goto-char (point-min))
         (re-search-forward "\n\n")
         (let ((response (json-read-from-string (buffer-substring (point) (point-max))))
               (text-position nil))
           (with-current-buffer (get-buffer-create "*deepseek*")
             (goto-char (point-max))
             (insert "\n\n")
             (setf text-position (point-max))
             (insert (tq-deepseek-extract-content response))
             (goto-char text-position))
           (message "deepseek-done"))))
     nil
     data)))


(defun tq-deepseek ()
  (interactive)
  (let ((content (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
    (tq-deepseek-chat-async "" content)))

(defun tq-deepseek-translate ()
  (interactive)
  (let ((content (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
    (tq-deepseek-chat-async "你是一名翻译员，擅长将外文翻译成中文" (concat "翻译 " content))))
