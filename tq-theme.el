(deftheme tq "A dark sea green color theme")

(custom-theme-set-variables
 'tq
 '(ansi-color-names-vector
   ["#111111" "#AA0000" "#00AA00" "#AA5500" "#0000AA" "#AA00AA" "#00AAAA" "#EDEDED"]))

(custom-theme-set-faces
 'tq
 '(compilation-column-number ((t nil)))
 '(compilation-line-number ((t nil)))
 '(custom-button ((t (:inherit link))))
 '(custom-group-tag ((t (:inherit custom-variable-tag))))
 '(custom-state ((t nil)))
 '(custom-variable-tag ((t (:weight bold))))
 '(custom-visibility ((t (:inherit link))))
 '(default ((t (:inherit nil :stipple nil :background "DarkSeaGreen" :foreground "#000000" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal))))
 '(dired-directory ((t (:foreground "#0000AA" :weight bold))))
 '(error ((t (:background "indian red" :foreground "white"))))
 '(fixed-pitch ((t nil)))
 '(font-lock-builtin-face ((t (:weight normal))))
 '(font-lock-comment-face ((t (:background "medium spring green"))))
 '(font-lock-constant-face ((t (:slant italic :weight bold))))
 '(font-lock-function-call-face ((t (:inherit font-lock-function-name-face))))
 '(font-lock-function-name-face ((t (:background "NavajoWhite2"))))
 '(font-lock-keyword-face ((t (:background "medium sea green"))))
 '(font-lock-preprocessor-face ((t (:weight bold :slant italic))))
 '(font-lock-string-face ((t (:background "medium spring green"))))
 '(font-lock-type-face ((t nil)))
 '(font-lock-variable-name-face ((t nil)))
 '(font-lock-warning-face ((t (:inherit error))))
 '(fringe ((t (:background "dark sea green"))))
 '(header-line ((t (:inherit mode-line))))
 '(highlight ((t (:underline nil :background "medium sea green"))))
 '(link ((t (:foreground "#0000AA" :underline t))))
 '(link-visited ((t (:inherit link :foreground "#440044"))))
 '(minibuffer-prompt ((t (:weight bold))))
 '(mode-line ((t (:background "sea green" :foreground "PaleGoldenrod"))))
 '(mode-line-inactive ((t (:inherit mode-line))))
 '(org-block ((t (:inherit org-block-begin-line))))
 '(org-block-end-line ((t (:inherit org-block-begin-line))) t)
 '(org-document-info ((t (:inherit org-meta-line))))
 '(org-document-info-keyword ((t (:inherit org-meta-line))))
 '(org-document-title ((t (:inherit org-meta-line :weight bold))))

 '(outline-1 ((t (:foreground "black" :background "DarkSeaGreen"))))
 '(outline-2 ((t (:foreground "black" :background "DarkSeaGreen"))))
 '(outline-3 ((t (:foreground "black" :background "DarkSeaGreen"))))
 '(outline-4 ((t (:foreground "black" :background "DarkSeaGreen"))))
 '(outline-5 ((t (:foreground "black" :background "DarkSeaGreen"))))
 '(outline-6 ((t (:foreground "black" :background "DarkSeaGreen"))))
 '(outline-7 ((t (:foreground "black" :background "DarkSeaGreen"))))
 '(outline-8 ((t (:foreground "black" :background "DarkSeaGreen"))))

 '(org-footnote ((t (:foreground "#772277"))))
 '(org-level-1 ((t (:inherit outline-1))))
 '(org-level-2 ((t (:inherit outline-2))))
 '(org-level-3 ((t (:inherit outline-3))))
 '(org-level-4 ((t (:inherit outline-4))))
 '(org-level-5 ((t (:inherit outline-5))))
 '(org-level-6 ((t (:inherit outline-6))))
 '(org-level-7 ((t (:inherit outline-7))))
 '(org-level-8 ((t (:inherit outline-8))))
 '(org-table ((t nil)))
 '(org-verbatim ((t (:inherit font-lock-comment-face))))
 '(region ((t (:background "turquoise"))))
 '(secondary-selection ((t (:background "#C0C0C0"))))
 '(underline ((t nil)))
 '(variable-pitch ((t nil)))
 '(warning ((t (:inherit error))))
 '(whitespace-empty ((t nil)))
 '(whitespace-indentation ((t (:foreground "#AAAAAA"))))
 '(whitespace-line ((t nil)))
 '(whitespace-newline ((t (:foreground "#AAAAAA"))))
 '(whitespace-space ((t (:foreground "#999999"))))
 '(whitespace-space-after-tab ((t (:foreground "#AAAAAA"))))
 '(whitespace-tab ((t (:foreground "#AAAAAA"))))
 '(whitespace-trailing ((t (:inherit whitespace-space))))
 '(widget-button ((t (:inherit link))))
 '(widget-field ((t (:background "#DEDEDE"))))
 '(window-divider ((t (:foreground "LightSalmon3"))))
 '(window-divider-first-pixel ((t (:inherit window-divider))))
 '(window-divider-last-pixel ((t (:inherit window-divider))))

 '(org-code ((t (:foreground "dark slate blue"))))
 ;; 设置已完成任务的标题样式
 '(org-headline-done ((t (:foreground "black" :weight normal))))
 ;; 设置 DONE 关键字的样式
 '(org-done ((t (:foreground "dark green" :weight bold))))
 ;; 设置所有任务状态的通用样式
 '(org-todo ((t (:foreground "pale green" :weight bold))))
 ;; 专门设置 DONE 关键字的样式
 '(org-todo-keyword-done ((t (:foreground "green" :weight bold :italic t)))))

;; Add theme directory to `custom-theme-load-path'
;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'tq)

