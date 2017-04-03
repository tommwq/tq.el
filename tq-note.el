;;; tq-note.el --- 关于笔记的脚本。
(require 'ox-publish)
(require 'ox-html)

(defvar tq-note-path "c:/Users/WangQian/Workspace/Notes/")

(setq org-publish-project-alist
      `(
        ("org-notes"
         :base-directory ,tq-note-path
         :base-extension "txt"
         :publishing-directory ,tq-note-path
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4          
         :auto-preamble nil
         :auto-sitemap t
	 :sitemap-filename "sitemap.txt"
         :sitemap-title "sitemap"
         :section-numbers nil
         :table-of-contents t
         :style "<link rel='stylesheet' type='text/css' href='css/org-manual.css' />"
         :style-include-default nil
	 )
        ("org"
	 :components ("org-notes" "org-static")
	 )
	)
      )

