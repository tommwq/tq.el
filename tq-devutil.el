;; File: develop-utils.el
;; Description: Develop utility functions.
;; Author: Wang Qian
;; Create: 2016-09-10
;; Modify: 2016-12-31

;; todo init-lisp-file
;; todo init-go-file
;; todo init-web-xml
;; todo init-bean-xml
;; todo add-denpendency

(provide 'tq-devutil)

(defun generate-org-file-header (title)
  "Generate org file header."
  (let* ((lines '("# -*- mode: org -*-\n"
		  "#+Title: ${title}\n"
		  "#+Author: Wang Qian\n"
		  "#+Date: Create: ${date}\n"
		  "#+Date: Modify: ${date}\n"
		  "#+Startup: showall\n"
		  "\n"))
	 (date (format-time-string "%Y-%m-%d"))
	 (replace-pairs (list "${date}" date
			      "${title}" title)))
    (replace-regexp-pairs replace-pairs (apply #'concat lines))))

(defun init-org-file (title)
  "Initialize a org file, insert org header lines, and switch on org-mode.
"
  (interactive "sTitle: ")
  (let ((text (generate-org-file-header title)))
    (beginning-of-buffer)
    (insert text)
    (org-mode)))

(defun replace-regexp-pairs (pairs text)
  "Replace regexp pairs in text. 
pairs is a list of string with even number, the odd ones are patterns, 
the even ones are replacements."
  (let ((pattern "")
	(replace ""))
    (while (< 0 (length pairs))
      (setf pattern (pop pairs))
      (setf replace (pop pairs))
      (setf text (replace-regexp-in-string pattern replace text)))
    text))

(defun generate-pom-xml (group-id artifact-id version packaging)
  "generate the pom.xml file.
"
  (let* ((lines
	  '("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
	    "<project xmlns=\"http://maven.apache.org/POM/4.0.0\" \n"
	    "         xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n"
	    "         xsi:schemaLocation=\"http://maven.apache.org/POM/4.0.0\n"
	    "                              http://maven.apache.org/xsd/maven-4.0.0.xsd\">\n"
	    "  <modelVersion>4.0.0</modelVersion>\n"
	    "  <groupId>${groupID}</groupId>\n"
	    "  <artifactId>${artifactID}</artifactId>\n"
	    "  <version>${version}</version>\n"
	    "  <packaging>${packaging}</packaging>\n"
	    "</project>"))
	 (pattern-pairs
	  (list "${groupID}" group-id
	        "${artifactID}" artifact-id
	        "${version}" version
	        "${packaging}" packaging))
	 (pattern nil)
	 (replace nil)
	 (text (apply #'concat lines)))
    (while (< 0 (length pattern-pairs))
      (setf pattern (pop pattern-pairs))
      (setf replace (pop pattern-pairs))
      (setf text (replace-regexp-in-string pattern replace text)))
    text))

(defun init-maven-project (root-path
			   group-id
			   artifact-id
			   version
			   packaging
			   package)
  (interactive "sroot path: \nsgroup-id: \nsartifact-id: \nsversion: \nspackaging: \nspackage: ")
  (create-maven-project root-path group-id artifact-id version packaging package))

(defun generate-web-xml ()
  "Generate content of file web.xml."
  (let ((lines '("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
		 "<web-app version=\"3.1\"\n"
		 "         xmlns=\"http://xmlns.jcp.org/xml/ns/javaee\"\n"
		 "         xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n"
		 "         xmlns:mvc=\"http://www.springframework.org/schema/mvc\"\n"
		 "         xsi:schemaLocation=\"http://xmlns.jcp.org/xml/ns/javaee\n"
		 "	                     http://xmlns.jcp.org/xml/ns/javaee/web-app_3_1.xsd\">\n"
		 "  <display-name></display-name>\n"
		 "\n"
		 "  <!--\n"
		 "  <context-param>\n"
		 "    <param-name></param-name>\n"
		 "    <param-value></param-value>\n"
		 "  </context-param>\n"
		 "  \n"
		 "  <listener>\n"
		 "    <listener-class></listener-class>\n"
		 "  </listener>\n"
		 "\n"
		 "  <filter>\n"
		 "    <filter-name></filter-name>\n"
		 "    <filter-class></filter-class>\n"
		 "    <async-supported></async-supported>\n"
		 "    <init-param>\n"
		 "      <description></description>\n"
		 "      <param-name></param-name>\n"
		 "      <param-value></param-value>\n"
		 "    </init-param>\n"
		 "  </filter>\n"
		 "  <filter-mapping>\n"
		 "    <filter-name></filter-name>\n"
		 "    <url-pattern></url-pattern>\n"
		 "  </filter-mapping>\n"
		 "  -->\n"
		 "\n"
		 "  <servlet>\n"
		 "    <servlet-name></servlet-name>\n"
		 "    <servlet-class></servlet-class>\n"
		 "    <init-param>\n"
		 "      <param-name></param-name>\n"
		 "      <param-value></param-value>\n"
		 "    </init-param>\n"
		 "    <load-on-startup></load-on-startup>\n"
		 "    <async-supported></async-supported>\n"
		 "  </servlet>\n"
		 "  <servlet-mapping>\n"
		 "    <servlet-name></servlet-name>\n"
		 "    <url-pattern>/</url-pattern>\n"
		 "  </servlet-mapping>\n"
		 "</web-app>\n")))
    (apply #'concat lines)))


(defun create-maven-project (root-path
			     group-id
			     artifact-id
			     version
			     packaging
			     package)
  "Create maven project
"
  (let* ((package-path (replace-regexp-in-string "\\." "/" package))
	 (paths (list (expand-file-name
		       (concat artifact-id "/src/main/java/" package-path) root-path)
		      (expand-file-name
		       (concat artifact-id "/src/test/java/" package-path) root-path)))
	 (pom-xml-file (expand-file-name
			(concat root-path "/" artifact-id "/pom.xml")))
	 (web-paths (list
		     (expand-file-name
		      (concat artifact-id "/src/main/webapp/WEB-INF/") root-path)))
	 (web-xml-file (expand-file-name
			(concat artifact-id "/src/main/webapp/WEB-INF/web.xml") root-path)))
    (dolist (path paths)
      (make-directory path t))
    (when (file-exists-p pom-xml-file)
      (delete-file pom-xml-file))
    (append-to-file (generate-pom group-id artifact-id version packaging)
		    nil pom-xml-file)
    (when (string= packaging "war")
      (dolist (path web-paths)
	(make-directory path t))
      (when (file-exists-p web-xml-file)
	(delete-file web-xml-file))
      (append-to-file (generate-web-xml) nil web-xml-file))))

(defun upcase-first-letter (word)
  "Upcase first letter of a word."
  (if (zerop (length word))
      word
    (concat (upcase (substring word 0 1))
	    (substring word 1))))

(defun downcase-first-letter (word)
  "Downcase first letter of a word."
  (if (zerop (length word))
      word
    (concat (downcase (substring word 0 1))
	    (substring word 1))))

(defun generate-java-class (package class-name description)
  (let* ((lines (list "/**\n"
		      " * File: ${className}.java\n"
		      " * Description: ${description}\n"
		      " * Author: Wang Qian\n"
		      " * Create: ${date}\n"
		      " * Modify: ${date}\n"
		      " */\n"
		      "\n"
		      "package ${package};\n"
		      "\n"
		      "public class ${className} {\n"
		      "    public ${className}() {\n"
		      "    }\n"
		      "}\n"
		      "\n"))
	 (pairs (list  "${className}" class-name
		       "${description}" description
		       "${package}" package
		       "${date}" (format-time-string "%Y-%m-%d"))))
    (replace-regexp-pairs pairs (apply #'concat lines))))

(defun init-java-class (package class-name description)
  "Initialize java class."
  (interactive "sPackage: \nsClass: \nsDescription: ")
  (let* ((package-path (replace-regexp-in-string "\\." "/" package))
	 (path (concat "src/main/java/" package-path))
	 (filename (concat path "/" class-name ".java")))
    (make-directory path t)
    (append-to-file
     (generate-java-class package class-name description)
     nil filename)
    (find-file filename)))

(defun generate-junit-test-class (package class-name method-name)
  "Generate junit test class. class-name is the name of class to be tested, 
method-name is the name of method to be tested."
  (let* ((lines (list "/**\n"
		      " * File: Test${className}.java\n"
		      " * Description: Unit test for ${className}.\n"
		      " * Author: Wang Qian\n"
		      " * Create: ${date}\n"
		      " * Modify: ${date}\n"
		      " */\n"
		      "\n"
		      "package ${package};\n"
		      "\n"
		      "import junit.framework.TestCase;\n"
		      "\n"
		      "public class Test${className} extends TestCase {\n"
		      "    public void test${methodName}() {\n"
		      "    }\n"
		      "}\n"
		      "\n"))
	 (pairs (list  "${className}" class-name
		       "${methodName}" (upcase-first-letter method-name)
		       "${package}" package
		       "${date}" (format-time-string "%Y-%m-%d"))))
    (replace-regexp-pairs pairs (apply #'concat lines))))

(defun init-junit-test-class (package class-name method-name)
  "Initialize junit test class."
  (interactive "sPackage: \nsClass: \nsMethod: ")
  (let* ((package-path (replace-regexp-in-string "\\." "/" package))
	 (path (concat "src/test/java/" package-path))
	 (filename (concat path "/Test" class-name ".java")))
    (make-directory path t)
    (append-to-file
     (generate-junit-test-class package class-name method-name)
     nil filename)
    (find-file filename)))

(defun tq-generate-html-file (title)
  "Generate HTML file header."
  (let* ((lines (list
		  "<!DOCTYPE html>"
		  "<html>"
		  "<head>"
		  (format "<title>%s</title>" title)
		  "<meta charset=utf-8 />"
		  "</head>"
		  "<body>"
		  "</body>"
		  "</html>"
		  )))
    (apply #'concat (mapcar (lambda (x) (concat x "\n")) lines))))

(defun tq-init-html-file (title)
  "Initialize a html file, insert header lines, and switch on html-mode.
"
  (interactive "sTitle: ")
  (let ((text (tq-generate-html-file title)))
    (beginning-of-buffer)
    (insert text)
    (html-mode)))
