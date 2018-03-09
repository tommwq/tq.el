;; tq.el
;; 设置emacs样式，提供开发使用的辅助功能。
;; 2018年03月08日

;; todo
;; TODO init shell env
;; tq-gradle-program
;; tq-gen-lisp-file
;; tq-init-lisp-file
;; tq-create-lisp-file
;; tq-gen-go-file
;; tq-init-go-file
;; tq-create-go-file
;; tq-gen-bean-file
;; tq-init-bean-file
;; tq-create-bean-file
;; tq-init-java-class
;; tq-create-java-class
;; tq-init-note
;; tq-create-note
;; tq-gen-note
;; tq-gen-html
;; tq-init-html
;; tq-create-html
;; tq-create-gradle-project
;; 函数gen-xxx用于生成对应的内容。
;; 函数new-xxx用于创建新文件，将对应内容写入文件，并打开该文件。

(defun tq-update-chinese-font (symbol value)
  "设置tq-chinese-font或tq-chinese-font-size时调用。"
  (set-default symbol value)
  (when (and (boundp 'tq-chinese-font)
             (boundp 'tq-chinese-font-size))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font t
                        charset
                        (font-spec :family tq-chinese-font :size tq-chinese-font-size)))))

(defun tq-update-latin-font (symbol value)
  "设置tq-latin-font或tq-latin-font-size时调用。"
  (set-default symbol value)
  (when (and (boundp 'tq-latin-font)
             (boundp 'tq-latin-font-size))
    (set-frame-font (format "%s-%d" tq-latin-font tq-latin-font-size))))

(defcustom tq-chinese-font "微软雅黑"
  "中文字体。"
  :type 'string
  :group 'tq
  :set #'tq-update-chinese-font)

(defcustom tq-chinese-font-size 18
  "中文字体尺寸。"
  :type 'integer
  :group 'tq
  :set #'tq-update-chinese-font)

(defcustom tq-latin-font "Source Code Pro"
  "拉丁字母字体。"
  :type 'string
  :group 'tq
  :set #'tq-update-latin-font)

(defcustom tq-latin-font-size 14
  "拉丁字母字体尺寸。"
  :type 'integer
  :group 'tq
  :set #'tq-update-latin-font)

(defcustom tq-working-directory "."
  "工作目录。"
  :type 'directory
  :set #'(lambda (symbol value)
           (set-default symbol value)
           (setf default-directory tq-working-directory))
  :group 'tq)

(defcustom tq-git-program "git"
  "git程序路径（含文件名）。会影响magit-git-executable的值。"
  :type 'file
  :group 'tq)

(defcustom tq-python-program "python"
  "python程序路径（含文件名）。会影响python-shell-interpreter的值。"
  :type 'file
  :group 'tq)

(defun windows-maximize-window ()
  "w32全屏显示。"
  (interactive)
  (let ((sc-maximize 61488))
    (w32-send-sys-command sc-maximize)))

(defun linux-maximize-window ()
  "linux全屏显示。"
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NEW_WM_STATE_FULLSCREEN" 0)))

(defun maximize-window ()
  "让窗口全屏显示。"
  (if (string-equal system-type "windows-nt")
      (windows-maximize-window)
    (linux-maximize-window)))

(defun tq-insert-time ()
  "在buffer中插入时间字符串。"
  (interactive)
  (insert
   (format-time-string "%H时%M分%S秒")))

(defun tq-insert-date ()
  "在buffer中插入日期字符串。"
  (interactive)
  (insert
   (format-time-string "%Y年%m月%d日")))

(defun tq-insert-datetime ()
  "在buffer中插入日期时间字符串。"
  (interactive)
  (insert
   (format-time-string "%Y年%m月%d日 %H时%M分%S秒")))

(defun inch-to-centimeter (inch)
  "将英寸转换为厘米。"
  (* inch 2.54))

(defun centimeter-to-inch (cm)
  "将厘米转换为英寸。"
  (/ cm 2.54))

(defun calculate-mobilephone-screen-width (diagonal-length)
  "计算手机屏幕宽度。
diagonal-length是屏幕尺寸，如5.2寸。
返回以厘米为单位的屏幕宽度。
计算使用的长宽比为1.78:1。"
  (let ((dl (inch-to-centimeter diagonal-length)))
    (sqrt (/ (* dl dl) (+ 1 (* 1.78 1.78))))))

(defun calculate-mobilephone-screen-height (diagonal-length)
  "计算手机屏幕高度。"
  (* 1.78 (calculate-mobilephone-screen-width diagonal-length)))

(defun print-mobilephone-width-and-height (x)
  "打印手机屏幕长、宽。
x是屏幕寸数，如5.2寸。
"
  (let ((w (calculate-mobilephone-screen-width x))
        (h (calculate-mobilephone-screen-height x)))
    (princ (format "%.1f寸屏幕 长 %.2f 厘米, 宽 %.2f 厘米\n" x h w)))
  nil)

(defun calculate-mobilephone-dpi (dl height-pixels)
  "计算手机屏幕dpi。
dl是英尺数。
height-pixels是长边像素数。
"
  (let ((h (calculate-mobilephone-screen-height dl)))
    (/ height-pixels (centimeter-to-inch h))))

(defun calculate-mobilephone-ppi (diagonal-length width-pixels height-pixels)
  "计算手机屏幕PPI。
diagonal-length 屏幕尺寸。
width-pixels 屏幕宽像素数。
height-pixels 屏幕长像素数。
"
  (/ (sqrt (+ (* width-pixels width-pixels)
              (* height-pixels height-pixels)))
     diagonal-length))

(defun first-char (s)
  "返回字符串首字母。"
  (if (zerop (length s))
      ""
    (string (nth 0 (string-to-list s)))))

(defmacro tq-to-string (x)
  "Convert object or symbol to string.
将对象或符号转换为字符串。"
  `(replace-regexp-in-string "\\\\" "" (prin1-to-string ',x)))

(defun tq-upcase-first-letter (word)
  "Upcase first letter of a word."
  (if (zerop (length word))
      word
    (concat (upcase (substring word 0 1))
            (substring word 1))))

(defun tq-downcase-first-letter (word)
  "Downcase first letter of a word."
  (if (zerop (length word))
      word
    (concat (downcase (substring word 0 1))
            (substring word 1))))

(defun tq-replace-regexp-pairs (pairs text)
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

(defun tq-create-file (filename content)
  "Create an initialize a file."
  (let* ((absolute-filename
          (if (file-name-absolute-p filename)
              filename
            (expand-file-name filename default-directory)))	   
         (path (file-name-directory absolute-filename)))
    (when (file-exists-p absolute-filename)
      (error "File existed. path: %s." absolute-filename))
    (unless (file-exists-p path)
      (make-directory path t))
    (append-to-file content nil absolute-filename)))

(defun tq-create-file-then-open (filename content)
  "Create an initialize a file, then open it."
  (let* ((absolute-filename
          (if (file-name-absolute-p filename)
              filename
            (expand-file-name filename default-directory)))	   
         (path (file-name-directory absolute-filename)))
    (when (file-exists-p absolute-filename)
      (error "File existed. path: %s." absolute-filename))
    (unless (file-exists-p path)
      (make-directory path t))
    (find-file absolute-filename)
    (beginning-of-buffer)
    (insert content)
    (save-buffer)))

(defun tq-gen-org-file (title)
  "生成org文件头。"
  (let* ((lines '("# -*- mode: org -*-\n"
                  "#+TITLE: ${title}\n"
                  "#+AUTHOR: 汪千\n"
                  "#+DATE: ${date}\n"
                  "#+STARTUP: SHOWEVERYTHING\n"
                  "#+TODO: TODO(t) IN-ACTION(i@/!) WAIT(w@/!) | DONE(d!) CANCELED(c@)"
                  "\n"))
         (date (format-time-string "%Y-%m-%d"))
         (replace-pairs (list "${date}" date
                              "${title}" title)))
    (tq-replace-regexp-pairs replace-pairs (apply #'concat lines))))

(defun tq-create-org-file (filename)
  "建立并初始化org文件。"
  (interactive "sFileName: ")
  (let* ((title (file-name-base filename))
         (text (tq-gen-org-file title)))
    (tq-create-file-then-open filename text)
    (org-mode)))

(defun tq-gen-pom-file (group-id artifact-id version packaging)
  "generate the pom.xml file."
  (let* ((lines
          '("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
            "<project xmlns=\"http://maven.apache.org/POM/4.0.0\" \n"
            "\t xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n"
            "\t xsi:schemaLocation=\"http://maven.apache.org/POM/4.0.0\n"
            "\t\t\t     http://maven.apache.org/xsd/maven-4.0.0.xsd\">\n"
            "\t<modelVersion>4.0.0</modelVersion>\n"
            "\t<groupId>${groupID}</groupId>\n"
            "\t<artifactId>${artifactID}</artifactId>\n"
            "\t<version>${version}</version>\n"
            "\t<packaging>${packaging}</packaging>\n"
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

(defun tq-init-pom-file (group-id artifact-id version packaging)
  (interactive "sGroupId: \nsArtifactId: \nsVersion: \nsPackaging: ")
  (let ((text (tq-gen-pom-file group-id artifact-id version packaging)))
    (beginning-of-buffer)
    (insert text)
    (xml-mode)))

(defun tq-create-pom-file (group-id artifact-id version packaging directory)
  (interactive "sGroupId: \nsArtifactId: \nsVersion: \nsPackaging: \nsDirectory: ")
  (let* ((filename (expand-file-name "pom.xml"
                                     (if (string= "" directory)
                                         default-directory
                                       directory)))
         (text (tq-gen-pom-file group-id artifact-id version packaging)))
    (tq-create-file-then-open filename text)
    (xml-mode)))

(defun tq-gen-web-xml-file ()
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

(defun tq-init-web-xml-file ()
  (interactive)
  (let ((text (tq-gen-web-xml-file)))
    (beginning-of-buffer)
    (insert text)
    (xml-mode)))

(defun tq-create-web-xml-file ()
  (interactive "sFilename: ")
  (let ((text (tq-gen-web-xml-file)))
    (tq-create-file-then-open filename text)
    (xml-mode)))


(defun tq-init-html-file (title)
  "Initialize a html file, insert header lines, and switch on html-mode.
"
  (interactive "sTitle: ")
  (let ((text (tq-gen-html-file title)))
    (beginning-of-buffer)
    (insert text)
    (html-mode)))

(defun tq-gen-android-manifest-file (project label)
  "Generate AndroidManifest.xml file."
  (let ((template "<?xml version="1.0" encoding="utf-8"?>
<manifest xmlns:android="http://schemas.android.com/apk/res/android"
	  package="${package}"
	  android:versionCode="1"
	  android:versionName="1.0">

	<application android:label="${label}">
		<activity android:name="${activity}"
			  android:label="${label}">
			<intent-filter>
				<action android:name="android.intent.action.MAIN" />
				<category android:name="android.intent.category.LAUNCHER" />
			</intent-filter>
		</activity>
	</application>

 	<uses-permission android:name="android.permission.WRITE_EXTERNAL_STORAGE"/>
</manifest> 
"))
    (tq-replace-regexp-pairs (list "${project}" project
                                   "${label}" label))))

(defun tq-init-android-manifest-file (package label)
  "Initialize a AndroidManifest.xml file."
  (interactive "sPackage: \nsLabel: ")
  (let ((content (tq-gen-android-manifest-file package label)))
    (beginning-of-buffer)
    (insert content)
    (xml-mode)))

(defun tq-create-gradle-project (root-path project-name)
  "Create a gradle project for java."
  (interactive "sRootPath: \nsProjectName: ")
  (let ((subdirs (list "src/main/java" "src/test/java"))
        (project-path (expand-file-name project-name root-path)))
    (when (file-exists-p project-path)
      (error "Project directory existed. path: %s" project-path))
    (make-directory project-path t)
    (tq-create-file (expand-file-name "build.gradle" project-path)
                    "apply plugin: 'java'
apply plugin: 'application'
buildscript {
        repositories {
                jcenter()
        }
        dependencies {
        }
}
")
    (dolist (subdir subdirs)
      (make-directory (expand-file-name subdir project-path) t))))

(defun create-maven-project (root-path
                             group-id
                             artifact-id
                             version
                             packaging
                             package)
  "Create maven project"
  (let* ((package-path (replace-regexp-in-string "\\." "/" package))
         (paths (list (expand-file-name
                       (concat artifact-id "/src/main/java/" package-path) root-path)
                      (expand-file-name
                       (concat artifact-id "/src/test/java/" package-path) root-path)))
         (pom-file (expand-file-name
                    (concat root-path "/" artifact-id "/pom.xml")))
         (web-paths (list
                     (expand-file-name
                      (concat artifact-id "/src/main/webapp/WEB-INF/") root-path)))
         (web-xml-file (expand-file-name
                        (concat artifact-id "/src/main/webapp/WEB-INF/web.xml") root-path)))
    (dolist (path paths)
      (make-directory path t))
    (when (file-exists-p pom-file)
      (delete-file pom-file))
    (append-to-file (tq-gen-pom-file group-id artifact-id version packaging)
                    nil pom-file)
    (when (string= packaging "war")
      (dolist (path web-paths)
        (make-directory path t))
      (when (file-exists-p web-xml-file)
        (delete-file web-xml-file))
      (append-to-file (tq-gen-web-xml) nil web-xml-file))))

;; TODO rewrite it
(defun tq-gen-java-class (package class-name description)
  "Generate a java class."
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
    (tq-replace-regexp-pairs pairs (apply #'concat lines))))

;; TODO rewrite this
(defun tq-init-java-class (package class-name description)
  "Initialize java class."
  (interactive "sPackage: \nsClass: \nsDescription: ")
  (let* ((package-path (replace-regexp-in-string "\\." "/" package))
         (path (concat "src/main/java/" package-path))
         (filename (concat path "/" class-name ".java")))
    (make-directory path t)
    (append-to-file
     (tq-gen-java-class package class-name description)
     nil filename)
    (find-file filename)))

(defun tq-create-java-class (root-path package-name class-name)
  "Create a java file."
  (interactive "sPath: \nsPackage: \nsClass: ")
  (let ((filename (expand-file-name
                   (concat (replace-regexp-in-string "\\." "/" package-name)
                           "/"
                           class-name
                           ".java")
                   root-path))
        (content (tq-gen-java-class package-name class-name "")))
    (tq-create-file-then-open filename content)))

(defun tq-gen-junit-test-class (package class-name method-name)
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
                       "${methodName}" (tq-upcase-first-letter method-name)
                       "${package}" package
                       "${date}" (format-time-string "%Y-%m-%d"))))
    (tq-replace-regexp-pairs pairs (apply #'concat lines))))

(defun init-junit-test-class (package class-name method-name)
  "Initialize junit test class."
  (interactive "sPackage: \nsClass: \nsMethod: ")
  (let* ((package-path (replace-regexp-in-string "\\." "/" package))
         (path (concat "src/test/java/" package-path))
         (filename (concat path "/Test" class-name ".java")))
    (make-directory path t)
    (append-to-file
     (tq-gen-junit-test-class package class-name method-name)
     nil filename)
    (find-file filename)))

(defun tq-gen-html-file (title)
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

(defmacro tq-gen-java-simple-class (package class-name &rest field-pairs)
  "Generate code for simple class.

A simple class is a class whose methods are all getter/setters. 

odd ones of FIELD-PAIRS are data types, even ones are field names.

EXAMPLES:
(tq-gen-java-simple-class org.tq.smsmanager.domain Message int id)
"
  `(let* ((template "/**
* ${className}.java
* ${date}
*/

package ${package};

public class ${className} {
${body}
}
")
	(body "")
	(package-string (tq-to-string ,package))
	(class-name-string (tq-to-string ,class-name))
	(field-list (mapcar (lambda (x) (prin1-to-string x)) '(,@field-pairs)))
	(pair-count (/ (length field-list) 2))
	)

    (dotimes (pair-index pair-count)
      (let ((type (nth (* 2 pair-index) field-list))
	    (name (nth (1+ (* 2 pair-index)) field-list)))
	(setf body (concat body (format "        private %s %s;
" type name)))))

    (dotimes (pair-index pair-count)
      (let* ((type (nth (* 2 pair-index) field-list))
	     (field (nth (1+ (* 2 pair-index)) field-list))
	     (name (tq-upcase-first-letter field)))
	(setf body (concat body (tq-replace-regexp-pairs (list "${type}" type
							       "${name}" name
							       "${className}" class-name-string
							       "${field}" field
							       )
							 "
public ${type} get${name}() {
return this.${field};
}

public ${className} set${name}(${type} value) {
this.${field} = value;
return this;
}
")))))

    (tq-replace-regexp-pairs (list "${className}" class-name-string
				   "${date}" (format-time-string "%Y-%m-%d")
				   "${package}" package-string
				   "${body}" body)
			     template)))

(defmacro tq-create-java-simple-class (root-path package-name class-name &rest field-pairs)
  `(let ((filename (expand-file-name (concat (replace-regexp-in-string
                                              "\\." "/"
                                              (tq-to-string ,package-name)) "/" (tq-to-string ,class-name) ".java")
                                     ,root-path))
         
         (content (tq-gen-java-simple-class
                   ,package-name
                   ,class-name
                   ,@field-pairs)))
     (tq-create-file-then-open filename content)))

(defconst tq-android-layout-xml-template
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<layout xmlns:android=\"http://schemas.android.com/apk/res/android\">
        <data>
                <variable name=\"state\" type=\"${package}.State\" />
        </data>
        <LinearLayout android:orientation=\"vertical\"
        	      android:layout_width=\"fill_parent\"
        	      android:layout_height=\"fill_parent\"
        	      >
        	<TextView
        		android:layout_width=\"fill_parent\"
        		android:layout_height=\"wrap_content\"
        		android:text=\"@{state.message}\"
        		/>
        </LinearLayout>
</layout>
"
  "Android工程布局文件模板。")

(defconst tq-android-activity-class-template
  "package ${package}.activity;

import android.app.Activity;
import android.databinding.DataBindingUtil;
import android.os.Bundle;
import ${package}.R;
import ${package}.databinding.ActivityMainBinding;

public class ${activity} extends Activity {
        @Override
        public void onCreate(Bundle savedInstanceState) {
                super.onCreate(savedInstanceState);
                ActivityMainBinding binding = DataBindingUtil.setContentView(this, R.layout.activity_main);
        }
}
"
  "Android工程activity源代码模板。")

(defconst tq-android-manifest-xml-template
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<manifest xmlns:android=\"http://schemas.android.com/apk/res/android\"
	  package=\"${package}\"
	  android:versionCode=\"${versionCode}\"
	  android:versionName=\"${versionName}\">

	<application android:label=\"${label}\">
		<activity android:name=\".activity.${activity}\"
			  android:label=\"${label}\">
			<intent-filter>
				<action android:name=\"android.intent.action.MAIN\" />
				<category android:name=\"android.intent.category.LAUNCHER\" />
			</intent-filter>
		</activity>
	</application>

 	<uses-permission android:name=\"android.permission.WRITE_EXTERNAL_STORAGE\"/>
</manifest> 
"
  "AndroidManifest.xml文件模板。")

(defconst tq-android-project-build-gradle-content
  "buildscript {
        repositories {
                jcenter()
        }
        dependencies {
                classpath 'com.android.tools.build:gradle:2.2.3+'
        }
}

allprojects {
        repositories {
                jcenter()
                mavenCentral()
        }
}
"
  "Android工程build.gradle脚本文件内容。")


(defconst tq-android-project-settings-gradle-content
  "include ':app'"
  "Android工程settings.gradle脚本文件内容。")

(defconst tq-android-app-build-gradle-template
  "apply plugin: 'com.android.application'

android {
        compileSdkVersion ${compileSdkVersion}
        buildToolsVersion '${buildToolsVersion}'
        defaultConfig {
                applicationId '${applicationId}'
                minSdkVersion 19
                targetSdkVersion 19
                versionCode ${versionCode}
                versionName '${versionName}'
        }
        buildTypes {
                debug {
                        resValue 'string', 'app_name', 'Example DEBUG'
                }
                release {
                        resValue 'string', 'app_name', 'Example'
                        
                        minifyEnabled false
                        proguardFiles getDefaultProguardFile('proguard-android.txt'), 'proguard-rules.pro'
                }
        }
        dataBinding {
                enabled = true
        }
}

dependencies {
        compile fileTree(dir: 'libs', include: ['*.jar'])
}

buildscript {
        repositories {
                jcenter()
        }

        dependencies {
                classpath 'com.android.tools.build:gradle:+'
        }
}
"
  "Android工程app模块build.gradle文件模板。")

(defun tq-execute-template (variable-pairs template)
  "渲染模板。将模板中的${foo}替换为variable-pairs中foo对应的值。"
  (let ((pairs nil)
        (pattern "")
        (replace ""))
    (while variable-pairs
      (setf pattern (format "${%s}" (pop variable-pairs)))
      (setf replace (pop variable-pairs))
      (push replace pairs)
      (push pattern pairs))
    (tq-replace-regexp-pairs pairs template)))

(defun tq-create-android-app-project (root-path
                                      project-name
                                      compile-sdk-version
                                      build-tools-version
                                      package
                                      activity
                                      label
                                      version-code
                                      version-name
                                      )
  "创建Android工程。

参数

root-path 工程路径
project-name 工程名称
compile-sdk-version AndroidSDK版本
build-tools-version BuildTools版本
version-code 版本代码
version-name 版本名称
package 包
activity Activity名，不含包名
label 应用名

工程目录结构

新建的工程位于root-path下的project-name目录。project-name目录结构同gradle要求的一致，并增加了一些新的子目录。其结构如下
build.gradle 工程的Gradle脚本
app/ 应用目录
app/build.gradle 应用的Gradle脚本
app/src/main/java/${package}/ 应用源代码目录
app/src/main/java/${package}/activity Activity源代码目录
app/src/main/res 资源文件目录
app/src/main/res/layout/ 布局文件目录
app/src/main/res/layout/activity_*.xml Activity布局文件

"
  (interactive "sRootPath: 
sProjectName: 
sCompileSDKVersion: 
sBuildToolsVersion: 
sPackage: 
sActivity: 
sLabel: 
sVersionCode: 
sVersionName: ")
  (let* ((project-path
          (expand-file-name
           project-name
           (expand-file-name root-path)))
         (subdirs (list "app/src/main/java/"
                        "app/src/main/res/layout")))
    ;; 创建工程目录
    (print "建立工作目录")
    (when (file-exists-p project-path)
      (error "Directory existed. path: %s." project-path))
    (make-directory project-path t)
    (dolist (subdir subdirs)
      (make-directory (expand-file-name subdir project-path) t))

    ;; 生成工程build.gradle
    (print "建立工程build.gradle文件")
    (let ((filename
           (expand-file-name "build.gradle" project-path))
          (content tq-android-project-build-gradle-content))
      (tq-create-file filename content))

    ;; 生成工程settings.gradle
    (print "建立工程settings.gradle文件")
    (let ((filename
           (expand-file-name "settings.gradle" project-path))
          (content tq-android-project-settings-gradle-content))
      (tq-create-file filename content))

    ;; 生成app模块build.gradle
    (print "建立模块build.gradle文件")
    (let ((filename
           (expand-file-name "app/build.gradle" project-path))
          (content
           (tq-execute-template (list "applicationId" package
                                      "compileSdkVersion" compile-sdk-version
                                      "buildToolsVersion" build-tools-version
                                      "versionCode" version-code
                                      "versionName" version-name)
                                tq-android-app-build-gradle-template)))
      (tq-create-file filename content))

    ;; 生成app/src/main/AndroidManifest.xml
    (print "建立AndroidManifest.xml文件")
    (let ((filename
           (expand-file-name "app/src/main/AndroidManifest.xml" project-path))
          (content (tq-execute-template (list "package" package
                                              "versionCode" version-code
                                              "versionName" version-name
                                              "label" label
                                              "activity" activity) tq-android-manifest-xml-template)))
      (tq-create-file filename content))

    ;; 生成app/src/main/java/$PACKAGE/activity/$ACTIVITY.java
    (print (concat "建立" activity ".java文件"))
    (let* ((java-file-name (expand-file-name
                            (concat
                             "app/src/main/java/"
                             (replace-regexp-in-string "\\." "/" package)
                             "/activity/"
                             (replace-regexp-in-string "\\." "/" activity)
                             ".java")
                            project-path))
           (parent-directory (file-name-directory java-file-name))
           (content
            (tq-execute-template (list "package" package
                                       "activity" activity) tq-android-activity-class-template)))
      (unless (file-exists-p parent-directory)
        (make-directory parent-directory t))
      (tq-create-file java-file-name content))

    ;; 生成app/src/main/java/$PACKAGE/State.java
    (print "建立State.java文件")
    (let* ((java-file-name (expand-file-name
                            (concat
                             "app/src/main/java/"
                             (replace-regexp-in-string "\\." "/" package)
                             "/State.java")
                            project-path))
           (parent-directory (file-name-directory java-file-name))
           (content
            (tq-execute-template (list "package" package)
                                 tq-android-databinding-state-class-template)))
      (unless (file-exists-p parent-directory)
        (make-directory parent-directory t))
      (tq-create-file java-file-name content))
    
    ;; 生成app/src/main/res/layout/activity_main.xml
    (print "建立布局文件")
    (let ((filename
           (expand-file-name "app/src/main/res/layout/activity_main.xml" project-path))
          (content
           (tq-execute-template (list "package" package) tq-android-layout-xml-template)))
      (tq-create-file filename content))

    ;; TODO 生成State类。
    ;; TODO 初始化git仓库
    ;; TODO 提交git仓库
    ;; TODO 生成strings.xml
    ))


(defconst tq-android-databinding-state-class-template
  "package ${package};

public class State {
        public final String message = \"HELLO, WORLD!\";
}
")

(defconst tq-android-jar-build-gradle-template
  "buildscript {
    repositories {
        mavenCentral()
    }
    dependencies {
        classpath 'com.android.tools.build:gradle:2.3.0+'
    }
}

apply plugin: 'java'

sourceCompatibility = JavaVersion.VERSION_1_7
targetCompatibility = JavaVersion.VERSION_1_7

// see http://stackoverflow.com/questions/25040445/which-is-the-proper-gradle-plugin-to-support-provided-method
configurations {
        provided
}

sourceSets {
        main {
                compileClasspath += configurations.provided
        }
}

dependencies {
        provided files('${androidJar}')
}

")

(defun tq-create-android-jar-project (root-path
                                      project-name
                                      compile-sdk-version
                                      )
  "建立Android JAR库工程。

参数

root-path 根目录
project-name 工程名
compile-sdk-version 编译SDK版本
"
  (interactive
   "sRootPath: 
sProjectName: 
nCompileSDKVersion: ")

  ;; 初始化环境。
  (setenv "PATH" (concat (getenv "PATH") ";C:/Program Files/Git/bin/"))

  ;; 初始化工程根目录。
  ;; Android JAR工程目录结构如下：
  ;; src/main/java 源代码目录
  ;; src/test/java 测试代码目录
  ;; build.gradle Gradle脚本
  (let ((path (expand-file-name project-name root-path)))
    (when (file-exists-p path)
      (error "Project directory existed. path: %s" path))
    (make-directory path t)
    (dolist (subdir '("src/main/java"
                      "src/test/java"
                      )
                    )
      (make-directory (expand-file-name subdir path) t)
      (print (expand-file-name path subdir))
      )
    )
  ;; 生成build.gradle。
  (let* ((path (expand-file-name project-name root-path))
         (filename (expand-file-name "build.gradle" path))
         (content
          (tq-execute-template (list "androidJar"
                                     (concat (getenv "ANDROID_HOME")
                                             (format "/platforms/android-%d/android.jar" compile-sdk-version)
                                             )
                                     )
                               tq-android-jar-build-gradle-template
                               )
          )
         )
    (tq-create-file filename content)
    )

  ;; 初始化git仓库，建立.gitignore文件。
  (let ((path (expand-file-name project-name root-path)))
    (shell-command (concat "git init " path))
    )

  ;; TODO 提交git。
  )


(defconst tq-android-aar-build-gradle-template
  "
buildscript {
        repositories {
                jcenter()
        }
        dependencies {
                classpath 'com.android.tools.build:gradle:2.3+'
        }
}

apply plugin: 'com.android.library'

android {
        compileSdkVersion ${compileSdkVersion}
        buildToolsVersion '${buildToolsVersion}'
}
"
  "Android工程aar模块build.gradle文件模板。")

(defconst tq-android-aar-manifest-xml-template
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<manifest xmlns:android=\"http://schemas.android.com/apk/res/android\"
	  package=\"${package}\"
	  android:versionCode=\"${versionCode}\"
	  android:versionName=\"${versionName}\">
</manifest> 
"
  "AAR工程的AndroidManifest.xml文件模板。")

(defun tq-create-android-aar-project (root-path
                                      project-name
                                      compile-sdk-version
                                      build-tools-version
                                      package
                                      version-code
                                      version-name
                                      )
  "创建Android AAR工程。

参数

root-path 工程路径
project-name 工程名称
compile-sdk-version AndroidSDK版本
build-tools-version BuildTools版本
package
version-code
version-name

工程目录结构

新建的工程位于root-path下的project-name目录。project-name目录结构同gradle要求的一致，并增加了一些新的子目录。其结构如下
build.gradle 工程的Gradle脚本
build.gradle 应用的Gradle脚本
src/main/java/ 应用源代码目录
src/main/res 资源文件目录
"
  (interactive "sRootPath: 
sProjectName: 
sCompileSDKVersion: 
sBuildToolsVersion: 
sPackage: 
sVersionCode: 
sVersionName: ")
  (let* ((project-path (expand-file-name
                        project-name
                        (expand-file-name root-path)
                        ))
         (subdirs (list
                   "src/main/java/"
                   "src/main/res/layout"
                   )))
    ;; 创建工程目录
    (print "建立工作目录")
    (when (file-exists-p project-path)
      (error "Directory existed. path: %s." project-path))
    (make-directory project-path t)
    (dolist (subdir subdirs)
      (make-directory (expand-file-name subdir project-path) t))

    ;; 生成aar模块build.gradle
    (print "建立模块build.gradle文件")
    (let ((filename
           (expand-file-name "build.gradle" project-path))
          (content
           (tq-execute-template (list "applicationId" package
                                      "compileSdkVersion" compile-sdk-version
                                      "buildToolsVersion" build-tools-version
                                      "versionCode" version-code
                                      "versionName" version-name)
                                tq-android-aar-build-gradle-template)))
      (tq-create-file filename content))

    ;; 生成src/main/AndroidManifest.xml
    (print "建立AndroidManifest.xml文件")
    (let ((filename (expand-file-name
                     "src/main/AndroidManifest.xml"
                     project-path
                     ))
          (content (tq-execute-template
                    (list
                     "package" package
                     "versionCode" version-code
                     "versionName" version-name
                     )
                    tq-android-aar-manifest-xml-template
                    )
                   )
          )
      (tq-create-file filename content)
      )
    )
  )




(defun tq-java-package-to-directory (package)
  (replace-regexp-in-string "\\." "/" package))

(defconst kotlin-android-gitignore-content "
build/
.gradle
")

(defconst kotlin-android-build-gradle-content "

buildscript {
        ext.kotlin_version = \"1.1.2-2\"

        repositories {
                mavenCentral()
                jcenter()
        }

        dependencies {
                classpath \"com.android.tools.build:gradle:2.2.0\"
                classpath \"org.jetbrains.kotlin:kotlin-gradle-plugin:$kotlin_version\"
        }
}


apply plugin: \"com.android.application\"
apply plugin: \"kotlin-android\"

repositories {
        mavenCentral()
        jcenter()
}

dependencies {
        compile \"org.jetbrains.kotlin:kotlin-stdlib-jre7:$kotlin_version\"
}

// kotlin.incremental = true
android {
        compileSdkVersion 19
        buildToolsVersion \"25.0.2\"

        compileOptions {
                sourceCompatibility JavaVersion.VERSION_1_7
                targetCompatibility JavaVersion.VERSION_1_7
        }
        
}

")

(defconst kotlin-android-layout-xml-content "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<LinearLayout xmlns:android=\"http://schemas.android.com/apk/res/android\"
	      android:orientation=\"vertical\"
	      android:layout_width=\"fill_parent\"
	      android:layout_height=\"fill_parent\"
	      >
	<TextView
		android:layout_width=\"fill_parent\"
		android:layout_height=\"wrap_content\"
		android:text=\"HELLO WORLD!\"
		/>
</LinearLayout>
")

(defconst kotlin-android-androidmanifest-xml-format
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<manifest xmlns:android=\"http://schemas.android.com/apk/res/android\"
	  package=\"${package}\"
	  android:versionCode=\"1\"
	  android:versionName=\"1.00\">

	<application android:label=\"helloworld\">
		<activity android:name=\".activity.MainActivity\"
			  android:label=\"helloworld\">
			<intent-filter>
				<action android:name=\"android.intent.action.MAIN\" />
				<category android:name=\"android.intent.category.LAUNCHER\" />
			</intent-filter>
		</activity>
	</application>

 	<uses-permission android:name=\"android.permission.WRITE_EXTERNAL_STORAGE\"/>
</manifest> 

")



(defconst kotlin-android-activity-format
  "package ${package}.activity;

import android.os.Bundle
import android.app.Activity
import ${package}.R

class MainActivity: Activity() {

	override fun onCreate(savedInstanceState: Bundle?) {
		super.onCreate(savedInstanceState)
		setContentView(R.layout.main)
	}
}
")

(defun tq-create-kotlin-android-app-project (project-name
                                             package)
  (interactive "sProjectName: 
sPackage: ")

  ;; create project directory
  (make-directory project-name t)

  ;; create .gitignore
  (tq-create-file
   (expand-file-name
    ".gitignore"
    project-name)
   kotlin-android-gitignore-content)

  ;; create build.gradle
  (tq-create-file
   (expand-file-name
    "build.gradle"
    project-name)
   kotlin-android-build-gradle-content)

  ;; mkdir src/main/res/layout
  (make-directory
   (expand-file-name
    "src/main/res/layout"
    project-name)
   t)

  ;; mkdir src/main/java/{package}/activity
  (make-directory
   (expand-file-name
    (concat "src/main/java/"
            (tq-java-package-to-directory package)
            "/activity/")
    project-name)
   t)

  ;; create src/main/java/{package}/activity/MainActivity.kt
  (let ((filename (expand-file-name
                   (concat "src/main/java/"
                           (tq-java-package-to-directory package)
                           "/activity/MainActivity.kt")
                   project-name))
        (content
         (tq-replace-regexp-pairs
          (list "${package}" package)
          kotlin-android-activity-format)))
    (tq-create-file filename content))

  ;; create src/main/AndroidManifest.xml
  (tq-create-file
   (expand-file-name
    "src/main/AndroidManifest.xml"
    project-name)
   (tq-replace-regexp-pairs
    (list "${package}" package)
    kotlin-android-androidmanifest-xml-format))

  ;; create src/main/res/layout/main.xml
  (tq-create-file
   (expand-file-name
    "src/main/res/layout/main.xml"
    project-name)
   kotlin-android-layout-xml-content))

(defun gen-latex-code-sample (code)
  "生成LaTeX代码示例。"
  (let ((fmt "
\\begin{tabular}{@{} l @{} l @{}}
\\begin{minipage}{3in}
\\begin{verbatim}
%s
\\end{verbatim}
\\end{minipage}
&
\\begin{minipage}{3in}
%s
\\end{minipage}
\\end{tabular}
"))
    (format fmt code code)))







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


(defun tq-initialize-shell-mode ()
  "避免使用shell模式启动PowerShell时中文文件名出现乱码。"
  (set-buffer-process-coding-system 'gbk 'gbk))

(defun set-org-todo-keywords ()
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-ACTION(i@/!)" "WAIT(w@/!)"
                    "|"
                    "DONE(d!)" "CANCELED(c@)"))))

(defun set-encodings ()
  "设置字符编码。"
  (setf file-name-coding-system 'utf-8
        default-file-name-coding-system 'utf-8-unix
        default-buffer-file-coding-system 'utf-8-unix)
  (set-language-environment "UTF-8")
  ;; 设置文件缓冲区默认保存编码。
  (set-buffer-file-coding-system 'utf-8-unix)
  (prefer-coding-system 'chinese-gbk-dos)
  (prefer-coding-system 'chinese-gbk-unix)
  (prefer-coding-system 'utf-8-dos)
  (prefer-coding-system 'utf-8-unix))


(defun tq-initialize ()
  "初始化窗口。"

  (setq-default indent-tables-mode nil)

  ;; 隐藏菜单栏
  (menu-bar-mode -1)

  ;; 隐藏工具栏
  (tool-bar-mode -1)
  
  ;; 隐藏滚动条
  (scroll-bar-mode -1)

  (setq display-time-day-and-date 1)
  (display-time-mode 1)

  ;; (setq display-time-24hr-format t)
  
  ;; 启用缩写
  (fset 'yes-or-no-p 'y-or-n-p)
  
  ;; ;; 设置字体
  ;; (set-frame-font (format "%s-%d" tq-latin-font tq-latin-font-size))

  ;; ;; 设置中文字体
  ;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
  ;;   (set-fontset-font t
  ;; 		      charset
  ;; 		      (font-spec :family tq-chinese-font :size tq-chinese-font-size)))

  ;; 关闭启动界面
  (setq inhibit-startup-message t)
  (setq gnus-inhibit-startup-message t)
  
  ;; 显示行号
  (global-linum-mode t)

  ;; 显示列号
  (setq column-number-mode t)

  ;; 设置工作目录
  (setf default-directory tq-working-directory)

  (setf magit-git-executable tq-git-program)
  (setf python-shell-interpreter tq-python-program)

  
  ;; 设置备份目录
  (setq backup-directory-alist (quote (("." . "C:/Users/WangQian/Workspace/AutoBackup"))))
  
  ;; 设置命令搜索路径
  (add-to-list 'exec-path "C:\\Program Files\\Git\\bin")

  ;; 包管理系统
  (when (>= emacs-major-version 24)
    (require 'package)
    (setq package-archives
          '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
            ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
    (package-initialize))
  
  ;; 高亮当前行
  (global-hl-line-mode t)
  (set-cursor-color "white")
  (set-face-attribute hl-line-face nil :underline t)

  ;; 启用自动保存
  (setq auto-save-mode t)

  ;; 关闭自动备份
  ;; (setq make-backup-files nil)

  ;; 鼠标指针规避光标
  (mouse-avoidance-mode 'animate)

  ;; 全屏显示
  ;; (w32-maximize-window)

  ;;(desktop-save-mode 1)
  ;;(setq desktop-dirname "~/")

  ;; 设置快捷键
  (global-set-key [f2] 'clipboard-kill-ring-save)
  (global-set-key [f3] 'isearch-forward)

  ;; 光标样式
  (setq default-cursor-type 'box)
  (set-cursor-color "orange")
  ;; 
  (setq visible-bell t)

  ;; 缩进
  (setq tab-stop-list nil)

  ;; 设置钩子
  (add-hook 'c-mode-hook 'hs-minor-mode)
  (add-hook 'nxml-mode-hook 'hs-minor-mode)
  (add-hook 'java-mode-hook 'hs-minor-mode)

  (defun tq-c-mode-hook ()
    (c-set-style "tq-c-style")
    (setq tab-width 8
          indent-tabs-mode nil)
    (c-toggle-auto-newline t))
  
  (add-hook 'c-mode-common-hook 'tq-c-mode-hook)

  ;; 关联文件
  ;; Java
  (add-to-list 'auto-mode-alist '("\\.aidl\\'" . java-mode))
  ;; Lisp
  (add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

  ;; org mode
  (custom-set-variables
   '(org-agenda-files (list "C:/Users/WangQian/Workspace/Notes/Agenda/")))

  ;; nxml-mode
  (setf nxml-child-indent 8)
  (setf nxml-attribute-indent 8)

  ;; 编码
  (set-encodings)

  ;; 添加shell-mode钩子。
  (add-hook 'shell-mode-hook 'tq-initialize-shell-mode)

  ;; 显示时间
  (setq display-time-mode t)

  (set-org-todo-keywords)
  (switch-to-buffer "*scratch*")
  (delete-other-windows)
  (delete-region (point-min) (point-max)))


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

(defun tq-append-env-path (path)
  (setenv "PATH" (concat path ";" (getenv "PATH")))
  nil)

(tq-append-env-path "C:/Program Files/Git/bin")
(tq-append-env-path "D:/Gradle/gradle-4.6/bin")

(defun tq-execute-shell (command &optional work-directory)
  "execute shell command in work directory."
  (let ((stack nil))
    (when work-directory
      (push default-directory stack)
      (cd work-directory))
    (shell-command command)
    (when work-directory
      (cd (pop stack)))))

(defun tq-join-path (root &rest path-list)
  "Join path. "
  (expand-file-name (seq-reduce
                     (lambda (base path)
                       (if base
                           (concat base "/" path)
                         path))
                     path-list
                     nil)
                    root))

(defconst tq-spring-config-template "
<beans xmlns=\"http://www.springframework.org/schema/beans\"
       xmlns:context=\"http://www.springframework.org/schema/context\"
       xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
       xmlns:mvc=\"http://www.springframework.org/schema/mvc\"
       xsi:schemaLocation=\"
                           http://www.springframework.org/schema/beans
                           http://www.springframework.org/schema/beans/spring-beans.xsd
                           http://www.springframework.org/schema/mvc
                           http://www.springframework.org/schema/mvc/spring-mvc.xsd
                           http://www.springframework.org/schema/context
                           http://www.springframework.org/schema/context/spring-context.xsd \">

	<context:component-scan base-package=\"${Package}\" />

	<bean class=\"org.springframework.web.servlet.view.InternalResourceViewResolver\">
		<property name=\"viewClass\" value=\"org.springframework.web.servlet.view.JstlView\"/>
		<property name=\"prefix\" value=\"/WEB-INF/views/jsp/\" />
		<property name=\"suffix\" value=\".jsp\" />
	</bean>

	<mvc:resources mapping=\"/resources/**\" location=\"/resources/\" />
        <context:annotation-config/>
        <bean id=\"helloService\" class=\"${Package}.serviceprovider.HelloServiceProvider\" />        

	<mvc:annotation-driven />

</beans>
")

(defconst tq-spring-mvc-web-xml-content "
<web-app version=\"3.1\"
	 xmlns=\"http://xmlns.jcp.org/xml/ns/javaee\"
	 xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
	 xmlns:mvc=\"http://www.springframework.org/schema/mvc\"
	 xsi:schemaLocation=\"http://xmlns.jcp.org/xml/ns/javaee
		 	     http://xmlns.jcp.org/xml/ns/javaee/web-app_3_1.xsd\">
	<servlet>
		<servlet-name>HelloServlet</servlet-name>
		<servlet-class>org.springframework.web.servlet.DispatcherServlet</servlet-class>
                <init-param>
			<param-name>contextConfigLocation</param-name>
			<param-value>/WEB-INF/spring-mvc-config.xml</param-value>
		</init-param>
		<load-on-startup>1</load-on-startup>
	</servlet>
	<servlet-mapping>
		<servlet-name>HelloServlet</servlet-name>
		<url-pattern>/</url-pattern>
	</servlet-mapping>
</web-app>
")


(defconst tq-spring-web-build-gradle "
apply plugin: 'java'
apply plugin: 'war'

repositories {
        jcenter()
        mavenCentral()
}

dependencies {
        compile group: 'org.springframework', name: 'spring-core', version: '4.3.6.RELEASE'
        compile group: 'org.springframework', name: 'spring-context', version: '4.3.6.RELEASE'
        compile 'org.springframework:spring-webmvc:4.1.6.RELEASE'
	compile 'javax.servlet:jstl:1.2'
}  
")

(defconst tq-spring-web-serviceprovider-template "
package ${Package}.serviceprovider;

import ${Package}.service.HelloService;
import ${Package}.service.HelloService.Request;
import ${Package}.service.HelloService.Response;

public class HelloServiceProvider implements HelloService {
        public Response hello(Request request) {
                Response response = new Response();
                response.message = \"hello, \" + request.name;
                return response;
        }
}
")


(defconst tq-spring-web-service-template "
package ${Package}.service;

public interface HelloService {
        
        public class Request {
                public String name;
                public void setName(String name) {
                        this.name = name;
                }
        }
        
        public class Response {
                public String message;
        }

        public Response hello(Request request);
}
")

(defconst tq-spring-web-controller-template "
package ${Package}.controller;

import java.util.Map;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.ModelAndView;

@Controller
public class Hello {
	@RequestMapping(value = \"/\", method = RequestMethod.GET)
        @ResponseBody
	public String index(Map<String, Object> model) {
		return \"index\";
	}
}
")

(defun tq-write-file (filename content &optional overwrite)
  "Write content to a file, create or overwrite it in need."
  (let* ((absolute-filename
          (if (file-name-absolute-p filename)
              filename
            (expand-file-name filename default-directory)))	   
         (path (file-name-directory absolute-filename)))
    (if (and (not overwrite)
             (file-exists-p absolute-filename))
        (error "File existed. path: %s." absolute-filename))
    (if (not (file-exists-p path))
        (make-directory path t))
    (append-to-file content nil absolute-filename)))

(defun tq-new-gitignore (&optional path)
  "建立gitignore文件"
  (interactive "sPath: ")
  (if (string-equal path "")
      (setf path default-directory))
  (tq-write-file (concat path "/.gitignore") "
.gradle
gradle/
build/
gradlew
gradlew.bat

*~
\#*
*.swp

*.exe
*.obj
"))


(defun tq-new-spring-web (root-directory
                          project-name
                          package)
  "建立Spring MVC项目。"
  (interactive "sRootDirectory: 
sProjectName: 
sPackage: ")
  
  (let ((project-directory (expand-file-name project-name root-directory)))
    ;; 建立目录
    (make-directory project-directory t)

    ;; 初始化gradle
    (tq-execute-shell "gradle init" project-directory)

    ;; 生成build.gradle
    (tq-write-file (tq-join-path project-directory "build.gradle")
                   tq-spring-web-build-gradle t)

    ;; 生成Java代码
    (tq-write-file
     (tq-join-path project-directory
                   "src/main/java/"
                   (replace-regexp-in-string "\\." "/" package)
                   "controller/Hello.java")
     (tq-execute-template
      (list "Package"
            package)
      tq-spring-web-controller-template))

    (tq-write-file
     (tq-join-path project-directory
                   "src/main/java/"
                   (replace-regexp-in-string "\\." "/" package)
                   "service/HelloService.java")
     (tq-execute-template
      (list "Package"
            package)
      tq-spring-web-service-template))

    (tq-write-file
     (tq-join-path project-directory
                   "src/main/java/"
                   (replace-regexp-in-string "\\." "/" package)
                   "serviceprovider/HelloServiceProvider.java")
     (tq-execute-template
      (list "Package"
            package)
      tq-spring-web-serviceprovider-template))
    
    ;; 生成web.xml
    (tq-write-file (tq-join-path project-directory "src/main/webapp/WEB-INF/web.xml")
                   tq-spring-mvc-web-xml-content 
                   t)
    
    ;; 生成context config文件
    (tq-write-file (tq-join-path project-directory "src/main/webapp/WEB-INF/spring-mvc-config.xml")
                   (tq-execute-template (list "Package" package) tq-spring-config-template)
                   t)

    ;; 初始化git仓库
    (tq-new-gitignore (tq-join-path project-directory ""))
    (dolist (command (list "git init ."
                           "git add ."
                           "git commit -m \"initial commit\""))
      (tq-execute-shell command project-directory))
    
    ;; 使用gradle 打包
    (tq-execute-shell "gradle war" project-directory)

    ;; 打开工程目录
    (find-file project-directory)))

(defconst tq-spring-boot-app-template
  "package ${Package};

public class App {
        public static void main(String[] args) {
                System.out.println(\"ok\");
        }
}
")

(defconst tq-spring-boot-app-build-gradle
  "buildscript {
  repositories {
    mavenCentral()
  }
  dependencies {
    classpath 'org.springframework.boot:spring-boot-gradle-plugin:2.0.0.RELEASE'
  }
}

apply plugin: 'org.springframework.boot'
apply plugin: 'java'

bootJar {
        mainClassName = 'com.foo.bar.App'
}

bootJar {
        launchScript()
}
")

(defun tq-new-spring-boot-app (root-directory
                               project-name
                               package)
  "建立Spring Boot项目。"
  (interactive "sRootDirectory: 
sProjectName: 
sPackage: ")
  
  (let ((project-directory (expand-file-name project-name root-directory)))
    ;; 建立目录
    (make-directory project-directory t)

    ;; 初始化gradle
    (tq-execute-shell "gradle init" project-directory)

    ;; 生成build.gradle
    (tq-write-file (tq-join-path project-directory "build.gradle")
                   tq-spring-boot-app-build-gradle t)

    ;; 生成Java代码
    (tq-write-file
     (tq-join-path project-directory
                   "src/main/java/"
                   (replace-regexp-in-string "\\." "/" package)
                   "App.java")
     (tq-execute-template
      (list "Package"
            package)
      tq-spring-boot-app-template))

    ;; 初始化git仓库
    (tq-new-gitignore (tq-join-path project-directory ""))
    (dolist (command (list "git init ."
                           "git add ."
                           "git commit -m \"initial commit\""))
      (tq-execute-shell command project-directory))
    
    ;; 构建
    (tq-execute-shell "gradle bootRun" project-directory)

    ;; 打开工程目录
    (find-file project-directory)))

(defconst tq-spring-boot-web-application-template
  "package ${Package};

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;

@SpringBootApplication
public class Application {

        public static void main(String[] args) {
                SpringApplication.run(Application.class, args);
        }
}

")


(defconst tq-spring-boot-web-controller-template
  "package ${Package};

import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.RequestMapping;

@RestController
public class Controller {

        @RequestMapping(\"/\")
        public String index() {
                return \"hello\";
        }
}

")

(defconst tq-spring-boot-web-build-gradle-template
  "// https://spring.io/guides/gs/spring-boot/#scratch
buildscript {
        repositories {
                mavenCentral()
        }
        dependencies {
                classpath 'org.springframework.boot:spring-boot-gradle-plugin:2.0.0.RELEASE'
        }
}

apply plugin: 'org.springframework.boot'
apply plugin: 'io.spring.dependency-management'
apply plugin: 'java'

bootJar {
        baseName = '${ProjectName}'
        version = '0.1.0'
}

repositories {
        mavenCentral()
}

dependencies {
        compile('org.springframework.boot:spring-boot-starter-web')
        testCompile('junit:junit')
}
")

(defun tq-new-spring-boot-web (root-directory
                               project-name
                               package)
  "建立Spring Boot Web项目。"
  (interactive "sRootDirectory: 
sProjectName: 
sPackage: ")
  
  (let ((project-directory (expand-file-name project-name root-directory)))
    ;; 建立目录
    (make-directory project-directory t)

    ;; 初始化gradle
    (tq-execute-shell "gradle init" project-directory)

    ;; 生成build.gradle
    (tq-write-file (tq-join-path project-directory "build.gradle")
                   (tq-execute-template (list "ProjectName" project-name) tq-spring-boot-web-build-gradle-template)
                   t)

    ;; 生成Java代码
    (tq-write-file
     (tq-join-path project-directory
                   "src/main/java/"
                   (replace-regexp-in-string "\\." "/" package)
                   "Application.java")
     (tq-execute-template
      (list "Package"
            package)
      tq-spring-boot-web-application-template))

    (tq-write-file
     (tq-join-path project-directory
                   "src/main/java/"
                   (replace-regexp-in-string "\\." "/" package)
                   "/controller/Controller.java")
     (tq-execute-template
      (list "Package"
            package)
      tq-spring-boot-web-controller-template))

    ;; 初始化git仓库
    (tq-new-gitignore (tq-join-path project-directory ""))
    (dolist (command (list "git init ."
                           "git add ."
                           "git commit -m \"initial commit\""))
      (tq-execute-shell command project-directory))
    
    ;; 构建
    (tq-execute-shell "gradle build" project-directory)

    ;; 打开工程目录
    (find-file project-directory)))

(defun tq-gen-pojo (start end)  
  "convert a region to pojo source code.
example:

User
String name
String password
=>
public class User {
  private String name;
  private String password;
  public String getName() {
    return name;
  }
  public String getPassword() {
    return password;
  } 
  public void setName(String name) {
    this.name = name;
  }
  public void setPassword(String password) {
    this.password = password;
  }
}
"
  (interactive "r")
  (let ((pojo-name "")
        (members nil)
        (type "")
        (name "")
        (source1 "")
        (source2 ""))
    (seq-reduce (lambda (value next)
                  (if (< 0 (length (string-trim next)))
                      (if (not value)
                          (setf pojo-name next
                                value next)
                        (push next members))))
                (split-string (buffer-substring start end))
                nil)
    (if (= 1 (mod (length members) 2))
        (error "bad pojo definition."))
    (while members
      (setf name (pop members))
      (setf type (pop members))
      (setf source1 (format "private %s %s;
%s" type name source1))
      (setf source2 (format "public %s get%s() {
  return %s;
}

public void set%s(%s %s) {
  this.%s = %s;
}
%s" type (capitalize name) name (capitalize name) type name name name source2))
      )
    (setf source1 (format "public class %s {
%s
%s}
"
                          pojo-name source1 source2))
    (delete-region start end)
    (insert source1)
    (indent-region start (+ start (length source1)))
    (move-end-of-line)))

;;;; 统计相关函数。

(defun sum (&rest numbers)
  "求和。"
  (apply '+ numbers))

(defun average (&rest numbers)
  "计算算数平均数。"
  (let ((count (length numbers)))
    (if (= 0 count)
        0
      (/ (apply '+ numbers) (* 1.0 count)))))

(defun approximate-sigma-square (&rest numbers)
  "计算sigma^2的近似值。
计算公式为 S^2 = \frac{\sum_{i=1}^n(x_i-\overline{x})^2}{n-1}
"
  (let ((len (length numbers))
        (avg (apply 'average numbers)))
    (if (<= len 1)
        0.0
      (/ (apply '+ (mapcar #'(lambda (x)(* (- x avg) (- x avg))) numbers))
         (- len 1)))))

;; 将字符串转移为tex可识别的字符串。
(defmacro escape-to-tex (string)
  `(concat ,@(mapcar #'tex-underline-escape-decorator string)))

;; 将"_"转换为"\\_"以便在tex中使用。
(defun tex-underline-escape-decorator (ch)
  (if (char-equal ch ?_)
      "\\_"
    (char-to-string ch)))

(defun tq-delta-range (value delta)
  "计算value*(1+/-delta)的值。"
  (let* ((change (* delta value))
         (max (+ value change))
         (min (- value change)))
    (list min max)))

(defun tq-in-delta-range (test-value base-value delta)
  "计算test-value是否在base-value*(1+/-delta)的范围内。"
  (let* ((range (delta-range base-value delta))
         (max (elt range 1))
         (min (elt range 0)))
    (and (<= test-value max)
         (>= test-value min))))


(tq-initialize)
(provide 'tq)
