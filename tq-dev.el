
;; File: develop-utils.el
;; Description: Develop utility functions.
;; Author: Wang Qian
;; Create: 2016-09-10
;; Modify: 2017-02-05

;; todo
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
;; tq-gen-gitignore
;; tq-init-gitignore
;; tq-create-gitignore
;; tq-gen-html
;; tq-init-html
;; tq-create-html
;; tq-create-gradle-project
;; tq-create-cocos2dx-project

;; 函数gen-xxx用于生成对应的内容。
;; 函数init-xxx用于将生成的内容写入当前缓冲区。
;; 函数create-xxx用于创建新文件，将对应内容写入文件，并打开该文件。

(provide 'tq)

(defmacro tq-to-string (x)
  "Convert object or symbol to string."
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
  "Generate org file header."
  (let* ((lines '("# -*- mode: org -*-\n"
		  "#+title: ${title}\n"
		  "#+author: Wang Qian\n"
		  "#+date: ${date}\n"
		  "#+startup: showeverything\n"
		  "\n"))
	 (date (format-time-string "%Y-%m-%d"))
	 (replace-pairs (list "${date}" date
			      "${title}" title)))
    (tq-replace-regexp-pairs replace-pairs (apply #'concat lines))))

(defun tq-init-org-file (title)
  "Initialize a org file, insert org header lines, and switch on org-mode."
  (interactive "sTitle: ")
  (let ((text (tq-gen-org-file title)))
    (beginning-of-buffer)
    (insert text)
    (org-mode)))

(defun tq-create-org-file (filename title)
  "Create and initialize a org file."
  (interactive "sFilename: \nsTitle: ")
  (let ((text (tq-gen-org-file title)))
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
	    ;; TODO 增加property。
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
  "package ${package};

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

(defun tq-render-template (variable-pairs template)
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

(defun tq-create-android-project (root-path
				  project-name
				  compile-sdk-version
				  build-tools-version
				  package
				  activity
				  label
				  version-code
				  version-name)
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
	   (tq-render-template (list "applicationId" package
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
	  (content (tq-render-template (list "package" package
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
	    (tq-render-template (list "package" package
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
	    (tq-render-template (list "package" package)
				tq-android-databinding-state-class-template)))
      (unless (file-exists-p parent-directory)
	(make-directory parent-directory t))
      (tq-create-file java-file-name content))
    
    ;; 生成app/src/main/res/layout/activity_main.xml
    (print "建立布局文件")
    (let ((filename
	   (expand-file-name "app/src/main/res/layout/activity_main.xml" project-path))
	  (content
	   (tq-render-template (list "package" package) tq-android-layout-xml-template)))
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
