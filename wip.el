
(defconst tq-android-jar-build-gradle-template
  "buildscript {
    repositories {
        mavenCentral()
    }
    dependencies {
        classpath 'com.android.tools.build:gradle:2.3.0+'
    }
}
apply plugin: 'android-library'

android {
    compileSdkVersion '${compileSdkVersion}'
    buildToolsVersion '${buildToolsVersion}'
}
")

(defun tq-create-android-jar-project (
				      root-path
				      project-name
				      compile-sdk-version
				      build-tools-version)
  "建立Android JAR库工程。

参数

root-path 根目录
project-name 工程名
compile-sdk-version SDK版本
build-tools-version 构建工具版本"
  (interactive
   "sRootPath: 
sProjectName: 
sCompileSDKVersion: 
sBuildToolsVersion: ")

  ;; 初始化环境。
  (setenv "PATH"
	  (concat (getenv "PATH") ";C:/Program Files/Git/bin/")
	  )

  ;; 初始化工程根目录。
  ;; Android JAR工程目录结构如下：
  ;; src/main/java 源代码目录
  ;; src/test/java 测试代码目录
  ;; build.gradle Gradle脚本
  (let ((path (expand-file-name project-name root-path)))
    (when (file-exists-p path)
      (error "Project directory existed. path: %s" path))
    (make-directory path t)
    (dolist (subdir '(
		     "src/main/java"
		     "src/test/java"))
      (make-directory (expand-file-name subdir path) t)
      (print (expand-file-name path subdir))
      )
    )
  ;; 生成build.gradle。
  (let* ((path (expand-file-name project-name root-path))
	 (filename (expand-file-name "build.gradle" path))
	 (content (tq-render-template
		   (list "compileSdkVersion" compile-sdk-version
			 "buildToolsVersion" build-tools-version
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

