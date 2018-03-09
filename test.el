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
