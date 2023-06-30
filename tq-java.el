(require 'tq-template)

(defun tq-generate-pom (group artifact version packaging)
  "生成pom.xml文件内容。"
  (tq-template-render-sequence "<?xml version='1.0' encoding='UTF-8'?>
<project xmlns='http://maven.apache.org/POM/4.0.0' 
     xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'
     xsi:schemaLocation='http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd'>
    <modelVersion>4.0.0</modelVersion>
    <groupId>${groupID}</groupId>
    <artifactId>${artifactID}</artifactId>
    <version>${version}</version>
    <packaging>${packaging}</packaging>

    <properties>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
        <maven.compiler.encoding>UTF-8</maven.compiler.encoding>
        <maven.compiler.source>1.8</maven.compiler.source>
        <maven.compiler.target>1.8</maven.compiler.target>
    </properties>

    <dependencies>
    </dependencies>

    <build>
        <plugins>
            <plugin>
                <groupId>org.codehaus.mojo</groupId>
                <artifactId>exec-maven-plugin</artifactId>
                <version>1.6.0</version>
                <configuration>
                    <mainClass>${mainClass}</mainClass>
                </configuration>
            </plugin>
        </plugins>
    </build>
</project>
"
                               "groupID" group
                               "artifactID" artifact
                               "version" version
                               "packaging" packaging
                               "mainClass" (concat group ".App")))


(defconst tq-java-template "/**
 * 文件: ${className}.java
 * 说明: ${description}
 * 创建日期: ${date}
 * 最近修改日期: ${date}
 */
${firstLine}

import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.stream.*;
import java.io.*;
import java.nio.file.*;

public class ${className} {
  public ${className} () {
  }

  public void run(String... args) throws Exception {
    System.out.println(\"ok\");
  }

  public static void main(String... args) throws Exception {
    ${className} app = new ${className}();
    app.run(args);
  }
}
")

(defun tq-generate-java (package class-name description)
  "生成java类。"
  (let ((firstLine ""))
    (if (and package
             (not (string-empty-p package)))
        (setf firstLine (format "package %s;" package)))
    (tq-template-render-sequence tq-java-template
                                 "className" class-name
                                 "description" description
                                 "firstLine" firstLine
                                 "date" (format-time-string "%Y-%m-%d"))))

(defun tq-insert-pom-file (group-id artifact-id version packaging)
  "生成pom.xml文件，插入到缓冲区。"
  (interactive "sGroupId: 
sArtifactId: 
sVersion: 
sPackaging: ")
  (beginning-of-buffer)
  (insert (tq-generate-pom group-id artifact-id version packaging))
  (end-of-buffer))

(defun tq-new-pom-file (group-id artifact-id &optional version packaging)
  "新建并打开pom.xml文件。"
  (interactive "sGroupId: 
sArtifactId "): 
  (beginning-of-buffer)
  (tq-file-write-and-open "pom.xml" (tq-generate-pom group-id artifact-id (or version "0.1.0-SNAPSHOT") (or packaging "jar")))
  (end-of-buffer))

(defun tq-insert-java-class-file (package class-name description)
  (interactive "sPackage: 
sClassName:
sDescription: ")
  (beginning-of-buffer)
  (insert (tq-generate-java package class-name description))
  (end-of-buffer))


(defun tq-create-java (package class-name description)
  "创建Java源代码文件。

保存在当前目录的<package>/<class-name>.java文件中，并打开文件。
"
  (interactive "s包：
s类：
s说明：")
  (let ((full-class-name (expand-file-name (concat class-name ".java")
                                           (replace-regexp-in-string "\\." "/" package))))
    (tq-file-write-and-open full-class-name
                            (tq-generate-java package class-name description))))

;; (defun tq-new-java-application (root project package)
;;   (interactive "sRoot:
;; sProject:
;; sPackage:
;; ")
;;   (let* ((origin-directory default-directory)
;;          (package-path (replace-regexp-in-string "\\." "/" package))
;;          (project-directory (expand-file-name project root))
;;          (env (tq-util-make-string-hash "package" package
;;                                    "project" project
;;                                    "root" root))
;;          (command (tq-render-template
;;                    "gradle -no-daemon init --dsl groovy --type java-application --package ${package} --project-name ${project}"
;;                    env)))
;;     (tq-workflow-execute
;;      (make-instance 'tq-workflow
;;                     :environment env
;;                     :steps (list (make-instance 'tq-workflow-step-make-directory
;;                                                 :path project-directory)
;;                                  (make-instance 'tq-workflow-step-change-directory
;;                                                 :path project-directory)
;;                                  (make-instance 'tq-workflow-step-run-shell-command
;;                                                 :command command)
;;                                  (make-instance 'tq-workflow-step-change-directory
;;                                                 :path origin-directory)
;;                                  (make-instance 'tq-workflow-step-open-file
;;                                                 :file-name
;;                                                (expand-file-name (concat "src/main/java/" package-path "/App.java") project-directory)))))))


(defun tq-create-maven-project (project-name group artifact)
  "创建maven项目。

在当前目录创建project-name子目录，并在其中创建pom.xml和maven目录结构。
"
  (interactive "s项目名字：\ns组：\ns工件：")
  (let* ((project-path project-name)
         (pom-path (expand-file-name "pom.xml" project-path))
         (prop-path (expand-file-name "application.properties" (concat project-path "/" "src/" "main/" "resources/")))
         (app-java-path (expand-file-name "App.java" (concat project-path "/" "src/" "main/" "java/" (replace-regexp-in-string "\\." "/" group)))))
    ;; 检查project-name子目录是否存在。
    (if (file-exists-p project-path)
        (error "目录%s已存在" project-path))
    ;; 创建project-name子目录。
    (make-directory project-path t)
    ;; 创建project-name/pom.xml文件。
    (tq-file-write pom-path (tq-generate-pom group artifact "0.1.0-SNAPSHOT" "jar"))
    ;; 创建project-name/src/resources/application.properties文件。
    (tq-file-write prop-path "")
    ;; 创建project-name/src/main/java/{GROUP}/App.java文件。
    (tq-file-write-and-open app-java-path (tq-generate-java group "App" "应用入口类"))))


(defun tq-maven-add-dependency (group artifact version)
  "在缓冲区当前位置插入 maven 依赖 <dependency> 标签。"
  (interactive "s组：\ns工件：\ns版本：")
  (insert (tq-template-render-sequence "<dependency>
    <groupId>${group}</groupId>
    <artifactId>${artifact}</artifactId>
    <version>${version}</version>
</dependency>"
                                       "group" group
                                       "artifact" artifact
                                       "version" version)))



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

;; (defun tq-create-web-xml-file (filename)
;;   (interactive "sFilename: ")
;;   (let ((text (tq-gen-web-xml-file)))
;;     (tq-file-write-and-open filename text)
;;     (xml-mode)))


(defun tq-init-html-file (title)
  "Initialize a html file, insert header lines, and switch on html-mode.
"
  (interactive "sTitle: ")
  (let ((text (tq-gen-html-file title)))
    (beginning-of-buffer)
    (insert text)
    (html-mode)))

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

(defconst tq-spring-web-xml-content "
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
    response.setMessage(\"hello, \" + request.getName());
    return response;
  }
}
")

(defconst tq-spring-web-service-template "package ${Package}.service;

public interface HelloService {

  public class Request {
    private String name;
    public String getName() {
      return name;
    }

    public void setName(String name) {
      this.name = name;
    }
  }

  public class Response {
    private String message;
    public String getMessage() {
      return message;
    }

    public void setMessage(String message) {
      this.message = message;
    }
  }
  
  public Response hello(Request request);
}
")

(defconst tq-spring-web-controller-template "
package ${Package}.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import ${Package}.service.HelloService;
import ${Package}.service.HelloService.Request;
import ${Package}.service.HelloService.Response;

@Controller
public class Hello {
  @Autowired
  private HelloService helloService;
  
  @RequestMapping(value = \"/\", method = RequestMethod.GET)
  @ResponseBody
  public String hello(Request request) {
    return helloService.hello(request).getMessage();
  }
}
")

;; (defun tq-new-spring-web (root-directory
;;                           project-name
;;                           package)
;;   "建立Spring MVC项目。"
;;   (interactive "sRootDirectory: 
;; sProjectName: 
;; sPackage: ")

;;   (let ((project-directory (expand-file-name project-name root-directory)))
;;     ;; 建立目录
;;     (make-directory project-directory t)

;;     ;; 初始化gradle
;;     (tq-execute-shell "gradle init" project-directory)

;;     ;; 生成build.gradle
;;     (tq-file-write (tq-path-join project-directory "build.gradle")
;;                    tq-spring-web-build-gradle t)

;;     ;; 生成Java代码
;;     (tq-file-write
;;      (tq-path-join project-directory
;;                    "src/main/java/"
;;                    (replace-regexp-in-string "\\." "/" package)
;;                    "controller/Hello.java")
;;      (tq-execute-template
;;       (list "Package"
;;             package)
;;       tq-spring-web-controller-template))

;;     (tq-file-write
;;      (tq-path-join project-directory
;;                    "src/main/java/"
;;                    (replace-regexp-in-string "\\." "/" package)
;;                    "service/HelloService.java")
;;      (tq-execute-template
;;       (list "Package"
;;             package)
;;       tq-spring-web-service-template))

;;     (tq-file-write
;;      (tq-path-join project-directory
;;                    "src/main/java/"
;;                    (replace-regexp-in-string "\\." "/" package)
;;                    "serviceprovider/HelloServiceProvider.java")
;;      (tq-execute-template
;;       (list "Package"
;;             package)
;;       tq-spring-web-serviceprovider-template))

;;     ;; 生成web.xml
;;     (tq-file-write (tq-path-join project-directory "src/main/webapp/WEB-INF/web.xml")
;;                    tq-spring-web-xml-content 
;;                    t)

;;     ;; 生成context config文件
;;     (tq-file-write (tq-path-join project-directory "src/main/webapp/WEB-INF/spring-mvc-config.xml")
;;                    (tq-execute-template (list "Package" package) tq-spring-config-template)
;;                    t)

;;     ;; 初始化git仓库
;;     (tq-new-gitignore (tq-path-join project-directory ""))
;;     (dolist (command (list "git init ."
;;                            "git add ."
;;                            "git commit -m \"initial commit\""))
;;       (tq-execute-shell command project-directory))

;;     ;; 使用gradle 打包
;;     (tq-execute-shell "gradle war" project-directory)

;;     ;; 打开工程目录
;;     (find-file project-directory)))

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

;; (defun tq-new-spring-boot-app (root-directory
;;                                project-name
;;                                package)
;;   "建立Spring Boot项目。"
;;   (interactive "sRootDirectory: 
;; sProjectName: 
;; sPackage: ")

;;   (let ((project-directory (expand-file-name project-name root-directory)))
;;     ;; 建立目录
;;     (make-directory project-directory t)

;;     ;; 初始化gradle
;;     (tq-execute-shell "gradle init" project-directory)

;;     ;; 生成build.gradle
;;     (tq-file-write (tq-path-join project-directory "build.gradle")
;;                    tq-spring-boot-app-build-gradle t)

;;     ;; 生成Java代码
;;     (tq-file-write
;;      (tq-path-join project-directory
;;                    "src/main/java/"
;;                    (replace-regexp-in-string "\\." "/" package)
;;                    "App.java")
;;      (tq-execute-template
;;       (list "Package"
;;             package)
;;       tq-spring-boot-app-template))

;;     ;; 初始化git仓库
;;     (tq-new-gitignore (tq-path-join project-directory ""))
;;     (dolist (command (list "git init ."
;;                            "git add ."
;;                            "git commit -m \"initial commit\""))
;;       (tq-execute-shell command project-directory))

;;     ;; 构建
;;     (tq-execute-shell "gradle bootRun" project-directory)

;;     ;; 打开工程目录
;;     (find-file project-directory)))

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

;; (defun tq-new-spring-boot-web (root-directory
;;                                project-name
;;                                package)
;;   "建立Spring Boot Web项目。"
;;   (interactive "sRootDirectory: 
;; sProjectName: 
;; sPackage: ")

;;   (let ((project-directory (expand-file-name project-name root-directory)))
;;     ;; 建立目录
;;     (make-directory project-directory t)

;;     ;; 初始化gradle
;;     (tq-execute-shell "gradle init" project-directory)

;;     ;; 生成build.gradle
;;     (tq-file-write (tq-path-join project-directory "build.gradle")
;;                    (tq-execute-template (list "ProjectName" project-name) tq-spring-boot-web-build-gradle-template)
;;                    t)

;;     ;; 生成Java代码
;;     (tq-file-write
;;      (tq-path-join project-directory
;;                    "src/main/java/"
;;                    (replace-regexp-in-string "\\." "/" package)
;;                    "Application.java")
;;      (tq-execute-template
;;       (list "Package"
;;             package)
;;       tq-spring-boot-web-application-template))

;;     (tq-file-write
;;      (tq-path-join project-directory
;;                    "src/main/java/"
;;                    (replace-regexp-in-string "\\." "/" package)
;;                    "/controller/Controller.java")
;;      (tq-execute-template
;;       (list "Package"
;;             package)
;;       tq-spring-boot-web-controller-template))

;;     ;; 初始化git仓库
;;     (tq-new-gitignore (tq-path-join project-directory ""))
;;     (dolist (command (list "git init ."
;;                            "git add ."
;;                            "git commit -m \"initial commit\""))
;;       (tq-execute-shell command project-directory))

;;     ;; 构建
;;     (tq-execute-shell "gradle build" project-directory)

;;     ;; 打开工程目录
;;     (find-file project-directory)))



(defconst tq-go-content
  "package main

import (
        \"fmt\"
)

func main() {
        fmt.Println(\"Hello, world!\")
}
")

(defun tq-new-go (root-directory
                  project-name)
  (interactive "sroot directory: 
sproject: ")
  (let ((filename (tq-path-join root-directory project-name "main.go")))
    (tq-file-write filename tq-go-content)
    (find-file filename)))


;; (defun tq-new-java-app (root-directory
;;                         project-name)
;;   "建立Java App项目。"
;;   (interactive "sroot directory: 
;; sproject name: ")
;;   (let ((project-directory (tq-path-join root-directory project-name)))
;;     (message "建立模块目录。")
;;     (make-directory project-directory t)
;;     (message "初始化gradle。")
;;     (tq-execute-shell "gradle init --type java-application" project-directory)
;;     (find-file (tq-path-join project-directory "src/main/java/App.java"))))

(defun tq-java-primary-type-p (type-name)
  (let ((result nil))
    (dolist (primary-type (list "byte" "short" "char" "int"
                                "long" "float" "double" "boolean"))
      (if (string-equal primary-type type-name)
          (setf result t)))
    result))


(defun tq-java-class-file-name (src-directory java-package class-name)
  "生成Java类文件名。"
  (tq-path-join src-directory (tq-java-package-to-directory java-package) (concat class-name ".java")))


(defun tq-java-package-to-directory (package)
  (replace-regexp-in-string "\\." "/" package))

(provide 'tq-java)
