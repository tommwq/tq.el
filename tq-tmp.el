
;; (defun tq-set-cmd-variable (variable value)
;;   (setenv variable value))

;; (defun tq-set-powershell-variable (variable value)
;;   (let ((buffer (get-buffer "*PowerShell*")))
;;     (when buffer (powershell-invoke-command-silently
;;                   (get-buffer-process buffer)
;;                   (format "$env:%s='%s'" variable value)))))

;; (defun tq-set-cmd-variables (system-variables)
;;   (let ((pairs system-variables)
;;         (variable "")
;;         (value ""))
;;     (while pairs
;;       (setf variable (car pairs))
;;       (setf value (car (cdr pairs)))
;;       (setf pairs (cdr (cdr pairs)))
;;       (tq-set-cmd-variable (prin1-to-string variable) (prin1-to-string value)))))

;; (defun tq-set-powershell-variables (system-variables)
;;   (let ((pairs system-variables)
;;         (variable "")
;;         (value ""))
;;     (while pairs
;;       (setf variable (car pairs))
;;       (setf value (car (cdr pairs)))
;;       (setf pairs (cdr (cdr pairs)))
;;       (tq-set-powershell-variable (prin1-to-string variable) (prin1-to-string value)))))

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "done" "todo"))))


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
    (tq-write-file-then-open filename text)
    (xml-mode)))


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



(defun tq-java-package-to-directory (package)
  (replace-regexp-in-string "\\." "/" package))


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
    (c-basic-offset . 4)
    (c-comment-only-line-offset . 0)
    (c-echo-syntactic-information-p . t)
    (c-cleanup-list . (
                       ;; brace-else-brace
                       ;; brace-elseif-brace
                       ;; brace-catch-brace
                       ;; empty-defun-braces
                       ;; one-liner-defun
                       ;; defun-close-semi
                       ;; list-close-comma
                       ;; scope-operator
                       ;; space-before-funcall
                       ;; compact-empty-funcall
                       ;; comment-close-slash
                       ))
    (c-hanging-braces-alist . (;; (substatement-open after)
                               ;; (inline-open after)
                               ;; (class-open after)
                               ;; (class-close nil)
                               ;; (defun-open after)
			                   ;; (defun-close nil)
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
                               ;; (substatement-open nil)
                               ;; (inline-open nil)
                               ;; (class-open nil)
                               ;; (class-close nil)
                               ;; (defun-open nil)
                               ;; (defun-close nil)
                               ;; (brace-entry-open nil)
                               ;; (statement nil)
                               ;; (case-label nil)
                               ;; (else-case)
                               ;; (block-close nil)
                               ;; (access-label nil)
                               ;; (do-while-closure nil)
                               ;; (catch-clause nil)
                               ;; (member-init-intro nil)
                               ;; (brace-list-open nil)
                               ))
    (c-hanging-colons-alist .  (
                                ;;(member-init-intro before)
                                ;;(inher-intro)
                                ;;(case-label after)
                                ;;(access-label after)
                                ))
    (c-offsets-alist . ((substatement-open . 0)
			            (statement-case-open . +)
                        (label . 0)
			            (inline-open . 0)
                        (case-label . 0)
                        (block-open . 0))))
  "tq c style")

(c-add-style "tq-c-style" tq-c-style)



(defun tq-initialize-shell-mode ()
  "避免使用shell模式启动PowerShell时中文文件名出现乱码。"
  (set-buffer-process-coding-system 'gbk 'gbk))

(defun tq-initialize-powershell-mode ()
  "避免使用PowerShell模式启动PowerShell时中文文件名出现乱码。"
  (set-buffer-process-coding-system 'utf-8 'utf-8))

;; (defun set-org-todo-keywords ()
;;   "设置org-mode中的todo阶段。"
;;   (setq org-todo-keywords
;;         '((sequence "todo(t)" "in-action(i!)" "delegate(e/!)" "delay(y/!)"
;;                     "|"
;;                     "done(d/!)" "canceled(c/!)"))))
(defun set-org-todo-keywords ()
  "设置org-mode中的todo阶段。"
  (setq org-todo-keywords
        '((sequence "todo(t)" "in-action(i!)" "delegate(e!)" "delay(y!)"
                    "|"
                    "done(d!)" "canceled(c!)"))))

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

;; (defvar tq-note-path "c:/Users/WangQian/Workspace/Notes/")

;; (setq org-publish-project-alist
;;       `(
;;         ("org-notes"
;;          :base-directory ,tq-note-path
;;          :base-extension "txt"
;;          :publishing-directory ,tq-note-path
;;          :recursive t
;;          :publishing-function org-html-publish-to-html
;;          :headline-levels 4          
;;          :auto-preamble nil
;;          :auto-sitemap t
;;          :sitemap-filename "sitemap.txt"
;;          :sitemap-title "sitemap"
;;          :section-numbers nil
;;          :table-of-contents t
;;          :style "<link rel='stylesheet' type='text/css' href='css/org-manual.css' />"
;;          :style-include-default nil
;;          )
;;         ("org"
;;          :components ("org-notes" "org-static")
;;          )
;;         )
;;       )


(defun tq-add-cmd-path (path)
  (setenv "PATH" (concat path ";" (getenv "PATH")))
  nil)

(defun tq-add-powershell-path (path)
  "在*PowerShell*中增加系统路径。"
  (let ((buffer (get-buffer "*PowerShell*")))
    (when buffer (powershell-invoke-command-silently
                  (get-buffer-process buffer)
                  (format "$env:PATH+=';%s'" path)))))

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


(defun tq-new-gitignore (&optional directory)
  "建立gitignore文件"
  (interactive "sdirectory: ")
  (if (string-equal directory "")
      (setf path default-directory))
  (tq-write-file (concat directory "/.gitignore") "
.gradle
.vs/
build/
gradle/
Debug/
Release/
gradlew
gradlew.bat
*~
\#*
*.exe
*.idb
*.ilk
*.htm
*.log
*.obj
*.pch
*.pdb
*.swp
*.tli
*.tlh
*.tlog
*.user
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
                   tq-spring-web-xml-content 
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
  (let ((filename (tq-join-path root-directory project-name "main.go")))
    (tq-write-file filename tq-go-content)
    (find-file filename)))


(defun tq-new-java-app (root-directory
                        project-name)
  "建立Java App项目。"
  (interactive "sroot directory: 
sproject name: ")
  (let ((project-directory (tq-join-path root-directory project-name)))
    (message "建立模块目录。")
    (make-directory project-directory t)
    (message "初始化gradle。")
    (tq-execute-shell "gradle init --type java-application" project-directory)
    (find-file (tq-join-path project-directory "src/main/java/App.java"))))

(defun tq-c-mode-hook ()
  ;;  (message "tq-c-mode-hook")
  ;;  (c-set-style "linux")
  ;; (c-set-style "")
  (setq tab-width 4
	    indent-tabs-mode nil)
  ;;  (c-toggle-auto-newline t)
  )

(defun tq-tmp-generate-gpql-log ()
  "生成测试用的gsProblemQuickLocate日志"
  (let ((today (format-time-string "%Y%m%d"))
        (sql-format
"INSERT INTO gsProblemQuickLocate.dbo.T_QUICK_LOCATE_REQUEST_gstrade_%s
(COMPONENT, CLUSTER_NAME, HOST_NAME, TRACE_ID, REQ_METHOD, PKG, MOBILE, FUND_ID, SOFT_NAME, SOFT_VERSION, REQ_TIME, CLIENT_REQ_TIME, REQ_DATE, MEMORY_USER, INPUT_TYPE)
VALUES('test', 'test', 'test', 'test', 'test', 'test', 'test', 'test', 'test', 'test', '%s', '%s', '%s', 0, 'test');

INSERT INTO gsProblemQuickLocate.dbo.T_QUICK_LOCATE_RESPONSE_DETAIL_gstrade_%s
(COMPONENT, TRACE_ID, RESP_METHOD, RESP_TIME, RESP_DATE, RESP_CONTENT)
VALUES('', '', '', '%s', '%s', '');
"))
    (princ (format sql-format today today today today today today today)))
  nil)

