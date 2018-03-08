(tq-append-env-path "C:/Program Files/Git/bin")
(tq-append-env-path "D:/Gradle/gradle-4.6/bin")

(defun tq-append-env-path (path)
  (setenv "PATH" (concat path ";" (getenv "PATH")))
  nil)


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

(setf tq-spring-config-template "
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
        <bean id=\"helloService\" class=\"${Package}.service.HelloServiceProvider\" />        

	<mvc:annotation-driven />

</beans>
")

(setf tq-spring-mvc-web-xml-content "
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
    

(setf tq-spring-mvc-build-gradle "
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

(setf tq-service-template "
package ${Package}.service;

import ${Package}.api.HelloService;
import ${Package}.api.HelloService.Request;
import ${Package}.api.HelloService.Response;

public class HelloServiceProvider implements HelloService {
        public Response hello(Request request) {
                Response response = new Response();
                response.message = \"hello, \" + request.name;
                return response;
        }
}
")


(setf tq-service-api-template "
package ${Package}.api;

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

(setf tq-controller-template "
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

(defun tq-create-gitignore-file (filename)
  (tq-write-file filename "
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


(defun tq-create-spring-mvc-project (root-directory
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
                   tq-spring-mvc-build-gradle t)

    ;; 生成Java代码
    (tq-write-file
     (tq-join-path project-directory
                   "src/main/java/"
                   (replace-regexp-in-string "\\." "/" package)
                   "controller/Hello.java")
     (tq-render-template
      (list "Package"
            package)
      tq-controller-template))

    (tq-write-file
     (tq-join-path project-directory
                   "src/main/java/"
                   (replace-regexp-in-string "\\." "/" package)
                   "api/HelloService.java")
     (tq-render-template
      (list "Package"
            package)
      tq-service-api-template))

    (tq-write-file
     (tq-join-path project-directory
                   "src/main/java/"
                   (replace-regexp-in-string "\\." "/" package)
                   "service/HelloServiceProvider.java")
     (tq-render-template
      (list "Package"
            package)
      tq-service-template))
    
    ;; 生成web.xml
    (tq-write-file (tq-join-path project-directory "src/main/webapp/WEB-INF/web.xml")
                   tq-spring-mvc-web-xml-content 
                   t)
    
    ;; 生成context config文件
    (tq-write-file (tq-join-path project-directory "src/main/webapp/WEB-INF/spring-mvc-config.xml")
                   (tq-render-template (list "Package" package) tq-spring-config-template)
                   t)

    ;; 初始化git仓库
    (message project-directory)
    (message (tq-join-path project-directory ".gitignore"))
    (tq-create-gitignore-file (tq-join-path project-directory ".gitignore"))
    (dolist (command (list "git init ."
                           "git add ."
                           "git commit -m \"initial commit\""))
      (tq-execute-shell command project-directory))
    
    ;; 使用gradle 打包
    (tq-execute-shell "gradle war" project-directory)

    ;; 打开工程目录
    (find-file project-directory)))



(defun tq-create-pojo (start end)  
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

