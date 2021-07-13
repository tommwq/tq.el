(defun tq-generate-pom (group artifact version packaging)
  "生成pom.xml文件内容。"
  (tq-render-template-from-sequence "<?xml version='1.0' encoding='UTF-8'?>
<project xmlns='http://maven.apache.org/POM/4.0.0' 
     xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'
     xsi:schemaLocation='http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd'>
    <modelVersion>4.0.0</modelVersion>
    <groupId>${groupID}</groupId>
    <artifactId>${artifactID}</artifactId>
    <version>${version}</version>
    <packaging>${packaging}</packaging>

    <properties>
    </properties>

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


(defconst tq-java-class-file-template "/**
 * File: ${className}.java
 * Description: ${description}
 * Create: ${date}
 * Modify: ${date}
 */

package ${package};

public class ${className} {
        public ${className} () {
        }

        public static void main(String... args) {
                new ${className}();
        }
}
")


(defun tq-generate-java (package class-name description)
  "生成java类。"
  (tq-render-template-from-sequence tq-java-class-file-template
                                    "className" class-name
                                    "description" description
                                    "package" package
                                    "date" (format-time-string "%Y-%m-%d")))

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
  (tq-write-file-then-open "pom.xml" (tq-generate-pom group-id artifact-id (or version "0.1.0-SNAPSHOT") (or packaging "jar")))
  (end-of-buffer))

(defun tq-insert-java-class-file (package class-name description)
  (interactive "sPackage: 
sClassName:
sDescription: ")
  (beginning-of-buffer)
  (insert (tq-generate-java package class-name description))
  (end-of-buffer))

(defun tq-new-java-class-file (file-name package class-name description)
  (interactive "sFileName:
sPackage: 
sClassName:
sDescription: ")
  (tq-write-file-then-open file-name (tq-generate-java package class-name description)))

;; (defun tq-new-java-application (root project package)
;;   (interactive "sRoot:
;; sProject:
;; sPackage:
;; ")
;;   (let* ((origin-directory default-directory)
;;          (package-path (replace-regexp-in-string "\\." "/" package))
;;          (project-directory (expand-file-name project root))
;;          (env (tq-make-string-hash "package" package
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

(defun tq-capture-pojo (start end)  
  "将区域内的文字转换成POJO类。


示例输入：

User
String name
String password

转换后的代码：

public class User {
  private String name;
  private String password;

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getPassword() {
    return password;
  } 

  public void setPassword(String password) {
    this.password = password;
  }
}


示例输入：

String name

转换后的代码：

private String name;

public String getName() {
  return name;
}

public void setName(String name) {
  this.name = name;
}
"
  (interactive "r")
  (let* ((captured (buffer-substring-no-properties start end))
         (sequence (split-string captured))
         (class-name nil)
         (members (make-hash-table :test #'equal))
         (declare-statement "private %s %s;\n")
         (getter-statement "
public %s get%s() {
    return %s;
}
")
         (setter-statement "
public void set%s(%s value) {
    %s = value;
}
"))
    (if (= 1 (mod (length sequence) 2))
        (setf class-name (pop sequence)))
    (while (> (length sequence) 0)
      (let ((type "")
            (field ""))
        (setf type (pop sequence))
        (setf field (pop sequence))
        (setf (gethash field members) type)))
    (let ((declare-part "")
          (getset-part "")
          (source "")
          (type ""))
      (dolist (field (hash-table-keys members))
        (setf type (gethash field members))
        (setf declare-part (concat declare-part (format declare-statement type field)))
        (setf getset-part (concat getset-part (format getter-statement
                                                      type
                                                      (tq-upcase-first-letter field)
                                                      field)))
        (setf getset-part (concat getset-part (format setter-statement
                                                      (tq-upcase-first-letter field)
                                                      type
                                                      field))))
      (setf source (concat declare-part getset-part))
      (if class-name
          (setf source (format "
/*

%s

*/
public class %s {
%s
}
" captured class-name (string-join (mapcar (lambda (s) (concat "    " s)) (split-string source "\n")) "\n"))))
      (delete-region start end)
      (insert source)
      (move-end-of-line))))

(defun tq-create-maven-project (project-name group artifact)
  "创建maven项目。

在当前目录创建project-name子目录，并在其中创建pom.xml和maven目录结构。
"
  (interactive "s项目名字：\ns组：\ns工件：")
  (let* ((project-path project-name)
         (pom-path (expand-file-name "pom.xml" project-path))
         (app-java-path (expand-file-name "App.java" (concat project-path "/" "src/" "main/" "java/" (replace-regexp-in-string "\\." "/" group)))))
    ;; 检查project-name子目录是否存在。
    (if (file-exists-p project-path)
        (error "目录 %s 已存在" project-path))
    ;; 创建project-name子目录。
    (make-directory project-path t)
    ;; 创建project-name/pom.xml文件。
    (tq-write-file pom-path (tq-generate-pom group artifact "0.1.0-SNAPSHOT" "jar"))
    ;; 创建project-name/src/main/java/{GROUP}/App.java。
    (tq-write-file-then-open app-java-path (tq-generate-java group "App" "应用入口类"))))


