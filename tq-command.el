(provide 'tq-command)

(require 'tq-util)

(defun tq-open (buffer-type)
  "打开新的缓冲区，并设置对应的模式。缓冲区名字格式为 *temp-<LANG><n>* 。"
  (interactive "sbuffer type: ")
  (let* ((buffer-name "")
         (number 1)
         (mode-table (tq-util-make-hash "java" #'java-mode
                                        "go" #'go-mode
                                        "xml" #'xml-mode
                                        "c" #'c-mode
			                                  "c++" #'c++-mode
                                        "powershell" #'powershell-mode
                                        "shell" #'shell-mode
                                        "lisp" #'lisp-interaction-mode
                                        "python" #'python-mode
                                        "r" #'r-mode
                                        "org" #'org-mode
                                        "javascript" #'javascript-mode
                                        "css" #'css-mode
                                        "sql" #'sql-mode
                                        "gradle" #'groovy-mode
			                                  "kotlin" #'kotlin-mode
			                                  "dockerfile" #'dockerfile-mode
			                                  "typescript" #'typescript-mode
                                        "elisp" #'lisp-interaction-mode
                                        "php" #'php-mode
                                        "plantuml" #'plantuml-mode
                                        "groovy" #'groovy-mode
                                        "" #'text-mode))
         (mode-setter (gethash buffer-type mode-table)))
    (setf buffer-name (format "*temp-%s%d*" buffer-type number))
    (while (get-buffer buffer-name)
      (setf number (1+ number))
      (setf buffer-name (format "*temp-%s%d*" buffer-type number)))
    (unless mode-setter (setf mode-setter #'text-mode))
    (switch-to-buffer buffer-name)
    (funcall mode-setter)))

(defun tq-set-font (&optional font-size)
  "设置字体大小。"
  (interactive "n字体大小: ")
  (if (and (boundp 'tq-latin-font)
           (boundp 'tq-chinese-font)
           (boundp 'tq-font-size))
      (let* ((size (or font-size tq-font-size))
             (latin-font (format "%s-%d" tq-latin-font size))
             (chinese-font (format "%s-%d" tq-chinese-font size)))
        (message (format "update font %s %s %s"
                         latin-font
                         chinese-font
                         (frame-list)))
        (set-frame-font latin-font t (frame-list))
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font t charset chinese-font)))))

(defun tq-format-cpp ()
  (interactive)
  (untabify (point-min) (point-max))
  (beginning-of-buffer)
  (replace-regexp " +" " ")
  (beginning-of-buffer)
  (replace-string "\n{\n" " {\n")
  (beginning-of-buffer)
  (replace-string "( " "(")
  (beginning-of-buffer)
  (replace-string " )" ")")
  (beginning-of-buffer)
  (replace-string " ;" ";")
  (beginning-of-buffer)
  (replace-regexp ")[ \n\r]+{" ") {")
  (indent-region (point-min) (point-max)))

(defun tq-record ()
  "打开日记录文件。如果文件不存在，创建文件。

日记录文件位于周记录目录中，周记录目录是tq-record-directory目录下的一个子目录。
周记录目录名字类似2022-q3-w35，日记录文件名字类似2022-q3-w35-20220829.org。
"
  (interactive)
  (let* ((year (format-time-string "%Y"))
         (season (number-to-string (tq-util-season-number)))
         (date (format-time-string "%Y年%m月%d日"))
         (week-record-directory-name 
          (format-time-string (concat "%Y-q" season  "-w%V")))
         (day-record-file-name
          (format-time-string (concat "%Y-q" season  "-w%V-%Y%m%d.org")))
         (root-path nil))
    ;; 保证根目录存在。
    (dolist (sub (list tq-record-directory year (concat year "-q" season) week-record-directory-name))
      (setf root-path (expand-file-name sub root-path)))
    (if (not (file-exists-p root-path))
        (make-directory root-path t))
    (setf day-record-file-name (expand-file-name day-record-file-name root-path))
    (if (file-exists-p day-record-file-name)
        (find-file day-record-file-name)
      (tq-file-write-and-open day-record-file-name
                              (tq-template-render-sequence "# -*- mode: org -*-
#+options: ^:nil
#+todo: todo(t) delegate(e@/!) | done(d@/!) cancel(c@/!)
#+property: header-args :exports code
#+html_head: <style>body {font-size:large; font-family:Iosevka,新宋体; line-height:1.5em; background-color:#fffff0;}</style>
#+title: ${date}
#+date: ${date}

" "date" date)))))

(defun tq-set-indent (indent)
  "设置缩进。"
  (interactive "n缩进：")
  (setf tab-width indent
        c-basic-offset indent
        indent-tabs-mode nil
        nxml-attribute-indent indent
        js-indent-level indent
        nxml-child-indent indent
        css-indent-offset indent
        python-indent-offset indent
        powershell-indent indent
        css-indent-offset indent
        plantuml-indent-level indent)
  (if (not (= indent tq-indent-offset))
      (setf tq-indent-offset indent)))

(defun tq-record-week ()
  "打开周记录文件。如果文件不存在，创建文件。

周记录文件位于周记录目录中，周记录目录是tq-record-directory目录下的一个子目录。
周记录目录名字类似2022-q3-w35，周记录文件名字类似2022-q3-w35.org。
"
  (interactive)
  (let* ((year (format-time-string "%Y"))
         (season (number-to-string (tq-util-season-number)))
         (week-string (format-time-string "%Y年%V周"))
         (week-record-directory-name 
          (format-time-string (concat "%Y-q" season  "-w%V")))
         (week-record-file-name
          (concat week-record-directory-name ".org"))
         (root-path nil))
    ;; 保证根目录存在。
    (dolist (sub (list tq-record-directory year (concat year "-q" season) week-record-directory-name))
      (setf root-path (expand-file-name sub root-path)))
    (if (not (file-exists-p root-path))
        (make-directory root-path t))
    (setf week-record-file-name (expand-file-name week-record-file-name root-path))
    (if (file-exists-p week-record-file-name)
        (find-file week-record-file-name)
      (tq-file-write-and-open week-record-file-name
                              (tq-template-render-sequence "# -*- mode: org -*-
#+options: ^:nil
#+todo: todo(t) delegate(e@/!) | done(d@/!) cancel(c@/!)
#+property: header-args :exports code
#+html_head: <style>body {font-size:large; font-family:Iosevka,新宋体; line-height:1.5em; background-color:#fffff0;}</style>
#+title: ${date}
#+date: ${date}
* todo 本周工作 [%]

" "date" week-string)))))

(defun tq-capture-entity (start end)  
  "将区域内的文字转换成JPA Entity类。


示例输入：

User
Long id
String name
String password

转换后的代码：
import javax.persistence.*;

@Entity
public class User {
  @Id
  @GeneratedValue(strategy=GenerationType.IDENTITY)
  pivate Long id;
  private String name;
  private String password;

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

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
    this.%s = value;
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
                                                      (tq-str-upcase-first-char field)
                                                      field)))
        (setf getset-part (concat getset-part (format setter-statement
                                                      (tq-str-upcase-first-char field)
                                                      type
                                                      field))))
      (setf source (concat declare-part getset-part))
      (if class-name
          (setf source (format "

import javax.persistence.*;

/*
%s
*/
@Entity
public class %s {
  @Id
  @GeneratedValue(strategy=GenerationType.IDENTITY)
%s
}
" captured class-name (string-join (mapcar (lambda (s) (concat "  " s)) (split-string source "\n")) "\n"))))
      (delete-region start end)
      (insert source)
      (move-end-of-line))))

(defun tq-capture-find-view-by-id (start end)  
  "将区域内的文字转换成findViewById代码段。

示例输入：

Button cancel_button

转换后的代码：

Button cancelButton;
cancelButton = (Button) findViewById(R.id.switch_button);

"
  (interactive "r")
  (let* ((captured (buffer-substring-no-properties start end))
         (sequence (split-string captured))
         (widgets (make-hash-table :test #'equal))
         (widget-type "")
         (widget-id "")
         (widget-name "")
         (source-code ""))
    (while (> (length sequence) 0)
      (setf widget-type (pop sequence))
      (setf widget-id (pop sequence))
      (setf (gethash widget-id widgets) widget-type))
    (dolist (id (hash-table-keys widgets))
      (setf widget-name (tq-str-snake-to-camel id))
      (setf widget-type (gethash id widgets))
      (setf source-code (concat source-code "\n" widget-type " " widget-name ";")))
    (setf source-code (concat source-code "\n"))
    (dolist (id (hash-table-keys widgets))
      (setf widget-name (tq-str-snake-to-camel id))
      (setf widget-type (gethash id widgets))
      (setf source-code (concat source-code "\n" widget-type " " widget-name " = (" widget-type ") findViewById(R.id." widget-id ");")))
    (delete-region start end)
    (insert source-code)
    (move-end-of-line)))


(defun tq-capture-sqlcase (start end)  
  "将区域内的文字转换成SQL case语句。


示例输入：

foo
1 f
2 g
x

转换后的代码：

case foo
  when 1 then 'f'
  when 2 then 'g'
  else 'x'
end
"
  (interactive "r")
  (let* ((captured (buffer-substring-no-properties start end))
         (sequence (split-string captured))
         (len (length sequence))
         (n 0)
         (k nil)
         (v nil)
         (result ""))
    (if (zerop len)
        (error "捕获区域内容为空。"))
    (setf result (concat "case " (nth 0 sequence)))
    (pop sequence)
    (setf len (- len 1))
    (while (< n len)
      (setf k (nth n sequence))
      (setf v nil)
      (setf n (+ 1 n))
      (if (< n len)
          (setf v (nth n sequence)))
      (if v
          (setf result (concat result "\n  when " k " then '" v "'"))
        (setf result (concat result "\n  else '" k "'\nend")))
      (setf n (+ 1 n)))
    (delete-region start end)
    (insert result)
    (move-end-of-line)))

(defun tq-capture-convert (start end)
  "将区域内的文字转换成convert代码。

示例输入：

a b c

生成的代码：

to.setA(from.getA());
to.setB(from.getB());
to.setC(from.getC());
"
  (interactive "r")
  (let* ((words (split-string
                 (buffer-substring-no-properties start end)))
         (code-format "to.set%s(from.get%s());\n")
         (code ""))
    (dolist (word words)
      (setf code (concat code (format 
                               code-format 
                               (tq-upcase-first-char word)
                               (tq-upcase-first-char word)))))
    (delete-region start end)
    (insert code)))

(defun tq-capture-protobuf-fields (start end first-index)
  "将区域内的文字转换成 Protocal Buffer 域定义。

示例输入：

a
b

转换后：

string a = 1;
string b = 2;
"
  (interactive "r\nn初始下标：")
  (let ((fields (split-string (buffer-substring-no-properties start end)))
        (content ""))
    (dotimes (index (length fields))
      (setf content (concat content (format "string %s = %d;\n" (nth index fields) (+ index first-index)))))
    (delete-region start end)
    (insert content)))

(defun tq-capture-javabean (start end)  
  "将区域内的文字转换成JavaBean类。

示例输入：

User long id String name

转换后的代码：

/*
User long id String name
*/
public class User {
  private long id;
  private String name;

  public User(long id, String name) {
    if (name == null) { throw new IllegalArgumentException(); }

    this.id = id;
    this.name = name;
  }

  public static User of(long id, String name) {
    return new User(id, name);
  }

  public static User create(long id, String name) {
    return new User(id, name);
  }

  public static User copy(User other) {
    return new User(other.id, other.name);
  }

  @Override public boolean equals(Object other) {
    if (other == this) { return true; }
    if (other == null) { return false; }
    if (!(other instanceof User)) { return false; }
    if (!getClass().getCanonicalName().equals(other.getClass().getCanonicalName())) { return false; }
    if (id != ((User) other).id) { return false; }
    if (!name.equals(((User) other).name)) { return false; }
    return true;
  }

  public long getId() { return id; }
  public String getName() { return name; }
}
"
  (interactive "r")
  (let* ((captured (buffer-substring-no-properties start end))
         (sequence (split-string captured))
         (declare-statement-generator (lambda (type-and-name)
                                        (format "  private %s %s;" (nth 0 type-and-name) (nth 1 type-and-name))))
         (getter-statement-generator (lambda (type-and-name)
                                       (format "  public %s get%s() { return %s; }" (nth 0 type-and-name)
                                               (tq-str-upcase-first-char (nth 1 type-and-name))
                                               (nth 1 type-and-name))))
         (constructor-parameter-generator (lambda (type-and-name)
                                            (format "%s %s" (nth 0 type-and-name) (nth 1 type-and-name))))
         (constructor-assert-generator (lambda (type-and-name)
                                         (format "    if (%s == null) { throw new IllegalArgumentException(); }"
                                                 (nth 1 type-and-name)
                                                 (nth 1 type-and-name)
                                                 (nth 1 type-and-name))))
         (constructor-statement-generator (lambda (type-and-name)
                                            (format "    this.%s = %s;"
                                                    (nth 1 type-and-name)
                                                    (nth 1 type-and-name)
                                                    (nth 1 type-and-name))))
         (source-code-format "
/*
%s
*/
public class %s {
%s

  public %s(%s) {
%s

%s
  }

  public static %s of(%s) {
    return new %s(%s);
  }

  public static %s create(%s) {
    return new %s(%s);
  }

  public static %s copy(%s other) {
    return new %s(%s);
  }

  @Override public boolean equals(Object other) {
    if (other == this) { return true; }
    if (other == null) { return false; }
    if (!(other instanceof %s)) { return false; }
    if (!getClass().getCanonicalName().equals(other.getClass().getCanonicalName())) { return false; }
%s
    return true;
  }

%s
}
")
         (class-name nil)
         (fields nil)
         (non-primary-fields nil)
         (field-type nil)
         (field-name nil)
         (source-code nil))
    ;; 读取类名字。
    (if (not (= 1 (mod (length sequence) 2)))
        (error "you need provide a class name."))
    (setf class-name (pop sequence))
    ;; 读取域名字和类型。
    (while (> (length sequence) 0)
      (setf field-type (pop sequence))
      (setf field-name (pop sequence))
      (setf fields (cons (list field-type field-name) fields))
      (if (not (tq-java-primary-type-p field-type))
          (setf non-primary-fields (cons (list field-type field-name) non-primary-fields))))
    (setf fields (nreverse fields))
    (setf non-primary-fields (nreverse non-primary-fields))
    ;; 生成Java代码。
    (setf source-code (format source-code-format
                              captured
                              class-name
                              ;; 域声明
                              (string-join (mapcar declare-statement-generator fields) "\n")
                              ;; 构造函数
                              class-name
                              (string-join (mapcar constructor-parameter-generator fields) ", ")
                              (string-join (mapcar constructor-assert-generator non-primary-fields) "\n")
                              (string-join (mapcar constructor-statement-generator fields) "\n")
                              ;; of方法
                              class-name
                              (string-join (mapcar constructor-parameter-generator fields) ", ")
                              class-name
                              (string-join (mapcar (lambda (type-and-name) (nth 1 type-and-name)) fields) ", ")
                              ;; create方法
                              class-name
                              (string-join (mapcar constructor-parameter-generator fields) ", ")
                              class-name
                              (string-join (mapcar (lambda (type-and-name) (nth 1 type-and-name)) fields) ", ")
                              ;; copy方法
                              class-name
                              class-name
                              class-name
                              (string-join (mapcar (lambda (type-and-name) (format "other.%s" (nth 1 type-and-name))) fields) ", ")
                              ;; equals方法
                              class-name
                              (string-join (mapcar (lambda (type-and-name) 
                                                     (let* ((field-type (nth 0 type-and-name))
                                                            (field-name (nth 1 type-and-name))
                                                            (compare-line-format (if (tq-java-primary-type-p field-type)
                                                                                     "    if (%s != ((%s) other).%s) { return false; }"
                                                                                   "    if (!%s.equals(((%s) other).%s)) { return false; }")))
                                                       (format compare-line-format field-name class-name field-name))
                                                     )
                                                   fields) "\n")

                              ;; get方法
                              (string-join (mapcar getter-statement-generator fields) "\n")))
    (delete-region start end)
    (insert source-code)
    (end-of-buffer)))

(cl-defstruct tq-database-table-definition table-name primary-key columns)

(defun tq-generate-mybatis-mapper-insert (database-table-definition)
  "生成 MyBatis SQL Mapper 插入标签。"
  (let* ((tab "    ")
         (nl "\n")
         (tname (tq-database-table-definition-table-name database-table-definition))
         (clist (tq-database-table-definition-columns database-table-definition))
         (first t)
         (head (concat tab "<insert id=\"insert\"" nl
                       tab tab tab "parameterType=\"com.example.User.Insert\"" nl
                       tab tab tab "resultType=\"com.example.User\">" nl
                       tab tab "INSERT INTO " tname " ("))
         (body "")
         (tail (concat nl
                       tab tab ") VALUES (")))
    (dolist (c clist)
      (when (not first)
        (setf body (concat body ","))
        (setf tail (concat tail ",")))
      (setf first nil)
      (setf body (concat body nl
                         tab tab tab c))
      (setf tail (concat tail nl
                         tab tab tab "#{" c "}")))
    (setf tail (concat tail nl
                       tab tab ")" nl
                       tab "</insert>" nl))
    (concat head body tail)))

(defun tq-generate-mybatis-mapper-update (database-table-definition)
  "生成 MyBatis SQL Mapper 更新标签。"
  (let* ((tab "    ")
         (nl "\n")
         (tname (tq-database-table-definition-table-name database-table-definition))
         (pk (tq-database-table-definition-primary-key database-table-definition))
         (clist (tq-database-table-definition-columns database-table-definition))
         (first t)
         (head (concat tab "<update id=\"update\"" nl
                       tab tab tab "parameterType=\"com.example.User.Update\"" nl
                       tab tab tab "resultType=\"com.example.User\">" nl
                       tab tab "UPDATE " tname " SET "))
         (body "")
         (tail (concat nl
                       tab tab "WHERE " pk "=#{" pk "}" nl
                       tab "</update>" nl)))
    (dolist (c clist)
      (when (not first)
        (setf body (concat body ",")))
      (setf first nil)
      (setf body (concat body nl
                         tab tab tab c "=#{" c "}")))
    (concat head body tail)))

(defun tq-generate-mybatis-mapper-delete (database-table-definition)
  "生成 MyBatis SQL Mapper 删除标签。"
  (let* ((tab "    ")
         (nl "\n")
         (tname (tq-database-table-definition-table-name database-table-definition))
         (pk (tq-database-table-definition-primary-key database-table-definition)))
    (concat tab "<delete id=\"delete\"" nl
            tab tab tab "parameterType=\"com.example.User\"" nl
            tab tab tab "resultType=\"com.example.User\">" nl
            tab tab "DELETE FROM " tname " WHERE " pk "=#{" pk "}" nl
            tab "</delete>" nl)))

(defun tq-generate-mybatis-mapper-select-one (database-table-definition)
  "生成 MyBatis SQL Mapper 查询标签。"
  (let* ((tab "    ")
         (nl "\n")
         (tname (tq-database-table-definition-table-name database-table-definition))
         (pk (tq-database-table-definition-primary-key database-table-definition)))
    (concat tab "<select id=\"selectOne\"" nl
            tab tab tab "parameterType=\"com.example.User\"" nl
            tab tab tab "resultType=\"com.example.User\">" nl
            tab tab "SELECT * FROM " tname " WHERE " pk "=#{" pk "}" nl
            tab "</select>" nl)))

(defun tq-generate-mybatis-mapper-select (database-table-definition)
  "生成 MyBatis SQL Mapper 查询标签。"
  (let* ((tab "    ")
         (nl "\n")
         (tname (tq-database-table-definition-table-name database-table-definition))
         (pk (tq-database-table-definition-primary-key database-table-definition)))
    (concat tab "<select id=\"select\"" nl
            tab tab tab "parameterType=\"com.example.User\"" nl
            tab tab tab "resultType=\"com.example.User\">" nl
            tab tab "SELECT * FROM " tname nl
            tab "</select>" nl)))


(defun tq-generate-mybatis-mapper (database-table-definition)
  "生成 MyBastis SQL Mapper。"
  (let* ((tab "    ")
         (nl "\n")
         (tname (tq-database-table-definition-table-name database-table-definition))
         (pk (tq-database-table-definition-primary-key database-table-definition))
         (clist (tq-database-table-definition-columns database-table-definition))
         (head (concat "<!DOCTYPE mapper" nl
                       tab tab "PUBLIC \"-//mybatis.org//DTD Mapper 3.0//EN\"" nl
                       tab tab "\"http://mybatis.org/dtd/mybatis-3-mapper.dtd\">" nl
                       "<mapper namespace=\"com.guosen.zebra.ia.dao.SqlMapper\">" nl))
         (tail (concat nl "</mapper>"))
         (body ""))
    (dolist (generator (list #'tq-generate-mybatis-mapper-insert
                             #'tq-generate-mybatis-mapper-select
                             #'tq-generate-mybatis-mapper-select-one
                             #'tq-generate-mybatis-mapper-update
                             #'tq-generate-mybatis-mapper-delete))
      (setf body (concat body (funcall generator database-table-definition))))
    (concat "<!--" nl
            tname nl
            pk nl
            (string-join clist nl) nl
            "-->" nl
            head body tail)))

(defun tq-generate-mybatis-annotation (database-table-definition)
  "生成 MyBastis 接口类。"
  (let* ((tname (tq-database-table-definition-table-name database-table-definition))
         (pk (tq-database-table-definition-primary-key database-table-definition))
         (clist (tq-database-table-definition-columns database-table-definition))
         (value-table (make-hash-table :test #'equal))
         (column-block "")
         (assign-block "")
         (placeholder-block "")
         (tab "    ")
         (nl "\n")
         (dquote "\"")
         (first t)
         (column "")
         (optional-comma "")
         (template ""))
    (setf template "import org.apache.ibatis.annotations.*;
import java.util.List;

@Mapper
public interface ${table-name}Repository {

    @Select(\"SELECT * FROM ${table-name}\")
    List<${table-name}> select${table-name}();

    @Select(\"SELECT * FROM ${table-name} WHERE ${primary-key}=#{record.${primary-key}}\")
    List<${table-name}> selectOne${table-name}(@Param(\"record\") ${table-name} ${table-name-lower});

    @Delete(\"DELETE FROM ${table-name} WHERE ${primary-key}=#{record.${primary-key}}\")
    List<${table-name}> deleteOne${table-name}(@Param(\"record\") ${table-name} ${table-name-lower});

    @Insert(\"INSERT INTO ${table-name} (\" + ${column-block}
            \") \" +
            \"VALUES (\" + ${placeholder-block}
            \")\")
    void insert${table-name}(@Param(\"record\") ${table-name} record);

    @Update(\"UPDATE ${table-name} SET \" + ${assign-block}
            \"WHERE ${primary-key}=#{record.${primary-key}}\")
    void update${table-name}(@Param(\"record\") ${table-name} record);
}")
    (puthash "table-name" tname value-table)
    (puthash "table-name-lower" (downcase tname) value-table)
    (puthash "primary-key" pk value-table)
    (dotimes (index (length clist))
      (setf column (nth index clist))
      (setf optional-comma (if (< index (- (length clist) 1))
                               "," 
                             ""))
      (setf column-block (concat column-block nl
                                 tab tab tab dquote column optional-comma dquote " + "))
      (setf placeholder-block (concat placeholder-block nl
                                      tab tab tab dquote "#{record." column "}" optional-comma dquote " + "))
      (setf assign-block (concat assign-block nl
                                 tab tab tab dquote column "=#{record." column "}" optional-comma dquote " + ")))

    (puthash "column-block" column-block value-table)
    (puthash "placeholder-block" placeholder-block value-table)
    (puthash "assign-block" assign-block value-table)
    (tq-render-template template value-table)))

(defun tq-capture-database-table-definition (start end)
  "将区域内的数据表说明内容转换成 tq-database-table-definition。

区域：

tableName
pkColumn
otherColumns...
"
  (interactive "r")
  (let* ((region (buffer-substring-no-properties start end))
         (words (split-string region))
         (tname (nth 0 words))
         (pk (nth 1 words)))
    (pop words)
    (pop words)
    (make-tq-database-table-definition :table-name tname :primary-key pk :columns words)))

(defun tq-capture-mybatis (start end output-format)
  "将区域内的数据表说明内容转换成 MyBatis Mapper。

参数
output-format 输出格式。支持 annotation 和 xml。默认为 annotation。

输入
User
id
name
email

生成

import org.apache.ibatis.annotations.*;
import java.util.List;

@Mapper
public interface UserRepository {

    @Select(\"SELECT * FROM User\")
    List<User> selectUser();

    @Select(\"SELECT * FROM User WHERE id=#{record.id}\")
    List<User> selectOneUser(@Param(\"record\") User user);

    @Delete(\"DELETE FROM User WHERE id=#{record.id}\")
    List<User> deleteOneUser(@Param(\"record\") User user);

    @Insert(\"INSERT INTO User (\" + 
            \"name,\" + 
            \"email\" + 
            \") \" +
            \"VALUES (\" + 
            \"#{record.name},\" + 
            \"#{record.email}\" + 
            \")\")
    void insertUser(@Param(\"record\") User record);

    @Update(\"UPDATE User SET \" + 
            \"name=#{record.name},\" + 
            \"email=#{record.email}\" + 
            \"WHERE id=#{record.id}\")
    void updateUser(@Param(\"record\") User record);
}
"
  (interactive "r\ns输出格式：")
  (let* ((generator (if (string-equal output-format "xml")
                        #'tq-generate-mybatis-mapper
                      #'tq-generate-mybatis-annotation))
         (definition (tq-capture-database-table-definition start end)))
    (delete-region start end)
    (insert (funcall generator definition))))

(defun tq-tablify (table-width &rest values)
  "将格式为 k1 v2 k2 v2...的list转换为org-mode的表格。

table-width 表格的宽度，即一行包含多少个单元格
values 值列表"

  (let ((size (length values))
        (index 0))
    (while (< index size)
      (princ (format "| %s " (nth index values)))

      (setf index (+ 1 index))

      (if (= (% index table-width) 0)
          (princ "|\n"))))
  (princ "|\n"))


(defun tq-make-mybatis-select-xml (java-package-name table-name column-name-list)
  "生成MyBatis查询语句Mapper标签。"
  (let ((head-format "    <select id=\"select%s\" resultType=\"com.guosen.openaccount.persistence.entity.%s\">
        select * from %s where 1=1
")
        (condition-line-format "        <if test=\"%s != null\"> AND %s=#{%s} </if>
")
        (tail "    </select>
")
        (column-name "")
        (result ""))

    (setf result (concat result (format head-format (tq-upcase-first-char table-name) table-name table-name)))

    (dolist (column column-name-list)
      (setf column-name (prin1-to-string column))
      (setf result (concat result (format condition-line-format column-name column-name column-name))))

    (setf result (concat result tail))))

(defun tq-print-mybatis-select-xml (java-package-name table-name column-name-list)
    (princ (tq-make-mybatis-select-xml java-package-name table-name column-name-list))
    nil)

(defun tq-make-mybatis-update-xml (java-package-name table-name primary-key-column-name column-name-list)
  "生成MyBatis更新语句Mapper标签。"
  (let ((head-format "    <update id=\"update%s\">\n        UPDATE %s SET \n")
        (assign-line-format "        <if test=\"%s != null\"> %s=#{%s}, </if> \n")
        (assign-tail-format "        %s=%s")
        (condition-line-format "\n        WHERE %s=#{%s} \n")
        (tail "    </update> \n")
        (column-name "")
        (result ""))

    (setf result (concat result (format head-format (tq-upcase-first-char table-name) table-name)))

    (dolist (column column-name-list)
      (setf column-name (prin1-to-string column))
      (if (not (string-equal column-name primary-key-column-name))
          (setf result (concat result (format assign-line-format column-name column-name column-name)))))
    (setf result (concat result (format assign-tail-format primary-key-column-name primary-key-column-name)))

    (setf result (concat result (format condition-line-format primary-key-column-name primary-key-column-name)))
    (setf result (concat result tail))))

(defun tq-print-mybatis-update-xml (java-package-name table-name primary-key-column-name column-name-list)
    (princ (tq-make-mybatis-update-xml java-package-name table-name primary-key-column-name column-name-list))
    nil)


(defun tq-make-mybatis-insert-xml (java-package-name table-name column-name-list)
  "生成MyBatis更新插入Mapper标签。"
  (let ((head-format "    <insert id=\"insert%s\">\n        INSERT INTO %s (\n")
        (value-line "\n        ) VALUES ( \n")
        (tail "\n        )\n    </insert> \n")
        (result ""))

    (setf column-name-list (mapcar #'prin1-to-string column-name-list))

    (setf result (concat result (format head-format (tq-upcase-first-char table-name) table-name)))
    (setf result (concat result "        " (string-join column-name-list ",")))
    (setf result (concat result value-line))
    (setf result (concat result "        #{" (string-join column-name-list "},#{") "}"))
    (setf result (concat result tail))))

(defun tq-print-mybatis-insert-xml (java-package-name table-name column-name-list)
    (princ (tq-make-mybatis-insert-xml java-package-name table-name column-name-list))
    nil)

(defun tq-make-pojo (class-name field-list)
  "生成POJO类。"
  (let ((class-head-format "public class %s {\n")
        (class-tail "}\n")
        (field-format "    public String %s;\n")
        (name-list (mapcar #'prin1-to-string field-list))
        (result ""))
    
    (setf result (format class-head-format class-name))
    (dolist (name name-list)
      (setf result (concat result (format field-format name))))
    (setf result (concat result class-tail))
    result))

(defun tq-print-pojo (class-name field-list)
  "打印生成的POJO代码。"
  (princ (tq-make-pojo class-name field-list)))


(defun tq-make-ia-method (method-name input-parameter-list output-parameter-list)
  "生成IA方法代码。

示例：

(tq-make-ia-method \"getStationManageOrg\" '(eb_area_id) '(eb_area_id province city org_code))
"
  (let* ((apply-name (concat (tq-upcase-first-char method-name) "Apply"))
         (reply-name (concat (tq-upcase-first-char method-name) "Reply"))
         (method-format "List<%s> %s(%s apply);")
         (apply-class (tq-make-pojo apply-name input-parameter-list))
         (reply-class (tq-make-pojo reply-name output-parameter-list)))
    (concat (format method-format reply-name method-name apply-name) "\n" apply-class "\n" reply-class)))

(defun tq-print-ia-method (method-name input-parameter-list output-parameter-list)
  "打印生成的IA方法代码。

示例：

(tq-print-ia-method \"getStationManageOrg\" '(eb_area_id) '(eb_area_id province city org_code))
"
  (princ (tq-make-ia-method method-name input-parameter-list output-parameter-list))
  nil)


(defun tq-print-table (vector1 &rest other-vectors)
  "打印表格。

vector1，以及other-vectors的元素都是一个列表，标识不同的维度。"
  (let* ((vectors (cons vector1 other-vectors))
         (len (length vectors))
         (sizes (mapcar #'length vectors))
         (break nil)
         (pos 0)
         (x 0)
         (index (make-list len 0))
         (total (seq-reduce #'(lambda (multiplier vector) (* multiplier (length vector))) vectors 1)))

    (dotimes (ignore total)
      ;; 打印元素组合
      (dotimes (i len)
        (princ (format "| %s " (nth (nth i index)  (nth i vectors)))))
      (princ "|\n")

      ;; 计算下一个组合
      (setf pos (- len 1))
      (setf break nil)
      (while (and (not break) (<= 0 pos))
        (setf x (1+ (nth pos index)))
        (if (< x (nth pos sizes))
            (progn
              (setf (nth pos index) x)
              (setf break t))
          (setf (nth pos index) 0))
        (setf pos (- pos 1))))
    nil))

(defun tq-compose-dropdown-menu (list-name value-name)
  (let ((expand-value-name (concat value-name "Expand")))
    (format 
     "
var %s by remember { mutableStateOf(false) }
var %s by remember { mutableStateOf(\"\") }

ExposedDropdownMenuBox(
        expanded = %s,
        onExpandedChange = { %s = !%s },
        modifier = Modifier.fillMaxWidth()
) {

    TextField(
            value = %s,
            onValueChange = {},
            label = { Text(text=%s) },
            modifier = Modifier
                    .menuAnchor()
                    .fillMaxWidth(),
            trailingIcon = { ExposedDropdownMenuDefaults.TrailingIcon(expanded = %s) },
            colors = ExposedDropdownMenuDefaults.textFieldColors(),
            readOnly = true
    )

    ExposedDropdownMenu(
            expanded = %s,
            onDismissRequest = {
                %s = false
            },
    ) {
        %s.forEach { item ->
            DropdownMenuItem(
                    text = { Text(item) },
                    onClick = {
                        %s = item
                        %s = false
                    },
                    contentPadding = ExposedDropdownMenuDefaults.ItemContentPadding
            )
        }
    }
}
"
     expand-value-name value-name expand-value-name expand-value-name
     expand-value-name value-name value-name expand-value-name 
     expand-value-name expand-value-name list-name value-name
     expand-value-name)))

(defun tq-compose-dropdown-menu-print (list-name value-name)
  (princ (tq-compose-dropdown-menu list-name value-name))
  nil)

(defun tq-make-jetpack-room-dao (package-name table-name &optional id-field-name)
"
生成Jetpack Room DAO类代码。

package-name Java包名字，不含“.dao”。
table-name 数据表名字。
id-field-name ID列名字，默认为“id”。
"
  (let ((java-file-template
"package ${package-name}.dao

import androidx.room.*
import kotlinx.coroutines.flow.Flow
import ${package-name}.record.*

@Dao
interface ${table-name}Dao {
  @Insert(onConflict = OnConflictStrategy.REPLACE)
  suspend fun insert(record: ${table-name})

  @Query(\"SELECT * FROM ${table-name} WHERE ${id-field-name}=:id\")
  suspend fun select(id: Long): ${table-name}

  @Query(\"SELECT * FROM ${table-name}\")
  suspend fun select(): List<${table-name}>

  @Update
  suspend fun update(record: ${table-name})

  @Delete
  suspend fun delete(record: ${table-name})

  suspend fun save(record: ${table-name}) {
    if (record.${id-field-name} == 0L) insert(record) else update(record)
  }

  @Query(\"SELECT * FROM ${table-name} WHERE ${id-field-name}=:id\")
  fun watch(id: Long): Flow<${table-name}>

  @Query(\"SELECT * FROM ${table-name}\")
  fun watch(): Flow<List<${table-name}>>
}
")
        (values (make-hash-table :test 'equal)))
    (puthash "package-name" package-name values)
    (puthash "table-name" table-name values)
    (puthash "id-field-name" (or id-field-name "id") values)
    (tq-template-render java-file-template values)))


(defun tq-print-jetpack-room-dao (package-name table-name &optional id-field-name)
  "生成并打印Jetpack Room DAO类代码。"
  (princ (tq-make-jetpack-room-dao package-name table-name id-field-name))
  nil)

(defun tq-print-jetpack-room-repository (package-name room-database-class aggregate-root &optional id-field-name entity-names)
  (princ (tq-make-jetpack-room-repository package-name room-database-class aggregate-root id-field-name entity-names))
  nil)

(defun tq-make-jetpack-room-repository (package-name room-database-class aggregate-root &optional id-field-name entity-names)
  "
生成Jetpack Room Repository代码。

pakcage-name Java包名字，不含“.database...”。
room-database-class RoomDatabase派生类名字。
aggregate-root 聚合根类名字。
id-field-name ID列名字。
entity-names 聚合中除聚合根之外的实体名字列表。

示例：

(tq-print-jetpack-room-repository
 \"com.example\"
 \"MyDatabase\"
 \"AggregateRoot\"
 \"id\"
 '(\"Entity1\"
   \"Entity2\"
   \"Entity3\"))
"
  (let ((values (make-hash-table :test 'equal))
        (template "
package ${package-name}.database.repository

import androidx.room.withTransaction
import ${package-name}.database.${room-database-class}
import ${package-name}.database.dao.*
import ${package-name}.database.record.*
import ${package-name}.domain.*
import javax.inject.*

@Singleton
class ${aggregate-root}Repository @Inject constructor(
    private val database: ${room-database-class},
    private val ${aggregate-root-camel}Dao: ${aggregate-root}Dao
${entity-dao-block}
) {
    suspend fun create(/* TODO */) = ${aggregate-root}(/* TODO */)

    suspend fun get(id: Long) = from(${aggregate-root-camel}Dao.select(id))

    suspend fun get() = ${aggregate-root-camel}Dao.select().map { from(it) }

    suspend fun add(${aggregate-root-camel}: ${aggregate-root}) {
        database.withTransaction {
            // TODO 添加${aggregate-root}。
        }
    }

    suspend fun remove(${aggregate-root-camel}: ${aggregate-root}) {
        database.withTransaction {
            // TODO 删除${aggregate-root}。
        }
    }

    suspend fun save(${aggregate-root-camel}: ${aggregate-root}) {
        database.withTransaction {
            // TODO 保存${aggregate-root}。
        }
    }

    fun watch(id: Long): Flow<${aggregate-root}> {
        return ${aggregate-root-camel}Dao.flow(id).map { from(it) }
    }

    fun watch(): Flow<List<${aggregate-root}>> {
        return ${aggregate-root-camel}Dao.flow().map { it.map { item -> from(item) } }
    }

    suspend fun from(record: ${package-name}.record.${aggregate-root}): ${aggregate-root} {
        // TODO
        return ${aggregate-root}()
    }

    suspend fun to(record: ${aggregate-root}): ${package-name}.record.${aggregate-root} {
        // TODO
        return ${package-name}.record.${aggregate-root}()
    }

${entity-converter-block}
}
"))
    (puthash "package-name" package-name values)
    (puthash "room-database-class" room-database-class values)
    (puthash "aggregate-root" aggregate-root values)
    (puthash "aggregate-root-camel" (tq-str-downcase-first-char aggregate-root) values)
    (puthash "entity-dao-block" 
             (string-join 
              (mapcar
               (lambda (x)
                 (format "    private val %sDao: %sDao" (tq-str-downcase-first-char x) x))
               entity-names) ",\n") values)
    (puthash "entity-converter-block" 
             (string-join
              (mapcar
               (lambda (x)
                 (puthash "entity-name" x values)
                 (tq-template-render 
"
    suspend fun from(record: ${package-name}.record.${entity-name}): ${entity-name} {
        // TODO
        return ${entity-name}()
    }

    suspend fun to(record: ${entity-name}): ${package-name}.record.${entity-name} {
        // TODO
        return ${package-name}.record.${entity-name}()
    }
" values)) entity-names) "\n") values)
    (tq-template-render template values)))


(defun tq-make-defclass (class-name comment-string slot-names)
  "生成类定义代码。
class-name 类名字
comment-string 类注释
slot-names 槽名字列表"
  (let ((slot-part "")
        (slot-format "
      (%s :initarg :%s
          :initform \"\"
          :type string)")
        (method-part "")
        (method-format "
(cl-defmethod to-sql ((object %s))
  (format \"INSERT INTO %s (%s) VALUES (%s);\" %s))
"))

    (dolist (slot-name slot-names)
      (setf slot-part (concat slot-part (format slot-format slot-name slot-name))))
    (setf slot-part (string-trim slot-part))

    (setf method-part (format method-format 
                              class-name
                              (tq-str-kebab-to-pascal (prin1-to-string class-name))
                              (string-join (mapcar (lambda (x) (tq-str-kebab-to-camel (prin1-to-string x))) slot-names) ",")
                              (string-join (mapcar (lambda (x) "'%s'") slot-names) ",")
                              (string-join (mapcar (lambda (x) (format "(oref object %s)" x)) slot-names) " ")
                              ))

    (format
     "
  (defclass %s ()
    (%s)
    \"%s\")

%s
" class-name slot-part comment-string method-part)))

(defun tq-print-defclass (class-name comment-string slot-names)
  "打印类定义代码。
class-name 类名字
comment-string 类注释
slot-names 槽名字列表"
  (princ (tq-make-defclass class-name comment-string slot-names))
  nil)
