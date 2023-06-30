(defun tq-util-find-definition-file (fn)
  "返回包含fn定义的文件名。"
  (let* ((buffer (car (find-definition-noselect fn nil)))
         (file-name (buffer-file-name buffer)))
    file-name))

(defun tq-util-remove-last (sequence)
  "移除列表中最后一个元素。"
  (let ((n-seq (nreverse sequence)))
    (if (= 0 (length n-seq))
        sequence
      (progn
        (pop n-seq)
        (nreverse n-seq)))))

(defun tq-util-insert-time ()
  "在缓冲区中插入时间字符串。"
  (interactive)
  (insert (format-time-string "%H时%M分%S秒")))

(defun tq-util-insert-date ()
  "在缓冲区中插入日期字符串。"
  (interactive)
  (insert (format-time-string "%Y年%m月%d日")))

(defun tq-util-insert-datetime ()
  "在buffer中插入日期时间字符串。"
  (interactive)
  (insert (format-time-string "%Y年%m月%d日%H时%M分%S秒")))

(defun tq-util-insert-datetime-short ()
  "在buffer中插入日期时间字符串。"
  (interactive)
  (insert (format-time-string "%d/%m %H:%M")))


(defun tq-util-maximize-window-windows ()
  "w32全屏显示。"
  (interactive)
  (let ((sc-maximize 61488))
    (w32-send-sys-command sc-maximize)))

(defun tq-util-maximize-window-linux ()
  "linux全屏显示。"
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NEW_WM_STATE_FULLSCREEN" 0)))

(defun tq-util-maximize-window ()
  "让窗口全屏显示。"
  (interactive)
  (if (string-equal system-type "windows-nt")
      (tq-util-maximize-window-windows)
    (tq-util-maximize-window-linux)))

(defun tq-util-make-string-hash (&rest elements)
  "从序列生成散列表。散列表的键和值都是字符串类型。序列a b c d生成的散列表是{a=>b, c=>d}。"
  (let ((key "")
        (value "")
        (table (make-hash-table :test #'equal)))
    (while (< 0 (length elements))
      (setf key (tq-str-stringify (pop elements)))
      (setf value (if (< 0 (length elements))
                      (tq-str-stringify (pop elements))
                    ""))
      (setf (gethash key table) value))
    table))

(defun tq-util-make-hash (&rest elements)
  "从序列生成散列表。序列a b c d生成的散列表是{a=>b, c=>d}。"
  (let ((key nil)
        (value nil)
        (table (make-hash-table :test #'equal)))
    (while (< 0 (length elements))
      (setf key (pop elements))
      (setf value (if (< 0 (length elements))
                      (pop elements)
                    nil))
      (setf (gethash key table) value))
    table))

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
                                               (tq-upcase-first-letter (nth 1 type-and-name))
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



(defun tq-season-number ()
  "获取季度序号。"
  (+ 1 (/ (string-to-number (format-time-string "%m")) 4)))

(defun tq-open-week-record ()
  "打开周记录文件。如果文件不存在，创建文件。

周记录文件位于周记录目录中，周记录目录是tq-record-directory目录下的一个子目录。
周记录目录名字类似2022-q3-w35，周记录文件名字类似2022-q3-w35.org。
"
  (interactive)
  (let* ((year (format-time-string "%Y"))
         (season (number-to-string (tq-season-number)))
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
#+todo: todo(t) | done(d/!) cancel(c/!)
#+title: ${date}
#+date: ${date}
* todo 本周工作 [%]
* 记录
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
                                                      (tq-upcase-first-letter field)
                                                      field)))
        (setf getset-part (concat getset-part (format setter-statement
                                                      (tq-upcase-first-letter field)
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

(defun tq-snake-to-camel (snake-case)
  "将snake-case字符串转换为camelCase字符串。"
  (let* ((blocks (split-string snake-case "_"))
         (first (car blocks))
         (tail (cdr blocks)))
    (dolist (part tail)
      (setf first (concat first (capitalize part))))
    first))

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
      (setf widget-name (tq-snake-to-camel id))
      (setf widget-type (gethash id widgets))
      (setf source-code (concat source-code "\n" widget-type " " widget-name ";")))
    (setf source-code (concat source-code "\n"))
    (dolist (id (hash-table-keys widgets))
      (setf widget-name (tq-snake-to-camel id))
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

(defun tq-upcase-first-char (field)
  "将首字母改成大写字母。与capitalize不同，不会将其他字母改成小写。"
  (if (= 0 (length field))
      field
    (concat (upcase (substring field 0 1))
            (substring field 1))))

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

(defun tq-java-primary-type-p (type-name)
  (let ((result nil))
    (dolist (primary-type (list "byte" "short" "char" "int"
                                "long" "float" "double" "boolean"))
      (if (string-equal primary-type type-name)
          (setf result t)))
    result))


(defun tq-path-join (root &rest path-parts)
  "拼接文件路径。"
  (dolist (part path-parts)
    (setf root (expand-file-name part root)))
  root)

(defun tq-java-class-file-name (src-directory java-package class-name)
  "生成Java类文件名。"
  (tq-path-join src-directory (tq-java-package-to-directory java-package) (concat class-name ".java")))



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
    (c-basic-offset . 2)
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
;;                     "done(d/!)" "cancel(c/!)"))))
(defun set-org-todo-keywords ()
  "设置org-mode中的todo阶段。"
  (setq org-todo-keywords
        '((sequence "todo(t)"
                    "|"
                    "done(d!)" "cancel(c!)"))))

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


(defun tq-add-cmd-path (path)
  (setenv "PATH" (concat path ";" (getenv "PATH")))
  nil)

(defun tq-add-powershell-path (path)
  "在*PowerShell*中增加系统路径。"
  (let ((buffer (get-buffer "*PowerShell*")))
    (when buffer (powershell-invoke-command-silently
                  (get-buffer-process buffer)
                  (format "$env:PATH+=';%s'" path)))))

(defun tq-c-mode-hook ()
  ;;  (message "tq-c-mode-hook")
  ;;  (c-set-style "linux")
  ;; (c-set-style "")
  ;; (setq tab-width 4
	;;     indent-tabs-mode nil)
  ;;  (c-toggle-auto-newline t)
  (tq-set-indent tq-indent-offset))


;; https://stackoverflow.com/questions/9547912/emacs-calendar-show-more-than-3-months
;; 12-MONTH CALENDAR -- SCROLLS BY MONTH (FORWARDS / BACKWARDS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                            ;;;
;;; Scroll a yearly calendar by month -- in a forwards or backwards direction. ;;;
;;;                                                                            ;;;
;;; To try out this example, evaluate the entire code snippet and type:        ;;;
;;;                                                                            ;;;
;;;     M-x year-calendar                                                      ;;;
;;;                                                                            ;;;
;;; To scroll forward by month, type the key:  >                               ;;;
;;;                                                                            ;;;
;;; To scroll backward by month, type the key:  <                              ;;;
;;;                                                                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load "calendar" '(progn
                               (define-key calendar-mode-map "<" 'lawlist-scroll-year-calendar-backward)
                               (define-key calendar-mode-map ">" 'lawlist-scroll-year-calendar-forward) ))

(defmacro lawlist-calendar-for-loop (var from init to final do &rest body)
  "Execute a for loop.
Evaluate BODY with VAR bound to successive integers from INIT to FINAL,
inclusive.  The standard macro `dotimes' is preferable in most cases."
  `(let ((,var (1- ,init)))
     (while (>= ,final (setq ,var (1+ ,var)))
       ,@body)))

(defun year-calendar (&optional month year)
  "Generate a one (1) year calendar that can be scrolled by month in each direction.
This is a modification of:  http://homepage3.nifty.com/oatu/emacs/calendar.html
See also:  http://ivan.kanis.fr/caly.el"
  (interactive)
  (require 'calendar)
  (let* ((current-year (number-to-string (nth 5 (decode-time (current-time)))))
         (month (if month month
                  (string-to-number
                   (read-string "Please enter a month number (e.g., 1):  " nil nil "1"))))
         (year (if year year
                 (string-to-number
                  (read-string "Please enter a year (e.g., 2014):  "
                               nil nil current-year)))))
    (switch-to-buffer (get-buffer-create calendar-buffer))
    (when (not (eq major-mode 'calendar-mode))
      (calendar-mode))
    (setq displayed-month month)
    (setq displayed-year year)
    (setq buffer-read-only nil)
    (erase-buffer)
    ;; horizontal rows
    (lawlist-calendar-for-loop j from 0 to 3 do
                               ;; vertical columns
                               (lawlist-calendar-for-loop i from 0 to 2 do
                                                          (calendar-generate-month
                                                           ;; month
                                                           (cond
                                                            ((> (+ (* j 3) i month) 12)
                                                             (- (+ (* j 3) i month) 12))
                                                            (t
                                                             (+ (* j 3) i month)))
                                                           ;; year
                                                           (cond
                                                            ((> (+ (* j 3) i month) 12)
                                                             (+ year 1))
                                                            (t
                                                             year))
                                                           ;; indentation / spacing between months
                                                           (+ 5 (* 25 i))))
                               (goto-char (point-max))
                               (insert (make-string (- 10 (count-lines (point-min) (point-max))) ?\n))
                               (widen)
                               (goto-char (point-max))
                               (narrow-to-region (point-max) (point-max)))
    (widen)
    (goto-char (point-min))
    (setq buffer-read-only t)))

(defun lawlist-scroll-year-calendar-forward (&optional arg event)
  "Scroll the yearly calendar by month in a forward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (unless arg (setq arg 1))
  (save-selected-window
    (if (setq event (event-start event)) (select-window (posn-window event)))
    (unless (zerop arg)
      (let ((month displayed-month)
            (year displayed-year))
        (calendar-increment-month month year arg)
        (year-calendar month year)))
    (goto-char (point-min))
    (run-hooks 'calendar-move-hook)))

(defun lawlist-scroll-year-calendar-backward (&optional arg event)
  "Scroll the yearly calendar by month in a backward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (lawlist-scroll-year-calendar-forward (- (or arg 1)) event))

(defun calculate-origin-value (new-value delta-percentage)
  (/ new-value (+ 1 (/ delta-percentage 100.0))))

(defun delta-percentage (origin-value new-value)
  (* 100 (/ (- new-value origin-value) origin-value)))

(defun calculate-change (new-value delta-percentage)
  (- new-value (calculate-origin-value new-value delta-percentage)))




(defun kilocalorie-by-kilogram (kilogram)
  "计算一个人一天需要的热量。

kilogram 体重（公斤）

返回一个列表，其中包含2个数字，分别是热量下限和热量上限，单位千卡。"
  (let ((protein-low 1.2)
        (protein-high 1.5)
        (protein-kcal 4)
        (carbohydrate-low 2)
        (carbohydrate-high 3)
        (carbohydrate-kcal 4)
        (fat-low 0.6)
        (fat-high 0.8)
        (fat-kcal 9)
        (low 0)
        (high 0))
    (setf low (+ (* protein-low kilogram protein-kcal)
                 (* carbohydrate-low kilogram carbohydrate-kcal)
                 (* fat-low kilogram fat-kcal)))
    (setf high (+ (* protein-high kilogram protein-kcal)
                  (* carbohydrate-high kilogram carbohydrate-kcal)
                  (* fat-high kilogram fat-kcal)))
    (list low high)))

(defun kilocalorie-by-nutrition (protein-gram carb-gram fat-gram)
  "计算食物热量。

protein-gram 蛋白质克数
carb-gram 碳水克数
fat-gram 脂肪克数

返回热量，单位千卡。"
  (let ((protein-kcal 4)
        (carb-kcal 4)
        (fat-kcal 9))
    (+ (* protein-kcal protein-gram)
       (* carb-kcal carb-gram)
       (* fat-kcal fat-gram))))


(provide 'tq-util)
