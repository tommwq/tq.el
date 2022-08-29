(defun tq-amdahl-speedup (parallel-ratio parallel-node-count)
  "计算按照Amdahl法则可以获得的优化效果"
  (tq-round (/ 1.0 (+ (- 1.0 parallel-ratio) (/ parallel-ratio parallel-node-count))) 2))

(defun tq-amdahl-speedup-limit (parallel-ratio)
  "计算按照Amdahl法则可以获得的最大优化效果"
  (tq-round (/ 1.0 (- 1.0 parallel-ratio)) 2))

(defun tq-fail-probability (mttf period)
  "预测在接下来的period时间里，模块发生故障的概率。mttf是模块的平均故障发生时间。"
  (let ((e 2.71828))
    (- 1 (expt e (* -1 (/ period mttf))))))

(defun tq-reload ()
  "重新加载tq库。"
  (interactive)
  (let* ((install-directory (eval-when-compile default-directory))
         (tq-file-name (expand-file-name "tq.el" install-directory)))
    (message "reload tq.el")
    (load tq-file-name)))

(defun tq-inch-to-cm (inch)
  "将英寸转换为厘米。"
  (* inch 2.54))

(defun tq-cm-to-inch (cm)
  "将厘米转换为英寸。"
  (/ cm 2.54))

(defun tq-insert-time ()
  "在缓冲区中插入时间字符串。"
  (interactive)
  (insert (format-time-string "%H时%M分%S秒")))

(defun tq-insert-date ()
  "在缓冲区中插入日期字符串。"
  (interactive)
  (insert (format-time-string "%Y年%m月%d日")))

(defun tq-insert-datetime ()
  "在buffer中插入日期时间字符串。"
  (interactive)
  (insert (format-time-string "%Y年%m月%d日%H时%M分%S秒")))

(defun tq-insert-datetime-short ()
  "在buffer中插入日期时间字符串。"
  (interactive)
  (insert (format-time-string "%d/%m %H:%M")))


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

(defun max-window-windows ()
  "w32全屏显示。"
  (interactive)
  (let ((sc-maximize 61488))
    (w32-send-sys-command sc-maximize)))

(defun max-window-linux ()
  "linux全屏显示。"
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NEW_WM_STATE_FULLSCREEN" 0)))

(defun max-window ()
  "让窗口全屏显示。"
  (interactive)
  (if (string-equal system-type "windows-nt")
      (max-window-windows)
    (max-window-linux)))

(defun tq-make-string-hash (&rest elements)
  "从序列生成散列表。散列表的键和值都是字符串类型。序列a b c d生成的散列表是{a=>b, c=>d}。"
  (let ((key "")
        (value "")
        (table (make-hash-table :test #'equal)))
    (while (< 0 (length elements))
      (setf key (tq-stringify (pop elements)))
      (setf value (if (< 0 (length elements))
                      (tq-stringify (pop elements))
                    ""))
      (setf (gethash key table) value))
    table))

(defun tq-make-hash (&rest elements)
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

(defun tq-open-buffer (buffer-type)
  "打开新的缓冲区，并设置对应的模式。"
  (interactive "sbuffer type: ")
  (let* ((buffer-name (format "*temporary-%s*" buffer-type))
         (mode-table (tq-make-hash "java" #'java-mode
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
                                   "" #'text-mode))
         (mode-setter (gethash buffer-type mode-table)))
    (unless mode-setter (setf mode-setter #'text-mode))
    (switch-to-buffer buffer-name)
    (funcall mode-setter)))



(defun amdahl-accelerate-ratio (processor-number parallel-ratio)
  "根据Amdahl定律计算加速比。"
  (let ((n processor-number)
        (p parallel-ratio))
    (/ 1 (+ (- 1 p) (/ p n)))))

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
  "将区域内的文字转换成 JavaBean 类。


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
  "将区域内的数据表说明内容转换成 tq-database-table-definition。"
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
"
  (interactive "r\ns输出格式：")
  (let* ((generator (if (string-equal output-format "xml")
                        #'tq-generate-mybatis-mapper
                      #'tq-generate-mybatis-annotation))
         (definition (tq-capture-database-table-definition start end)))
    (delete-region start end)
    (insert (funcall generator definition))
    (move-end-of-line)))

(defun tq-set-frame-font (latin-font chinese-font)
  (interactive)
  (set-frame-font latin-font)
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font t charset chinese-font)))


(defun tq-set-font (&optional font-size)
  "设置字体

size: 3tiny ttiny tiny(default) small medium large huge"
  (interactive "s字体大小(ttiny/tiny/small/medium/large/huge): ")
  (let* ((font '("LM Mono 12" "方正FW筑紫古典S明朝 简"))
         (size (cond
                ((string-equal "3tiny" font-size) 9)
                ((string-equal "ttiny" font-size) 12)
                ((string-equal "tiny" font-size) 15)
                ((string-equal "small" font-size) 18)
                ((string-equal "medium" font-size) 21)
                ((string-equal "large" font-size) 24)
                ((string-equal "huge" font-size) 27)
                (t 15))))

    (tq-set-frame-font
     (format "%s-%d" (nth 0 font) size)
     (format "%s-%d" (nth 1 font) size))))

(defun tq-reformat-cpp ()
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
        python-indent-offset indent)
  (if (not (= indent tq-indent-offset))
      (setf tq-indent-offset indent)))


(defcustom tq-record-directory "~/record"
  "日志目录"
  :type 'string
  :group 'tq)



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
      (tq-write-file-then-open week-record-file-name
                               (tq-render-template-from-sequence "# -*- mode: org -*-
#+options: ^:nil
#+todo: todo(t) | done(d@/!) canceled(c@/!)
#+title: ${date}
#+date: ${date}
* todo 本周工作 [%]
* 记录
" "date" week-string)))))


(defun tq-open-day-record ()
  "打开日记录文件。如果文件不存在，创建文件。

日记录文件位于周记录目录中，周记录目录是tq-record-directory目录下的一个子目录。
周记录目录名字类似2022-q3-w35，日记录文件名字类似2022-q3-w35-20220829.org。
"
  (interactive)
  (let* ((year (format-time-string "%Y"))
         (season (number-to-string (tq-season-number)))
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
      (tq-write-file-then-open day-record-file-name
                             (tq-render-template-from-sequence "# -*- mode: org -*-
#+options: ^:nil
#+todo: todo(t) | done(d@/!) canceled(c@/!)
#+title: ${date}
#+date: ${date}
* todo 今日工作 [%]
* 记录
" "date" date)))))


