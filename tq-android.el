

;; Android项目生成程序处理逻辑
;; 输入项目信息。
;; - 包名。
;; - 根目录。
;; 创建目录。
;; 读取目录模板。
;; 生成目录名。
;; 逐一建立目录。
;; 文件模板：文件名模板+文件内容模板。
;; ROOT
;; ROOT/app/src/androidTest/java/PACKAGE
;; ROOT/app/src/main/aidl/PACKAGE
;; ROOT/app/src/main/java/PACKAGE
;; ROOT/app/src/main/res/PACKAGE
;; ROOT/app/src/main/res/drawable
;; ROOT/app/src/main/res/layout
;; ROOT/app/src/main/res/values
;; ROOT/app/src/test/java/PACKAGE
;; 使用navigation/room/kotlin/
;; 创建gradle文件。
;; 初始化gradle。
;; 创建kotlin文件。
;; 初始化git仓库。

(load-file "tq-android-template.el")

(defconst tq-android-placeholder-pattern "\\($[A-Z_]*\\)")
(defun tq-android-make-file-template (name-template content-template)
  (list name-template content-template))

(defun tq-android-render-file-template(file-template)
  (list tq-android-render-template (first file-template)
        tq-android-render-template (second file-template)))

(defun tq-android-find-placeholders(template)
  (let ((start (setf start (string-match tq-android-placeholder-pattern template)))
        (begin 0)
        (end 0)
        (result (make-hash-table)))
    (while start
      (setf begin (match-beginning 1))
      (setf end (match-end 1))
      (puthash (substring template begin end) t result)
      (setf start (string-match tq-android-placeholder-pattern template end)))
    result))

(defun tq-android-render-template(template value-table)
  (let ((symbol-table (tq-android-find-placeholders template))
        (result template)
        (replacement ""))
    (maphash
     (lambda (key value)
       (setf replacement (gethash (substring key 1) value-table ""))
       (setf result (replace-regexp-in-string key replacement result t)))
     symbol-table)
    result))

(defun tq-android-create-file-from-template(file-template value-table)
  (let ((filename (tq-android-render-template (car file-template) value-table))
        (content (tq-android-render-template (cadr file-template) value-table)))
    ;; 建立父目录
    (if (file-name-directory filename)
        (make-directory (file-name-directory filename) t))
    ;; 生成文件
    (append-to-file content nil filename)))

(defun test ()
  (setf vtbl (make-hash-table :test 'equal))
  (puthash "SDK_ROOT" "C/:/Users/guosen/AppData/Local/Android/Sdk" vtbl)
  (puthash "ROOT" "d:/workspace/project/tq.el/test/" vtbl)

  (dolist (tpl (list tq-android-template-settings-gradle
                     tq-android-template-local-properties
                     tq-android-template-gradle-properties
                     tq-android-template-build-gradle))
    (tq-android-create-file-from-template
     (tq-android-make-file-template (car tpl) (cadr tpl))
     vtbl)))

(defun tq-create-file (filename content)
  "建立文件。"
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


(defun tq-replace-regexp-pairs (pairs text)
  "将text中${xyz}格式的内容替换为pairs中${xyz}对应的值。"
  (let ((pattern "")
        (replace ""))
    (while (< 0 (length pairs))
      (setf pattern (pop pairs))
      (setf replace (pop pairs))
      (setf text (replace-regexp-in-string pattern replace text)))
    text))


(defconst tq-android-entity-template "package ${package}.database

import androidx.room.ColumnInfo
import androidx.room.Entity
import androidx.room.PrimaryKey

@Entity(tableName=\"${modelName}\")
data class ${modelName}(
    @PrimaryKey(autoGenerate=true)
    val id: Long = 0L
)
")


(defconst tq-android-dao-template "package ${package}.database

import androidx.lifecycle.LiveData
import androidx.room.Dao
import androidx.room.Delete
import androidx.room.Insert
import androidx.room.OnConflictStrategy
import androidx.room.Query

@Dao
interface ${modelName}Dao {

    @Query(\"SELECT id FROM ${modelName}\")
    fun queryAll(): LiveData<List<${modelName}>>
 
    @Query(\"SELECT id FROM ${modelName} WHERE id=:id\")
    fun query(id: Long): LiveData<${modelName}>
 
    @Insert(onConflict=OnConflictStrategy.REPLACE)
    fun insert(entity: ${modelName}): Long

    @Delete
    fun delete(entity: ${modelName})
}
")

(defconst tq-android-domain-model-template "package ${package}.domain

import com.tq.abc.DomainModel
import ${package}.manager.${modelName}Manager

class ${modelName}(manager: ${modelName}Manager): DomainModel<${package}.domain.${modelName}, ${package}.viewmodel.${modelName},${package}.database.${modelName}>(manager) {
}
")

(defconst tq-android-manager-template "package ${package}.manager

import android.content.Context
import androidx.lifecycle.ViewModel
import androidx.lifecycle.ViewModelProvider
import ${package}.database.AppDatabase
import ${package}.database.${modelName}Repository
import com.tq.abc.Manager

class ${modelName}Manager private constructor(private val repository: ${modelName}Repository):
Manager<${package}.domain.${modelName},${package}.viewmodel.${modelName},${package}.database.${modelName}>(repository) {

    override fun fromViewModel(viewModel: ${package}.viewmodel.${modelName}): ${package}.domain.${modelName} {
        val domainModel = ${package}.domain.${modelName}(this)
        fromViewModel(viewModel, domainModel)
        return domainModel
    }    
    override fun fromViewModel(viewModel: ${package}.viewmodel.${modelName}, domainModel: ${package}.domain.${modelName}) {
    }    
    override fun toViewModel(domainModel: ${package}.domain.${modelName}, viewModel: ${package}.viewmodel.${modelName}) {
    }
    override fun toViewModel(domainModel: ${package}.domain.${modelName}): ${package}.viewmodel.${modelName} {
        val viewModel = ${package}.viewmodel.${modelName}()
        toViewModel(domainModel, viewModel)
        return viewModel
    }
    override fun fromEntity(entity: ${package}.database.${modelName}): ${package}.domain.${modelName} {
        val domainModel = ${package}.domain.${modelName}(this)
        fromEntity(entity, domainModel)
        return domainModel
    }
    override fun fromEntity(entity: ${package}.database.${modelName}, domainModel: ${package}.domain.${modelName}) {
    }
    override fun toEntity(domainModel: ${package}.domain.${modelName}, entity: ${package}.database.${modelName}) {
    }
    override fun toEntity(domainModel: ${package}.domain.${modelName}): ${package}.database.${modelName} {
        val entity = ${package}.database.${modelName}()
        toEntity(domainModel, entity)
        return entity
    }
    override fun createDomainModel(): ${package}.domain.${modelName} {
        return ${package}.domain.${modelName}(this)
    }
    companion object {
        private var instance: ${modelName}Manager? = null
        fun get(repository: ${modelName}Repository): ${modelName}Manager {
            return instance ?: synchronized(this) {
                instance ?: ${modelName}Manager(repository)
            }
        }
        fun get(context: Context): ${modelName}Manager = get(${modelName}Repository.get(AppDatabase.get(context).get${modelName}Dao()))
    }
}
")

(defconst tq-android-view-model-template "package ${package}.viewmodel

import androidx.lifecycle.ViewModel
import ${package}.manager.${modelName}Manager

class ${modelName}(): ViewModel() {
    var id = 0L
}
")

(defconst tq-android-repository-template "package ${package}.database
import com.tq.abc.Repository

class ${modelName}Repository private constructor(private val dao: ${modelName}Dao) 
: Repository<${modelName}>{

    override fun readAll() = dao.queryAll()
    override fun read(id: Long) = dao.query(id)
    override fun write(entity: ${modelName}) = dao.insert(entity)
    override fun erase(entity: ${modelName}) = dao.delete(entity)

    companion object {
        private var instance: ${modelName}Repository? = null
        fun get(dao: ${modelName}Dao): ${modelName}Repository {
            return instance ?: synchronized(this) {
                instance ?: ${modelName}Repository(dao)
            }
        }
    }
}
")

(defconst tq-android-appdatabase-template "package ${package}.database

import android.content.Context
import androidx.room.Database
import androidx.room.Room
import androidx.room.RoomDatabase

@Database(entities = [/* TODO PUT_ENTITY_CLASSES_HERE */], version = 1, exportSchema = false)
abstract class AppDatabase : RoomDatabase() {

    // TODO abstract fun getSomeDao(): SomeDao

    companion object {
        val DATABASE_NAME = \"MYDATABASE\"

        private var instance: AppDatabase? = null
        fun get(context: Context): AppDatabase {
            return instance ?: synchronized(this) {
                instance ?: Room.databaseBuilder(context, AppDatabase::class.java, DATABASE_NAME).build()
            }
        }
    }
}
")

(defun tq-android-create-model (model-name package-name)
  (interactive "sModel name: 
sPackage name: ")
  (let ((name-and-body (list "database/${modelName}.kt"
                             tq-android-entity-template
                             "database/${modelName}Dao.kt"
                             tq-android-dao-template
                             "domain/${modelName}.kt"
                             tq-android-domain-model-template
                             "viewmodel/${modelName}.kt"
                             tq-android-view-model-template
                             "manager/${modelName}Manager.kt"
                             tq-android-manager-template
                             "database/${modelName}Repository.kt"
                             tq-android-repository-template))
        (optional-name-and-body (list "database/AppDatabase.kt" tq-android-appdatabase-template))
        (name "")
        (body "")
        (pairs (list "${package}"
                     package-name
                     "${modelName}"
                     model-name)))
    (while (< 0 (length name-and-body))
      (setf name (pop name-and-body))
      (setf body (pop name-and-body))
      (tq-create-file (tq-replace-regexp-pairs pairs name)
                      (tq-replace-regexp-pairs pairs body)))
    (while (< 0 (length optional-name-and-body))
      (setf name (pop optional-name-and-body))
      (setf body (pop optional-name-and-body))
      (unless (file-exists-p (tq-replace-regexp-pairs pairs name))
        (tq-create-file (tq-replace-regexp-pairs pairs name)
                        (tq-replace-regexp-pairs pairs body))))))


