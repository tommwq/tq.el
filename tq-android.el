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
    val id: Long
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
    fun queryAll${modelName}(): LiveData<List<${modelName}>>
 
    @Query(\"SELECT id FROM ${modelName} WHERE id=:id\")
    fun query${modelName}(id: Long): LiveData<${modelName}>
 
    @Insert(onConflict=OnConflictStrategy.REPLACE)
    fun insert(entity: ${modelName}): Long

    @Delete
    fun delete(entity: ${modelName})
}
")

(defconst tq-android-domain-model-template "package ${package}.domain

import com.tq.abc.BaseDomainModel
import ${package}.manager.${modelName}Manager

class ${modelName}(manager: ${modelName}Manager): BaseDomainModel<${package}.domain.${modelName}, ${package}.viewmodel.${modelName},${package}.database.${modelName}>(manager) {

    public var id: Long = 0L
}
")

(defconst tq-android-manager-template "package ${package}.manager

import androidx.lifecycle.ViewModel
import androidx.lifecycle.ViewModelProvider
import ${package}.database.${modelName}Repository
import com.tq.abc.Manager

class ${modelName}Manager private constructor(private val repository: ${modelName}Repository):
ViewModelProvider.NewInstanceFactory(),
Manager<${package}.domain.${modelName},${package}.viewmodel.${modelName},${package}.database.${modelName}> {
    
    @Suppress(\"UNCHECKED_CAST\")
    override fun <T: ViewModel?> create(modelClass: Class<T>) = ${package}.viewmodel.${modelName}(this) as T

    override fun createViewModel(): ${package}.viewmodel.${modelName} {
        return ${package}.viewmodel.${modelName}(this)
    }

    override fun fromViewModel(viewModel: ${package}.viewmodel.${modelName}): ${package}.domain.${modelName} {
        val domainModel = ${package}.domain.${modelName}(this)
        return domainModel
    }    
    override fun fromEntity(entity: ${package}.database.${modelName}): ${package}.domain.${modelName} {
        val domainModel = ${package}.domain.${modelName}(this)
        return domainModel
    }
    override fun toViewModel(domainModel: ${package}.domain.${modelName}): ${package}.viewmodel.${modelName} {
        val vm = createViewModel()
        return vm
    }
    override fun toEntity(domainModel: ${package}.domain.${modelName}): ${package}.database.${modelName} {
        return ${package}.database.${modelName}(domainModel.id)
    }
    override fun readDomainModel(id: String): ${package}.domain.${modelName}? {
        val entity = repository.read${modelName}(id.toLong()).value
        return if (entity != null) fromEntity(entity) else null
    }
    
    override fun writeDomainModel(domainModel: ${package}.domain.${modelName}): Long {
        return repository.write${modelName}(toEntity(domainModel))
    }
    
    companion object {
        private var instance: ${modelName}Manager? = null
        fun get(repository: ${modelName}Repository): ${modelName}Manager {
            return instance ?: synchronized(this) {
                instance ?: ${modelName}Manager(repository)
            }
        }
    }
}
")

(defconst tq-android-view-model-template "package ${package}.viewmodel

import ${package}.manager.${modelName}Manager
import com.tq.abc.BaseViewModel

class ${modelName}(private val manager: ${modelName}Manager): BaseViewModel<${package}.domain.${modelName},${package}.viewmodel.${modelName},${package}.database.${modelName}>(manager) {
    var id = 0L
}
")

(defconst tq-android-repository-template "package ${package}.database

class ${modelName}Repository private constructor(private val database: AppDatabase) {

    fun readAll${modelName}() = database.get${modelName}Dao().queryAll${modelName}()
    fun read${modelName}(id: Long) = database.get${modelName}Dao().query${modelName}(id)
    fun write${modelName}(model: ${modelName}) = database.get${modelName}Dao().insert(model)
    fun delete${modelName}(model: ${modelName}) = database.get${modelName}Dao().delete(model)

    companion object {
        private var instance: ${modelName}Repository? = null
        fun get(database: AppDatabase): ${modelName}Repository {
            return instance ?: synchronized(this) {
                instance ?: ${modelName}Repository(database)
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

@Database(entities = [/* TODO put entity classes here */], version = 1, exportSchema = false)
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
                             "manager/${modelName}.kt"
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

