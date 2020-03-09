;; tq-android.el
;; 帮助android程序开发的函数。
;; 建立日期：2019年09月22日
;; 修改日期：2019年09月24日

;; todo 建立androidTest文件。
;; todo 建立test文件。
;; todo 增加kotlin。
;; todo 加入git仓库。
;; todo 增加app/proguard-rules.pro。

(defconst tq-android-root-build-gradle-template "
buildscript {
    ext {
        kotlin_version = '1.3.31'
        room_version = '2.1.0-alpha03'
        workVersion = '1.0.0-beta01'
    }
    repositories {
        maven { url \"http://maven.aliyun.com/nexus/content/groups/public\" }
        mavenCentral()
        google()
        jcenter()
    }
    dependencies {
        classpath 'com.android.tools.build:gradle:3.4.1'
        classpath \"org.jetbrains.kotlin:kotlin-gradle-plugin:$kotlin_version\"
        classpath \"org.jetbrains.kotlin:kotlin-android-extensions:$kotlin_version\"
    }
}

allprojects {
    repositories {
        maven { url \"http://maven.aliyun.com/nexus/content/groups/public\" }
        mavenCentral()
        google()
        jcenter()
    }
}

task clean(type: Delete) {
    delete rootProject.buildDir
}
")

(defconst tq-android-app-build-gradle-template "
apply plugin: 'com.android.application'

android {
    compileSdkVersion 28
    defaultConfig {
        applicationId \"${applicationId}\"
        minSdkVersion 16
        targetSdkVersion 28
        versionCode 1
        versionName \"1.0\"
        testInstrumentationRunner \"android.support.test.runner.AndroidJUnitRunner\"
    }
    buildTypes {
        release {
            minifyEnabled false
            proguardFiles getDefaultProguardFile('proguard-android-optimize.txt'), 'proguard-rules.pro'
        }
    }
}

dependencies {
    implementation fileTree(dir: 'libs', include: ['*.jar'])
    implementation 'androidx.appcompat:appcompat:1.0.0'
    implementation 'com.android.support.constraint:constraint-layout:1.1.3'
    testImplementation 'junit:junit:4.12'
    androidTestImplementation 'com.android.support.test:runner:1.0.2'
    androidTestImplementation 'com.android.support.test.espresso:espresso-core:3.0.2'
}
")

(defconst tq-android-activity-template "
package ${package};

import androidx.appcompat.app.AppCompatActivity;
import android.os.Bundle;
import android.util.Log;

public class ${activityCapitalize}Activity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_${activity});
    }

    // TODO 增加生命周期的其他回调函数。
}
")

(defconst tq-android-settings-gradle-template
  "include ':app'")

(defconst tq-android-local-properties-template
  "sdk.dir=${androidSdkRoot}")

(defconst tq-android-gradle-properties-template
  "org.gradle.jvmargs=-Xmx1536m
android.useAndroidX=true
android.enableJetifier=true
")

(defconst tq-android-manifest-xml-template
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<manifest xmlns:android=\"http://schemas.android.com/apk/res/android\"
	  package=\"${package}\"
	  android:versionCode=\"1\"
	  android:versionName=\"1.00\">

	<application android:label=\"helloworld\">
		<activity android:name=\".activity.MainActivity\"
			  android:label=\"helloworld\">
			<intent-filter>
				<action android:name=\"android.intent.action.MAIN\" />
				<category android:name=\"android.intent.category.LAUNCHER\" />
			</intent-filter>
		</activity>
	</application>

 	<uses-permission android:name=\"android.permission.WRITE_EXTERNAL_STORAGE\"/>
</manifest>")

(defconst tq-android-activity-layout-xml-template
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<android.support.constraint.ConstraintLayout xmlns:android=\"http://schemas.android.com/apk/res/android\"
    xmlns:app=\"http://schemas.android.com/apk/res-auto\"
    xmlns:tools=\"http://schemas.android.com/tools\"
    android:layout_width=\"match_parent\"
    android:layout_height=\"match_parent\"
    tools:context=\".${activityCapitalize}Activity\">

    <TextView
        android:layout_width=\"wrap_content\"
        android:layout_height=\"wrap_content\"
        android:text=\"Hello World!\"
        app:layout_constraintBottom_toBottomOf=\"parent\"
        app:layout_constraintLeft_toLeftOf=\"parent\"
        app:layout_constraintRight_toRightOf=\"parent\"
        app:layout_constraintTop_toTopOf=\"parent\" />
</android.support.constraint.ConstraintLayout>")


(defconst tq-android-drawable-launcher-xml-template
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<vector xmlns:android=\"http://schemas.android.com/apk/res/android\"
    android:width=\"108dp\"
    android:height=\"108dp\"
    android:viewportWidth=\"108\"
    android:viewportHeight=\"108\">
    <path
        android:fillColor=\"#008577\"
        android:pathData=\"M0,0h108v108h-108z\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M9,0L9,108\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M19,0L19,108\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M29,0L29,108\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M39,0L39,108\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M49,0L49,108\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M59,0L59,108\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M69,0L69,108\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M79,0L79,108\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M89,0L89,108\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M99,0L99,108\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M0,9L108,9\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M0,19L108,19\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M0,29L108,29\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M0,39L108,39\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M0,49L108,49\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M0,59L108,59\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M0,69L108,69\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M0,79L108,79\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M0,89L108,89\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M0,99L108,99\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M19,29L89,29\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M19,39L89,39\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M19,49L89,49\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M19,59L89,59\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M19,69L89,69\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M19,79L89,79\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M29,19L29,89\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M39,19L39,89\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M49,19L49,89\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M59,19L59,89\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M69,19L69,89\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
    <path
        android:fillColor=\"#00000000\"
        android:pathData=\"M79,19L79,89\"
        android:strokeWidth=\"0.8\"
        android:strokeColor=\"#33FFFFFF\" />
</vector>
")


(defconst tq-android-values-styles-xml-template
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<resources>
    <color name=\"colorPrimary\">#008577</color>
    <color name=\"colorPrimaryDark\">#00574B</color>
    <color name=\"colorAccent\">#D81B60</color>
</resources>
")

(defconst tq-android-values-strings-xml-template
  "<resources>
    <string name=\"app_name\">My Application</string>
</resources>
")

(defconst tq-android-values-colors-xml-template
  "<resources>

    <!-- Base application theme. -->
    <style name=\"AppTheme\" parent=\"Theme.AppCompat.Light.DarkActionBar\">
        <!-- Customize your theme here. -->
        <item name=\"colorPrimary\">@color/colorPrimary</item>
        <item name=\"colorPrimaryDark\">@color/colorPrimaryDark</item>
        <item name=\"colorAccent\">@color/colorAccent</item>
    </style>

</resources>
")
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst kotlin-android-gitignore-content "
build/
.gradle
")

(defconst kotlin-android-build-gradle-content "

buildscript {
        ext.kotlin_version = \"1.1.2-2\"

        repositories {
                mavenCentral()
                jcenter()
        }

        dependencies {
                classpath \"com.android.tools.build:gradle:2.2.0\"
                classpath \"org.jetbrains.kotlin:kotlin-gradle-plugin:$kotlin_version\"
        }
}


apply plugin: \"com.android.application\"
apply plugin: \"kotlin-android\"

repositories {
        mavenCentral()
        jcenter()
}

dependencies {
        compile \"org.jetbrains.kotlin:kotlin-stdlib-jre7:$kotlin_version\"
}

// kotlin.incremental = true
android {
        compileSdkVersion 19
        buildToolsVersion \"25.0.2\"

        compileOptions {
                sourceCompatibility JavaVersion.VERSION_1_7
                targetCompatibility JavaVersion.VERSION_1_7
        }
        
}

")

(defconst kotlin-android-layout-xml-content "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<LinearLayout xmlns:android=\"http://schemas.android.com/apk/res/android\"
	      android:orientation=\"vertical\"
	      android:layout_width=\"fill_parent\"
	      android:layout_height=\"fill_parent\"
	      >
	<TextView
		android:layout_width=\"fill_parent\"
		android:layout_height=\"wrap_content\"
		android:text=\"HELLO WORLD!\"
		/>
</LinearLayout>
")

(defconst kotlin-android-activity-format
  "package ${package}.activity;

import android.os.Bundle
import android.app.Activity
import ${package}.R

class MainActivity: Activity() {

	override fun onCreate(savedInstanceState: Bundle?) {
		super.onCreate(savedInstanceState)
		setContentView(R.layout.main)
	}
}
")

(defun tq-create-kotlin-android-app-project (project-name
                                             package)
  (interactive "sProjectName: 
sPackage: ")

  ;; create project directory
  (make-directory project-name t)

  ;; create .gitignore
  (tq-write-file
   (expand-file-name
    ".gitignore"
    project-name)
   kotlin-android-gitignore-content)

  ;; create build.gradle
  (tq-write-file
   (expand-file-name
    "build.gradle"
    project-name)
   kotlin-android-build-gradle-content)

  ;; mkdir src/main/res/layout
  (make-directory
   (expand-file-name
    "src/main/res/layout"
    project-name)
   t)

  ;; mkdir src/main/java/{package}/activity
  (make-directory
   (expand-file-name
    (concat "src/main/java/"
            (tq-java-package-to-directory package)
            "/activity/")
    project-name)
   t)

  ;; create src/main/java/{package}/activity/MainActivity.kt
  (let ((filename (expand-file-name
                   (concat "src/main/java/"
                           (tq-java-package-to-directory package)
                           "/activity/MainActivity.kt")
                   project-name))
        (content
         (tq-replace-regexp-pairs
          (list "${package}" package)
          kotlin-android-activity-format)))
    (tq-write-file filename content))

  ;; create src/main/AndroidManifest.xml
  (tq-write-file
   (expand-file-name
    "src/main/AndroidManifest.xml"
    project-name)
   (tq-replace-regexp-pairs
    (list "${package}" package)
    kotlin-android-androidmanifest-xml-format))

  ;; create src/main/res/layout/main.xml
  (tq-write-file
   (expand-file-name
    "src/main/res/layout/main.xml"
    project-name)
   kotlin-android-layout-xml-content))


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

@Database(entities = [${modelClassList}], version = 1, exportSchema = false)
abstract class AppDatabase : RoomDatabase() {

${daoBlock}

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

(defun tq-android-create-one-model (model-name package-name)
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
        (name "")
        (body "")
        (pairs (list "${package}"
                     package-name
                     "${modelName}"
                     model-name)))
    (while (< 0 (length name-and-body))
      (setf name (pop name-and-body))
      (setf body (pop name-and-body))
      (tq-create-file-force (tq-replace-regexp-pairs pairs name)
                            (tq-replace-regexp-pairs pairs body)))))

(defun tq-android-create-appdatabase (model-name-list package-name)
  "生成AppDatabase代码。"
  (interactive "sModel name(seperated by space): 
sPackage name: ")
  (tq-create-file-force "database/AppDatabase.kt"
                        (tq-replace-regexp-pairs (list "${package}"
                                                       package-name
                                                       "${modelClassList}"
                                                       (seq-reduce (lambda (model-class-list model-name)
                                                                     (let ((sep ""))
                                                                       (if (< 0 (length model-class-list))
                                                                           (setf sep ","))
                                                                       (concat model-class-list sep (format "%s::class" model-name))))
                                                                   (split-string model-name-list) "")
                                                       "${daoBlock}"
                                                       (seq-reduce (lambda (block model-name)
                                                                     (concat block (format "    abstract fun get%sDao(): %sDao\n"
                                                                                           model-name
                                                                                           model-name)))
                                                                   (split-string model-name-list) ""))
                                                 tq-android-appdatabase-template)))

(defun tq-android-create-model (model-name-list package-name)
  (interactive "sModel name(seperated by space): 
sPackage name: ")
  ;; 生成模块代码
  (dolist (model-name (split-string model-name-list " "))
    (tq-android-create-one-model model-name package-name))
  (tq-android-create-appdatabase model-name-list package-name))

;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst tq-android-fragment-template "package ${packageName}

import android.os.Bundle
import androidx.fragment.app.Fragment
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import ${applicationPackage}.databinding.Fragment${fragmentName}Binding

class ${fragmentName}Fragment() : Fragment() {

    private lateinit var binding: Fragment${fragmentName}Binding

    override fun onCreateView(
        inflater: LayoutInflater,
        container: ViewGroup?,
        savedInstanceState: Bundle?
    ): View? {
        binding = Fragment${fragmentName}Binding.inflate(inflater, container, false)
        return binding.root
    }

    companion object {
    }
}
")

(defconst tq-android-fragment-layout-template "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<layout xmlns:android=\"http://schemas.android.com/apk/res/android\"
    xmlns:tools=\"http://schemas.android.com/tools\">

    <FrameLayout
        android:layout_width=\"match_parent\"
        android:layout_height=\"match_parent\">

    </FrameLayout>
</layout>
")



(defun tq-new-android-application (root applicationId package android-sdk-root)
  (let* ((activity "main")
         (env (tq-make-string-hash "applicationId" applicationId
                                   "package" package
                                   "packagePath" (replace-regexp-in-string "\\." "/" package)
                                   "androidSdkRoot" android-sdk-root
                                   "activity" activity
                                   "activityCapitalize" (tq-upcase-first-letter activity)
                                   "root" root)))
    (tq-workflow-execute
     (make-instance 'tq-workflow
                    :environment env
                    :steps (list (make-instance 'tq-workflow-step-run-shell-command
                                                :command "gradle -no-daemon init --dsl groovy --type basic")
                                 (make-instance 'tq-workflow-step-render-file
                                                :file-name-template "${root}/build.gradle"
                                                :content-template tq-android-root-build-gradle-template
                                                :environment env
                                                :overwrite t)
                                 (make-instance 'tq-workflow-step-render-file
                                                :file-name-template "${root}/app/build.gradle"
                                                :content-template tq-android-app-build-gradle-template
                                                :environment env
                                                :overwrite t)
                                 (make-instance 'tq-workflow-step-render-file
                                                :file-name-template "${root}/app/src/main/java/${packagePath}/${activityCapitalize}Activity.java"
                                                :content-template tq-android-activity-template
                                                :environment env
                                                :overwrite t)
                                 (make-instance 'tq-workflow-step-render-file
                                                :file-name-template "${root}/settings.gradle"
                                                :content-template tq-android-settings-gradle-template
                                                :environment env
                                                :overwrite t)
                                 (make-instance 'tq-workflow-step-render-file
                                                :file-name-template "${root}/local.properties"
                                                :content-template tq-android-local-properties-template
                                                :environment env
                                                :overwrite t)
                                 (make-instance 'tq-workflow-step-render-file
                                                :file-name-template "${root}/gradle.properties"
                                                :content-template tq-android-gradle-properties-template
                                                :environment env
                                                :overwrite t)
                                 (make-instance 'tq-workflow-step-render-file
                                                :file-name-template "${root}/app/src/main/AndroidManifest.xml"
                                                :content-template tq-android-manifest-xml-template
                                                :environment env
                                                :overwrite t)
                                 (make-instance 'tq-workflow-step-render-file
                                                :file-name-template "${root}/app/src/main/res/layout/activity_${activity}.xml"
                                                :content-template tq-android-activity-layout-xml-template
                                                :environment env
                                                :overwrite t)
                                 (make-instance 'tq-workflow-step-render-file
                                                :file-name-template "${root}/app/src/main/res/drawable/ic_launcher_background.xml"
                                                :content-template tq-android-drawable-launcher-xml-template
                                                :environment env
                                                :overwrite t)
                                 (make-instance 'tq-workflow-step-render-file
                                                :file-name-template "${root}/app/src/main/res/values/strings.xml"
                                                :content-template tq-android-values-strings-xml-template
                                                :environment env
                                                :overwrite t)
                                 (make-instance 'tq-workflow-step-render-file
                                                :file-name-template "${root}/app/src/main/res/values/colors.xml"
                                                :content-template tq-android-values-colors-xml-template
                                                :environment env
                                                :overwrite t)
                                 (make-instance 'tq-workflow-step-render-file
                                                :file-name-template "${root}/app/src/main/res/values/styles.xml"
                                                :content-template tq-android-values-styles-xml-template
                                                :environment env
                                                :overwrite t)
                                 (make-instance 'tq-workflow-step-message
                                                :text "android project created."))))))

(defun tq-android-create-fragment (root application-package package-name fragment-name)
  (let ((env (tq-make-string-hash "root" root
                                  "applicationPackage" application-package
                                  "packageName" package-name
                                  "packagePath" (replace-regexp-in-string "\\." "/" package-name)
                                  "fragmentName" fragment-name
                                  "layoutName" (capitalize-to-underscore fragment-name))))
    (tq-workflow-execute
     (make-instance 'tq-workflow
                    :environment env
                    :steps (list (make-instance 'tq-workflow-step-render-file
                                                :file-name-template "${root}/app/src/main/res/layout/fragment${layoutName}.xml"
                                                :content-template tq-android-fragment-layout-template
                                                ;; todo remove it
                                                :overwrite t
                                                :environment env)
                                 (make-instance 'tq-workflow-step-render-file
                                                :file-name-template "${root}/app/src/main/java/${packagePath}/${fragmentName}Fragment.kt"
                                                :content-template tq-android-fragment-template
                                                :environment env
                                                :overwrite t))))))
