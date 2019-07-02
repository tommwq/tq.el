;; 输入：
;; ROOT
;; SDK_ROOT

(setf tq-android-template-settings-gradle
      '("$ROOT/settings.gradle"
        "include ':app'"))

(setf tq-android-template-local-properties
      '("$ROOT/local.properties"
        "sdk.dir=$SDK_ROOT"))

(setf tq-android-template-gradle-properties
      '("$ROOT/gradle.properties"
        "org.gradle.jvmargs=-Xmx1536m
android.useAndroidX=true
android.enableJetifier=true
kotlin.code.style=official
"))

(setf tq-android-template-build-gradle
      '("$ROOT/build.gradle"
        "buildscript {
    ext {
        kotlin_version = '1.3.31'
        room_version = '2.1.0-alpha03'
        workVersion = '1.0.0-beta01'
    }
    repositories {
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
        google()
        jcenter()
    }
}

task clean(type: Delete) {
    delete rootProject.buildDir
}
"))

;; (setf tq-android-template-settings-gradle
;;       '("$ROOT/settings.gradle"
;;         "include ':app'"))

