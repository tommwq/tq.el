(defun tq-is-java-source-file (file-name)
  "根据文件后缀判断文件是否是Java源代码文件。"
  (string= "java" (file-name-extension file-name)))

(defun tq-current-file-name ()
  "返回当前文件的绝对路径。"
  (expand-file-name (buffer-name (current-buffer))))

(defun tq-current-java-src-directory ()
  "返回src目录路径。"
  (let* ((file-name (tq-current-file-name))
	 (lookup "src/main/java")
	 (pos (string-match lookup file-name)))
    (if pos
	(substring file-name 0 (+ pos 3))
      nil)))

(defun tq-current-java-class-name ()
  "如果当前文件是Java源代码文件，返回类名（含包名）。否则返回nil。

假设文件路径为src/main/java/...。"
  (let* ((file-name (tq-current-file-name))
	 (lookup "src/main/java")
	 (pos (string-match lookup file-name)))
    (if (tq-is-java-source-file file-name)
	(if pos
	    (string-trim (replace-regexp-in-string
			  "/"
			  "."
			  (substring file-name (+ (length lookup) pos))) "." ".java")
	  (file-name-base file-name))
      nil)))

(defun tq-java-package-name (class-name)
  "从类名中提取包名。"
  (let* ((reversed (reverse class-name))
	 (pos (string-match "\\." reversed)))
    (if pos
	(string-trim (reverse (substring reversed pos)) "" "\\.")
    "")))

(defun tq-new-junit5-test ()
  "为当前java类建立JUnit5测试类。"
  (interactive)
  ;; 判断当前文件是否是Java源代码。
  (let* ((current-file (tq-current-file-name))
	 (class-name (tq-current-java-class-name))
	 (src-dir (tq-current-java-src-directory))
	 (package-name (tq-java-package-name class-name))
	 (test-file-dir (concat src-dir "/test/java/" (replace-regexp-in-string "\\." "/" package-name)))
	 (test-file-name (expand-file-name (concat (file-name-base current-file) "Test.java") test-file-dir))
	 (template "package %s;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;
    
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
    
class %s {
    
        @BeforeAll static void initAll() {
        }
    
        @BeforeEach void init() {
        }
    
        @Test void succeedingTest() {
        }
        
        @AfterEach void tearDown() {
        }
    
        @AfterAll static void tearDownAll() {
        }
}
"))
    (message "test")
    (message class-name)
    (message package-name)
    (message test-file-name)
    (message "test over")
    (when (not (tq-is-java-source-file current-file))
      (message "current file is not a java source file.")
      (return nil))
    (when (file-exists-p test-file-name)
	(find-file test-file-name)
	(return nil))
    (if (not (file-exists-p test-file-dir))
	(make-directory test-file-dir))
    (find-file test-file-name)
    (insert (format template package-name (concat (file-name-base current-file) "Test")))
    (write-file test-file-name)))

