(defun tq-quarkus-resource-parameter (full-resource-name)
  "根据资源全名生成方法中的参数列表。

(tq-quarkus-resource-parameter \"Service/Function/Parameter\") => @PathParam(\"serviceId\") long serviceId, @PathParam(\"functionId\") long functionId, 
"
  (let ((path-parts (mapcar #'tq-pascal-to-camel
                            (tq-remove-last (tq-split-slash full-resource-name)))))
    (if (not path-parts) ""
      (string-join (mapcar (lambda (s) (format "@PathParam(\"%sId\") long %sId, " s s)) path-parts) ""))))


(defun tq-quarkus-restful-name (resource-name)
  "根据资源名字（PascalCase命名法）生成Web资源名字，从Dictionary生成dictionaries。"
  (tq-pascal-to-kebab (tq-string-plural resource-name)))


(defun tq-quarkus-restful-path (resource-name)
  "根据资源名字（PascalCase命名法）生成Web资源路径。如果资源名字类似/Foo/Bar/Goo，将生成/foo/{fooId}/bar/{barId}/goo。"
  (concat (string-join (mapcar (lambda (s) (format "%s/{%sId}/"
                                                   (tq-pascal-to-kebab s)
                                                   (tq-pascal-to-camel s)))
                               (tq-remove-last (tq-split-slash resource-name)))
                       "")
          (tq-quarkus-restful-name (car (last (tq-split-slash resource-name))))))

(defun tq-quarkus-generate-resource-class (java-package full-resource-name field-list)
  (let* ((resource-name (car (last (tq-split-slash full-resource-name))))
         (field-list (remove '(long id) field-list))
         (resource-parameter (tq-quarkus-resource-parameter full-resource-name))
         (source-format "
package %s.resource;

import %s.service.*;
import %s.entity.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.stream.*;
import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.ws.rs.*;
import javax.ws.rs.core.*;
//import org.jboss.resteasy.annotations.jaxrs.*;

@ApplicationScoped
@Path(\"/%s\")
@Produces(MediaType.APPLICATION_JSON)
public class %sResource {

  @Inject private %sService %sService;

  @Path(\"/\")
  @GET
  public List<%s> get(%s) {
    return %sService.get();
  }

  @Path(\"/\")
  @POST
  public %s post(%s%s value) {
    return %sService.create(value);
  }

  @Path(\"/{id}\")
  @GET
  public %s get(%s@PathParam(\"id\") long id) {
    return %sService.get(id);
  }

  @Path(\"/{id}\")
  @PUT
  public void put(%s@PathParam(\"id\") long id, %s value) {
    return %sService.set(id, value);
  }

  @Path(\"/{id}\")
  @PATCH
  public void patch(%s@PathParam(\"id\") long id, %s value) {
    return %sService.update(id, value);
  }

  @Path(\"/{id}\")
  @DELETE
  public void delete(%s@PathParam(\"id\") long id) {
    return %sService.delete(id);
  }
}
"))
    (format source-format
            ;; package
            java-package
            ;; import
            java-package
            java-package
            ;; @Path
            (tq-quarkus-restful-path full-resource-name)
            ;; class
            resource-name
            ;; inject
            resource-name
            (tq-pascal-to-camel resource-name)
            ;; method
            ;; get
            resource-name
            resource-parameter
            (tq-pascal-to-camel resource-name)
            ;; post
            resource-name
            resource-parameter
            resource-name
            (tq-pascal-to-camel resource-name)
            ;; get by id
            resource-name
            resource-parameter
            (tq-pascal-to-camel resource-name)
            ;; put
            resource-name
            resource-parameter
            (tq-pascal-to-camel resource-name)
            ;; patch
            resource-name
            resource-parameter
            (tq-pascal-to-camel resource-name)
            ;; delete
            resource-parameter
            resource-name)))


(defun tq-quarkus-generate-service-class (java-package resource-name field-list)
  (let ((resource-name (car (last (tq-split-slash resource-name))))
        (field-list (remove '(long id) field-list))
        (source-format "
package %s.service;

import %s.entity.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.stream.*;
import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;

@ApplicationScoped
public class %sService {

  public List<%s> get() {
    return %s.listAll();
  }

  public %s create(%s value) {
    if (value == null) { throw new IllegalArgumentException(); }
    value.persist();
    return value;
  }

  public %s get(long id) {
    return %s.findById(id);
  }

  public void update(long id, %s value) {
    if (value == null) { throw new IllegalArgumentException(); }
    %s origin = %s.findById(id);
%s
    origin.persist();
  }

  public void set(long id, %s value) {
    if (value == null) { throw new IllegalArgumentException(); }
    value.id = id;
    value.persist();
  }

  public void delete(long id) {
    %s.deleteById(id);
  }
}
"))
    (format source-format
            ;; package
            java-package
            ;; import
            java-package
            ;; class
            resource-name
            ;; method
            ;; get
            resource-name
            resource-name
            ;; create
            resource-name
            resource-name
            ;; get by id
            resource-name
            resource-name
            ;; update
            resource-name
            resource-name
            resource-name
            (string-join (mapcar (lambda (type-and-name) 
                                   (if (tq-java-primary-type-p (nth 0 type-and-name))
                                       (format "    origin.%s = value.%s;" (nth 1 type-and-name) (nth 1 type-and-name))
                                     (format "    if (value.%s != null) { origin.%s = value.%s; }" (nth 1 type-and-name) (nth 1 type-and-name) (nth 1 type-and-name))))
                                 field-list)
                         "\n")
            ;; set
            resource-name
            ;; delete
            resource-name)))



(defun tq-quarkus-generate-entity-class (java-package resource-name field-list)
  (let ((resource-name (car (last (tq-split-slash resource-name))))
        (field-list (remove '(long id) field-list))
        (source-format "
package %s.entity;

import java.time.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.stream.*;
import javax.persistence.Entity;
import io.quarkus.hibernate.orm.panache.PanacheEntity;

@Entity
public class %s extends PanacheEntity {
%s
}
"))
    (format source-format
            ;; package
            java-package
            ;; class
            resource-name
            ;; fields
            (string-join (mapcar (lambda (type-and-name) (format "  public %s %s;" (nth 0 type-and-name) (nth 1 type-and-name))) field-list) "\n"))))


;; TODO
;; (defun tq-create-restful-app (src-directory java-package resource-name field-list)
;; ;; 生成Resource。
;; ;; 生成Service。
;; ;; 生成Entity。


;; )
