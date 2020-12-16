(defpackage #:qlot/source/dist
  (:nicknames #:qlot.source.dist)
  (:use #:cl
        #:qlot/source/base)
  (:import-from #:qlot/utils
                #:make-keyword)
  (:import-from #:qlot/utils/ql
                #:make-versioned-distinfo-url)
  (:import-from #:qlot/errors
                #:invalid-definition)
  (:export #:source-dist
           #:source-dist-project
           #:source-distribution
           #:source-distinfo-url))
(in-package #:qlot/source/dist)

(defclass source-dist-project (source)
  ((%version :initarg :%version)
   (distinfo :initarg :distinfo
             :initform nil
             :accessor source-distinfo-url)))

(defclass source-dist (source-dist-project)
  ((distribution :initarg :distribution
                 :accessor source-distribution)))

(defmethod source-distribution ((source source-dist-project))
  (error "Must be implemented in subclasses"))

(eval-when (:compile-toplevel :load-toplevel)
  (defun make-source-symbol-name (project-name)
    (concatenate 'string "SOURCE-" (string-upcase project-name))))

(defmacro make-new-source-dist (project-name distribution)
  (let ((source-class-name (gensym))
        (source-class-kw (gensym))
        (usage (gensym)))
    `(let ((,source-class-name ,(intern (make-source-symbol-name project-name)))
           (,source-class-kw ,(make-keyword (symbol-name project-name))))
       (format t "~a~%~a~%" ,source-class-name ,source-class-kw)
       (defclass ,source-class-name (source-dist-project)
         ())
       (export ',source-class-name)
       (defmethod initialize-instance ((source ,source-class-name) &rest initargs &key distribution)
         (declare (ignore initargs distribution))
         (call-next-method))
       (defmethod source-distribution ((source ,source-class-name))
         ,distribution)
       (defmethod make-source ((source (eql ,source-class-kw)) &rest args)
         (handler-case
             (destructuring-bind (project-name &optional (version :latest))
                 initargs
               (check-type project-name string)
               (check-type version (or string (eql :latest)))
               (make-instance ',source-class-name
                              :project-name project-name
                              :%version version))
           (error ()
             (let ((,usage (format nil "~(~a~) <project name> [<version>]" ,source-class-name)))
               (error 'invalid-definition
                      :source ,source-class-kw
                      :usage ,usage))))))))

(defmethod make-source ((source (eql :dist)) &rest initargs)
  (handler-case
      (destructuring-bind (project-name distribution &optional (version :latest))
          initargs
        (unless (or (find-symbol (make-source-symbol-name project-name)) (string= project-name "quicklisp"))
          (format t "making a new source dist called ~a~%" project-name)
          (make-new-source-dist project-name distribution)
          (format t "done~%~%"))
        (make-instance 'source-dist
                       :project-name project-name
                       :distribution distribution
                       :%version version))
    ;; TODO(Cameron): Remove the condition part after debugging
    ;; -> error: qlot/source/base:source-project-name is unbound... but why?
    (error (condition)
      (format t "~&the error was ~a" condition)
      (error 'invalid-definition
             :source :dist
             :usage "dist <project name> <distribution URL> [<version>]"))))

(defmethod defrost-source :after ((source source-dist-project))
  (when (slot-boundp source 'qlot/source/base::version)
    (setf (slot-value source '%version)
          (subseq (source-version source)
                  (length (source-version-prefix source))))))

(defmethod print-object ((source source-dist-project) stream)
  (print-unreadable-object (source stream :type t :identity t)
    (format stream "~A ~A ~A"
            (source-project-name source)
            (source-distribution source)
            (if (slot-boundp source 'qlot/source/base::version)
                (source-version source)
                (slot-value source '%version)))))

(defmethod source= ((source1 source-dist-project) (source2 source-dist-project))
  (and (string= (source-project-name source1)
                (source-project-name source2))
       (string= (source-distribution source1)
                (source-distribution source2))
       (string= (slot-value source1 '%version)
                (slot-value source2 '%version))))

(defmethod source-version-prefix ((source source-dist))
  "")
