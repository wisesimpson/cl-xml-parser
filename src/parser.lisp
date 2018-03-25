(in-package :cl-user)
(defpackage :cl-xml-parser
  (:use :cl)
  (:export :dom-to-xml :xml-to-dom :element-content :element-attributes :element-attribute :xml))
(in-package :cl-xml-parser)

(defparameter white-space-characters '(#\newline #\linefeed #\space #\tab))

(defun read-non-space-char (stream &optional (eof-error-p t) eof-value)
  (loop
     for char = (read-char stream eof-error-p eof-value)
     unless (member char white-space-characters)
     return char))

(defun xml-to-dom (xml)
  (cond ((streamp xml)
         (if (eq #\< (read-non-space-char xml))
             (let ((char (read-non-space-char xml)))
               (cond ((eq char #\?)
                      (read-xml-declaration xml)
                      (if (eq #\< (read-non-space-char xml))
                          (read-element xml)
                          (error "No element.")))
                     ((eq char #\!)
                      (read-comment xml)
                      (if (eq #\< (read-non-space-char xml))
                          (read-element xml)
                          (error "No element.")))
                     (t
                      (read-element xml char))))
             (error "Wrong format.")))
        ((ignore-errors (probe-file xml))
         (with-open-file (stream (probe-file xml))
           (xml-to-dom stream)))
        ((stringp xml)
         (with-open-stream (stream (make-string-input-stream xml))
           (xml-to-dom stream)))))

(defun read-xml-declaration (stream)
  (if (and (eq (read-char stream) #\x)
           (eq (read-char stream) #\m)
           (eq (read-char stream) #\l)
           (member (read-char stream) white-space-characters))
      (let ((attributes (loop for char = (read-non-space-char stream)
                           until (eq char #\?)
                           appending
                             (list
                              (intern
                               (coerce
                                (cons char
                                      (loop for char = (read-char stream)
                                         until (eq char #\=)
                                         collect char))
                                'string)
                               :keyword)
                              (if (eq (read-char stream) #\")
                                  (coerce
                                   (loop for char = (read-char stream)
                                      until (eq char #\")
                                      collect char)
                                   'string)
                                  (error "Wrong attribute value format")))
                           into attributes
                           do
                             (let ((char (read-char stream)))
                               (cond ((eq char #\?)
                                      (return attributes))
                                     ((not (member char white-space-characters))
                                      (error "Wrong attribute ending."))))
                           finally (return attributes))))
        (if (eq (read-char stream) #\>)
            (cons :|?xml| attributes)
            (error "Wrong tag")))
      (error "Wrong XML declaration.")))

(defun read-comment (stream)
  (if (and (eq #\- (read-char stream))
           (eq #\- (read-char stream)))
      (list :!--
            (coerce
             (loop
                for i = (read-char stream) then j
                and j = (read-char stream) then k
                and k = (read-char stream)
                until (and (eq i #\-)
                           (eq j #\-)
                           (eq k #\>))
                collect i)
             'string))
      (error "Wrong comment format.")))

(defun read-element (stream &optional current-char)
  ;; The stream starts after the first char - #\<
  (if current-char
      (let ((tag-name (coerce
                       (loop for char = current-char then (read-char stream)
                          until (or (member char white-space-characters)
                                    (eq char #\/)
                                    (eq char #\>))
                          collect char
                          finally
                            (setf current-char
                                  (if (member char white-space-characters)
                                      (read-non-space-char stream)
                                      char)))
                       'string)))
        (let ((attributes
               (loop for char = current-char then (let ((char (read-char stream)))
                                                    (cond ((or (eq char #\/)
                                                               (eq char #\>))
                                                           char)
                                                          ((member char white-space-characters)
                                                           (read-non-space-char stream))
                                                          (t (error "Wrong attribute ending."))))
                  until (or (eq char #\/)
                            (eq char #\>))
                  append
                    (list
                     (intern
                      (coerce
                       (cons char
                             (loop for char = (read-char stream)
                                until (eq char #\=)
                                collect char))
                       'string)
                      :keyword)
                     (if (eq (read-char stream) #\")
                         (coerce
                          (loop for char = (read-char stream)
                             until (eq char #\")
                             collect char)
                          'string)
                         (error "Wrong attribute value format")))
                  finally (setf current-char char))))
          (cond ((eq current-char #\/)
                 (if (eq (read-char stream) #\>)
                     (cons (intern tag-name :keyword) attributes)
                     (error "Wrong format 3.")))
                ((eq current-char #\>)
                 (let ((content (loop for char = (read-non-space-char stream) then next-char
                                   as next-char = (if (eq char #\<) (read-char stream))
                                   until (and next-char (eq next-char #\/))
                                   if (eq char #\<)
                                   collect (read-element stream next-char)
                                   and do (setf next-char (read-non-space-char stream))
                                   else collect
                                     (string-trim white-space-characters
                                                  (coerce
                                                   (cons char
                                                         (loop for char = (read-char stream)
                                                            until (eq char #\<)
                                                            collect char))
                                                   'string))
                                   and do (setf next-char #\<))))
                   (loop for char across tag-name
                      unless (eq char (read-char stream))
                      return (error "Wrong close tag. tag-name: ~S" tag-name))
                   (if (eq (read-char stream) #\>)
                       (append (list (intern tag-name :keyword)) attributes content)
                       (error "Wrong close tag. tag-name: ~S" tag-name)))))))
      (let ((char (read-char stream)))
        (if (eq char #\!)
            (progn
              (read-comment stream)
              (if (eq #\< (read-non-space-char stream))
                  (read-element stream)
                  (error "No element.")))
            (read-element stream char)))))

(defun element-content (element)
  (subseq element (+ (or (position-if #'symbolp (cdr element) :from-end t) -2) 3)))

(defun element-attributes (element)
  (loop for i on (cdr element) by #'cddr
     while (symbolp (car i))
     append (subseq i 0 2)))

(defun element-attribute (element attribute)
  (getf (element-attributes element) attribute))

(defun dom-to-xml (element &rest elements)
  (format nil "~a~@[~a~]"
          (if (listp element)
              (if (> (length element) 0)
                  (let* ((tag (car element))
                         (attributes (loop for (key value) on (subseq element 1) by #'cddr
                                        while (keywordp key)
                                        collect (if value
                                                    (format nil "~a=\"~a\"" key value)
                                                    key)))
                         (children (let ((index (+ 1 (* 2 (length attributes)))))
                                     (if (> (length element) index)
                                         (loop for child in (subseq element index)
                                            collect (dom-to-xml child))))))
                    (if (element-content element)
                        (let ((tag-name tag))
                          (format nil "<~a~{ ~a~}>~{~a~}</~a>" tag-name attributes children tag-name))
                        (format nil "<~a~{ ~a~}/>" tag attributes)))
                  "")
              (format nil "~a" element))
          (if elements
              (apply #'dom-to-xml elements))))

(defmacro make-element-list (element)
  (if (and (listp element)
           (keywordp (car element)))
      (append (list 'list (car element))
              (loop for child in (subseq element 1)
                 collect `(make-element-list ,child)))
      element))

(defmacro xml (&rest elements)
  `(format nil "~{~a~}"
           (loop for element
              in ,(cons 'list
                        (loop for element in elements
                           collect `(make-element-list ,element)))
              collect (dom-to-xml element))))
