(in-package :cl-user)
(defpackage :cl-xml-parser
  (:use :cl)
  (:export :dom-to-xml :xml-to-dom :element-content :element-attributes :element-attribute :xml :html-to-dom))
(in-package :cl-xml-parser)

(defparameter white-space-characters '(#\newline #\return #\space #\tab))

(defun read-non-white-space-character (stream &optional (eof-error-p t) eof-value)
  (loop
     for char = (read-char stream eof-error-p eof-value)
     unless (member char white-space-characters)
     return char))

(defun string-unescape (string)
  (cl-ppcre:regex-replace-all
   "&amp;"
   (cl-ppcre:regex-replace-all
    "&gt;"
    (cl-ppcre:regex-replace-all
     "&lt;"
     (cl-ppcre:regex-replace-all
      "&apos;"
      (cl-ppcre:regex-replace-all "&quot;" string "\"")
      "'")
     "<")
    ">")
   "&"))

(defun xml-to-dom (xml)
  (cond ((streamp xml)
         (let (xml-declaration DTDs element)
           (loop for char = (read-non-white-space-character xml)
              if (eq char #\<)
              do (let ((char (read-char xml)))
                   (cond ((eq char #\?)
                          (if DTDs
                              (error "XML declaration should be the first.")
                              (setf xml-declaration (read-xml-declaration xml))))
                         ((eq char #\!)
                          (let ((char (read-char xml)))
                            (cond ((eq char #\-)
                                   (read-comment xml))
                                  ((eq char #\D)
                                   (push (read-DTD xml) DTDs))
                                  (t (error "Unknown element. ~a" char)))))
                         (t (return (read-element xml char)))))
              else do (error "Wrong format. Expacting <"))))
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
      (let ((attributes (loop for char = (read-non-white-space-character stream)
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
                                  (string-unescape
                                   (coerce
                                    (loop for char = (read-char stream)
                                       until (eq char #\")
                                       collect char)
                                    'string))
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
  (if (eq #\- (read-char stream))
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

(defun read-DTD (stream)
  (if (and (eq #\O (read-char stream))
           (eq #\C (read-char stream))
           (eq #\T (read-char stream))
           (eq #\Y (read-char stream))
           (eq #\P (read-char stream))
           (eq #\E (read-char stream))
           (member (read-char stream) white-space-characters))
      (coerce
       (loop for char = (read-char stream)
          until (eq char #\>)
          collect char)
       'string)
      (error "Wrong DTD format.")))

(defun read-cdata (stream)
  (if (and (eq #\C (read-char stream))
           (eq #\D (read-char stream))
           (eq #\A (read-char stream))
           (eq #\T (read-char stream))
           (eq #\A (read-char stream))
           (eq #\[ (read-char stream)))
      (coerce
       (loop
          for i = (read-char stream) then j
          and j = (read-char stream) then k
          and k = (read-char stream)
          until (and (eq i #\])
                     (eq j #\])
                     (eq k #\>))
          collect i)
       'string)
      (error "Wrong CDATA format.")))

(defun read-element (stream &optional current-char)
  ;; The stream starts after the first char - #\<
  (unless current-char
    (setf current-char (read-char stream)))
  (cond ((eq current-char #\!)
         (let ((char (read-char stream)))
           (cond ((eq char #\-)
                  (read-comment stream))
                 ((eq char #\[)
                  (read-cdata stream))
                 (t (error "Unknown element. ~a" char)))))
        (t
         (let ((tag-name (coerce
                          (loop for char = current-char then (read-char stream)
                             until (or (member char white-space-characters)
                                       (eq char #\/)
                                       (eq char #\>))
                             collect char
                             finally
                               (setf current-char
                                     (if (member char white-space-characters)
                                         (read-non-white-space-character stream)
                                         char)))
                          'string)))
           (let ((attributes
                  (loop for char = current-char then (let ((char (read-char stream)))
                                                       (cond ((or (eq char #\/)
                                                                  (eq char #\>))
                                                              char)
                                                             ((member char white-space-characters)
                                                              (read-non-white-space-character stream))
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
                            (string-unescape
                             (coerce
                              (loop for char = (read-char stream)
                                 until (eq char #\")
                                 collect char)
                              'string))
                            (error "Wrong attribute value format")))
                     finally (setf current-char char))))
             (cond ((eq current-char #\/)
                    (if (eq (read-char stream) #\>)
                        (cons (intern tag-name :keyword) attributes)
                        (error "Wrong format 3.")))
                   ((eq current-char #\>)
                    (let ((content (loop for char = (read-non-white-space-character stream) then next-char
                                      as next-char = (if (eq char #\<) (read-char stream))
                                      until (and next-char (eq next-char #\/))
                                      if (eq char #\<)
                                      collect (read-element stream next-char)
                                      and do (setf next-char (read-non-white-space-character stream))
                                      else collect
                                        (string-trim white-space-characters
                                                     (string-unescape
                                                      (coerce
                                                       (cons char
                                                             (loop for char = (read-char stream)
                                                                until (eq char #\<)
                                                                collect char))
                                                       'string)))
                                      and do (setf next-char #\<))))
                      (restart-case
                          (progn
                            (loop for char across tag-name
                               as i from 0
                               as char2 = (read-char stream)
                               unless (eq char char2)
                               do
                                 (unread-char char2 stream)
                                 (if (> i 0)
                                     (loop for j from (- i 1) to 0
                                        do (unread-char (elt tag-name j) stream)))
                                 (unread-char #\/ stream)
                                 (unread-char #\< stream)
                               and return (error 'open-tag
                                                 :element (append (list (intern tag-name :keyword)) attributes content)))
                            (let ((char (read-char stream)))
                              (if (eq char #\>)
                                  (append (list (intern tag-name :keyword)) attributes content)
                                  (progn
                                    (unread-char char stream)
                                    (error 'open-tag
                                           :element (append (list (intern tag-name :keyword)) attributes content))))))
                        (accept-open-tag () (append (list (intern tag-name :keyword)) attributes content)))))))))))


(define-condition open-tag (error)
  ((element :initarg :element :reader element)))

(defun element-content (element)
  (subseq element (+ (or (position-if #'symbolp (cdr element) :from-end t) -2) 3)))

(defun element-attributes (element)
  (subseq element 1
          (loop for i on (cdr element) by #'cddr
             for j = 1 then (+ j 2)
             until (not (symbolp (car i)))
             finally (return j))))

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

(defun html-to-dom (element &rest elements)
  (handler-bind ((open-tag
                  #'(lambda (condition)
                      (invoke-restart 'accept-open-tag))))
    (apply 'xml-to-dom element elements)))

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
