(in-package :cl-user)
(defpackage :cl-xml-parser
  (:use :cl)
  (:export :list-to-xml :xml-to-list :get-element-content))
(in-package :cl-xml-parser)

(defparameter white-space-characters '(#\newline #\linefeed #\space #\tab))

(defun read-non-space-char (stream)
  (loop
     for char = (read-char stream)
     unless (member char white-space-characters)
     return char))

(defun xml-to-list (xml)
  (if (probe-file xml)
      (with-open-file (stream (probe-file xml))
        (read-xml-declaration stream)
        (let ((char (read-non-space-char stream)))
          (if (eq char #\<)
              (read-element stream)
              (error "Wrong format1."))))
      (with-open-stream (stream (make-string-input-stream xml))
        (read-xml-declaration stream)
        (let ((char (read-non-space-char stream)))
          (if (eq char #\<)
              (read-element stream)
              (error "Wrong format2."))))))

(defun read-xml-declaration (stream)
  (if (and (eq (read-non-space-char stream) #\<)
           (eq (read-char stream) #\?)
           (eq (read-char stream) #\x)
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

(defun read-element (stream &optional first-char)
  ;; The stream starts after the first char - #\<
  (let ((tag-name (coerce        
                   (loop for char = (if first-char
                                        (prog1 first-char
                                          (setf first-char nil))
                                        (read-char stream))
                      until (or (member char white-space-characters)
                                (eq char #\/)
                                (eq char #\>))
                      collect char
                      finally (setf first-char char))
                   'string)))
    (if (member first-char white-space-characters)
        (setf first-char (read-non-space-char stream)))
    (let ((attributes
           (loop for char = (if first-char
                                (prog1 first-char
                                  (setf first-char nil))
                                (read-non-space-char stream))
              until
                (or (eq char #\/)
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
              do
                (let ((char (read-char stream)))
                  (cond ((or (eq char #\/)
                             (eq char #\>))
                         (setf first-char char))
                        ((not (member char white-space-characters))
                         (error "Wrong attribute ending."))))                   
              finally (setf first-char char))))
      (cond ((eq first-char #\/)
             (if (eq (read-char stream) #\>)
                 (cons (intern tag-name :keyword) attributes)
                 (error "Wrong format3.")))
            ((eq first-char #\>)
             (setf first-char (read-non-space-char stream))
             (loop with content
                for char = (if first-char
                               (prog1 first-char
                                 (setf first-char nil))
                               (read-non-space-char stream))
                if (eq char #\<)
                do
                  (let ((char (read-char stream)))
                    (cond ((eq char #\/)
                           (loop for char across tag-name
                              unless (eq char (read-char stream))
                              return (error "Wrong close tag. tag-name: ~S" tag-name))
                           (unless (eq (read-char stream) #\>)
                             (error "Wrong close tag.2"))
                           (return
                             (append (list (intern tag-name :keyword)) attributes content)))
                          (t
                           (setf content
                                 (append content
                                         (list (read-element stream char)))))))
                else do
                  (setf content
                        (append content
                                (list
                                 (string-trim white-space-characters
                                              (coerce
                                               (cons char
                                                     (loop for char = (read-char stream)
                                                        until (eq char #\<)
                                                        collect char))
                                               'string)))))
                  (setf first-char #\<)))))))

(defun get-element-content (element)
  (subseq element (+ (or (position-if #'symbolp (cdr element) :from-end t) -2) 3)))
