(in-package :cl-user)

(asdf:defsystem cl-xml-parser
  :author "Wise Simpson"
  :version "0.1"
  :depends-on (:cl-ppcre)
  :components ((:module "src"
                        :components
                        ((:file "parser"))))
  :description "Tool for parsing xml, like converting xml file to list or other way around.")
