(defsystem "my-compress"
  :version "0.1.0"
  :author "Masaaki Horie"
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "my-compress/tests"))))

(defsystem "my-compress/tests"
  :author "Masaaki Horie"
  :license ""
  :depends-on ("my-compress"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for my-compress"
  :perform (test-op (op c) (symbol-call :rove :run c)))
