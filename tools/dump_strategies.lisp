(require :asdf)
(load "swimmy.asd")
(asdf:load-system :swimmy)

(in-package :swimmy.school)

(defun dump-all-strategies ()
  (let ((all-strats (append *strategy-knowledge-base* *evolved-strategies*)))
    (format t "~%[DUMP] Found ~d strategies.~%" (length all-strats))
    (let ((json-list 
            (mapcar (lambda (s) 
                      (strategy-to-json s))
                    all-strats)))
      (with-open-file (stream "strategies.json" 
                              :direction :output 
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (write-string (jsown:to-json json-list) stream))
      (format t "[DUMP] Successfully wrote strategies.json~%"))))

(dump-all-strategies)
(sb-ext:exit)
