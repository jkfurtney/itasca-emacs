
;; write out keywords for use in auto completion

(require 'itasca)

(defun write-ac-dict-file (filename word-list)
  (with-temp-file filename
    (dolist (word word-list)
      (insert
       (format "%s\n" word)))))

(write-ac-dict-file "ac-dict/itasca-udec-mode"
		    itasca-udec-functions)

(write-ac-dict-file "ac-dict/itasca-pfc-mode"
		    itasca-pfc-functions)
