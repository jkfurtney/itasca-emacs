
;; This file is not needed for the Itasca modes,
;; it writes out keywords for use in auto completion
;; which should already be included in the ac-dict/ directory

(require 'itasca)

(defun write-ac-dict-file (filename word-list)
  (with-temp-file filename
    (dolist (word word-list)
      (insert
       (format "%s\n" word)))))

(defun itasca-write-ac-defs ()
  (write-ac-dict-file "ac-dict/itasca-general-mode"
                      itasca-general-function-list)
  (write-ac-dict-file "ac-dict/itasca-pfc-mode"
                      itasca-pfc-function-list)
  ;; (write-ac-dict-file "ac-dict/itasca-flac-mode"
  ;;                itasca-flac-function-list)
  (write-ac-dict-file "ac-dict/itasca-flac3d-mode"
                      itasca-flac3d-function-list)
  (write-ac-dict-file "ac-dict/itasca-pfc5-mode"
                      itasca--pfc5-function-list)
  (write-ac-dict-file "ac-dict/itasca-3dec-mode"
                      itasca-3dec-function-list)
  (write-ac-dict-file "ac-dict/itasca-udec-mode"
                      itasca-udec-function-list))

(itasca-write-ac-defs)
