(setq kw-up "def loop command if case_of section")

(setq kw-down "end end_loop end_command end_if end_case
end_section")

(setq kw-down-up "case else")

(setq kw-fish "array local global argument while null then")

(setq kw (mapcan #'split-string (list kw-up kw-down kw-down-up kw-fish)))

; general functions

(setq vector-functions "vector xcomp ycomp zcomp cross dot unit")

(setq string-functions "buildstr char float input int output parse
pre_parse string strlen substr")

(setq math-functions "abs acos and asin atan atan2 cos degrad exp
grand ln log lshift mag mag2 max min not or pi round rshift sgn
sin sqrt tan urand")

(setq general-functions "array_dim array_size buildstr char clock
code_majorversion code_minorversion code_name ddfromnorm
dipfromnorm environment error fc_arg find_range fish_majorversion
fish_minorversion float from_principal get_array in in_range
inbox index index_type input int lose_array msgbox normfromdip
normfromdipdd notify null out pointer_type principal_stress
realtime sleep string strlen substr tolower toupper type")

(setq functions
      (regexp-opt (mapcan #'split-string (list vector-functions string-functions general-functions)) 'words))


(require 'generic-x) ;; we need this
(define-generic-mode
    'itasca-fish-mode
  '(";")
  kw
  ;(list (regexp-opt kw 'word))
  (list (cons functions font-lock-function-name-face)
        (cons "[-+]?[0-9]*\\.?[0-9]+\\([eE][-+]?[0-9]+\\)?"
              font-lock-variable-name-face))
;        (cons "\\(\\+\\|-\\)?[0-9]+\\(\\.[0-9]+\\)?"
;              font-lock-variable-name-face))
  '("\\.dat$" "\\.fis$")  ;; files for which to activate this mode
  nil
  "Mode for Itasca data files")
