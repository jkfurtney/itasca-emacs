;; Emacs modes for editing Itasca software data files.
;; ---------------------------------------------------
;; FLAC FLAC3D UDEC 3DEC PFC www.itascacg.com/software
;;
;; Installation: put this code into your .emacs file
;;
;; to set the mode on a per-file basis
;; -*- mode: itasca-general -*-
;; -*- mode: itasca-flac -*-
;; -*- mode: itasca-pfc -*-
;;
;; to do:
;; general table functions are missing
;; FLAC3D mode
;; functions to automatically format and indent FISH code
;; redo  FLAC mode
;; case insensitivity for highlighting
;; optional _ in some keywords


(setq kw-up "def define loop command if case_of section")

(setq kw-down "end end_loop end_command end_if end_case
end_section")

(setq kw-down-up "case else")

(setq kw-fish "array local global argument while null then")

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

(setq io-functions "write read close open sopen swrite sclose xmlparse")

(setq general-functions
      (regexp-opt (mapcan #'split-string (list vector-functions string-functions general-functions io-functions )) 'words))

(setq kw (mapcan #'split-string (list kw-up kw-down kw-down-up kw-fish)))

(require 'generic-x)

(define-generic-mode  'itasca-general-mode
  '(";")
  kw
  (list (cons general-functions 'font-lock-type-face)
        (cons "[-+]?[0-9]*\\.?[0-9]+\\([eE][-+]?[0-9]+\\)?"
              'font-lock-variable-name-face))
  '("\\.dat$" "\\.fis$")
  nil
  "Mode for Itasca data files (not code specific)")

;; pfc 4.0 specific

(setq pfc-fish-functions
"ball_head wall_head contact_head circ_head max_bid max_wid max_cid
cycle step ccount gravx gravy gravz grav time tdel given_tdel
safety_fac local_damp maddr e_kinetic e_friction e_strain e_bond
e_body e_bound pre_cycle av_unbal max_unbal clump_head max_clid
find_ball find_wall find_meas ball_near2 ball_near3 par_part av_cforce
max_cforce inrange th_dtmax th_flag th_mflag th_safefac th_tdel
th_time th_applydeltemp

multi_installBrick multi_totBrick multi_totProc multi_thisProc
multi_rootProc multi_numProc multi_BrickData multi_BrickType
multi_FBrickData multi_putCodeInExtra multi_numNd multi_nd_vfix
multi_nd_vfob multi_nd_vpos multi_nd_vvel multi_type find_clump
clump_near2 clump_near3 per_flag per_extent per_srate

cell_xnum cell_ynum cell_znum cell_size cell_xlow cell_ylow cell_zlow
cylinder_intersect overlap f_tdel

b_next b_clist b_ctype b_xfix b_yfix b_zfix b_vfix b_rxfix b_ryfix
b_rzfix b_rfix b_id b_x b_y b_z b_vpos b_ux b_uy b_uz b_vu b_xvel
b_yvel b_zvel b_vvel b_rxvel b_ryvel b_rzvel b_rvel b_xfob b_yfob
b_zfob b_vfob b_xfap b_yfap b_zfap b_vfap b_xmom b_ymom b_zmom b_mom
b_rad b_mass b_realmass b_moi b_dens b_kn b_ks b_shearmod b_poiss
b_fric b_ex del_ball b_color b_xmap b_ymap b_zmap b_map b_shared
b_type b_rot b_damp b_realmoi b_clump b_cllist b_extra b_stress
b_vrvel b_vmom b_vmap b_vrfix b_xdisp b_ydisp b_zdisp b_vdisp b_delete

b_thexp b_thfix b_thpob b_thpsrc b_thsheat b_thtemp b_thdeltemp
b_perflag b_perBall b_xffap b_yffap b_zffap b_vffap b_multi_type
b_realmassset b_realmoiset

c_next c_ball1 c_ball2 c_b1clist c_b2clist c_gobj1 c_gobj2 c_go1clist
c_go2clist c_wseg c_type c_bflag c_broken c_x c_y c_z c_vpos c_nforce
c_xsforce c_ysforce c_zsforce c_sforce c_kn c_ks c_hn c_hs c_fric
c_nstrength c_sstrength c_ex c_pb c_xun c_yun c_zun c_vun c_jset
c_slipwork c_inhibit c_prop c_model c_extra c_vsforce c_dampn c_damps
c_thactive c_thlen c_thpipe c_thpow c_thres c_active c_installpb
c_ondisk c_ondisk c_nvforce c_xsvforce c_ysvforce c_zsvforce c_svforce
c_vsvforce c_knset c_ksset c_fricset c_dampnt c_ontri c_mom c_tmom
c_vbmom

measure m_coord m_poros m_sfrac m_s11 m_s12 m_s21 m_s22 m_s13 m_s31
m_s23 m_s32 m_s33 m_ed11 m_ed12 m_ed21 m_ed22 m_ed13 m_ed31 m_ed23
m_ed32 m_ed33 m_x m_y m_z m_vpos m_rad m_id m_next m_tc11 m_tc12
m_tc21 m_tc22 m_tc13 m_tc31 m_tc23 m_tc32 m_tc33

pb_rad pb_kn pb_ks pb_nstrength pb_sstrength pb_nstress pb_sstress
pb_nforce pb_xsforce pb_ysforce pb_zsforce pb_sforce pb_tmom pb_xbmom
pb_ybmom pb_zbmom pb_mom pb_vsforce pb_vmom pb_mfac pb_coh pb_fa

cl_add cl_extra cl_id cl_list cl_next cl_rel cl_scale cl_color cl_damp
cl_mass cl_realmass cl_moi cl_vmoi cl_vol cl_volgiven cl_vfix cl_xfix
cl_yfix cl_zfix cl_rfix cl_vrfix cl_rxfix cl_ryfix cl_rzfix cl_vfap
cl_xfap cl_yfap cl_zfap cl_map cl_vmap cl_xmap cl_ymap cl_zmap cl_vpos
cl_x cl_y cl_z cl_vvel cl_xvel cl_yvel cl_zvel cl_rvel cl_vrvel
cl_rxvel cl_ryvel cl_rzvel cl_vfob cl_xfob cl_yfob cl_zfob cl_mom
cl_vmom cl_xmom cl_ymom cl_zmom cl_stress cl_vdisp cl_xdisp cl_ydisp
cl_zdisp

ws_next ws_prev ws_x ws_y ws_xvel ws_yvel ws_xun ws_yun ws_length
ws_ux ws_uy

wf_next wf_id wf_xun wf_yun wf_zun wf_vnum wf_vpos

w_next w_clist w_id w_x w_y w_z w_pos w_ux w_uy w_uz w_vu w_xvel
w_yvel w_zvel w_vvel w_xfob w_yfob w_zfob w_vfob w_kn w_ks w_fric w_ex
del_wall w_color w_flist w_wlist w_extra w_fix w_delete w_rxvel
w_ryvel w_rzvel w_rvel w_vrvel w_xmom w_ymom w_zmom w_mom w_vmom
w_type w_radvel w_radfob w_radend1 w_radend2 w_posend1 w_posend2 w_rad")


(setq pfc-functions
      (regexp-opt (mapcan #'split-string (list vector-functions string-functions general-functions io-functions pfc-fish-functions )) 'words))

(define-generic-mode  'itasca-pfc-mode
  '(";")
  kw
  (list (cons pfc-functions 'font-lock-type-face)
        (cons "[-+]?[0-9]*\\.?[0-9]+\\([eE][-+]?[0-9]+\\)?"
              'font-lock-variable-name-face))
  '("\\.p3dat$")
  nil
  "Mode for Itasca PFC 4.0  data files (not code specific)")

;; FLAC 7.0 specific

(setq flac-fish-functions "\\<\\(a\\(?:bs\\|cos\\|n\\(?:d\\|gle\\|isotropic\\)\\|pp\\(?:_pnt\\|gw_pnt\\|ly\\|th_pnt\\)\\|r\\(?:ea\\|ray\\)\\|s\\(?:in\\|pect\\|x[xy]\\|yy\\|zz\\)\\|t\\(?:an2?\\|t\\(?:_pnt\\|ach\\)\\)\\|[34]\\)\\|b\\(?:a\\(?:ck\\|ud\\)\\|icoe\\|s\\(?:x[xy]\\|yy\\|zz\\)\\)\\|c\\(?:a\\(?:ll\\|se\\(?:_?of\\)?\\)\\|f_\\(?:axi\\|creep\\|dyn\\|ext\\|gw\\|ps\\|therm\\)\\|ga\\|har\\|lo\\(?:ck\\|se\\)\\|m_max\\|o\\(?:lumns\\|mmand\\|n\\(?:fig\\|stitutive\\(?:_?model\\)\\)\\|s\\)\\|parse\\|r\\(?:dt\\|eep\\|t\\(?:del\\|ime\\)\\)\\|s\\(?:c\\|x[xy]\\|yy\\|zz\\)\\|ycle\\)\\|d\\(?:a\\(?:mp\\(?:ing\\)?\\|tum\\)\\|e\\(?:fine\\|grad\\|nsity\\)\\|o_update\\|s\\(?:x[xy]\\|yy\\|zz\\)\\|ump\\|y\\(?:_state\\|dt\\(?:_gp[ij]\\)?\\|namic\\|t\\(?:del\\|ime\\)\\)\\|[ty]\\)\\|e\\(?:_p\\|cho\\|ga\\|l\\(?:astic\\|se\\)\\|nd\\(?:_\\(?:c\\(?:ase\\|ommand\\)\\|if\\|loop\\|section\\)\\|c\\(?:ase\\|ommand\\)\\|if\\|loop\\|section\\)?\\|rror\\|v_\\(?:p\\|tot\\)\\|x\\(?:it\\|p\\)\\)\\|f\\(?:2mod\\|_prop\\|c_arg\\|i\\(?:lcolor\\|sh\\(?:_msg\\|call\\)?\\|x\\)\\|l\\(?:ags\\|o\\(?:at\\|w\\)\\|prop\\)\\|m\\(?:em\\|od\\)\\|o\\(?:b[lu]\\|rce\\|s\\(?:_f\\)?\\)\\|r\\(?:ee\\|iend\\)\\|s\\(?:tring\\|[ir]\\)\\|tens\\)\\|g\\(?:2flow\\|e\\(?:n\\|t_mem\\)\\|flow\\|msmul\\|p\\(?:_copy\\|p\\)\\|r\\(?:\\(?:an\\|i\\)d\\)\\|w\\(?:dt\\|t\\(?:del\\|ime\\)\\)\\)\\|h\\(?:b[ms]\\|elp\\|is\\(?:file\\)?\\)\\|i\\(?:e\\(?:b\\(?:_pnt\\)?\\|rr\\)\\|face\\|gp\\|m\\(?:em\\|plicit\\)\\|n\\(?:formation\\|i\\(?:\\(?:mode\\|tia\\)l\\)\\|t\\(?:_pnt\\|erface\\)?\\)\\|tasca\\|zones\\|[fn]\\)\\|j\\(?:err\\|gp\\|zones\\)\\|l\\(?:arge\\|egend\\|ff_pnt\\|i\\(?:mits\\|st\\)\\|mul\\|n\\|o\\(?:g\\|op\\|se_mem\\)\\)\\|m\\(?:a\\(?:rk\\|t_\\(?:\\(?:inver\\|transpo\\)se\\)\\|x\\(?:dt\\)?\\)\\|e\\(?:chanical\\|m\\(?:ory\\)?\\|ssage\\)\\|in\\(?:dt\\)?\\|o\\(?:del?\\|hr-coulomb\\|\\(?:nchrom\\|vi\\)e\\)\\)\\|n\\(?:c\\(?:ontours\\|write\\)\\|e\\(?:rr\\(?:_fish\\)?\\|w\\)\\|grwater\\|mechanical\\|ot\\|step\\|thermal\\|ull\\|wgpp\\)\\|o\\(?:pen\\|r\\|ut\\)\\|p\\(?:_stress\\|a\\(?:c\\|\\(?:lett\\|[ru]s\\)e\\)\\|fast\\|l\\(?:ot\\|t\\(?:angle\\|\\(?:cohes\\|frict\\|tens\\)ion\\)\\)\\|o\\(?:ro2\\|wer\\)\\|r\\(?:e\\(?:_?parse\\)\\|int\\|op\\)\\|slow\\|[ip]\\)\\|quit\\|r\\(?:_integrate\\|a\\(?:nge\\|yleigh\\)\\|e\\(?:ad\\|s\\(?:et\\|tore\\)\\|turn\\|z_exe\\)\\|\\(?:ff_pn\\|sa\\)t\\)\\|s\\(?:_\\(?:3dd\\|dyn\\|echo\\|flow\\|imp\\|log\\|m\\(?:e\\(?:ch\\|ss\\)\\|ovie\\)\\|therm\\)\\|a\\(?:t\\|ve\\)\\|cl\\(?:in\\|ose\\)\\|e\\(?:ction\\|t\\)\\|gn\\|i\\(?:g[12]\\|n\\)\\|m\\(?:_max\\|all\\)\\|o\\(?:lve\\|pen\\)\\|qrt\\|read\\|s\\(?:[ir]3d\\|[ir]\\)?\\|t\\(?:ate\\|ep\\|op\\|r\\(?:_pnt\\|ing\\|ucture\\)\\)\\|write\\|x[xy]\\|y[sy]\\|zz\\)\\|t\\(?:a\\(?:b\\(?:_pnt\\|le\\(?:_size\\)?\\)\\|n\\)\\|e\\(?:mperature\\|n\\(?:flg\\|sion\\)\\)\\|flow\\|h\\(?:dt\\|e\\(?:n\\|rmal\\|ta\\)\\|t\\(?:del\\|ime\\)\\)\\|itle\\|olerance\\|rac\\(?:_pnt\\|k\\)\\|ype\\)\\|u\\(?:biquitous\\|cs\\|d\\(?:coe\\|m_pnt\\)\\|mul\\|n\\(?:b\\(?:al\\|flow\\)\\|mark\\)\\|rand\\)\\|v\\(?:_n\\(?:gw\\|mech\\|therm\\)\\|ector\\|g\\(?:a\\|p\\(?:0\\|c\\(?:n?w\\)\\)\\)\\|is\\(?:cous\\|rat\\)\\|ol_strain\\|s\\(?:x[xz]\\|yy\\|zz\\|[ir]\\)?\\)\\|w\\(?:ater\\|b\\(?:iot\\|ulk\\)\\|dens\\|hile\\(?:_?stepping\\)?\\|i\\(?:ndow\\|pp\\)\\|k\\(?:1[12]\\|22\\)\\|rite\\)\\|x\\(?:acc\\|body\\|disp\\|f\\(?:low\\|or\\(?:ce\\|m\\)\\)\\|grav\\|nwflow\\|reaction\\|table\\|vel\\|ywrite\\)\\|y\\(?:acc\\|body\\|disp\\|f\\(?:low\\|orce\\)\\|grav\\|nwflow\\|reaction\\|table\\|vel\\)\\|z\\(?:_\\(?:copy\\|group\\|hyst\\|model\\|prop\\)\\|art\\|d\\(?:e\\(?:1[12]\\|22\\|33\\)\\|pp\\|rot\\)\\|msmul\\|poros\\|s\\(?:1[12]\\|22\\|33\\|ub\\)\\|t\\(?:e[a-d]\\|s[a-d]\\)\\|visc\\|xbar\\)\\|[rxy]\\)\\>")

(define-generic-mode  'itasca-flac-mode
  '(";")
  kw
  (list
    (cons flac-fish-functions  'font-lock-type-face)
    (cons "[-+]?[0-9]*\\.?[0-9]+\\([eE][-+]?[0-9]+\\)?"
          'font-lock-variable-name-face))
  '("\\.fdat$")
   nil
  "A mode for Itasca FLAC data files")
