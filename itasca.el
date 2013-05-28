;; Emacs modes for editing Itasca software data files.
;; ---------------------------------------------------
;; FLAC FLAC3D UDEC 3DEC PFC www.itascacg.com/software
;;
;; see Readme.txt for more information
;;
;; to do:
;; fix global namespace pollution
;; redo FLAC mode
;; case insensitivity for highlighting
;; 3DEC mode

(require 'generic-x)

(defconst itasca-mode-keywords "def define loop command if
case_of caseof section end end_loop endloop end_command
endcommand end_if endif end_case endcase end_section endsection
case else array local global argument while null then
while_stepping whilestepping exit")

(defconst itasca-general-functions "array_dim array_size
buildstr char clock code_majorversion code_minorversion code_name
ddfromnorm dipfromnorm environment error fc_arg find_range
fish_majorversion fish_minorversion float from_principal
get_array in in_range inbox index index_type input int lose_array
msgbox normfromdip normfromdipdd notify null out pointer_type
principal_stress realtime sleep string strlen substr tolower
toupper type

vector xcomp ycomp zcomp cross dot unit buildstr char float input
int output parse pre_parse string strlen substr abs acos and asin
atan atan2 cos degrad exp grand ln log lshift mag mag2 max min
not or pi round rshift sgn sin sqrt tan urand

write read close open sopen swrite sclose
xmlparse get_socket lose_socket sread

del_table get_table table table_id
table_name table_size vtable xtable ytable

get_mem lose_mem mem")

;; new in FLAC3D 5.0
(defconst itasca-new-framework-functions "uds_list uds_head
uds_next uds_find uds_near uds_create uds_id uds_group
uds_isgroup uds_removegroup uds_extra uds_pos uds_remove
uds_value udv_list udv_head udv_next udv_find udv_near udv_create
udv_id udv_group udv_isgroup udv_removegroup udv_extra udv_pos
udv_value udv_mag udv_dip udv_dd udv_dir udv_remove udt_list
udt_head udt_next udt_find udt_near udt_create udt_id udt_group
udt_isgroup udt_removegroup udt_extra udt_pos udt_value udt_prin
udt_setdir udt_remove

label_arrow label_create label_delete
label_end label_find label_head label_next label_pos label_text

mail_addattachment mail_addrecipient mail_clear
mail_deleterecipient mail_deleteattachment mail_send
mail_setaccount mail_setbody mail_setdomain mail_sethost
mail_setpassword mail_setsubject

gset_find gset_create gset_list gset_remove gset_id gset_name
gn_find gn_near gn_create gn_list gn_remove gn_id gn_group yes
gn_isgroup gn_groupnum gn_groupex gn_extra yes gn_startedge
gn_startindex gn_pos yes ge_find ge_near ge_create ge_list
ge_remove ge_id ge_group yes ge_isgroup ge_groupnum ge_groupex
ge_extra yes ge_node ge_nextedge ge_nextindex ge_startpoly
ge_startindex ge_dir ge_pos yes ge_cen gpol_find gpol_near
gpol_create gpol_addedge gpol_addnode gpol_close gpol_check
gpol_list gpol_remove gpol_id gpol_group yes gpol_isgroup
gpol_extra yes gpol_size gpol_edge gpol_node gpol_normal
gpol_nextpoly gpol_nextindex gpol_cen gpol_area

dfn_list dfn_find dfn_typeid dfn_typename dfn_p10 dfn_p10geom
dfn_p20 dfn_p21 dfn_avetrace dfn_number dfn_delete dfn_add dfn_id
dfn_flist dfn_ilist dfn_finbox dfn_carray dfn_dcenter dfn_density
dfn_percolation

dfrac_find dfrac_near dfrac_typeid dfrac_typename dfrac_remove
dfrac_disk dfrac_prop dfrac_carray dfrac_iarray dfrac_varray
dfrac_radius dfrac_pos dfrac_dip dfrac_dipd dfrac_normal
dfrac_area dfrac_extra dfrac_group dfrac_number dfrac_id
dfrac_pnear dfrac_dfn dfrac_isgroup dfrac_removegroup

dvert_find dvert_typeid dvert_typename dvert_id dvert_pos

di_find di_typeid di_typename di_end1 di_end2 di_pos1 di_pos2

dtp_list dtp_find dtp_get dtp_id dtp_name dtp_stype dtp_snbp
dtp_sparam dtp_smin dtp_smax dtp_otype dtp_onbp dtp_oparam
dtp_dmin dtp_dmax dtp_ddmin dtp_ddmax dtp_ptype dtp_pnbp
dtp_pparam dtp_pmin dtp_pmax")

(defconst itasca-pfc-functions "ccfd_nele ccfd_nnode
ccfd_elenode ccfd_xnode ccfd_ynode ccfd_znode ccfd_por ccfd_xvel
ccfd_yvel ccfd_zvel ccfd_xdrag ccfd_ydrag ccfd_zdrag ccfd_t_s
ccfd_fite ccfd_xgradp ccfd_ygradp ccfd_zgradp ccfd_elevol
ccfd_xelecent ccfd_yelecent ccfd_zelecent ccfd_xyzele
ccfd_ballele ccfd_fint ccfd_xballff ccfd_yballff ccfd_zballff
ccfd_elemu ccfd_elerho ccfd_elepress

fc_x fc_y fc_z fc_xvel fc_yvel fc_zvel fc_pre fc_por fc_xfap fc_yfap
fc_zfap fc_bxvel fc_byvel fc_bzvel fc_temp

ball_head wall_head contact_head circ_head max_bid max_wid max_cid
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

;; FLAC 7.0 specific
;; fix this
(defconst itasca-flac-function-regexp "\\<\\(a\\(?:bs\\|cos\\|n\\(?:d\\|gle\\|isotropic\\)\\|pp\\(?:_pnt\\|gw_pnt\\|ly\\|th_pnt\\)\\|r\\(?:ea\\|ray\\)\\|s\\(?:in\\|pect\\|x[xy]\\|yy\\|zz\\)\\|t\\(?:an2?\\|t\\(?:_pnt\\|ach\\)\\)\\|[34]\\)\\|b\\(?:a\\(?:ck\\|ud\\)\\|icoe\\|s\\(?:x[xy]\\|yy\\|zz\\)\\)\\|c\\(?:a\\(?:ll\\|se\\(?:_?of\\)?\\)\\|f_\\(?:axi\\|creep\\|dyn\\|ext\\|gw\\|ps\\|therm\\)\\|ga\\|har\\|lo\\(?:ck\\|se\\)\\|m_max\\|o\\(?:lumns\\|mmand\\|n\\(?:fig\\|stitutive\\(?:_?model\\)\\)\\|s\\)\\|parse\\|r\\(?:dt\\|eep\\|t\\(?:del\\|ime\\)\\)\\|s\\(?:c\\|x[xy]\\|yy\\|zz\\)\\|ycle\\)\\|d\\(?:a\\(?:mp\\(?:ing\\)?\\|tum\\)\\|e\\(?:fine\\|grad\\|nsity\\)\\|o_update\\|s\\(?:x[xy]\\|yy\\|zz\\)\\|ump\\|y\\(?:_state\\|dt\\(?:_gp[ij]\\)?\\|namic\\|t\\(?:del\\|ime\\)\\)\\|[ty]\\)\\|e\\(?:_p\\|cho\\|ga\\|l\\(?:astic\\|se\\)\\|nd\\(?:_\\(?:c\\(?:ase\\|ommand\\)\\|if\\|loop\\|section\\)\\|c\\(?:ase\\|ommand\\)\\|if\\|loop\\|section\\)?\\|rror\\|v_\\(?:p\\|tot\\)\\|x\\(?:it\\|p\\)\\)\\|f\\(?:2mod\\|_prop\\|c_arg\\|i\\(?:lcolor\\|sh\\(?:_msg\\|call\\)?\\|x\\)\\|l\\(?:ags\\|o\\(?:at\\|w\\)\\|prop\\)\\|m\\(?:em\\|od\\)\\|o\\(?:b[lu]\\|rce\\|s\\(?:_f\\)?\\)\\|r\\(?:ee\\|iend\\)\\|s\\(?:tring\\|[ir]\\)\\|tens\\)\\|g\\(?:2flow\\|e\\(?:n\\|t_mem\\)\\|flow\\|msmul\\|p\\(?:_copy\\|p\\)\\|r\\(?:\\(?:an\\|i\\)d\\)\\|w\\(?:dt\\|t\\(?:del\\|ime\\)\\)\\)\\|h\\(?:b[ms]\\|elp\\|is\\(?:file\\)?\\)\\|i\\(?:e\\(?:b\\(?:_pnt\\)?\\|rr\\)\\|face\\|gp\\|m\\(?:em\\|plicit\\)\\|n\\(?:formation\\|i\\(?:\\(?:mode\\|tia\\)l\\)\\|t\\(?:_pnt\\|erface\\)?\\)\\|tasca\\|zones\\|[fn]\\)\\|j\\(?:err\\|gp\\|zones\\)\\|l\\(?:arge\\|egend\\|ff_pnt\\|i\\(?:mits\\|st\\)\\|mul\\|n\\|o\\(?:g\\|op\\|se_mem\\)\\)\\|m\\(?:a\\(?:rk\\|t_\\(?:\\(?:inver\\|transpo\\)se\\)\\|x\\(?:dt\\)?\\)\\|e\\(?:chanical\\|m\\(?:ory\\)?\\|ssage\\)\\|in\\(?:dt\\)?\\|o\\(?:del?\\|hr-coulomb\\|\\(?:nchrom\\|vi\\)e\\)\\)\\|n\\(?:c\\(?:ontours\\|write\\)\\|e\\(?:rr\\(?:_fish\\)?\\|w\\)\\|grwater\\|mechanical\\|ot\\|step\\|thermal\\|ull\\|wgpp\\)\\|o\\(?:pen\\|r\\|ut\\)\\|p\\(?:_stress\\|a\\(?:c\\|\\(?:lett\\|[ru]s\\)e\\)\\|fast\\|l\\(?:ot\\|t\\(?:angle\\|\\(?:cohes\\|frict\\|tens\\)ion\\)\\)\\|o\\(?:ro2\\|wer\\)\\|r\\(?:e\\(?:_?parse\\)\\|int\\|op\\)\\|slow\\|[ip]\\)\\|quit\\|r\\(?:_integrate\\|a\\(?:nge\\|yleigh\\)\\|e\\(?:ad\\|s\\(?:et\\|tore\\)\\|turn\\|z_exe\\)\\|\\(?:ff_pn\\|sa\\)t\\)\\|s\\(?:_\\(?:3dd\\|dyn\\|echo\\|flow\\|imp\\|log\\|m\\(?:e\\(?:ch\\|ss\\)\\|ovie\\)\\|therm\\)\\|a\\(?:t\\|ve\\)\\|cl\\(?:in\\|ose\\)\\|e\\(?:ction\\|t\\)\\|gn\\|i\\(?:g[12]\\|n\\)\\|m\\(?:_max\\|all\\)\\|o\\(?:lve\\|pen\\)\\|qrt\\|read\\|s\\(?:[ir]3d\\|[ir]\\)?\\|t\\(?:ate\\|ep\\|op\\|r\\(?:_pnt\\|ing\\|ucture\\)\\)\\|write\\|x[xy]\\|y[sy]\\|zz\\)\\|t\\(?:a\\(?:b\\(?:_pnt\\|le\\(?:_size\\)?\\)\\|n\\)\\|e\\(?:mperature\\|n\\(?:flg\\|sion\\)\\)\\|flow\\|h\\(?:dt\\|e\\(?:n\\|rmal\\|ta\\)\\|t\\(?:del\\|ime\\)\\)\\|itle\\|olerance\\|rac\\(?:_pnt\\|k\\)\\|ype\\)\\|u\\(?:biquitous\\|cs\\|d\\(?:coe\\|m_pnt\\)\\|mul\\|n\\(?:b\\(?:al\\|flow\\)\\|mark\\)\\|rand\\)\\|v\\(?:_n\\(?:gw\\|mech\\|therm\\)\\|ector\\|g\\(?:a\\|p\\(?:0\\|c\\(?:n?w\\)\\)\\)\\|is\\(?:cous\\|rat\\)\\|ol_strain\\|s\\(?:x[xz]\\|yy\\|zz\\|[ir]\\)?\\)\\|w\\(?:ater\\|b\\(?:iot\\|ulk\\)\\|dens\\|hile\\(?:_?stepping\\)?\\|i\\(?:ndow\\|pp\\)\\|k\\(?:1[12]\\|22\\)\\|rite\\)\\|x\\(?:acc\\|body\\|disp\\|f\\(?:low\\|or\\(?:ce\\|m\\)\\)\\|grav\\|nwflow\\|reaction\\|table\\|vel\\|ywrite\\)\\|y\\(?:acc\\|body\\|disp\\|f\\(?:low\\|orce\\)\\|grav\\|nwflow\\|reaction\\|table\\|vel\\)\\|z\\(?:_\\(?:copy\\|group\\|hyst\\|model\\|prop\\)\\|art\\|d\\(?:e\\(?:1[12]\\|22\\|33\\)\\|pp\\|rot\\)\\|msmul\\|poros\\|s\\(?:1[12]\\|22\\|33\\|ub\\)\\|t\\(?:e[a-d]\\|s[a-d]\\)\\|visc\\|xbar\\)\\|[rxy]\\)\\>")

(defconst itasca-flac3d-functions "at_pos at_slave at_type
at_master at_masterzn at_zoneface at_delete at_weight at_qweight
at_snap at_create

exb_id exb_check exb_group exb_isgroup exb_groupnum
exb_removegroup exb_extra exb_size exb_block exb_edge exb_flip
exb_point exb_nextblock exb_nextindex exb_center exb_area
exb_multiplier exb_gridsizei exb_gridsizej exb_countzones
exb_gridpos exb_parampos

exe_id exe_group exe_isgroup exe_groupnum exe_removegroup
exe_extra exe_point exe_nextedge exe_nextindex exe_startblock
exe_startindex exe_direction exe_center exe_length exe_gridpos
exe_parampos exe_blocked exe_simple exe_curve exe_type
exe_ellipsecenter exe_ellipsewith exe_gridsize exe_gridratio
exe_gridratio_isolate exe_polysize exe_polypoint exe_polylocalu
exe_polylocalv exe_polyadd exe_polyremove

exp_id exp_group exp_isgroup exp_groupnum exp_removegroup
exp_extra exp_startedge exp_startindex exp_pos

exset_find exset_create exset_list exset_remove exset_id
exset_name exset_plist exset_psize exset_pfind exset_pcreate
exset_premove exset_pnear exset_elist exset_esize exset_efind
exset_ecreate exset_eremove exset_enear exset_blist exset_bsize
exset_bfind exset_bcreate exset_bremove exset_bnear exset_zauto
exset_ztargetedge exset_ztargetsize exset_ztargettotal
exset_zautodir exset_zcount2 exset_zcount3 exset_origin
exset_uaxis exset_vaxis exset_naxis exset_setuv exset_setun
exset_setvn exset_seg_count exset_seg_length exset_seg_group
exset_seg_isgroup exset_seg_groupnum exset_seg_removegroup
exset_seg_gridratio exset_seg_gridsize exset_node_add
exset_node_remove exset_node_pos exset_node_group
exset_node_isgroup exset_node_groupnum exset_node_removegroup
exset_msize exset_mkey exset_mvalue exset_mfindkey

dim crdt crtime do_update dydt dytime find_gp find_zone fldt
fltime fluid_ratio fos_f gen_ratio gp_head gp_near gp_near_live
gp_nearall large mech_ratio ngp nzone step thdt therm_ratio
thtime timestep unbal xgrav ygrav z_containing z_near z_near_live
z_nearall zgrav zone_head grav processors aunbc munbc strnorm
z_list gp_list

gp_biotmod gp_copy gp_dynmul gp_finvol gp_fload gp_flow gp_fmod
gp_ftens gp_locsmall gp_mass gp_next gp_pp gp_ppfix gp_psource
gp_sat gp_staterest gp_statesave gp_temp gp_tfix gp_xaccel
gp_xdisp gp_xfapp gp_xfix gp_xfunbal gp_xload gp_xpos gp_xvel
gp_yaccel gp_ydisp gp_yfapp gp_yfix gp_yfunbal gp_yload gp_ypos
gp_yvel gp_zaccel gp_zdisp gp_zfapp gp_zfix gp_zfunbal gp_zload
gp_zpos gp_zvel gp_massadd gp_gravmass gp_obv gp_cfmod gp_accel
gp_disp gp_fapp gp_fix gp_funbal gp_load gp_pos gp_vel
gp_smalldisp gp_linkzone gp_linkindex gp_groupnum gp_stiff

i_find i_head i_next i_id i_node_head i_elem_head ie_id ie_vert
ie_join ie_area ie_norm ie_zhost ie_fhost ie_next in_id in_area
in_nstr in_sstr in_nstr_add in_sdisp in_ctol in_pen in_zhost
in_fhost in_hweight in_ztarget in_ftarget in_tweight in_pos
in_disp in_vel in_prop in_next in_extra ie_extra i_list
i_elem_list i_node_list

sb_dist sb_length sb_volume sb_emod sb_nu sb_pmom sb_xcarea
sb_xciy sb_xciz sb_xcj sb_ydir sb_force sb_mom sb_nforce sb_vydir
sb_vforce sb_vnforce sb_vdist sb_vmom

sc_length sc_volume sc_emod sc_xcarea sc_ycomp sc_yten sc_grcoh
sc_grfric sc_grk sc_grper sc_slide sc_slidetol sc_force sc_nforce
sc_stress sc_yield sc_grconf sc_grdisp sc_grslip sc_grstr
sc_grstrdir sc_vnforce sc_vgrstrdir

s_cid s_conn s_delete s_mark s_next s_node s_numnd s_type s_dens
s_lsys s_pos s_thexp s_vlsys s_vpos

sg_area sg_csscoh sg_cssfric sg_cssk sg_etype sg_iso sg_mprop
sg_nforce sg_ortho sg_press sg_rconf sg_rdisp sg_rstr sg_rstrdir
sg_ryield sg_slide sg_slidetol sg_thick sg_volume sg_vnforce
sg_beta sg_anis sg_vrstrdir

sl_area sl_csncut sl_csnk sl_csscoh sl_csscohres sl_cssfric
sl_cssk sl_etype sl_iso sl_mprop sl_nforce sl_ortho sl_press
sl_rdisp sl_rstr sl_rstrdir sl_ryield sl_slide sl_slidetol
sl_thick sl_volume sl_csncut2 sl_csnk2 sl_csscoh2 sl_csscohres2
sl_cssfric2 sl_cssk2 sl_vnforce sl_beta sl_anis sl_embedded
sl_vrstrdir

lk_delete lk_next lk_node lk_side lk_target lk_type lk_usedby
lk_attach lk_slide lk_slidetol lk_ldarea lk_ldk lk_ndarea
lk_ndgap lk_ndk lk_ndycomp lk_ndyten lk_ldrdisp lk_ldrfor
lk_ndrdisp lk_ndrfor lk_ndrgap lk_ndryield

nd_link nd_link2 nd_mark nd_next nd_apply nd_applysys nd_fix
nd_ldamp nd_lfix nd_lsys nd_mass nd_pos nd_stiff nd_rdisp nd_rfob
nd_rvel nd_vapply nd_vlsys nd_vmass nd_vpos nd_vstiff nd_vrdisp
nd_vrfob nd_vrvel nd_tempinc

sp_dist sp_length sp_volume sp_emod sp_nu sp_pmom sp_xcarea
sp_xciy sp_xciz sp_xcj sp_ydir sp_csncoh sp_csnfric sp_csngap
sp_csnk sp_csscoh sp_cssfric sp_cssk sp_per sp_slide sp_slidetol
sp_cscfinc sp_cscftab sp_cssctab sp_cssftab sp_rock sp_tfstr
sp_tyield sp_force sp_mom sp_nforce sp_rconf sp_rdisp sp_rgap
sp_rstr sp_rstrdir sp_ryield sp_vdist sp_vydir sp_vforce sp_vmom
sp_vnforce sp_vrstrdir

nd_ssys nd_ssysx nd_svalid sst_depfac sst_pstr sst_sres
sst_sresvalid sst_str sst_strvalid nd_vssys sst_vpstr

ss_area ss_etype ss_iso ss_mprop ss_nforce ss_ortho ss_press
ss_thick ss_volume ss_vnforce ss_beta ss_anis

zfd_dataname zfd_dataindex zfd_methodname zfd_methodindex
zfd_extra zfd_effective zfd_property zfd_radratio zfd_power
zfd_tolerance zfd_initialize zfd_getdata zfd_getgpdata zfd_reset
zfd_hidemechnull zfd_hidefluidnull zfd_hidethermnull

z_id gp_id

z_aspect z_code z_copy z_density z_dynmul z_facegp z_facesize
z_flx z_fly z_flz z_fri z_frr z_fsi z_fsr z_geomtest
z_gettetstress z_gp z_hyst z_inimodel z_islive z_join z_model
z_next z_nsmalltetvol z_numgp z_numoverlays z_numtets z_ortho
z_planarity z_pp z_prop z_pstress z_puttetstress z_qx z_qy z_qz
z_sig1 z_sig2 z_sig3 z_sonplane z_ssi z_ssr z_state z_staterest
z_statesave z_sxx z_sxy z_sxz z_syy z_syz z_szz z_temp z_tetgps
z_vol_deformed z_volume z_vsi z_vsr z_xcen z_ycen z_zcen z_wpvol
z_wpshear z_wptot z_wevol z_weshear z_wetot z_nummechprops
z_mechpropname z_flmodel z_flprop z_thmodel z_thprop z_fl z_q
z_prin z_cen z_facenorm z_xfacenorm z_yfacenorm z_zfacenorm
z_findface z_linkzone z_linkindex z_apply z_iecreate
z_qualitytest z_facegroup z_faceextra z_faceremovegroup
z_faceingroup")

(defconst itasca-udec-functions "tdel step time xgrav ygrav
grav_x grav_y block_head contact_head domain_head m_jkn m_jks
b_next b_x b_y b_xvel b_yvel str_node_head str_elem_head
cable_node_head cable_elem_head cycle unbal m_jfriction
m_jcohesion m_jtension m_jdilation m_density m_bulk m_shear
m_friction m_cohesion m_tension m_dilation b_type b_corner b_mat
b_cons b_fix b_rvel b_area b_mass b_moi b_dsf b_xforce b_yforce
b_mom b_xload b_yload b_bex b_gp b_zone gp_next gp_corner gp_x
gp_y gp_xvel gp_yvel gp_xforce gp_yforce gp_mass gp_dsf gp_xdis
gp_ydis z_next z_gp z_x z_y z_sxx z_sxy z_syy z_szz z_mass z_rot
z_state z_zex c_next c_type c_x c_y c_mat c_cons c_b1 c_b2
c_link1 c_link2 c_d1 c_d2 c_sdis c_ndis c_sforce c_nforce
c_length c_nx c_ny c_obj_type d_obj_type d_next d_vol d_pp
d_contact d_fix d_x d_y cor_obj_type cor_link cor_rlink cor_block
cor_x cor_y cor_xvel cor_yvel cor_gp cor_bou imem fmem
outer_domain fluid_densityfluid_bulk fracb fracz sup_head r_head
bou_head gp_near z_near b_near cor_near d_near c_near z_block
z_mat z_bulk z_shear gp_bou bou_gp table_head ftime thtime thdt
str_int_head c_jex z_pp cor_xdis cor_ydis r_prop r_astiff
r_sstiff r_length r_uaxial r_ushear r_rfac r_aexp r_sexp r_str
ibou_head b_extra c_extra cor_extra d_extra gp_extra z_extra
crtdel crtime cf_creep cf_thermal cf_pstress cf_cell cf_axi
cf_fluid z_prop m_jrfriction m_jrescoh m_jrtension sol_ratio
sol_fob sol_fmag sol_rloc sol_rmax code_name version sub_version
rel_version z_model j_model j_prop d_temp set_error z_inside
bou_xreaction bou_yreactionbou_near z_fsi z_fsr z_density z_biot
z_group b_group c_group gp_addxmass tgps_head tgps_next tgps_type
tgps_strength tgps_decay tgps_timeth tgps_gp tgps_cor gp_thmass")

;; FISH function lists for each mode
(defconst itasca-mode-keyword-list
  (split-string itasca-mode-keywords))

(defconst itasca-general-function-list
  (cl-mapcan #'split-string
             (list itasca-general-functions)))
(defconst itasca-general-function-regexp
  (regexp-opt itasca-general-function-list 'words))

(defconst itasca-pfc-function-list
  (cl-mapcan #'split-string
             (list itasca-general-functions itasca-pfc-functions)))
(defconst itasca-pfc-function-regexp
  (regexp-opt itasca-pfc-function-list 'words))

; fix the FLAC one

(defconst itasca-flac3d-function-list
  (cl-mapcan #'split-string
             (list itasca-general-functions itasca-new-framework-functions
                   itasca-flac3d-functions)))
(defconst itasca-flac3d-function-regexp
  (regexp-opt itasca-flac3d-function-list 'words))

(defconst itasca-udec-function-list
  (cl-mapcan #'split-string
             (list itasca-general-functions itasca-udec-functions)))
(defconst itasca-udec-function-regexp
  (regexp-opt itasca-udec-function-list 'words))

; define the modes
(define-generic-mode 'itasca-general-mode
  '(";")
  itasca-mode-keyword-list
  (list (cons itasca-general-function-regexp 'font-lock-type-face)
        (cons "[-+]?[0-9]*\\.?[0-9]+\\([eE][-+]?[0-9]+\\)?"
              'font-lock-variable-name-face))
  '("\\.dat$" "\\.fis$" "\\.fin$")
  (list (lambda ()
          (itasca-setup-mode)
          (set (make-local-variable 'mode-name) "Itasca")))
  "Mode for Itasca data files (not code specific)")

(define-generic-mode  'itasca-pfc-mode
  '(";")
  itasca-mode-keyword-list
  (list (cons itasca-pfc-function-regexp 'font-lock-type-face)
        (cons "[-+]?[0-9]*\\.?[0-9]+\\([eE][-+]?[0-9]+\\)?"
              'font-lock-variable-name-face))
  '("\\.p3dat$" "\\.p2dat")
  (list (lambda ()
          (itasca-setup-mode)
          (set (make-local-variable 'mode-name) "PFC")))
  "Mode for Itasca PFC 4.0  data files")

(define-generic-mode  'itasca-flac-mode
  '(";")
  itasca-mode-keyword-list
  (list
   (cons itasca-flac-function-regexp 'font-lock-type-face)
   (cons "[-+]?[0-9]*\\.?[0-9]+\\([eE][-+]?[0-9]+\\)?"
         'font-lock-variable-name-face))
  '("\\.fdat$")
  (list (lambda ()
          (itasca-setup-mode)
          (set (make-local-variable 'mode-name) "FLAC")))
  "A mode for Itasca FLAC data files")

(define-generic-mode  'itasca-flac3d-mode
  '(";")
  itasca-mode-keyword-list
  (list (cons itasca-flac3d-function-regexp 'font-lock-type-face)
        (cons "[-+]?[0-9]*\\.?[0-9]+\\([eE][-+]?[0-9]+\\)?"
              'font-lock-variable-name-face))
  '("\\.f3dat$")
  (list (lambda ()
          (itasca-setup-mode)
          (set (make-local-variable 'mode-name) "FLAC3D")))
  "Mode for Itasca FLAC3D data files")

(define-generic-mode  'itasca-udec-mode
  '(";")
  itasca-mode-keyword-list
  (list (cons itasca-udec-function-regexp 'font-lock-type-face)
        (cons "[-+]?[0-9]*\\.?[0-9]+\\([eE][-+]?[0-9]+\\)?"
              'font-lock-variable-name-face))
  '("\\.udat$")
  (list (lambda ()
          (itasca-setup-mode)
          (set (make-local-variable 'mode-name) "UDEC")))
  "Mode for Itasca UDEC 6.0 data files")

;C:/Users/Itasca/Desktop/UDEC500/Exe32/udec500.exe
(defconst udec-exe "C:/src/udec_git/binaries/dbgexe32/UDEC600_dbg.exe")
(setq udec-process nil)
(defun start-udec () (interactive)
  (setq udec-process  (start-process "udec" "*udec-output*"
                                     udec-exe "test" (buffer-file-name))))
(defun udec-buffer () (interactive)
  (process-send-string "*udec-output*" (format "call %s\n"(buffer-file-name))))
(defun end-udec () (interactive)
  (delete-process udec-process))

;; major mode support functions

(defun itasca-copy-call-buffer-filename-as-kill ()
  "Insert the string: 'call file-name' to the clipboard where
file-name is the full path and filename of the current buffer.
Useful when editing a datafile in emacs and loading it into an
Itasca code."
  (interactive)
  (let ((s (format "call \"%s\"" (buffer-file-name))))
    (kill-new s)
    (message "Copied: %s to clipboard" s)))

(defun fish-indent-line ()
  "Indent current line as FISH code"
  (interactive)
  (beginning-of-line)
  (let ((re-kw-up "^[ \t]*\\(def\\|define\\|loop\\|command\\|if\\|case_of\\|caseof\\|section\\|case\\|else\\)")
        (re-kw-down "^[ \t]*\\(end\\|end_loop\\|endloop\\|end_command\\|endcommand\\|end_if\\|endif\\|end_case\\|endcase\\|end_section\\|endsection\\|case[^_o]\\|else\\)")
        (re-kw-down2 "^[ \t]*\\(end\\|end_loop\\|endloop\\|end_command\\|endcommand\\|end_if\\|endif\\|end_case\\|endcase\\|end_section\\|endsection\\)"))
   (if (bobp)
       (indent-line-to 0)
     (let ((not-indented t) (indent-width 2) cur-indent)
       (if (looking-at re-kw-down)
           (progn
             (save-excursion
               (forward-line -1)
               (setq cur-indent (- (current-indentation) indent-width)))
             (if (< cur-indent 0)
                 (setq cur-indent 0)))
         (save-excursion
           (while not-indented

             (forward-line -1)
             (if (looking-at re-kw-down2)
                 (progn
                   (setq cur-indent (current-indentation))
                   (setq not-indented nil))
               (if (looking-at re-kw-up)
                   (progn
                     (setq cur-indent (+ (current-indentation) indent-width))
                     (setq not-indented nil))
                 (if (bobp)
                     (setq not-indented nil)))))))
       (if cur-indent
           (indent-line-to cur-indent)
         (indent-line-to 0))))))

(defconst itasca-defun-start-regexp "^\s*def\s+\\([a-z_]+\\)")
(defconst itasca-defun-end-regexp "^ *end\\( +\\|;+\\|$\\)")

(defun itasca-begining-of-defun-function ()
  "Move point up to the current FISH function definition line. If
point is on a function definition line jump up to the next
FISH function definition."
  (interactive)
  (beginning-of-line)
  (if (looking-at itasca-defun-start-regexp)
      (forward-line -1))
  (while (and (not (looking-at itasca-defun-start-regexp))
              (not (bobp)))
    (forward-line -1)))

(defun itasca-end-of-defun-function ()
  "Move point down to the end of the current FISH function definition.
If point is on the end of a function already jump down to the end
of the next FISH function definition"
  (interactive)
  (beginning-of-line)
  (if (looking-at itasca-defun-end-regexp)
      (forward-line))
  (while (and (not (looking-at itasca-defun-end-regexp))
              (not (eobp)))
    (forward-line)))

(defun itasca-change-syntax-table ()
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?' "\""))

(defun itasca-setup-mode ()
  "set buffer local variables for itasca modes"
  (interactive)
  (local-set-key (kbd "C-c M-c") 'itasca-copy-call-buffer-filename-as-kill)
  (set (make-local-variable 'indent-line-function) 'fish-indent-line)
  (set (make-local-variable 'imenu-case-fold-search) t)
  (set (make-local-variable  'imenu-generic-expression)
       (list (list nil itasca-defun-start-regexp 1)))
  (set (make-local-variable 'beginning-of-defun-function)
       #'itasca-begining-of-defun-function)
  (set (make-local-variable 'end-of-defun-function)
       #'itasca-end-of-defun-function)
  (itasca-change-syntax-table))

;;; tests

(defvar itasca-test-fish-code "; a comment

DEF func1;
  oo=end_time
  ff=1234.
end ; junk

;def old_function
;junk commented out
;end

defjunk
endjunk
 endjunk
 defjunk
endJUNK loooong l ********************************************************************************

def  func2 ; comment
  stuff

  a comment
 end

 def func3
  stuff and junk
  foo
end;

")

(ert-deftest itasca-test-defun-nav ()
  (with-temp-buffer
    (insert itasca-test-fish-code)
    (goto-char (point-max))

    (itasca-begining-of-defun-function)
    (let ((current-line (buffer-substring-no-properties
                         (point) (point-at-eol))))
      (should (equal current-line " def func3")))

    (itasca-begining-of-defun-function)
    (let ((current-line (buffer-substring-no-properties
                         (point) (point-at-eol))))
      (should (equal current-line "def  func2 ; comment")))

    (itasca-begining-of-defun-function)
    (let ((current-line (buffer-substring-no-properties
                         (point) (point-at-eol))))
      (should (equal current-line "DEF func1;")))
    (itasca-end-of-defun-function)
    (let ((current-line (buffer-substring-no-properties
                         (point) (point-at-eol))))
      (should (equal current-line "end ; junk")))
    (itasca-end-of-defun-function)
    (let ((current-line (buffer-substring-no-properties
                         (point) (point-at-eol))))
      (should (equal current-line " end")))
    (itasca-end-of-defun-function)
    (let ((current-line (buffer-substring-no-properties
                         (point) (point-at-eol))))
      (should (equal current-line "end;")))))

(ert-deftest itasca-smoketest ()
  (with-temp-buffer
    (itasca-general-mode))
  (with-temp-buffer
    (itasca-pfc-mode))
  (with-temp-buffer
    (itasca-flac-mode))
  (with-temp-buffer
    (itasca-flac3d-mode))
  (with-temp-buffer
    (itasca-udec-mode)))

(provide 'itasca)
