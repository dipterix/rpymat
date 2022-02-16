#' @Align_Centers -base $t1  -dset $ct
#' 3dresample -input CT_highresRAI_shft.nii -prefix CT_highresRAI_res_shft.nii  -master $t1  -dxyz 1 1 1 -rmode NN
#'
#' align_epi_anat.py -dset1 $t1 -dset2  CT_highresRAI_res_shft.nii -dset1_strip None -dset2_strip None -dset2to1 -suffix _al  -feature_size 1  -overwrite -cost nmi -giant_move -rigid_body > status.txt
#'
#' 3dcopy  CT_highresRAI_res_shft_al+orig CT_highresRAI_res_al.nii
#'
#' 3dcopy $t1 ./temp_ANAT.nii
#'
#' afni -com "SWITCH_UNDERLAY temp_ANAT.nii" -com "SWITCH_OVERLAY CT_highresRAI_res_al.nii" -niml -yesplugouts

wd <- "~/Downloads/iELVis_Localization/"
ct <- shQuote("CT_highresRAI.nii", type = "cmd")
t1 <- shQuote("YAS_MRI_old.nii", type = "cmd")
setwd(wd)
rpymat::ensure_rpymat()
run_afni(c(
  "@Align_Centers -base { t1 } -dset { ct }",
  "3dresample -input CT_highresRAI_shft.nii -prefix CT_highresRAI_res_shft.nii  -master { t1 } -dxyz 1 1 1 -rmode NN",
  "align_epi_anat.py -dset1 { t1 } -dset2  CT_highresRAI_res_shft.nii -dset1_strip None -dset2_strip None -dset2to1 -suffix _al  -feature_size 1  -overwrite -cost nmi -giant_move -rigid_body > status.txt",
  "3dcopy  CT_highresRAI_res_shft_al+orig CT_highresRAI_res_al.nii",
  "3dcopy { t1 } ./temp_ANAT.nii",
  'afni -com "SWITCH_UNDERLAY temp_ANAT.nii" -com "SWITCH_OVERLAY CT_highresRAI_res_al.nii" -niml -yesplugouts',
  ""
))

ct <- "CT_highresRAI.nii"
t1 <- "YAS_MRI_old"
run_afni(c(
  # make T1 real skull bright by inverting and mask
  "3dAutomask -apply_prefix { t1 } -dilate 3 DBS.023.anat.nii.gz",
  "set max = `3dBrickStat -max { t1 }+orig`",
  '3dcalc -a { t1 }+orig. -expr "step(a)*(${{max}}-a)" -prefix { t1 }_rev',
  # align T1 to CT
  "align_epi_anat.py -dset1 { t1 }_rev+orig -dset2 CT_sh_rs+orig  -dset1_strip None -dset2_strip None -suffix _al2ct_nmi -feature_size 1  -overwrite -cost nmi -giant_move -rigid_body",
  # "align_epi_anat.py -dset1 { t1 } -dset2  CT_highresRAI_res_shft.nii -dset1_strip None -dset2_strip None -dset2to1 -suffix _al  -feature_size 1  -overwrite -cost nmi -giant_move -rigid_body > status.txt",
  # "3dcopy  CT_highresRAI_res_shft_al+orig CT_highresRAI_res_al.nii",
  # "3dcopy { t1 } ./temp_ANAT.nii",
  # 'afni -com "SWITCH_UNDERLAY temp_ANAT.nii" -com "SWITCH_OVERLAY CT_highresRAI_res_al.nii" -niml -yesplugouts',
  ""
))


run_afni('afni -com "SWITCH_UNDERLAY temp_ANAT.nii" -com "SWITCH_OVERLAY CT_highresRAI_res_al.nii" -niml -yesplugouts')

anat <- oro.nifti::readNIfTI("temp_ANAT.nii", reorient = FALSE)
align <- oro.nifti::readNIfTI("CT_highresRAI_res_al.nii", reorient = FALSE)

dim(anat@.Data)
dim(align@.Data)
