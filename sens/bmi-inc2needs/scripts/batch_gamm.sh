#!/bin/bash -l

. /u/local/Modules/default/init/modules.sh
module use /u/project/CCN/apps/modulefiles

module load R  # or whichever version you need

brains=("area")  # here you can add whatever metric you want to include (#"volume" "thickness" "area")
sexes=("f" "m")

# loopy over brain metrics and sexes
for brain in "${brains[@]}"; do
  for sex in "${sexes[@]}"; do

    if [ "$brain" == "volume" ]; then
      regions=("smri_vol_scs_cbwmatterlh" "smri_vol_scs_ltventriclelh" "smri_vol_scs_inflatventlh" "smri_vol_scs_crbwmatterlh" "smri_vol_scs_crbcortexlh" "smri_vol_scs_tplh" "smri_vol_scs_caudatelh" "smri_vol_scs_putamenlh" "smri_vol_scs_pallidumlh" "smri_vol_scs_3rdventricle" "smri_vol_scs_4thventricle" "smri_vol_scs_bstem" "smri_vol_scs_hpuslh" "smri_vol_scs_amygdalalh" "smri_vol_scs_csf" "smri_vol_scs_aal" "smri_vol_scs_vedclh" "smri_vol_scs_cbwmatterrh" "smri_vol_scs_ltventriclerh" "smri_vol_scs_inflatventrh" "smri_vol_scs_crbwmatterrh" "smri_vol_scs_crbcortexrh" "smri_vol_scs_tprh" "smri_vol_scs_caudaterh" "smri_vol_scs_putamenrh" "smri_vol_scs_pallidumrh" "smri_vol_scs_hpusrh" "smri_vol_scs_amygdalarh" "smri_vol_scs_aar" "smri_vol_scs_vedcrh" "smri_vol_scs_wmhint" "smri_vol_scs_ccps" "smri_vol_scs_ccmidps" "smri_vol_scs_ccct" "smri_vol_scs_ccmidat" "smri_vol_scs_ccat" "smri_vol_scs_wholeb" "smri_vol_scs_latventricles" "smri_vol_scs_allventricles" "smri_vol_scs_intracranialv" "smri_vol_scs_suprateialv" "smri_vol_scs_subcorticalgv")
    elif [ "$brain" == "thickness" ]; then
      regions=("smri_thick_cdk_banksstslh" "smri_thick_cdk_cdacatelh" "smri_thick_cdk_cdmdfrlh" "smri_thick_cdk_cuneuslh" "smri_thick_cdk_ehinallh" "smri_thick_cdk_fusiformlh" "smri_thick_cdk_ifpllh" "smri_thick_cdk_iftmlh" "smri_thick_cdk_ihcatelh" "smri_thick_cdk_locclh" "smri_thick_cdk_lobfrlh" "smri_thick_cdk_linguallh" "smri_thick_cdk_mobfrlh" "smri_thick_cdk_mdtmlh" "smri_thick_cdk_parahpallh" "smri_thick_cdk_paracnlh" "smri_thick_cdk_parsopclh" "smri_thick_cdk_parsobislh" "smri_thick_cdk_parstgrislh" "smri_thick_cdk_pericclh" "smri_thick_cdk_postcnlh" "smri_thick_cdk_ptcatelh" "smri_thick_cdk_precnlh" "smri_thick_cdk_pclh" "smri_thick_cdk_rracatelh" "smri_thick_cdk_rrmdfrlh" "smri_thick_cdk_sufrlh" "smri_thick_cdk_supllh" "smri_thick_cdk_sutmlh" "smri_thick_cdk_smlh" "smri_thick_cdk_frpolelh" "smri_thick_cdk_tmpolelh" "smri_thick_cdk_trvtmlh" "smri_thick_cdk_insulalh" "smri_thick_cdk_banksstsrh" "smri_thick_cdk_cdacaterh" "smri_thick_cdk_cdmdfrrh" "smri_thick_cdk_cuneusrh" "smri_thick_cdk_ehinalrh" "smri_thick_cdk_fusiformrh" "smri_thick_cdk_ifplrh" "smri_thick_cdk_iftmrh" "smri_thick_cdk_ihcaterh" "smri_thick_cdk_loccrh" "smri_thick_cdk_lobfrrh" "smri_thick_cdk_lingualrh" "smri_thick_cdk_mobfrrh" "smri_thick_cdk_mdtmrh" "smri_thick_cdk_parahpalrh" "smri_thick_cdk_paracnrh" "smri_thick_cdk_parsopcrh" "smri_thick_cdk_parsobisrh" "smri_thick_cdk_parstgrisrh" "smri_thick_cdk_periccrh" "smri_thick_cdk_postcnrh" "smri_thick_cdk_ptcaterh" "smri_thick_cdk_precnrh" "smri_thick_cdk_pcrh" "smri_thick_cdk_rracaterh" "smri_thick_cdk_rrmdfrrh" "smri_thick_cdk_sufrrh" "smri_thick_cdk_suplrh" "smri_thick_cdk_sutmrh" "smri_thick_cdk_smrh" "smri_thick_cdk_frpolerh" "smri_thick_cdk_tmpolerh" "smri_thick_cdk_trvtmrh" "smri_thick_cdk_insularh" "smri_thick_cdk_meanlh" "smri_thick_cdk_meanrh" "smri_thick_cdk_mean")
    elif [ "$brain" == "area" ]; then
      regions=("smri_area_cdk_banksstslh" "smri_area_cdk_cdacatelh" "smri_area_cdk_cdmdfrlh" "smri_area_cdk_cuneuslh" "smri_area_cdk_ehinallh" "smri_area_cdk_fusiformlh" "smri_area_cdk_ifpllh" "smri_area_cdk_iftmlh" "smri_area_cdk_ihcatelh" "smri_area_cdk_locclh" "smri_area_cdk_lobfrlh" "smri_area_cdk_linguallh" "smri_area_cdk_mobfrlh" "smri_area_cdk_mdtmlh" "smri_area_cdk_parahpallh" "smri_area_cdk_paracnlh" "smri_area_cdk_parsopclh" "smri_area_cdk_parsobislh" "smri_area_cdk_parstgrislh" "smri_area_cdk_pericclh" "smri_area_cdk_postcnlh" "smri_area_cdk_ptcatelh" "smri_area_cdk_precnlh" "smri_area_cdk_pclh" "smri_area_cdk_rracatelh" "smri_area_cdk_rrmdfrlh" "smri_area_cdk_sufrlh" "smri_area_cdk_supllh" "smri_area_cdk_sutmlh" "smri_area_cdk_smlh" "smri_area_cdk_frpolelh" "smri_area_cdk_tmpolelh" "smri_area_cdk_trvtmlh" "smri_area_cdk_insulalh" "smri_area_cdk_banksstsrh" "smri_area_cdk_cdacaterh" "smri_area_cdk_cdmdfrrh" "smri_area_cdk_cuneusrh" "smri_area_cdk_ehinalrh" "smri_area_cdk_fusiformrh" "smri_area_cdk_ifplrh" "smri_area_cdk_iftmrh" "smri_area_cdk_ihcaterh" "smri_area_cdk_loccrh" "smri_area_cdk_lobfrrh" "smri_area_cdk_lingualrh" "smri_area_cdk_mobfrrh" "smri_area_cdk_mdtmrh" "smri_area_cdk_parahpalrh" "smri_area_cdk_paracnrh" "smri_area_cdk_parsopcrh" "smri_area_cdk_parsobisrh" "smri_area_cdk_parstgrisrh" "smri_area_cdk_periccrh" "smri_area_cdk_postcnrh" "smri_area_cdk_ptcaterh" "smri_area_cdk_precnrh" "smri_area_cdk_pcrh" "smri_area_cdk_rracaterh" "smri_area_cdk_rrmdfrrh" "smri_area_cdk_sufrrh" "smri_area_cdk_suplrh" "smri_area_cdk_sutmrh" "smri_area_cdk_smrh" "smri_area_cdk_frpolerh" "smri_area_cdk_tmpolerh" "smri_area_cdk_trvtmrh" "smri_area_cdk_insularh" "smri_area_cdk_totallh" "smri_area_cdk_totalrh" "smri_area_cdk_total")
    fi

    for region in "${regions[@]}"; do
      job_name="sens_combat_gamm_${brain}_${sex}_${region}"
      output_log=".../projects/smri-pub-abcd/sens/bmi-inc2needs/out/${job_name}.out"
      error_log=".../projects/smri-pub-abcd/sens/bmi-inc2needs/error/${job_name}.err"
      
      cmd="qsub -cwd -N combat_gamm_${brain}_${sex}_${region} -o "$output_log" -e "$error_log" -l h_data=60G,h_rt=24:00:00 -v brain='$brain',sex='$sex',region='$region' run_R_gamm.sh"

      eval $cmd
      echo "job submitted"

      echo "submitted job: $job_name"
    done
  done
done
