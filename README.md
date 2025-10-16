## Exploration of Pubertal Timing and Tempo’s Influence on Cortical and Subcortical Maturation in Adolescence

*project intended for publication in 2025!* 

### the way the pipeline works 

1. Within the [abcd_pub_smri_prep_desc.Rmd](https://github.com/clarefmccann/abcd-pub-smri-publication/blob/main/abcd_pub_smri_prep_desc.Rmd) file:
    - Creates dataframes to be used in analysis:
        - **combine:**
            - puberty (output from: LINK)
            - brain:
                - `imaging/mri_y_smr_vol_aseg.csv`
                - `imaging/mri_y_smr_thk_dsk.csv`
                - `imaging/mri_y_smr_area_dsk.csv`
                - `imaging/mri_y_adm_info.csv` (to harmonize: `mri_info_manufacturersmn`)
            - age (`interview_age` from `abcd-general/abcd_y_lt.csv`)
        - Remove participants:
            - missing brain, timing or tempo data
            - recommended for exclusion re: imaging (`imgincl_t1w_include`)
            - duplicate `rel_family_id` (*randomly selected one id from each family*)
         
    - Generates descriptives & inclusion/exclusion likelihood
        - loads in race, ethnicity, and income from `abcd-general/abcd_p_demo.csv`
        - generates sample characteristic breakdown
        - calculates the likelihood of being excluded based on any of these domains
     
    - Creates a PDS by Wave visualization
    
2. longCombat (with guidance from [Beer et al., 2020](https://www.sciencedirect.com/science/article/pii/S1053811920306157))
  - uses output generated above to harmonize the imaging data to remove the extent to which scanner type contributes to variance observed
  - the script where everything happens is here: [longComBat-pub-sMRI-abcd.R](https://github.com/clarefmccann/abcd-pub-smri-publication/blob/main/longComBat/longComBat-pub-sMRI-abcd.R)
    - make sure models match as closely to the actual models to be tested
  - the markdown to run the script and generate the html of reports is here: [longComBat-reports.Rmd](https://github.com/clarefmccann/abcd-pub-smri-publication/blob/main/longComBat/longComBat-reports.Rmd)
  
-----------------------------------------------------------------------
*on hoffman, but can also be run from local if you have the permissions*

3. run models!
   - run in terminal:
      - [batch_gamm.sh](https://github.com/clarefmccann/abcd-pub-smri-publication/blob/main/scripts/batch_gamm.sh)
         - sets the variables for [gamm_models.R](https://github.com/clarefmccann/abcd-pub-smri-publication/blob/main/scripts/gamm_models.R) and calls on [run_R_gamm.sh](https://github.com/clarefmccann/abcd-pub-smri-publication/blob/main/scripts/run_R_gamm.sh) to actually run the models
       
3. clean!
   - [clean_results_csvs.R](https://github.com/clarefmccann/abcd-pub-smri-publication/blob/main/scripts/clean_results_csvs.R): combine all results into csvs, identify best-fitting models for each region
   - [fdr_correction.R](https://github.com/clarefmccann/abcd-pub-smri-publication/blob/main/scripts/fdr_correction.R): generates fdr corrected p-values for all terms and clean csvs with estimates to be used for reporting
  
4. visualizing
   - [vis.R](https://github.com/clarefmccann/abcd-pub-smri-publication/blob/main/scripts/vis.R): generate predicted interaction plots // the ones intended for publication are moved into a for_pub folder
   - [smri-abcd-ggseg.Rmd](https://github.com/clarefmccann/abcd-pub-smri-publication/blob/main/scripts/smri-abcd-ggseg.Rmd): generate ROI plots 
   - [visual_tabs.R](https://github.com/clarefmccann/abcd-pub-smri-publication/blob/main/scripts/visual_tabs.R): takes all of the visualizations in for_pub and generate an all_effects.html to view the trajectories of the effects

