#!/bin/bash


. /u/local/Modules/default/init/modules.sh
module use /u/project/CCN/apps/modulefiles

# Load R module
module load R  

Rscript gamm_models.R $brain $sex $region
