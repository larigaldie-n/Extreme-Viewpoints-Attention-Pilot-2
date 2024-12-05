library(png)
library(tidyverse)
library(eyeScrollR)
### NB: eyeScrollR is not on CRAN. To install eyeScrollR, you can use Devtools
# and the following command:
# devtools::install_github("larigaldie-n/eyeScrollR")
# More info in eyeScrollR's paper:
# https://doi.org/10.3758/s13428-024-02343-1

# Exclude bad or incomplete data sets (poor calibration, eye tracker files
# without an associated response file and conversely, no responses in the
# response file, low frequency of data
source("preprocessing.R")
exclude()

# Takes files from /raw_data/ET_raw (raw eye tracker files from iMotions), and
# outputs eyeScrollR-corrected files into /intermediate_data/ET/
source("eyeScrollR_make.R")
eyeScrollR_make()

# Takes files from /intermediate_data/ET/ (eyeScrollR-corrected files), and
# outputs files in /final_data/, which are complete data files with all
# statements, questionnaire ratings, experimental condition and total dwell
# times
source("dataset_merge.R")
datasets_merge()