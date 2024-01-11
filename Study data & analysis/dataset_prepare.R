library(png)
library(tidyverse)
library(eyeScrollR)

# Exclude bad or incomplete data sets (poor calibration, eye tracker files
# without an associated response file and conversely, no responses in the
# response file, low frequency of data
source("preprocessing.R")
exclude()

# Take files from /ET_raw/ (raw eye tracker files from iMotions), and output
# eyeScrollR-corrected files into /ET/
source("eyeScrollR_make.R")
eyeScrollR_make()

# Also outputs files in /final/, which are the total sum of fixation times for
# each statement
source("dataset_merge.R")
datasets_merge()