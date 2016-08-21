# load and process all files

rm(list = ls())

options(nwarnings = 500)

# load aceR
library(aceR)

# set paths
DATA_PATH = "~/Downloads/post_raw_ace"
RELEASE_PATH = "~/Desktop"
setwd(DATA_PATH)

demographics_file = "~/Downloads/Sea Crest Demographics.xlsx"
demographics = load_ace_demographics(demographics_file)
  
# load and process
dat = load_ace_bulk(path = DATA_PATH)
proc = proc_by_module(dat, verbose = TRUE)
proc_demo = lapply(proc, function(x){
  merge(x, demographics, by = "pid", all = FALSE)
})


