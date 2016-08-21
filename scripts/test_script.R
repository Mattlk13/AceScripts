
rm(list = ls())

# dependencies

library(aceR)
library(stringr)
library(ggplot2)
library(lme4)

# paths

rm(list = ls())

library(aceR)
library(stringr)
library(plyr)

# filter
RUN_ON = "BRT"

# paths

BASE_DIRECTORY = "~/Desktop/seacrest"
RAW_SEACREST_DIRECTORY = paste(BASE_DIRECTORY, "post_data", sep = "/")
MISSING_FILES_DIRECTORY = paste(BASE_DIRECTORY, "remaining_files", sep = "/")
PARTICIPANT_INFO_DIRECTORY = paste(BASE_DIRECTORY, "participant_info_files", sep = "/")

# files

DEMOGRAPHICS_FILE = paste(PARTICIPANT_INFO_DIRECTORY, "Sea Crest Demographics.xlsx", sep = "/")
GRADE_FILE = paste(PARTICIPANT_INFO_DIRECTORY, "School Pilot WJ Data GRADE.xlsx", sep = "/")

# constants

PID = "pid"
GENDER = "gender"
AGE = "grade"
SS_ID = "SS"
BAND_ID = "Band"

ACE_INCLUDE = c(
  "rt_")

ACE_IGNORE = c(
  "cohort",
  "median",
  "length",
  "se", 
  "sd", 
  "incorrect",
  "rt_count", 
  "rw_count", 
  "acc_", 
  "rw_",
  "correct", 
  "object_count")

# helper functions 

grab_ss = function(df) {
  cols = names(df)
  return (aceR:::multi_filter_vec(cols, c(SS_ID)))
}

grab_ace = function(df, include = c(), skip = c()) {
  cols = names(df)
  vars = aceR:::multi_filter_vec(cols, include)
  vars = aceR:::multi_filter_out_vec(vars, skip)
  return (vars)
}

is_empty = function(col) {
  return (sum(is.na(col)) == length(col))
}

valid_cols = function(df, cols) {
  valid = sapply(cols, function(y) {
    invalid = is_empty(df[, y])
    return (!invalid)
  })
  return (names(which(valid)))
}

load_missing_files = function(path) {
  files = list.files(path, pattern = RUN_ON)
  out = data.frame()
  for (file in files) {
    file_path = paste(path, file, sep = "/")
    dat = aceR:::load_ace_filtered_file(file_path)
    out = plyr::rbind.fill(out, dat)
  }
  return (out)
}

demographics = load_ace_demographics(DEMOGRAPHICS_FILE)
demographics$pid = standardize_seacrest_pid(demographics$pid)

ace = load_ace_bulk(path = RAW_SEACREST_DIRECTORY, pattern = RUN_ON, recursive = FALSE)
ace_missing = load_missing_files(path = MISSING_FILES_DIRECTORY)
ace = plyr::rbind.fill(ace, ace_missing)
ace_by_task = proc_by_module(ace, verbose = TRUE)

modules = names(ace_by_task)
print(modules)

for (module in ace_by_task) {
  
  module$pid = standardize_seacrest_pid(module$pid)
  ace_ys = grab_ace(module, ACE_INCLUDE, ACE_IGNORE)
  module_demographs = merge(module, demographics, by = "pid")
  valid_ace_ys = valid_cols(module_demographs, ace_ys)
  
  # get rid of infinites
  is.na(module_demographs) = sapply(module_demographs, is.infinite)
  
  for (ace_y in valid_ace_ys) {
    AceScripts::generate_plots_and_stats(module_demographs, AGE, ace_y)
    AceScripts::generate_plots_and_stats(module_demographs, GENDER, ace_y)
    AceScripts::generate_plots_and_stats(module_demographs, AGE, ace_y, cohort = GENDER)
    break
  }
  break
  
}