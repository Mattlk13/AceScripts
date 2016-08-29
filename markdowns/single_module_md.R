## ----df_from_proc_demo---------------------------------------------------
# should I expect the cached nice data frame list to be called proc_demo? I guess so
if(this_task == "BRT" & !BRT_PRINT) {} else { # edge case where BRT needs to be evaluated even if no print (and thus header turned off)
  paste0("#", this_task_long) %>%
    asis_output()}
cat("\n") %>% asis_output() # need this for line breaking

d_raw <- proc_demo[[this_task]]

## ----outlier_3rd_grade---------------------------------------------------
asis_output("##outlier testing")
cat("\n") %>% asis_output()
asis_output("###3rd grade") # setting header
cat("\n") %>% asis_output()
paste("####", vars_of_interest_long[1], sep = "") %>%
  asis_output()
d_raw %>%
  filter(grade == "3rd Grade") %>%
  with(
    grubbs.test(get(vars_of_interest[1]), two.sided = TRUE))

# note that I am couching the printouts for multiple output vars in a try statement
# so if there's only one var of interest, the other ones just won't print. easier than checking how many vars there are bc there's only gonna be 1 or 2, only 3 once (Flanker)
# sometimes a try statement doesn't work so in those instances using an if(gtools::invalid()) check
cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[2])) {
  paste("####", vars_of_interest_long[2], sep = "") %>%
    asis_output()}
try(
  d_raw %>%
    filter(grade == "3rd Grade") %>%
    with(
      grubbs.test(get(vars_of_interest[2]), two.sided = TRUE)), TRUE)

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[3])) {
  paste("####", vars_of_interest_long[3], sep = "") %>%
    asis_output()}
try(
  d_raw %>%
    filter(grade == "3rd Grade") %>%
    with(
      grubbs.test(get(vars_of_interest[3]), two.sided = TRUE)), TRUE)

## ----outlier_5th_grade---------------------------------------------------
asis_output("###5th grade")
cat("\n") %>% asis_output()
paste("####", vars_of_interest_long[1], sep = "") %>%
  asis_output()
d_raw %>%
  filter(grade == "5th Grade") %>%
  with(
    grubbs.test(get(vars_of_interest[1]), two.sided = TRUE))

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[2])) {
  paste("####", vars_of_interest_long[2], sep = "") %>%
    asis_output()}
try(
  d_raw %>%
    filter(grade == "5th Grade") %>%
    with(
      grubbs.test(get(vars_of_interest[2]), two.sided = TRUE)), TRUE)

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[3])) {
  paste("####", vars_of_interest_long[3], sep = "") %>%
    asis_output()}
try(
  d_raw %>%
    filter(grade == "5th Grade") %>%
    with(
      grubbs.test(get(vars_of_interest[3]), two.sided = TRUE)), TRUE)

## ----outlier_7th_grade---------------------------------------------------
asis_output("###7th grade")
cat("\n") %>% asis_output()
paste("####", vars_of_interest_long[1], sep = "") %>%
  asis_output()
d_raw %>%
  filter(grade == "7th Grade") %>%
  with(
    grubbs.test(get(vars_of_interest[1]), two.sided = TRUE))

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[2])) {
  paste("####", vars_of_interest_long[2], sep = "") %>%
    asis_output()}
try(
  d_raw %>%
    filter(grade == "7th Grade") %>%
    with(
      grubbs.test(get(vars_of_interest[2]), two.sided = TRUE)), TRUE)

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[3])) {
  paste("####", vars_of_interest_long[3], sep = "") %>%
    asis_output()}
try(
  d_raw %>%
    filter(grade == "7th Grade") %>%
    with(
      grubbs.test(get(vars_of_interest[3]), two.sided = TRUE)), TRUE)

## ----rm_outliers---------------------------------------------
# the approach is taken here to remove a subject from ALL metrics of a module
# if they are an outlier in ANY metrics of that module, to improve interpretation b/w metrics
# in any given module.
# any outliers removed in the BRT data will be excluded from ALL metrics for ALL modules.

bad_subs3 = NULL
bad_subs5 = NULL
bad_subs7 = NULL
for (i in 1:length(vars_of_interest)) {# for each grade, concatenates outliers ACROSS metrics of interest
  bad_subs3 <- bad_subs3 %>%
    c(
      d_raw %>%
        filter(grade == "3rd Grade") %>%
        with(
          get_outlier_subs(get(vars_of_interest[1]), pid)))
  
  bad_subs5 <- bad_subs5 %>%
    c(
      d_raw %>%
        filter(grade == "5th Grade") %>%
        with(
          get_outlier_subs(get(vars_of_interest[1]), pid)))
  
  bad_subs7 <- bad_subs7 %>%
    c(
      d_raw %>%
        filter(grade == "7th Grade") %>%
        with(
          get_outlier_subs(get(vars_of_interest[1]), pid)))
}
bad_subs_all = c(bad_subs3, bad_subs5, bad_subs7)

d <- d_raw %>%
  filter(!(pid %in% bad_subs_all))
## ----preproc-------------------------------------------------------------
if(this_task == "SPATIALSPAN") {d <- d_raw} # dumb edge case, can't run rm_outliers on span so must init d here
if(this_task == "BRT") { # edge case, don't append BRT to itself
  d <- d %>%
    clean_grade_gender()
} else {
  d <- d %>%
    brt_append() %>%
    clean_grade_gender()}

## ----qc_graphs-----------------------------------------------------------
asis_output("##quality control histograms")
cat("\n") %>% asis_output()
paste("###", vars_of_interest_long[1], sep = "") %>%
  asis_output()
d %>%
  ggplot(aes(x = get(vars_of_interest[1]), fill = factor(grade))) +
  geom_histogram(bins=20, position="dodge") +
  labs(x = vars_of_interest_long[1]) +
  guides(fill = guide_legend(title = "Grade"))

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[2])) {
  paste("###", vars_of_interest_long[2], sep = "") %>%
    asis_output()}
if(!invalid(vars_of_interest[2])) {
  d %>%
    ggplot(aes(x = get(vars_of_interest[2]), fill = factor(grade))) +
    geom_histogram(bins=20, position="dodge") +
    labs(x = vars_of_interest_long[2]) +
    guides(fill = guide_legend(title = "Grade"))}

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[3])) {
  paste("###", vars_of_interest_long[3], sep = "") %>%
    asis_output()}
if(!invalid(vars_of_interest[3])) {
  d %>%
    ggplot(aes(x = get(vars_of_interest[3]), fill = factor(grade))) +
    geom_histogram(bins=20, position="dodge") +
    labs(x = vars_of_interest_long[3]) +
    guides(fill = guide_legend(title = "Grade"))}
## ----score_feedback_means--------------------------------------------------
asis_output("##mean scores for giving feedback in task")
cat("\n") %>% asis_output()


#mutate_score = lazyeval::interp(~ ((((b-a)/a)*100)+100), a = as.name(score_vars[1]), b = as.name(score_vars[2]))
#mtcars %>% mutate_(.dots = setNames(list(mutate_call), new_col_name))

if(is.na(score_formula)) {
  d_score = d %>%
    group_by(grade) %>%
    select(one_of(score_vars))
  
  if(length(score_vars) == 1) {
    d_score = d_score %>%
      summarize_(var1_mean = interp(~mean(a, na.rm=T), a = as.name(score_vars[1])),
                var1_sd = interp(~sd(a, na.rm=T), a = as.name(score_vars[1])))
  } else if(length(score_vars) == 2) {
    d_score = d_score %>%
      summarize_(var1_mean = interp(~mean(a, na.rm=T), a = as.name(score_vars[1])),
                var1_sd = interp(~sd(a, na.rm=T), a = as.name(score_vars[1])),
                var2_mean = interp(~mean(a, na.rm=T), a = as.name(score_vars[2])),
                var2_sd = interp(~sd(a, na.rm=T), a = as.name(score_vars[2])))
  }
  print(score_vars)
  
} else if(score_formula == "fraction") {
  d_score = d %>%
    mutate_(score = interp(~((((b-a)/a)*100)+100), a = as.name(score_vars[1]), b = as.name(score_vars[2]))) %>%
    group_by(grade) %>%
    select(score) %>%
    summarize(score_mean = mean(score),
              score_sd = sd(score))
}

print(d_score)

## ----lms_brt_only----------------------------------------------------------
# edge case here where BRT can't be regressed against itself
asis_output("##regressions")
cat("\n") %>% asis_output()
paste("###", vars_of_interest_long[1], sep = "") %>%
  asis_output()
d %>%
  with(summary(lm(get(vars_of_interest[1]) ~ grade_num * gender)))

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[2])) {
  paste("###", vars_of_interest_long[2], sep = "") %>%
    asis_output()} # dumb: they have to be in two separate if statements to get the heading to render
if(!invalid(vars_of_interest[2])) {
  d %>%
    with(summary(lm(get(vars_of_interest[2]) ~ grade_num * gender)))}

## ----lms-----------------------------------------------------------------
asis_output("##regressions")
cat("\n") %>% asis_output()
paste("###", vars_of_interest_long[1], sep = "") %>%
  asis_output()
d %>%
  with(summary(lm(get(vars_of_interest[1]) ~ grade_num * gender + scale(brt_rt_mean.overall, scale = Z_SCORE_XREGS))))

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[2])) {
  paste("###", vars_of_interest_long[2], sep = "") %>%
    asis_output()} # dumb: they have to be in two separate if statements to get the heading to render
if(!invalid(vars_of_interest[2])) {
  d %>%
    with(summary(lm(get(vars_of_interest[2]) ~ grade_num * gender + scale(brt_rt_mean.overall, scale = Z_SCORE_XREGS))))}

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[3])) {
  paste("###", vars_of_interest_long[3], sep = "") %>%
    asis_output()}
if(!invalid(vars_of_interest[3])) {
  d %>%
    with(summary(lm(get(vars_of_interest[3]) ~ grade_num * gender + scale(brt_rt_mean.overall, scale = Z_SCORE_XREGS))))}

## ----graphs--------------------------------------------------------------
asis_output("##graphs")
cat("\n") %>% asis_output()
paste("###", vars_of_interest_long[1], sep = "") %>%
  asis_output()
d %>%
  ggplot(aes(x = grade, y = get(vars_of_interest[1]), fill = factor(gender))) +
  geom_boxplot() +
  labs(y = vars_of_interest_long[1]) +
  guides(fill = guide_legend(title = "Gender"))

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[2])) {
  paste("###", vars_of_interest_long[2], sep = "") %>%
    asis_output()}
if(!invalid(vars_of_interest[2])) {
  d %>%
    ggplot(aes(x = grade, y = get(vars_of_interest[2]), fill = factor(gender))) +
    geom_boxplot() +
    labs(y = vars_of_interest_long[2]) +
    guides(fill = guide_legend(title = "Gender"))
}

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[3])) {
  paste("###", vars_of_interest_long[3], sep = "") %>%
    asis_output()}
if(!invalid(vars_of_interest[3])) {
  d %>%
    ggplot(aes(x = grade, y = get(vars_of_interest[3]), fill = factor(gender))) +
    geom_boxplot() +
    labs(y = vars_of_interest_long[3]) +
    guides(fill = guide_legend(title = "Gender"))
}