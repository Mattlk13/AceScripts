## ----df_from_proc_demo---------------------------------------------------
# should I expect the cached nice data frame list to be called proc_demo? I guess so
if(this_task == "BRT" & !BRT_PRINT) {} else { # edge case where BRT needs to be evaluated even if no print (and thus header turned off)
  paste0("#", this_task_long) %>%
    asis_output()}
cat("\n") %>% asis_output() # need this for line breaking

d_raw <- proc_demo[[this_task]]

d_raw = d_raw %>% filter(time != "0", time != "TIME:")
## ----rm_outliers---------------------------------------------
# the approach is taken here to remove a subject from ALL metrics of a module
# if they are an outlier in ANY metrics of that module, to improve interpretation b/w metrics
# any outliers removed in the BRT data will be excluded from ALL metrics for ALL modules.

bad_subs = NULL
for (i in 1:length(vars_of_interest)) {# concatenates outliers ACROSS metrics of interest
    # need this to verify that there are >2 valid data pts to do the outlier test
    these_vals_temp = with(d_raw, get(vars_of_interest[i])[!is.na(get(vars_of_interest[i]))])
    if(length(these_vals_temp) > 2) {
      these_bad_subs = d_raw %>%
        with(
          get_outlier_subs(get(vars_of_interest[i]), pid))
    } else {these_bad_subs = NULL}
    if(!is.null(these_bad_subs)) {bad_subs = c(bad_subs, these_bad_subs)}
}

d <- d_raw %>%
  filter(!(pid %in% unlist(bad_subs)))

## ----rm_outliers_diff_vars-----------------------------------
bad_subs = NULL

for (i in 1:length(outlier_vars)) {# concatenates outliers ACROSS metrics of interest
    # need this to verify that there are >2 valid data pts in this grade level to do the outlier test
    these_vals_temp = with(d_raw, get(outlier_vars[i])[!is.na(get(outlier_vars[i]))])
    if(length(these_vals_temp) > 2) {
      these_bad_subs = d_raw %>%
        with(
          get_outlier_subs(get(outlier_vars[i]), pid))
    } else {these_bad_subs = NULL}
    if(!is.null(these_bad_subs)) {bad_subs <- c(bad_subs, these_bad_subs)}
}

d <- d_raw %>%
  filter(!(pid %in% unlist(bad_subs)))

## ----rm_short_rts--------------------------------------------------------
# this scrubs subs whose avg RT < 150 ms. written for those where RT is a var of interest
short_rt_subs = NULL

for (i in 1:min(c(length(vars_of_interest), 2))) { # if there are 3 vars of interest the 3rd one is cost, which should NOT be counted for this
  short_rt_subs = c(short_rt_subs, d$pid[d[, vars_of_interest[i]] < 150])
}
short_rt_subs = short_rt_subs[!is.na(short_rt_subs)]
d <- d %>%
  filter(!(pid %in% short_rt_subs))

asis_output("##subs w/ short RTs on ANY var of interest(< 150 ms)")
cat("\n") %>% asis_output()
print(short_rt_subs)

## ----rm_short_rts_diff_vars----------------------------------------------
# this scrubs subs whose avg RT < 150 ms. written for those where RT is a var of interest
short_rt_subs = NULL

for (i in 1:min(c(length(outlier_vars), 2))) { # if there are 3 vars of interest the 3rd one is cost, which should NOT be counted for this
  short_rt_subs = c(short_rt_subs, d$pid[d[, outlier_vars[i]] < 150])
}
short_rt_subs = short_rt_subs[!is.na(short_rt_subs)]
d <- d %>%
  filter(!(pid %in% short_rt_subs))

asis_output("##subs w/ short RTs on ANY var of interest(< 150 ms)")
cat("\n") %>% asis_output()
print(short_rt_subs)

## ----preproc-------------------------------------------------------------
if(this_task %in% c("SPATIALSPAN", "BACKWARDSSPATIALSPAN", "FILTER")) {d <- d_raw} # dumb edge case, can't run rm_outliers on span so must init d here
if(this_task == "BRT") { # edge case, don't append BRT to itself
  d <- d %>%
    clean_gender() %>%
    mutate(rt_mean.dominant = if_else(handedness == "R", rt_mean.right, rt_mean.left, NA_real_),
           rt_mean.nondominant = if_else(handedness == "L", rt_mean.right, rt_mean.left, NA_real_))
} else {
  d <- d %>%
    brt_append(brt) %>%
    clean_gender()
}
# NB: gender is effect coded female: -1 male: 1
# such that a POSITIVE beta estimate for RT change by gender indicates that FEMALES are faster (RT for males HIGHER)

asis_output("##n by gender")
cat("\n") %>% asis_output()
table(d$gender)

## ----qc_graphs-----------------------------------------------------------
asis_output("##quality control histograms")
cat("\n") %>% asis_output()
paste("###", vars_of_interest_long[1], sep = "") %>%
  asis_output()
d %>%
  ggplot(aes(x = get(vars_of_interest[1]))) +
  geom_histogram(bins=20, position="dodge") +
  labs(x = vars_of_interest_long[1])

cat("\n\n") %>% asis_output()
if(!invalid(vars_of_interest[2])) {
  paste("###", vars_of_interest_long[2], sep = "") %>%
    asis_output()}
if(!invalid(vars_of_interest[2])) {
  d %>%
    ggplot(aes(x = get(vars_of_interest[2]))) +
    geom_histogram(bins=20, position="dodge") +
    labs(x = vars_of_interest_long[2])}

cat("\n\n") %>% asis_output()
if(!invalid(vars_of_interest[3])) {
  paste("###", vars_of_interest_long[3], sep = "") %>%
    asis_output()}
if(!invalid(vars_of_interest[3])) {
  d %>%
    ggplot(aes(x = get(vars_of_interest[3]))) +
    geom_histogram(bins=20, position="dodge") +
    labs(x = vars_of_interest_long[3])}
## ----score_feedback_means--------------------------------------------------
asis_output("##mean scores for giving feedback in task")
cat("\n") %>% asis_output()

#notetaking to be deleted
#mutate_score = lazyeval::interp(~ ((((b-a)/a)*100)+100), a = as.name(score_vars[1]), b = as.name(score_vars[2]))
#mtcars %>% mutate_(.dots = setNames(list(mutate_call), new_col_name))

if(is.na(score_formula)) {
  d_score = d %>%
    select(one_of(score_vars))
  
  if(length(score_vars) == 1) {
    d_score = d_score %>%
      summarize_(var1_mean = interp(~mean(a, na.rm=T), a = as.name(score_vars[1])),
                 var1_sd = interp(~sd(a, na.rm=T), a = as.name(score_vars[1])))
    cat("var1: ", score_vars) 
  } else if(length(score_vars) == 2) {
    d_score = d_score %>%
      summarize_(var1_mean = interp(~mean(a, na.rm=T), a = as.name(score_vars[1])),
                 var1_sd = interp(~sd(a, na.rm=T), a = as.name(score_vars[1])),
                 var2_mean = interp(~mean(a, na.rm=T), a = as.name(score_vars[2])),
                 var2_sd = interp(~sd(a, na.rm=T), a = as.name(score_vars[2])))
    cat("var1:", score_vars[1], "\nvar2:", score_vars[2])
  }
  
} else if(score_formula == "fraction") {
  cat("score formula: ((((b-a)/a) * 100) + 100) \na:", score_vars[1], "\nb:", score_vars[2])
  d_score = d %>%
    mutate_(score = interp(~((((b-a)/a)*100)+100), a = as.name(score_vars[1]), b = as.name(score_vars[2]))) %>%
    select(score) %>%
    summarize(score_mean = mean(score),
              score_sd = sd(score))
}

print(d_score)

## ----lms-----------------------------------------------------------------
asis_output("##regressions, overall")
cat("\n") %>% asis_output()
paste("###", vars_of_interest_long[1], sep = "") %>%
  asis_output()
d %>%
  with(summary(lm(get(vars_of_interest[1]) ~ age * gender + scale(brt_rt_mean.dominant, scale = Z_SCORE_XREGS))))

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[2])) {
  paste("###", vars_of_interest_long[2], sep = "") %>%
    asis_output()} # dumb: they have to be in two separate if statements to get the heading to render
if(!invalid(vars_of_interest[2])) {
  d %>%
    with(summary(lm(get(vars_of_interest[2]) ~ age * gender + scale(brt_rt_mean.dominant, scale = Z_SCORE_XREGS))))}

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[3])) {
  paste("###", vars_of_interest_long[3], sep = "") %>%
    asis_output()}
if(!invalid(vars_of_interest[3])) {
  d %>%
    with(summary(lm(get(vars_of_interest[3]) ~ age * gender + scale(brt_rt_mean.dominant, scale = Z_SCORE_XREGS))))}

## ----lms_no_brt----------------------------------------------------------
# for ones where acc should not be regressed by BRT RT
asis_output("##regressions")
cat("\n") %>% asis_output()
paste("###", vars_of_interest_long[1], sep = "") %>%
  asis_output()
d %>%
  with(summary(lm(get(vars_of_interest[1]) ~ age * gender)))

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[2])) {
  paste("###", vars_of_interest_long[2], sep = "") %>%
    asis_output()
}
if(!invalid(vars_of_interest[2])) {
  d %>%
    with(summary(lm(get(vars_of_interest[2]) ~ age * gender)))
}

## ----lms_by_acc----------------------------------------------------------
asis_output("##regressions, by response accuracy")
cat("\n") %>% asis_output()
paste("###", vars_of_interest_long[1], sep = "") %>%
  asis_output()
d_acc <- d %>%
  select_("pid", "age", "gender", "brt_rt_mean.overall", correct = vars_of_interest_acc[[1]][1], incorrect = vars_of_interest_acc[[1]][2]) %>%
  gather(acc, rt_mean, -(pid:brt_rt_mean.overall)) %>%
  mutate(acc = as.factor(acc)) # leaving accuracy DUMMY coded here

d_acc %>%
  with(summary(lm(rt_mean ~ age * gender * acc)))

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[2])) {
  paste("###", vars_of_interest_long[2], sep = "") %>%
    asis_output()} # dumb: they have to be in two separate if statements to get the heading to render
if(!invalid(vars_of_interest[2])) {
  d_acc <- d %>%
    select_("pid", "age", "gender", "brt_rt_mean.overall", correct = vars_of_interest_acc[[2]][1], incorrect = vars_of_interest_acc[[2]][2]) %>%
    gather(acc, rt_mean, -(pid:brt_rt_mean.overall)) %>%
    mutate(acc = as.factor(acc)) # leaving accuracy DUMMY coded here
  
  d_acc %>%
    with(summary(lm(rt_mean ~ age * gender * acc)))
}

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[3])) {
  paste("###", vars_of_interest_long[3], sep = "") %>%
    asis_output()} # dumb: they have to be in two separate if statements to get the heading to render
if(!invalid(vars_of_interest[3])) {
  d_acc <- d %>%
    select_("pid", "age", "gender", "brt_rt_mean.overall", correct = vars_of_interest_acc[[3]][1], incorrect = vars_of_interest_acc[[3]][2]) %>%
    gather(acc, rt_mean, -(pid:brt_rt_mean.overall)) %>%
    mutate(acc = as.factor(acc)) # leaving accuracy DUMMY coded here
  
  d_acc %>%
    with(summary(lm(rt_mean ~ age * gender * acc)))
}
## ----graphs--------------------------------------------------------------
asis_output("##graphs")
cat("\n") %>% asis_output()
paste("###", vars_of_interest_long[1], sep = "") %>%
  asis_output()
d %>%
  filter(!is.na(gender)) %>%
  ggplot(aes(x = age, y = get(vars_of_interest[1]), color = factor(gender))) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(y = vars_of_interest_long[1]) +
  guides(color = guide_legend(title = "Gender"))

cat("\n\n") %>% asis_output()
if(!invalid(vars_of_interest[2])) {
  paste("###", vars_of_interest_long[2], sep = "") %>%
    asis_output()}
if(!invalid(vars_of_interest[2])) {
  d %>%
    filter(!is.na(gender)) %>%
    ggplot(aes(x = age, y = get(vars_of_interest[2]), color = factor(gender))) +
    geom_point() +
    geom_smooth(method = lm) +
    labs(y = vars_of_interest_long[2]) +
    guides(color = guide_legend(title = "Gender"))
}

cat("\n\n") %>% asis_output()
if(!invalid(vars_of_interest[3])) {
  paste("###", vars_of_interest_long[3], sep = "") %>%
    asis_output()}
if(!invalid(vars_of_interest[3])) {
  d %>%
    filter(!is.na(gender)) %>%
    ggplot(aes(x = age, y = get(vars_of_interest[3]), color = factor(gender))) +
    geom_point() +
    geom_smooth(method = lm) +
    labs(y = vars_of_interest_long[3]) +
    guides(color = guide_legend(title = "Gender"))
}

## ----graphs_by_acc-------------------------------------------------------
asis_output("##graphs, by response accuracy")
cat("\n") %>% asis_output()
paste("###", vars_of_interest_long[1], sep = "") %>%
  asis_output()
d %>%
  filter(!is.na(gender)) %>%
  ggplot(aes(x = age, y = get(vars_of_interest_acc[[1]][1]), color = factor(gender))) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(y = vars_of_interest_acc_long[[1]][1]) +
  guides(color = guide_legend(title = "Gender"))
d %>%
  filter(!is.na(gender)) %>%
  ggplot(aes(x = age, y = get(vars_of_interest_acc[[1]][2]), color = factor(gender))) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(y = vars_of_interest_acc_long[[1]][2]) +
  guides(color = guide_legend(title = "Gender"))

cat("\n\n") %>% asis_output()
if(!invalid(vars_of_interest[2])) {
  paste("###", vars_of_interest_long[2], sep = "") %>%
    asis_output()}
if(!invalid(vars_of_interest[2])) {
  d %>%
    filter(!is.na(gender)) %>%
    ggplot(aes(x = age, y = get(vars_of_interest_acc[[2]][1]), color = factor(gender))) +
    geom_point() +
    geom_smooth(method = lm) +
    labs(y = vars_of_interest_acc_long[[2]][1]) +
    guides(color = guide_legend(title = "Gender"))
}
if(!invalid(vars_of_interest[2])) {
  d %>%
    filter(!is.na(gender)) %>%
    ggplot(aes(x = age, y = get(vars_of_interest_acc[[2]][2]), color = factor(gender))) +
    geom_point() +
    geom_smooth(method = lm) +
    labs(y = vars_of_interest_acc_long[[2]][2]) +
    guides(color = guide_legend(title = "Gender"))
}

cat("\n\n") %>% asis_output()
if(!invalid(vars_of_interest[3])) {
  paste("###", vars_of_interest_long[3], sep = "") %>%
    asis_output()}
if(!invalid(vars_of_interest[3])) {
  d %>%
    filter(!is.na(gender)) %>%
    ggplot(aes(x = age, y = get(vars_of_interest_acc[[3]][1]), color = factor(gender))) +
    geom_point() +
    geom_smooth(method = lm) +
    labs(y = vars_of_interest_acc_long[[3]][1]) +
    guides(color = guide_legend(title = "Gender"))
}
if(!invalid(vars_of_interest[3])) {
  d %>%
    filter(!is.na(gender)) %>%
    ggplot(aes(x = age, y = get(vars_of_interest_acc[[3]][2]), color = factor(gender))) +
    geom_point() +
    geom_smooth(method = lm) +
    labs(y = vars_of_interest_acc_long[[3]][2]) +
    guides(color = guide_legend(title = "Gender"))
}