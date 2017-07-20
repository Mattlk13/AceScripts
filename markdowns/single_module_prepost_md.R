## ----df_from_proc_demo---------------------------------------------------
# should I expect the cached nice data frame list to be called proc_demo? I guess so
if(this_task == "BRT" & !BRT_PRINT) {} else { # edge case where BRT needs to be evaluated even if no print (and thus header turned off)
  paste0("#", this_task_long) %>%
    asis_output()}
cat("\n") %>% asis_output() # need this for line breaking

d_pre_raw <- proc_pre[[this_task]]
d_post_raw <- proc_post[[this_task]]

# d_raw <- d_raw[grep(paste0(prefixes, collapse = "|"), d_raw$pid),] # only including data from real Ss (with school-specific ID prefixes)

## ----rm_outliers---------------------------------------------
# the approach is taken here to remove a subject from ALL metrics of a module
# if they are an outlier in ANY metrics of that module, to improve interpretation b/w metrics
# any outliers removed in the BRT data will be excluded from ALL metrics for ALL modules.

bad_subs = vector("list", length(unique(d_raw$grade)))
for (i in 1:length(vars_of_interest)) {# for each grade, concatenates outliers ACROSS metrics of interest
  for (j in 1:length(unique(d_raw$grade))) {
    # need this to verify that there are >2 valid data pts in this grade level to do the outlier test
    these_vals_temp = with(d_raw, get(vars_of_interest[i])[grade == unique(grade)[j] & !is.na(get(vars_of_interest[i]))])
    if(length(these_vals_temp) > 2) {
      these_bad_subs = d_raw %>%
        filter(grade == unique(grade)[j]) %>%
        with(
          get_outlier_subs(get(vars_of_interest[i]), pid))
    } else {these_bad_subs = NULL}
    if(!is.null(these_bad_subs)) {bad_subs[[j]] <- c(bad_subs[[j]], these_bad_subs)}
  }
}

d <- d_raw %>%
  filter(!(pid %in% unlist(bad_subs)))
## ----preproc-------------------------------------------------------------
if(this_task %in% c("SPATIALSPAN", "BACKWARDSSPATIALSPAN")) {d <- d_raw} # dumb edge case, can't run rm_outliers on span so must init d here
if(this_task == "BRT") { # edge case, don't append BRT to itself
  d <- d %>%
    clean_grade_gender()
} else {
  d <- d %>%
    brt_append() %>%
    clean_grade_gender()}
# NB: gender is effect coded female: -1 male: 1
# such that a POSITIVE beta estimate for RT change by gender indicates that FEMALES are faster (RT for males HIGHER)

asis_output("##n by grade and gender")
cat("\n") %>% asis_output()
table(d$grade_label, d$gender)

## ----preproc_pre_vs_post-------------------------------------------------
if(this_task == "BRT") { # edge case, don't append BRT to itself
  d_pre <- d_pre %>%
    select(pid, grade:gender, grade_label, one_of(vars_of_interest))
  d_post <- d_post %>%
    select(pid, grade:gender, grade_label, one_of(vars_of_interest))
  d_all <- d_pre %>%
    inner_join(d_post, by = c("pid", "grade", "grade_label", "gender"), suffix = c(".pre", ".post"))
} else {
  d_pre <- d_pre %>%
    select(pid, grade:gender, grade_label, starts_with("brt"), one_of(vars_of_interest))
  d_post <- d_post %>%
    select(pid, grade:gender, grade_label, starts_with("brt"), one_of(vars_of_interest))
  d_all <- d_pre %>%
    inner_join(d_post, by = c("pid", "grade", "grade_label", "gender"), suffix = c(".pre", ".post")) %>%
    mutate(
      brt_rt_mean.overall.diff = brt_rt_mean.overall.post - brt_rt_mean.overall.pre,
      brt_rt_median.overall.diff = brt_rt_median.overall.post - brt_rt_median.overall.pre,
      brt_rt_mean.right.diff = brt_rt_mean.right.post - brt_rt_mean.right.pre,
      brt_rt_median.right.diff = brt_rt_median.right.post - brt_rt_median.right.pre,
      brt_rt_mean.left.diff = brt_rt_mean.left.post - brt_rt_mean.left.pre,
      brt_rt_median.left.diff = brt_rt_median.left.post - brt_rt_median.left.pre
    )
}
d_all <- mutate_(d_all, var1_diff = interp(~(b - a), a = as.name(paste0(vars_of_interest[1], ".post")), b = as.name(paste0(vars_of_interest[1], ".pre"))))
if(!invalid(vars_of_interest[2])) {
  d_all <- mutate_(d_all, var2_diff = interp(~(b - a), a = as.name(paste0(vars_of_interest[2], ".post")), b = as.name(paste0(vars_of_interest[2], ".pre"))))
}
if(!invalid(vars_of_interest[3])) {
  d_all <- mutate_(d_all, var3_diff = interp(~(b - a), a = as.name(paste0(vars_of_interest[3], ".post")), b = as.name(paste0(vars_of_interest[3], ".pre"))))
}


## ----qc_graphs-----------------------------------------------------------
asis_output("##quality control histograms")
cat("\n") %>% asis_output()
paste("###", vars_of_interest_long[1], sep = "") %>%
  asis_output()
d %>%
  ggplot(aes(x = get(vars_of_interest[1]), fill = factor(grade_label))) +
  geom_histogram(bins=20, position="dodge") +
  labs(x = vars_of_interest_long[1]) +
  guides(fill = guide_legend(title = "Grade"))

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[2])) {
  paste("###", vars_of_interest_long[2], sep = "") %>%
    asis_output()}
if(!invalid(vars_of_interest[2])) {
  d %>%
    ggplot(aes(x = get(vars_of_interest[2]), fill = factor(grade_label))) +
    geom_histogram(bins=20, position="dodge") +
    labs(x = vars_of_interest_long[2]) +
    guides(fill = guide_legend(title = "Grade"))}

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[3])) {
  paste("###", vars_of_interest_long[3], sep = "") %>%
    asis_output()}
if(!invalid(vars_of_interest[3])) {
  d %>%
    ggplot(aes(x = get(vars_of_interest[3]), fill = factor(grade_label))) +
    geom_histogram(bins=20, position="dodge") +
    labs(x = vars_of_interest_long[3]) +
    guides(fill = guide_legend(title = "Grade"))}
## ----score_feedback_means--------------------------------------------------
asis_output("##mean scores for giving feedback in task")
cat("\n") %>% asis_output()

#notetaking to be deleted
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
  with(summary(lm(get(vars_of_interest[1]) ~ grade * gender)))

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[2])) {
  paste("###", vars_of_interest_long[2], sep = "") %>%
    asis_output()} # dumb: they have to be in two separate if statements to get the heading to render
if(!invalid(vars_of_interest[2])) {
  d %>%
    with(summary(lm(get(vars_of_interest[2]) ~ grade * gender)))}

## ----lms-----------------------------------------------------------------
asis_output("##regressions")
cat("\n") %>% asis_output()
paste("###", vars_of_interest_long[1], sep = "") %>%
  asis_output()
d %>%
  with(summary(lm(get(vars_of_interest[1]) ~ grade * gender + scale(brt_rt_mean.overall, scale = Z_SCORE_XREGS))))

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[2])) {
  paste("###", vars_of_interest_long[2], sep = "") %>%
    asis_output()} # dumb: they have to be in two separate if statements to get the heading to render
if(!invalid(vars_of_interest[2])) {
  d %>%
    with(summary(lm(get(vars_of_interest[2]) ~ grade * gender + scale(brt_rt_mean.overall, scale = Z_SCORE_XREGS))))}

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[3])) {
  paste("###", vars_of_interest_long[3], sep = "") %>%
    asis_output()}
if(!invalid(vars_of_interest[3])) {
  d %>%
    with(summary(lm(get(vars_of_interest[3]) ~ grade * gender + scale(brt_rt_mean.overall, scale = Z_SCORE_XREGS))))}

## ----lms_brt_only_pre_vs_post----------------------------------------------
# edge case here where BRT can't be regressed against itself
asis_output("##regressions")
cat("\n") %>% asis_output()
paste("###", vars_of_interest_long[1], sep = "") %>%
  asis_output()
d_all %>%
  with(summary(lm(var1_diff ~ grade * gender)))

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[2])) {
  paste("###", vars_of_interest_long[2], sep = "") %>%
    asis_output()} # dumb: they have to be in two separate if statements to get the heading to render
if(!invalid(vars_of_interest[2])) {
  d_all %>%
    with(summary(lm(var2_diff ~ grade * gender)))}

## ----lms_pre_vs_post-------------------------------------------------------
asis_output("##regressions")
cat("\n") %>% asis_output()
paste("###", vars_of_interest_long[1], sep = "") %>%
  asis_output()
d_all %>%
  with(summary(lm(var1_diff ~ grade * gender + scale(brt_rt_mean.overall.diff, scale = Z_SCORE_XREGS))))

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[2])) {
  paste("###", vars_of_interest_long[2], sep = "") %>%
    asis_output()} # dumb: they have to be in two separate if statements to get the heading to render
if(!invalid(vars_of_interest[2])) {
  d_all %>%
    with(summary(lm(var2_diff ~ grade * gender + scale(brt_rt_mean.overall.diff, scale = Z_SCORE_XREGS))))}

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[3])) {
  paste("###", vars_of_interest_long[3], sep = "") %>%
    asis_output()}
if(!invalid(vars_of_interest[3])) {
  d_all %>%
    with(summary(lm(var3_diff ~ grade * gender + scale(brt_rt_mean.overall.diff, scale = Z_SCORE_XREGS))))}

## ----lms_filter_only-----------------------------------------------------
# this one has to be special because we're also regressing by distractor number which doesn't apply for other modules
asis_output("##regressions")
cat("\n") %>% asis_output()
paste("###", vars_of_interest_long[1], sep = "") %>%
  asis_output()
d %>%
  with(summary(lm(get(vars_of_interest[1]) ~ grade * gender * distractors + scale(brt_rt_mean.overall, scale = Z_SCORE_XREGS))))

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[2])) {
  paste("###", vars_of_interest_long[2], sep = "") %>%
    asis_output()} # dumb: they have to be in two separate if statements to get the heading to render
if(!invalid(vars_of_interest[2])) {
  d %>%
    with(summary(lm(get(vars_of_interest[2]) ~ grade * gender * distractors + scale(brt_rt_mean.overall, scale = Z_SCORE_XREGS))))}

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[3])) {
  paste("###", vars_of_interest_long[3], sep = "") %>%
    asis_output()}
if(!invalid(vars_of_interest[3])) {
  d %>%
    with(summary(lm(get(vars_of_interest[3]) ~ grade * gender * distractors + scale(brt_rt_mean.overall, scale = Z_SCORE_XREGS))))}

## ----graphs--------------------------------------------------------------
asis_output("##graphs")
cat("\n") %>% asis_output()
paste("###", vars_of_interest_long[1], sep = "") %>%
  asis_output()
d %>%
  ggplot(aes(x = grade_label, y = get(vars_of_interest[1]), fill = factor(gender))) +
  geom_boxplot() +
  labs(y = vars_of_interest_long[1]) +
  guides(fill = guide_legend(title = "Gender"))

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[2])) {
  paste("###", vars_of_interest_long[2], sep = "") %>%
    asis_output()}
if(!invalid(vars_of_interest[2])) {
  d %>%
    ggplot(aes(x = grade_label, y = get(vars_of_interest[2]), fill = factor(gender))) +
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
    ggplot(aes(x = grade_label, y = get(vars_of_interest[3]), fill = factor(gender))) +
    geom_boxplot() +
    labs(y = vars_of_interest_long[3]) +
    guides(fill = guide_legend(title = "Gender"))
}

## ----graphs_pre_vs_post--------------------------------------------------
asis_output("##graphs")
cat("\n") %>% asis_output()
paste("###", vars_of_interest_long[1], sep = "") %>%
  asis_output()
d_all %>%
  ggplot(aes(x = grade_label, y = var1_diff, fill = factor(gender))) +
  geom_boxplot() +
  labs(y = vars_of_interest_long[1]) +
  guides(fill = guide_legend(title = "Gender"))

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[2])) {
  paste("###", vars_of_interest_long[2], sep = "") %>%
    asis_output()}
if(!invalid(vars_of_interest[2])) {
  d_all %>%
    ggplot(aes(x = grade_label, y = var2_diff, fill = factor(gender))) +
    geom_boxplot() +
    labs(y = vars_of_interest_long[2]) +
    guides(fill = guide_legend(title = "Gender"))
}

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[3])) {
  paste("###", vars_of_interest_long[3], sep = "") %>%
    asis_output()}
if(!invalid(vars_of_interest[3])) {
  d_all %>%
    ggplot(aes(x = grade_label, y = var3_diff, fill = factor(gender))) +
    geom_boxplot() +
    labs(y = vars_of_interest_long[3]) +
    guides(fill = guide_legend(title = "Gender"))
}

## ----graphs_filter_only--------------------------------------------------
# again special because need to plot by distractor number
asis_output("##graphs")
cat("\n") %>% asis_output()
paste("###", vars_of_interest_long[1], sep = "") %>%
  asis_output()

d_plot <- d %>%
  group_by(grade_label, distractors) %>%
  summarize(k.2.mean = mean(k.2, na.rm = T),
            k.4.mean = mean(k.4, na.rm = T),
            k.2.se = sd(k.2, na.rm = T)/sqrt(length(!is.na(k.2))),
            k.4.se = sd(k.4, na.rm = T)/sqrt(!is.na(length(k.4))))

d_plot %>%
  ggplot(aes(x = distractors, y = get(paste0(vars_of_interest[1], ".mean")), color = factor(grade_label))) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = get(paste0(vars_of_interest[1], ".mean")) - get(paste0(vars_of_interest[1], ".se")),
                    ymax = get(paste0(vars_of_interest[1], ".mean")) + get(paste0(vars_of_interest[1], ".se")),
                    width = 0.5)) +
  labs(y = vars_of_interest_long[1]) +
  guides(color = guide_legend(title = "Grade"))

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[2])) {
  paste("###", vars_of_interest_long[2], sep = "") %>%
    asis_output()}
if(!invalid(vars_of_interest[2])) {
  d_plot %>%
    ggplot(aes(x = distractors, y = get(paste0(vars_of_interest[2], ".mean")), color = factor(grade_label))) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = get(paste0(vars_of_interest[2], ".mean")) - get(paste0(vars_of_interest[2], ".se")),
                      ymax = get(paste0(vars_of_interest[2], ".mean")) + get(paste0(vars_of_interest[2], ".se")),
                      width = 0.5)) +
    labs(y = vars_of_interest_long[2]) +
    guides(color = guide_legend(title = "Grade"))
}

cat("\n") %>% asis_output()
if(!invalid(vars_of_interest[3])) {
  paste("###", vars_of_interest_long[3], sep = "") %>%
    asis_output()}
if(!invalid(vars_of_interest[3])) {
  d_plot %>%
    ggplot(aes(x = distractors, y = get(paste0(vars_of_interest[3], ".mean")), color = factor(grade_label))) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = get(paste0(vars_of_interest[3], ".mean")) - get(paste0(vars_of_interest[3], ".se")),
                      ymax = get(paste0(vars_of_interest[3], ".mean")) + get(paste0(vars_of_interest[3], ".se")),
                      width = 0.5)) +
    labs(y = vars_of_interest_long[3]) +
    guides(color = guide_legend(title = "Grade"))
}