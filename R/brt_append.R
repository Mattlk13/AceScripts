require(plyr) # the calling markdown must have these loaded already
require(dplyr)

brt_append = function(df, brt, ext_demo_data = FALSE) {
  df <- df %>%
    left_join(
      select(brt,
             pid,
             brt_rt_mean.overall = rt_mean.overall,
             brt_rt_median.overall = rt_median.overall,
             brt_rt_mean.dominant = rt_mean.dominant,
             brt_rt_mean.nondominant = rt_mean.nondominant),
      by = c("pid" = "pid"))
  if(ext_demo_data) { df <- df %>%
    select(pid, time, gender:intervention_type, starts_with("brt"), everything())
  } else {
    df <- df %>%
      select(pid, age:time, starts_with("brt"), everything())
  }
  return(df)}
