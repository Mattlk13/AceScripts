require(plyr) # the calling markdown must have these loaded already
require(dplyr)

brt_append = function(df, ext_demo_data = FALSE) {
  df <- df %>%
    left_join(
      select(brt,
             pid,
             brt_rt_mean.overall = rt_mean.overall,
             brt_rt_median.overall = rt_median.overall,
             brt_rt_mean.right = rt_mean.right,
             brt_rt_median.right = rt_median.right,
             brt_rt_mean.left = rt_mean.left,
             brt_rt_median.left = rt_median.left),
      by = c("pid" = "pid"))
  if(ext_demo_data) { df <- df %>%
    select(pid, time, gender:intervention_type, starts_with("brt"), everything())
  } else {
    df <- df %>%
      select(pid, age:time, starts_with("brt"), everything())
  }
  return(df)}