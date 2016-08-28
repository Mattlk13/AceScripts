require(plyr) # the calling markdown must have these loaded already
require(dplyr)

brt_append = function(df) {
  df %>%
    select(pid, time, gender:intervention_type, everything()) %>%
    select(-bid, -file) %>%
    left_join(
      select(brt,
             pid,
             brt_rt_mean.overall = rt_mean.overall,
             brt_rt_median.overall = rt_median.overall,
             brt_rt_mean.right = rt_mean.right,
             brt_rt_median.right = rt_median.right,
             brt_rt_mean.left = rt_mean.left,
             brt_rt_median.left = rt_median.left),
      by = c("pid" = "pid")) %>%
    select(pid:intervention_type, starts_with("brt"), everything())
}