require(plyr) # the calling markdown must have these loaded already
require(dplyr)
clean_grade_gender = function(df) {
  df %>%
    mutate(grade = as.factor(grade),
           gender = as.factor(gender),
           grade_num = NA) %>%
    within({
      grade_num[grade == "3rd Grade"] = 3
      grade_num[grade == "5th Grade"] = 5
      grade_num[grade == "7th Grade"] = 7
    }) %>%
    mutate(grade_num = grade_num - 3) %>%
    within({
      contrasts(gender) = c(-1, 1)
    })
}