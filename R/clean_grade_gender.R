require(plyr) # the calling markdown must have these loaded already
require(dplyr)
clean_grade_gender = function(df) {
  df %>%
    mutate(grade = as.numeric(grade),
           grade_label = as.factor(paste("Grade", grade)),
           grade = grade - min(grade), # center to the YOUNGEST grade = 0
           gender = as.factor(gender)) %>%
    within({
      contrasts(gender) = c(-1, 1) # females, then males (expect females to be FASTER)
    })
}