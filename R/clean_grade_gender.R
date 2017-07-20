require(plyr) # the calling markdown must have these loaded already
require(dplyr)
clean_grade_gender = function(df) {
  df = df %>%
    mutate(grade = as.numeric(grade),
           grade_label = as.factor(paste("Grade", grade)),
           grade_label = na_if(grade_label, "Grade NA"),
           grade = grade - min(grade, na.rm = T), # center to the YOUNGEST grade = 0
           age = na_if(age, ""),
           age = na_if(age, "?"),
           gender = na_if(gender, ""),
           gender = na_if(gender, "?"),
           gender = as.factor(gender)) %>%
    within({
      contrasts(gender) = c(-1, 1) # females, then males (expect females to be FASTER)
    })
  if ("handedness" %in% names(df)) {
    df = df %>%
      mutate(handedness = na_if(handedness, ""),
             handedness = na_if(handedness, "?"))
  }
  return (df)
}
