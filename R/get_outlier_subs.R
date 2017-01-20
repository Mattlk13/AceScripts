# iterative outlier checking
# returns the subject IDs of those who are BAD!
# uses grubb's test in the outliers package
require(outliers)
get_outlier_subs = function(vec, id_col, alpha = .05) {
 test = outliers::grubbs.test(vec, two.sided = TRUE)
 bad_subs = NULL
 repeat{
   if(test$p.value > alpha) break
   bad_subs = c(bad_subs, id_col[outlier(vec, logical = TRUE) & !is.na(vec)])
   vec = vec[!outlier(vec, logical = TRUE)]
   if(length(vec) < 3) break
   test = grubbs.test(vec, two.sided = TRUE)
 }
  return(bad_subs)
}