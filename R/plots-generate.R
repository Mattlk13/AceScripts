#' Generate the Rmd ready plots and stats summaries
#'
#' Prints them out, used in the Rmd files
#'
#' @export
#' @param df The dataframe
#' @param x X-axis boxplot variable
#' @param stat_type the statistic we're interested in
#' @param cohort cohort to break out boxplots by

generate_plots_and_stats <- function (df, x, stat_type, cohort = NULL) {
  ace_formula_str = paste0(ace_y, " ~ ", x)
  ace_formula = formula(ace_formula_str)
  ace_title = paste(df$module[1], ":", stat_type, "by", x)
  ace_plot = make_box_plot(df, x, stat_type, ace_title, cohort = cohort)
  print(ace_plot)
  if (!is.null(cohort)) {
    groups = split( df , f = df[,cohort] )
    for (group in groups) {
      print_summaries(remove_outliers(group, stat_type), ace_formula, ace_formula_str)
    }
    xace_formula_str = paste0(ace_y, " ~ ", cohort)
    xace_formula = formula(xace_formula_str)
    xgroups = split( df , f = df[,x] )
    for (xgroup in xgroups) {
      print_summaries(remove_outliers(xgroup, stat_type), xace_formula, xace_formula_str)
    }
  } else {
    ace_formula_str = paste0(ace_y, " ~ ", x)
    ace_formula = formula(ace_formula_str)
    print_summaries(remove_outliers(df, stat_type), ace_formula, ace_formula_str)

    iace_formula_str = paste0(ace_y, " ~ grade * gender")
    iace_formula = formula(iace_formula_str)
    print_summaries(remove_outliers(df, stat_type), iace_formula, iace_formula_str)
  }
}

#' @keywords internal


remove_outliers <- function(x, column, ...) {
  lower_range = boxplot.stats(x[,column])$stats[1]
  upper_range = boxplot.stats(x[,column])$stats[5]
  valid_rows = which(x[,column] >= lower_range & x[,column] <= upper_range)
  
  return (x[valid_rows,])
}

#' @keywords internal

print_summaries <- function (df, ace_formula, ace_formula_str) {
  ace_mod = lm(formula = ace_formula, data = df)
  ace_mod_summary = summary(ace_mod)

  ace_mod_summary$call = paste(df$module[1], ":", ace_formula_str, sep = " ")

  print(ace_mod_summary)
}