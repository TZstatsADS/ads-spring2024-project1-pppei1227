# function used to compute statistics in a boxplot
get_box_stats <- function(y, upper_limit = 5 * 1.5) {
  return(data.frame(
    y = 0.95 * upper_limit,
    label = paste(
      "Count =", length(y), "\n",
      "Mean =", round(mean(y), 2), "\n",
      "Median =", round(median(y), 2), "\n"
    )
  ))
}