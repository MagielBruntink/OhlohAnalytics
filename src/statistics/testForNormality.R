require(data.table)

testsForNormality <- function (data_df, project_id_str, x_var_str, y_var_str) {
  hist(data_df$Y, breaks=20, main=paste("Histogram for", project_id_str, sep=" "), xlab=y_var_str)
  print(shapiro.test(data_df$Y))
  qqnorm(data_df$Y, main=paste("Normal Q-Q Plot for", project_id_str, sep=" "))
}

testsForNormality.example <- function (data_df, project_str, x_str, x_name_str, y_str, y_name_str, pruneOutliers=FALSE) {
  dataForProject <- data_df[project_str]
  selectedData <- data.frame(X = dataForProject[[x_str]], Y = dataForProject[[y_str]])
  if (pruneOutliers) {
    selectedData <- pruneOutliers(selectedData)
  }
  testsForNormality(selectedData, x_name_str, y_name_str, project_str)
}

pruneOutliers <- function(data_df) {
  outliers <- boxplot.stats(data_df$Y)$out
  return (subset(data_df, subset=!(Y %in% outliers)))
}




