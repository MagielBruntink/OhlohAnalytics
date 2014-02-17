
plotTrend <- function(data_df, title_str, xlab_str, ylab_str, regressionFunction) {
  plot(data_df, main=title_str, xlab=xlab_str, ylab=ylab_str)
  fit_obj<-regressionFunction(data_df)
  lines(fit_obj$fit)
  print(summary(fit_obj))
}

linearRegression <- function(data_df) {
  lfit <- lm(Y ~ X, data=data_df)
}

linearRegressionXY <- function(X_vec, Y_vec) {
  linearRegression(data.frame(X=X_vec,Y=Y_vec))
}

quadraticRegression <- function(data_df) {
  qfit <- lm(Y ~ X + I(X^2), data=data_df)
}

regressionAnalysis.growth <- function (data_dt, project_str) {
  data_for_project <- data_dt[project_str]
  data_to_regress <- data.frame(X=as.numeric(data_for_project$age_in_months),
                                Y=as.numeric(data_for_project$loc_fact))
  plotTrend(data_to_regress, title_str=paste("Linear regression of growth data for", project_str, sep=" "),
                             xlab_str="Age in months",
                             ylab_str="LOC", linearRegression)
  plotTrend(data_to_regress, title_str=paste("Quadratic regression of growth data for", project_str, sep=" "),
            xlab_str="Age in months",
            ylab_str="LOC", quadraticRegression)
}