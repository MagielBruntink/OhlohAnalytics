
plotTrend <- function(data_df, x_str, y_str) {
  plot(data_df, xlab=x_str, ylab=y_str)
  lfit<-linearRegression(data_df)
  lines(lfit$fit)
  print(lfit$coef)
}

linearRegression <- function(data_df) {
  lfit <- lm(Y ~ X, data=data_df)
}

linearRegressionXY <- function(X_vec, Y_vec) {
  linearRegression(data.frame(X=X_vec,Y=Y_vec))
}