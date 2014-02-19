
require(plyr)
require(data.table)
require(MASS)

plotTrend <- function(data_df, title_str, xlab_str, ylab_str, regressionFunction) {
  plot(data_df, main=title_str, xlab=xlab_str, ylab=ylab_str)
  fit_obj<-regressionFunction(data_df)
  lines(data_df$X,predict(fit_obj))
  print(summary(fit_obj))
}

linearRegression <- function(data_df) {
  lm(Y ~ X, data=data_df)
}

robustLinearRegression <- function(data_df) {
  rlm(Y ~ X, data=data_df)
}

linearRegressionXY <- function(X_vec, Y_vec) {
  linearRegression(data.frame(X=X_vec,Y=Y_vec))
}

quadraticRegression <- function(data_df) {
  lm(Y ~ X + I(X^2), data=data_df)
}

regressForProject <- function (data_dt, project_id_str, x_var_str, y_var_str, prune_outliers_bool = FALSE) {
  data_for_project <- data_dt[project_id_str]
  data_to_regress <- data.frame(X=as.numeric(data_for_project[[x_var_str]]),
                                Y=as.numeric(data_for_project[[y_var_str]]))
  if (prune_outliers_bool) {
    data_to_regress <- pruneOutliers(data_to_regress)
  }
  testsForNormality(data_to_regress, project_id_str, x_var_str, y_var_str)
  
  plotTrend(data_to_regress, title_str=paste("Linear regression of data for", project_id_str, sep=" "),
            xlab_str=x_var_str,
            ylab_str=y_var_str, linearRegression)
    
  plotTrend(data_to_regress, title_str=paste("Robust Linear regression of data for", project_id_str, sep=" "),
            xlab_str=x_var_str,
            ylab_str=y_var_str, robustLinearRegression)
  
  plotTrend(data_to_regress, title_str=paste("Quadratic regression of data for", project_id_str, sep=" "),
            xlab_str=x_var_str,
            ylab_str=y_var_str, quadraticRegression)
}

regressForAllProjects <- function(data_df, project_id_str, x_var_str, y_var_str, regression_function, prune_outliers_bool = FALSE, prefix_str = "") {
  res <- ddply(data_df,
        project_id_str,
        .fun <- function(df) {
          dfr <- data.frame(X=df[[x_var_str]], Y=df[[y_var_str]])
          if (prune_outliers_bool) {
            dfr <- pruneOutliers(dfr)
          }
          if (nrow(na.omit(dfr)) >= 2) {
            regress_obj <- regression_function(dfr)
            summarise(dfr,
                      regress_coef_1 = regress_obj$coef[2],
                      regress_coef_2 = regress_obj$coef[3],
                      adj_r_squarded = summary(regress_obj)$adj.r.squared)
          }
          else {
            warning("Fewer than 2 non-NA cases")
            summarise(dfr,
                      regress_coef_1 = NA,
                      regress_coef_2 = NA,
                      adj_r_squarded = NA)
          }
        })
  colnames(res)[2:4] <- lapply(colnames(res)[2:4], function(n) {paste(prefix_str, n, sep="")})
  return(res)
}