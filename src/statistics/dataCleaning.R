require(data.table)
require(plyr)

analysis_dir <- "~/git/OhlohAnalytics/validation"

monthlyFactsWithValidationDataBeforeCleaning = copy(monthlyFactsBeforeCleaning)
monthlyFactsDuringCleaning = copy(monthlyFactsBeforeCleaning)
setkey(monthlyFactsDuringCleaning, project_name_fact, year_fact, month_fact)

## Remove comment_ratio_fact feature
monthlyFactsDuringCleaning[,comment_ratio_fact:=NULL]

## Remove any cases with missing values in the count features
for(feature in c(countFeatures)) {
  monthlyFactsDuringCleaning <- subset(monthlyFactsDuringCleaning, subset=!is.na(monthlyFactsDuringCleaning[[feature]]))
}

## Remove any cases with negative values for the count features
for(feature in c(countFeatures)) {
  featureCheckName <- paste("negative_value",feature,sep="_")
  monthlyFactsDuringCleaning <- subset(monthlyFactsDuringCleaning, subset=is.na(monthlyFactsDuringCleaning[[featureCheckName]]) |
                                                            monthlyFactsDuringCleaning[[featureCheckName]] == FALSE)
}

## Remove any cases that are inconsecutive months
monthlyFactsDuringCleaning <- subset(monthlyFactsDuringCleaning,subset=is.na(inconsecutive_month) | inconsecutive_month == FALSE)

## Remove any cases that have a zero value for previous_month_loc (compatibility with Rascal analysis)
#monthlyFactsDuringCleaning <- subset(monthlyFactsDuringCleaning,subset=previous_month_loc_fact > 0)

## Remove any cases that have zero values for all size and activity facts
monthlyFactsDuringCleaning <- subset(monthlyFactsDuringCleaning,subset=is.na(zero_values_size_and_activity_fact) | zero_values_size_and_activity_fact == FALSE)

## Remove any cases with inconsistent values
monthlyFactsDuringCleaning <- subset(monthlyFactsDuringCleaning,subset=is.na(inconsistent_loc) | inconsistent_loc == FALSE)
monthlyFactsDuringCleaning <- subset(monthlyFactsDuringCleaning,subset=is.na(inconsistent_blanks) | inconsistent_blanks == FALSE)
monthlyFactsDuringCleaning <- subset(monthlyFactsDuringCleaning,subset=is.na(inconsistent_comments) | inconsistent_comments == FALSE)
monthlyFactsDuringCleaning <- subset(monthlyFactsDuringCleaning,subset=is.na(inconsistent_commits) | inconsistent_commits == FALSE)

monthlyFactsWithValidationDataAfterCleaning <- copy(monthlyFactsDuringCleaning)
monthlyFactsAfterCleaning <- subset(copy(monthlyFactsDuringCleaning),select=c(keyFeatures,countFeatures))
