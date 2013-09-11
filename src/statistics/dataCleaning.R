require(data.table)
require(plyr)

analysis_dir <- "~/git/OhlohAnalytics/validation"

cleanedMonthlyFacts = copy(preMonthlyValidationFacts)
setkey(cleanedMonthlyFacts, project_name_fact, year_fact, month_fact)

## Remove comment_ratio_fact feature
cleanedMonthlyFacts[,comment_ratio_fact:=NULL]

## Remove any cases with missing values in the count features
for(feature in c(countFeatures)) {
  cleanedMonthlyFacts <- subset(cleanedMonthlyFacts, subset=!is.na(cleanedMonthlyFacts[[feature]]))
}

## Remove any cases with negative values for the count features
for(feature in c(countFeatures)) {
  featureCheckName <- paste("negative_value",feature,sep="_")
  cleanedMonthlyFacts <- subset(cleanedMonthlyFacts, subset=is.na(cleanedMonthlyFacts[[featureCheckName]]) |
                                                            cleanedMonthlyFacts[[featureCheckName]] == FALSE)
}

## Remove any cases that are inconsecutive months
cleanedMonthlyFacts <- subset(cleanedMonthlyFacts,subset=is.na(inconsecutive_month) | inconsecutive_month == FALSE)

## Remove any cases with inconsistent values
cleanedMonthlyFacts <- subset(cleanedMonthlyFacts,subset=is.na(inconsistent_loc) | inconsistent_loc == FALSE)
cleanedMonthlyFacts <- subset(cleanedMonthlyFacts,subset=is.na(inconsistent_blanks) | inconsistent_blanks == FALSE)
cleanedMonthlyFacts <- subset(cleanedMonthlyFacts,subset=is.na(inconsistent_comments) | inconsistent_comments == FALSE)
cleanedMonthlyFacts <- subset(cleanedMonthlyFacts,subset=is.na(inconsistent_commits) | inconsistent_commits == FALSE)
