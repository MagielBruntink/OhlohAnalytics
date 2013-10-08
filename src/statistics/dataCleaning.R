require(data.table)
require(plyr)

analysis_dir <- "validation"

monthlyFactsDuringCleaning = copy(monthlyFactsBeforeCleaning)
setkey(monthlyFactsDuringCleaning, project_name_fact, year_fact, month_fact)

## Remove comment_ratio_fact feature
monthlyFactsDuringCleaning[,comment_ratio_fact:=NULL]

## Remove man_months_fact feature
monthlyFactsDuringCleaning[,man_months_fact:=NULL]


## Remove any cases with missing values in the core features
for(feature in c(coreFeatures)) {
  monthlyFactsDuringCleaning <- subset(monthlyFactsDuringCleaning, subset=!is.na(monthlyFactsDuringCleaning[[feature]]))
}

## Remove any cases with negative values for the core features
for(feature in c(coreFeatures)) {
  featureCheckName <- paste("negative_value",feature,sep="_")
  monthlyFactsDuringCleaning <- subset(monthlyFactsDuringCleaning, subset=is.na(monthlyFactsDuringCleaning[[featureCheckName]]) |
                                                            monthlyFactsDuringCleaning[[featureCheckName]] == FALSE)
}

## Remove any cases that are inconsecutive months
#monthlyFactsDuringCleaning <- subset(monthlyFactsDuringCleaning,subset=is.na(inconsecutive_month) | inconsecutive_month == FALSE)

## Remove any cases that have a zero value for previous_month_loc (compatibility with Rascal analysis)
#monthlyFactsDuringCleaning <- subset(monthlyFactsDuringCleaning,subset=previous_month_loc_fact > 0)

## Remove any cases that have zero values for all size facts
monthlyFactsDuringCleaning <- subset(monthlyFactsDuringCleaning,subset=is.na(zero_values_size_facts) | zero_values_size_facts == FALSE)

## Remove any cases with inconsistent values
monthlyFactsDuringCleaning <- subset(monthlyFactsDuringCleaning,subset=is.na(inconsistent_loc) | inconsistent_loc == FALSE)
monthlyFactsDuringCleaning <- subset(monthlyFactsDuringCleaning,subset=is.na(inconsistent_blanks) | inconsistent_blanks == FALSE)
monthlyFactsDuringCleaning <- subset(monthlyFactsDuringCleaning,subset=is.na(inconsistent_comments) | inconsistent_comments == FALSE)
monthlyFactsDuringCleaning <- subset(monthlyFactsDuringCleaning,subset=is.na(inconsistent_commits) | inconsistent_commits == FALSE)
monthlyFactsDuringCleaning <- subset(monthlyFactsDuringCleaning,subset=is.na(inconsistent_commits) | inconsistent_commits_loc == FALSE)
monthlyFactsDuringCleaning <- subset(monthlyFactsDuringCleaning,subset=is.na(inconsistent_commits) | inconsistent_commits_comments == FALSE)
monthlyFactsDuringCleaning <- subset(monthlyFactsDuringCleaning,subset=is.na(inconsistent_commits) | inconsistent_commits_blanks == FALSE)

monthlyFactsAfterCleaning <- subset(copy(monthlyFactsDuringCleaning),select=c(keyFeatures,coreFeatures))
setkey(monthlyFactsAfterCleaning, project_name_fact, year_fact, month_fact)
rm(monthlyFactsDuringCleaning)

# Size the cleaned data set

numberOfProjects <- length(unique(monthlyFactsAfterCleaning$project_name_fact))
print(paste("Number of projects after cleaning:", numberOfProjects))

missingValuesMatrix <- is.na.data.frame(subset(monthlyFactsAfterCleaning,select=coreFeatures))
missingValuesCounts <- table(missingValuesMatrix)
print(paste("Total number of values:", missingValuesCounts["FALSE"]))
print(paste("Total number of missing values (double-check):", missingValuesCounts["TRUE"]))
rm(missingValuesCounts,missingValuesMatrix)

completeCasesCount <- table(complete.cases(monthlyFactsAfterCleaning))
print(paste("Total number of cases:", completeCasesCount["TRUE"]))
print(paste("Total number of cases with missing values (double-check):", completeCasesCount["FALSE"]))

## month_fact != (previous_month + 1) &&| !(month == 1 && previous_month != 12)
augmentWithPreviousMonthFeature(monthlyFactsAfterCleaning, "month_fact", "project_name_fact")
augmentWithPreviousMonthFeature(monthlyFactsAfterCleaning, "year_fact", "project_name_fact")
monthlyFactsAfterCleaning[,inconsecutive_month:=(
  (ymd(paste(year_fact,month_fact,"01",sep="-")) %m+% months(-1))
  !=
  (ymd(paste(previous_month_year_fact,previous_month_month_fact,"01",sep="-")))),
                          by=project_name_fact][]

print(paste("Total number of projects that have inconsecutive months:",
    table(monthlyFactsAfterCleaning[,any(.SD$inconsecutive_month),by=project_name_fact][["V1"]])["TRUE"]))

monthlyFactsAfterCleaning[,previous_month_month_fact:=NULL]
monthlyFactsAfterCleaning[,previous_month_year_fact:=NULL]
monthlyFactsAfterCleaning[,inconsecutive_month:=NULL]

projectsBeforeCleaning<-unique(monthlyFactsBeforeCleaning$project_name_fact)
projectsAfterCleaning<-unique(monthlyFactsAfterCleaning$project_name_fact)
projectsDropped<-setdiff(projectsBeforeCleaning,projectsAfterCleaning)

write.csv(monthlyFactsAfterCleaning, paste(analysis_dir,"monthlyFactsAfterCleaning.csv", sep="/"))
write.csv(projectsDropped, paste(analysis_dir,"projectsDroppedDuringCleaning.csv", sep="/"))
