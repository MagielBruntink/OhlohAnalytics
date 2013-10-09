require(data.table)
require(plyr)

analysis_dir <- "validation"

monthlyFactsDuringCleaning = copy(monthlyFactsBeforeCleaning)
setkey(monthlyFactsDuringCleaning, project_name_fact, year_fact, month_fact)

## Remove comment_ratio_fact feature
monthlyFactsDuringCleaning[,comment_ratio_fact:=NULL]

## Remove man_months_fact feature
monthlyFactsDuringCleaning[,man_months_fact:=NULL]

## Re-identify missing values
monthlyFactsDuringCleaning <- cbind(monthlyFactsDuringCleaning,
                                    data.frame(case_has_missing_values = 
                                                 !complete.cases(subset(
                                                   monthlyFactsBeforeCleaning,select=coreFeatures))))

## Re-identify implausible values
monthlyFactsDuringCleaning[,case_has_implausible_value:=(negative_value_loc_fact==TRUE |
                                                           negative_value_loc_added_fact==TRUE |
                                                           negative_value_loc_deleted_fact==TRUE |
                                                           negative_value_blanks_fact==TRUE |
                                                           negative_value_blanks_added_fact==TRUE |
                                                           negative_value_blanks_deleted_fact==TRUE |
                                                           negative_value_comments_fact==TRUE |
                                                           negative_value_comments_added_fact==TRUE |
                                                           negative_value_comments_deleted_fact==TRUE |
                                                           negative_value_commits_fact==TRUE |
                                                           negative_value_contributors_fact==TRUE)][]

## Re-identify inconsistent values
monthlyFactsDuringCleaning[,case_has_inconsistent_values:=(zero_values_size_facts==TRUE |
                                                             inconsistent_loc==TRUE |
                                                             inconsistent_comments==TRUE |
                                                             inconsistent_blanks==TRUE |
                                                             inconsistent_commits==TRUE |
                                                             inconsistent_commits_loc==TRUE |
                                                             inconsistent_commits_comments==TRUE |
                                                             inconsistent_commits_blanks==TRUE)][]

write.csv(monthlyFactsDuringCleaning, paste(analysis_dir,"monthlyFactsBeforeCleaningCasesAnnotated.csv", sep="/"))

## Remove any cases with missing values in the core features
monthlyFactsDuringCleaning <- subset(monthlyFactsDuringCleaning, case_has_missing_values==FALSE)

## Remove any cases with implausible values for the core features
monthlyFactsDuringCleaning <- subset(monthlyFactsDuringCleaning, case_has_implausible_value==FALSE)

## Remove any cases that are inconsecutive months
#monthlyFactsDuringCleaning <- subset(monthlyFactsDuringCleaning,subset=is.na(inconsecutive_month) | inconsecutive_month == FALSE)

## Remove any cases that have a zero value for previous_month_loc (compatibility with Rascal analysis)
#monthlyFactsDuringCleaning <- subset(monthlyFactsDuringCleaning,subset=previous_month_loc_fact > 0)

## Remove any cases that have zero values for all size facts
monthlyFactsDuringCleaning <- subset(monthlyFactsDuringCleaning,subset=is.na(zero_values_size_facts) | zero_values_size_facts == FALSE)

## Remove any cases with inconsistent values
monthlyFactsDuringCleaning <- subset(monthlyFactsDuringCleaning,subset=is.na(case_has_inconsistent_values) | case_has_inconsistent_values == FALSE)

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
