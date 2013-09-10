require(data.table)
require(plyr)

analysis_dir <- "~/git/OhlohAnalytics/validation"

preMonthlyValidationFacts <- data.table(read.csv(paste(analysis_dir,"pre-monthly-validation-facts.csv", sep="/"),
                                      colClasses=c("character",
                                                   "character",
                                                   "character",
                                                   "integer",
                                                   "integer",
                                                   "integer",
                                                   "integer",
                                                   "integer",
                                                   "integer",
                                                   "integer",
                                                   "integer",
                                                   "integer",
                                                   "integer",
                                                   "integer",
                                                   "numeric",
                                                   "integer",
                                                   "integer")))

#rownames(preMonthlyValidationFacts) <-
#         apply(preMonthlyValidationFacts, 1, 
#               function(row) return(paste(row["project_name_fact"],row["year_fact"],row["month_fact"],sep="-")))

setkey(preMonthlyValidationFacts, project_name_fact, year_fact, month_fact)

countFeatures = c("loc_added_fact",
                  "loc_deleted_fact",
                  "comments_added_fact",
                  "comments_deleted_fact",
                  "blanks_added_fact",
                  "blanks_deleted_fact",
                  "commits_fact",
                  "contributors_fact",
                  "loc_fact",
                  "comments_fact",
                  "blanks_fact",
                  "cumulative_commits_fact",
                  "man_months_fact")

ratioFeatures = c("comment_ratio_fact")
		
# NUMBER OF PROJECTS

numberOfProjects <- length(unique(preMonthlyValidationFacts$project_name_fact))
print(paste("Number of projects:", numberOfProjects))

print("MISSING VALUES")

missingValuesMatrix <- is.na.data.frame(preMonthlyValidationFacts)
missingValuesCounts <- table(missingValuesMatrix)
print(paste("Total number of values:", missingValuesCounts["TRUE"] + missingValuesCounts["FALSE"]))
print(paste("Total number of missing values:", missingValuesCounts["TRUE"]))

print("Number of missing values per feature:")
print(sapply(preMonthlyValidationFacts, function(x) sum(is.na(x))))

completeCasesCount <- table(complete.cases(preMonthlyValidationFacts))
print(paste("Total number of cases:", completeCasesCount["FALSE"] + completeCasesCount["TRUE"]))
print(paste("Total number of cases with missing values (in any feature):", completeCasesCount["FALSE"]))

print("IMPLAUSIBLE VALUES")

print("Number of negative values per count or ratio feature:")
print(sapply(subset(preMonthlyValidationFacts, select=c(countFeatures,ratioFeatures)),
       function(x) table(x<0,useNA="ifany")["TRUE"]))

print(paste("Number of cases with negative values (in any count or ratio features):",
             table(apply(subset(preMonthlyValidationFacts, select=c(countFeatures,ratioFeatures)),
                         1, function(case) any(case < 0)),
                   useNA="ifany")["TRUE"]))

print("Number of 0 values per count or ratio feature:")
print(sapply(subset(preMonthlyValidationFacts, select=c(countFeatures,ratioFeatures)),
             function(x) table(x==0,useNA="ifany")["TRUE"]))

print(paste("Number of cases with 0 values (in any count or ratio features):",
            table(apply(subset(preMonthlyValidationFacts, select=c(countFeatures,ratioFeatures)),
                        1, function(case) any(case == 0)),
                  useNA="ifany")["TRUE"]))

print("CONSISTENCY CHECKS")

## loc_fact == previous_month_loc_fact + loc_added_fact - loc_deleted_fact

preMonthlyValidationFacts[,previous_month_loc_fact:=c(NA,
                                                      .SD$loc_fact)
                          [1:length(.SD$loc_fact)],
                          by=project_name_fact][]

preMonthlyValidationFacts[,consistent_loc:=(
  loc_fact==previous_month_loc_fact+
    loc_added_fact-
    loc_deleted_fact)][]

print(paste("Number of cases where 'loc_fact == previous_month_loc_fact + loc_added_fact - loc_deleted_fact' holds."))
print(table(preMonthlyValidationFacts$consistent_loc,useNA="ifany"))

## comments_fact == previous_month_comments_fact + comments_added_fact - comments_deleted_fact


preMonthlyValidationFacts[,previous_month_comments_fact:=c(NA,
                                                           .SD$comments_fact)
                          [1:length(.SD$comments_fact)],
                          by=project_name_fact][]

preMonthlyValidationFacts[,consistent_comments:=(
  comments_fact==previous_month_comments_fact+
    comments_added_fact-
    comments_deleted_fact)][]

print(paste("Number of cases where 'comments_fact == previous_month_comments_fact + comments_added_fact - comments_deleted_fact' holds."))
print(table(preMonthlyValidationFacts$consistent_comments,useNA="ifany"))


## blanks_fact == previous_month_blanks_fact + blanks_added_fact - blanks_deleted_fact

preMonthlyValidationFacts[,previous_month_blanks_fact:=c(NA,
                                                         .SD$blanks_fact)
                          [1:length(.SD$blanks_fact)],
                          by=project_name_fact][]

preMonthlyValidationFacts[,consistent_blanks:=(
  blanks_fact==previous_month_blanks_fact+
    blanks_added_fact-
    blanks_deleted_fact)][]

print(paste("Number of cases where 'blanks_fact == previous_month_blanks_fact + blanks_added_fact - blanks_deleted_fact' holds."))
print(table(preMonthlyValidationFacts$consistent_blanks,useNA="ifany"))

## cumulative_commits == previous_month_cumulative_commits + commits

preMonthlyValidationFacts[,previous_month_cumulative_commits_fact:=c(NA,
                                                         .SD$cumulative_commits_fact)
                          [1:length(.SD$cumulative_commits_fact)],
                          by=project_name_fact][]

preMonthlyValidationFacts[,consistent_commits:=(
  cumulative_commits_fact==previous_month_cumulative_commits_fact +
    commits_fact)][]

print(paste("Number of cases where 'cumulative_commits == previous_month_cumulative_commits + commits_facts' holds."))
print(table(preMonthlyValidationFacts$consistent_commits,useNA="ifany"))
