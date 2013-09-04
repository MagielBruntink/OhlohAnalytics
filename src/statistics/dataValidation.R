analysis_dir <- "~/git/OhlohAnalytics/analysis/searhus"

preMonthlyValidationFacts <- read.csv(paste(analysis_dir,"pre-monthly-validation-facts.csv", sep="/"))

# NUMBER OF PROJECTS

numberOfProjects <- length(unique(preMonthlyValidationFacts$project_name_fact))
format(paste("Number of projects:", numberOfProjects))

# MISSING VALUES

missingValuesMatrix <- is.na.data.frame(preMonthlyValidationFacts)
missingValuesCounts <- table(missingValuesMatrix)
format(paste("Total number of values:", missingValuesCounts["TRUE"] + missingValuesCounts["FALSE"]))
format(paste("Total number of missing values:", missingValuesCounts["TRUE"]))

## FEATURES
format("Number of missing values per feature")
sapply(preMonthlyValidationFacts, function(x) sum(is.na(x)))

## CASES

format("Number of cases with missing values (in any feature)")
completeCasesCount <- table(complete.cases(preMonthlyValidationFacts))
format(paste("Total number of cases:", completeCasesCount["FALSE"] + completeCasesCount["TRUE"]))
format(paste("Total number of cases with missing values:", completeCasesCount["FALSE"]))


# IMPLAUSIBLE VALUES



