require(data.table)

options(scipen=1000)

analysis_dir <- "validation"
output_dir <- "/Users/magielbruntink/Google Drive/UVA/Research/Writing/Quality of Software Evolution Data on Ohloh-SQM2014"

monthlyFactsBeforeCleaningAnnotated <- data.table(read.csv(file=paste(analysis_dir,"all","monthlyFactsBeforeCleaningAnnotated.csv",sep="/"),header=TRUE,sep=","))

identifySmellyProjects <- function (dt) {
  as.vector(
    unique(
      dt[#case_has_inconsistent_values==TRUE | 
        case_has_implausible_value == TRUE]$project_name_fact))
}

sampledSmellyProjects <- sample(smellyProjects,min(length(smellyProjects),800))

write.csv(sampledSmellyProjects,paste(analysis_dir, "sample", "sampledSmellyProjects.csv", sep="/"))

### REFETCH PROJECTS IN RASCAL & RE_VALIDATE

monthlyFactsBeforeCleaningAnnotatedSampled <- data.table(read.csv(file=paste(analysis_dir,"sample", "monthlyFactsBeforeCleaningAnnotated.csv",sep="/"),header=TRUE,sep=","))

identifySmellyProjects(monthlyFactsBeforeCleaningAnnotatedSampled)