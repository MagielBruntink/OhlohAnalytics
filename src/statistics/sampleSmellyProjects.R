require(data.table)

options(scipen=1000)

analysis_dir <- "validation"
output_dir <- "/Users/magielbruntink/Google Drive/UVA/Research/Writing/Quality of Software Evolution Data on Ohloh-SQM2014"

monthlyFactsBeforeCleaningAnnotated <- data.table(read.csv(file=paste(analysis_dir,"monthlyFactsBeforeCleaningAnnotated.csv",sep="/"),header=TRUE,sep=","))

smellyProjects <- as.vector(
  unique(
    monthlyFactsBeforeCleaningAnnotated[#case_has_inconsistent_values==TRUE | 
                                             case_has_implausible_value == TRUE]$project_name_fact))

sampledSmellyProjects <- sample(smellyProjects,min(length(smellyProjects),800))

write.csv(sampledSmellyProjects,paste(analysis_dir,"sampledSmellyProjects.csv", sep="/"))
