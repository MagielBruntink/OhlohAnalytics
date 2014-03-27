require(data.table)

goodSVNPatterns <- list(".*/trunk/?",
                        ".*/head/?",
                        ".*/sandbox/?",
                        ".*/site/?",
                        ".*/branches/\\w+",
                        ".*/tags/\\w+")

validateAllData <- function() {
  activityFacts <- data.table(read.csv("output/ActivityFacts.csv"))
  setkey(activityFacts, Project_ID, Year_Month)
  sizeFacts <- data.table(read.csv("output/SizeFacts.csv"))
  setkey(sizeFacts, Project_ID, Year_Month)
  mergedFacts <- merge(activityFacts, sizeFacts, all=TRUE)
  enlistments <- data.table(read.csv("output/Enlistments.csv"))
  metaData <- data.table(read.csv("output/MetaData.csv"))
}

validateEnlistments <- function(enlistments_dt) {
  validationResult <- data.frame(
    Validation_Bad_Enlistment =
      mapply(function(Type, URL) {
        (Type == "SvnRepository" | Type == "SvnSyncRepository") &
          all(lapply(goodSVNPatterns,
                     function(p) {
                       any(grepl(p, URL, ignore.case = TRUE)
                           == TRUE)
                     })
              == FALSE)
      },
      enlistments_dt$Type,
      enlistments_dt$URL))
  return(cbind(enlistments_dt, data.table(validationResult)))
}

augmentFactsWithValidatedEnlistments <- function(facts_dt, enlistments_dt) {
  validatedEnlistments <- validateEnlistments(enlistments_dt)
  facts_dt[list(Project_ID, Year_Month), 
           Validation_Bad_Enlistment := any(validatedEnlistments[Project_ID]$Validation_Bad_Enlistment)]
}
