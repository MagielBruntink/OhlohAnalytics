require(data.table)
require(plyr)

augmentWithValidationResult <- function(data_dt, validationName_str, validation_fun) {
  data_dt[,paste(validationName_str,sep="_") := validation_fun(.SD)]
}

goodSVNPatterns <- list(".*/trunk/?",
                        ".*/head/?",
                        ".*/sandbox/?",
                        ".*/site/?",
                        ".*/branches/\\w+",
                        ".*/tags/\\w+")

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
