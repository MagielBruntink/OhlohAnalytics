require(ggplot2)
require(survival)
require(plyr)

options(scipen=1000)

theme_set(theme_grey(base_size = 18))

analysis_dir <- "~/git/OhlohAnalytics/analysis/hossin"
output_dir <- "/Users/magielbruntink/Google Drive/UVA/Research/Writing/Future of Understanding Software"

monthlyFactsNoCutOff <- read.csv(file=paste(analysis_dir,"allMonthlyFacts.csv",sep="/"),header=TRUE,sep=",")
monthlyFactsByYearNoCutOff <- read.csv(file=paste(analysis_dir,"allYearlyFacts.csv",sep="/"),header=TRUE,sep=",")

cutOffYear <- 2012

monthlyFacts <- subset(monthlyFactsNoCutOff,subset=year<=cutOffYear)
monthlyFactsByYear <- subset(monthlyFactsByYearNoCutOff,subset=year<=cutOffYear)

projectDeath <- read.csv(file=paste(analysis_dir, "projectDeathStatus.csv",sep="/"),header=TRUE,sep=",")
projectRepositoryFacts <- read.csv(file=paste(analysis_dir, "projectRepositoryFacts.csv",sep="/"),header=TRUE,sep=",")

totalLOC <- sum(as.numeric(subset(monthlyFactsByYear,year==cutOffYear)$max_loc_total))

### Boxplots
boxPlot <- function(dataFrame,xLabel,yLabel,fileName,zoom=TRUE,statsDetailLevel=2) {
  allWhiskerLimits = ddply(dataFrame, .(xData), summarise, 
                           whiskerLimMin = boxplot.stats(yData)$stats[c(1)], 
                           whiskerLimMax = boxplot.stats(yData)$stats[c(5)])
  allObsCounts     = ddply(dataFrame, .(xData), summarise, obsCount = length(yData))
  allOutlierCounts = ddply(dataFrame, .(xData), summarise, outlierCount = length(boxplot.stats(yData)$out))  
  allMedians       = ddply(dataFrame, .(xData), summarise, medianValue = round(median(yData),digits=3))
  allIQRs          = ddply(dataFrame, .(xData), summarise, iqr = round(IQR(yData),digits=3))
  allLabelData     = cbind(allObsCounts,allOutlierCounts,allMedians,allIQRs)
  
  plotObj <- ggplot(dataFrame,aes(x=xData,y=yData)) +
    geom_boxplot() +
    labs (x=xLabel,y=yLabel) +
    geom_hline(aes(yintercept=1.000))
  
  if(statsDetailLevel==2) {
    plotObj = plotObj + 
              geom_text(data=allLabelData,
                        aes(x = xData,
                            y = medianValue,
                              label = paste("Med. =", medianValue,
                                          "Obs. =", obsCount,
                                          "Out. =", outlierCount,
                                          "IQR =", iqr,
                                          sep=" ")),
              size = 4, vjust = -1.5)
  }
  
  if(statsDetailLevel==1) {
    plotObj = plotObj + 
      geom_text(data=allLabelData,
                aes(x = xData,
                    y = medianValue,
                    label = paste("Med. =", medianValue,
                                  "Obs. =", obsCount,
                                  "IQR =", iqr,
                                  sep=" ")),
                size = 3, vjust = -1.5)
  }
  
  if(zoom) {
    whiskerLimits = c(min(allWhiskerLimits$whiskerLimMin),max(allWhiskerLimits$whiskerLimMax))
    plotObj = plotObj + coord_cartesian(ylim = whiskerLimits*1.05)
  }
  
  ggsave(file=paste(output_dir,fileName,sep="/"),plot=plotObj)
}

### Scatter plots
scatterPlot <- function(dataFrame,xLabel,yLabel,fileName,zoom=TRUE) {
  plotObj <- ggplot(dataFrame,aes(x=xData,y=yData)) +
    geom_point() +
    labs (x=xLabel,y=yLabel)
  
  ggsave(file=paste(output_dir,fileName,sep="/"),plot=plotObj)
}


######### CODE SIZE

dataSelection = subset(monthlyFactsByYear,year==2012)
dataToPlot <- data.frame(xData="Projects in 2012",yData=dataSelection$max_loc_total)
boxPlot(dataToPlot,
        "",
        "Code Size (CS)",
        "boxplot-codesize-2012.pdf")

dataToPlot <- data.frame(xData=factor(monthlyFactsByYear$age),yData=monthlyFactsByYear$max_loc_total)
boxPlot(dataToPlot,
        "Project age (years)",
        "Code Size (CS)",
        "boxplots-codesize-by-age.pdf",
        TRUE,0)

dataSelection = subset(monthlyFactsByYear,age<=5)
dataToPlot <- data.frame(xData=factor(dataSelection$age),yData=dataSelection$max_loc_total)
boxPlot(dataToPlot,
        "Project age (years)",
        "Code Size (CS)",
        "boxplots-codesize-by-age-5-years.pdf",
        TRUE,0)


######### CODE GROWTH INDEXED

dataToPlot <- data.frame(xData="All project years",yData=monthlyFactsByYear$prod_loc_growth_factor)
boxPlot(dataToPlot,
        "",
        "Code Growth indexed (CGi)",
        "boxplot-codegrowth-indexed.pdf")

dataToPlot <- data.frame(xData=factor(monthlyFactsByYear$age),yData=monthlyFactsByYear$prod_loc_growth_factor)
boxPlot(dataToPlot,
        "Project age (years)",
        "Code Growth indexed (CGi)",
        "boxplots-codegrowth-indexed-by-age.pdf",
        TRUE,0)

dataSelection = subset(monthlyFactsByYear,age<=5)
dataToPlot <- data.frame(xData=factor(dataSelection$age),yData=dataSelection$prod_loc_growth_factor)
boxPlot(dataToPlot,
        "Project age (years)",
        "Code Growth indexed (CGi)",
        "boxplots-codegrowth-indexed-by-age-5-years.pdf",
        TRUE,0)

dataSelection = subset(monthlyFactsByYear,age<=20)
dataToPlot <- data.frame(xData=factor(dataSelection$age),yData=dataSelection$prod_loc_growth_factor)
boxPlot(dataToPlot,
        "Project age (years)",
        "Code Growth indexed (CGi)",
        "boxplots-codegrowth-indexed-by-age-20-years.pdf",
        TRUE,0)

pdf(file=paste(output_dir,"hist-codegrowth-indexed-year.pdf",sep="/"))
hist(monthlyFactsByYear$prod_loc_growth_factor, xlab="Code Growth indexed (CGi)",
     xlim=c(1,2),
     ylab="Probability", 
     main=paste("Probability density of relative code growth in a year.", "N =", length(monthlyFactsByYear$projectName), sep=" " ),
     freq=FALSE,
     breaks=20)
dev.off()

pdf(file=paste(output_dir,"hist-codegrowth-indexed-month.pdf",sep="/"))
hist(monthlyFacts$loc_growth_factor, xlab="Code Growth indexed (CGi)",
     #xlim=c(1,2),
     ylab="Probability", 
     main=paste("Probability density of relative code growth in a month.", "N =", length(monthlyFacts$projectName), sep=" " ),
     freq=FALSE,
     breaks=20)
dev.off()


######### CODE GROWTH ABSOLUTE

dataToPlot <- data.frame(xData="All project years",yData=monthlyFactsByYear$sum_abs_loc_growth)
boxPlot(dataToPlot,
        "",
        "Code Growth absolute (CGa)",
        "boxplot-codegrowth-absolute.pdf")

dataToPlot <- data.frame(xData=factor(monthlyFactsByYear$age),yData=monthlyFactsByYear$sum_abs_loc_growth)
boxPlot(dataToPlot,
        "Project age (years)",
        "Code Growth absolute (CGa)",
        "boxplots-codegrowth-absolute.pdf-by-age.pdf",
        TRUE,0)

dataSelection = subset(monthlyFactsByYear,age<=5)
dataToPlot <- data.frame(xData=factor(dataSelection$age),yData=dataSelection$sum_abs_loc_growth)
boxPlot(dataToPlot,
        "Project age (years)",
        "Code Growth absolute (CGa)",
        "boxplots-codegrowth-absolute.pdf-by-age-5-years.pdf",
        TRUE,0)

pdf(file=paste(output_dir,"hist-codegrowth-absolute-year.pdf",sep="/"))
hist(monthlyFactsByYear$sum_abs_loc_growth, xlab="Code Growth absolute (CGa)",
     #xlim=c(1,2),
     ylab="Probability", 
     main=paste("Probability density of absolute code growth in a year.", "N =", length(monthlyFactsByYear$projectName), sep=" " ),
     freq=FALSE,
     breaks=20)
dev.off()

pdf(file=paste(output_dir,"hist-codegrowth-absolute-month.pdf",sep="/"))
hist(monthlyFacts$abs_loc_growth, xlab="Code Growth absolute (CGa)",
     #xlim=c(1,2),
     ylab="Probability", 
     main=paste("Probability density of absolute code growth in a month.", "N =", length(monthlyFacts$projectName), sep=" " ),
     freq=FALSE,
     breaks=20)
dev.off()


######### TEAM ACTIVITY

dataToPlot <- data.frame(xData="All project years",yData=monthlyFactsByYear$sum_commits)
boxPlot(dataToPlot,
        "",
        "Team Activity (TA)",
        "boxplot-teamactivity.pdf")

dataToPlot <- data.frame(xData=factor(monthlyFactsByYear$age),yData=monthlyFactsByYear$sum_commits)
boxPlot(dataToPlot,
        "Project age (years)",
        "Team Activity (TA)",
        "boxplots-teamactivity-by-age.pdf",
        TRUE,0)

dataSelection = subset(monthlyFactsByYear,age<=5)
dataToPlot <- data.frame(xData=factor(dataSelection$age),yData=dataSelection$sum_commits)
boxPlot(dataToPlot,
        "Project age (years)",
        "Team Activity (TA)",
        "boxplots-teamactivity-by-age-5-years.pdf",
        TRUE,0)

######### TEAM ACTIVITY

dataToPlot <- data.frame(xData="All project years",yData=monthlyFactsByYear$median_contributors)
boxPlot(dataToPlot,
        "",
        "Team Size (TS)",
        "boxplot-teamsize.pdf")

dataToPlot <- data.frame(xData=factor(monthlyFactsByYear$age),yData=monthlyFactsByYear$median_contributors)
boxPlot(dataToPlot,
        "Project age (years)",
        "Team Size (TS)",
        "boxplots-teamsize-by-age.pdf",
        TRUE,0)

dataSelection = subset(monthlyFactsByYear,age<=5)
dataToPlot <- data.frame(xData=factor(dataSelection$age),yData=dataSelection$median_contributors)
boxPlot(dataToPlot,
        "Project age (years)",
        "Team Size (TS)",
        "boxplots-teamsize-by-age-5-years.pdf",
        TRUE,0)

### Plot of project ages in 2012
pdf(file=paste(output_dir,"hist-age-in-2012.pdf",sep="/"))
dataToPlot <- subset(monthlyFactsByYear,subset=year==2012,select=age)
hist(dataToPlot$age, xlab="Project age in 2012",
     ##xlim=c(0,40),
     ylab="Number of occurrences", 
     main=paste("Histogram of project ages in 2012, N =", length(dataToPlot$age), "projects, median =", median(dataToPlot$age), sep=" " ),
     freq=TRUE,
     breaks=40)
dev.off()

### Plot of project survival curve
survivalCurve <- survfit(Surv(age,status) ~ 1, data=projectDeath)
pdf(file=paste(output_dir,"surv-projects.pdf",sep="/"))
plot(survivalCurve, xlab="Project age in years", ylab="Project Survival Probability (SP)",
     main=paste("Kaplan-Meier survival of",length(projectDeath$projectName),"projects",sep=" ")
)
dev.off()

### Age of death
ageAtDeath <- subset(projectDeath, status==2, select=age)
dataToPlot <- data.frame(xData="Dead projects",yData=ageAtDeath$age)
boxPlot(dataToPlot,
        "",
        "Age of Death (AD)",
        "boxplot-age-of-death.pdf",
        TRUE,2)

### Plot of age at projects deaths
pdf(file=paste(output_dir,"hist-age-death.pdf",sep="/"))
hist(ageAtDeath$age, xlab="Project age at death in years",
     xlim=c(0,40),
     ylab="Number of occurrences", 
     main=paste("Histogram of the age at death of", length(ageAtDeath$age), "projects", sep=" " ),
     freq=TRUE,
     breaks=20)
dev.off()

### Plot of calendar year at project deaths
yearOfDeath <- subset(projectDeath, status==2, select=yearOfEvent)
pdf(file=paste(output_dir,"hist-year-death.pdf",sep="/"))
hist(yearOfDeath$yearOfEvent, xlab="Year of death",
     ylab="Number of occurrences", 
     main=paste("Histogram of the year of death of", length(yearOfDeath$yearOfEvent), "projects", sep=" " ),
     freq=TRUE)
dev.off()

### Plot of code size in 2012
dataToPlot <- subset(monthlyFactsByYear, year==2012, select=max_loc_total)
pdf(file=paste(output_dir,"hist-code-size-2012.pdf",sep="/"))
hist(dataToPlot$max_loc_total, xlab="Code size",
     ylab="Number of occurrences", 
     main=paste("Histogram of the code size in 2012", length(dataToPlot$max_loc_total), "projects", sep=" " ),
     freq=TRUE)
dev.off()


### Projects are dying
projectsThatDied <- subset(projectDeath, status==2, select= c(projectName,yearOfEvent))
isDeadYear<-function(row) {
  m<-match(row[["projectName"]],projectsThatDied$projectName,FALSE)
  if(m != FALSE) {
    yearOfDeath <- projectsThatDied$yearOfEvent[[m[[1]]]]
    row[["year"]] >= yearOfDeath
  }
  else FALSE
}
monthlyFactsByYear$deadYear <- apply(monthlyFactsByYear,1,isDeadYear)
yearlyFactsOnDyingProjects <- subset(monthlyFactsByYear, deadYear == FALSE & projectName %in% projectsThatDied$projectName)

### CODE GROWTH INDEXED FOR DYING PROJECTS
dataToPlot<-data.frame(xData=c(rep("Dying projects",length(yearlyFactsOnDyingProjects$projectName)),
                    rep("Base rate",length(monthlyFactsByYear$projectName))),
               yData=c(yearlyFactsOnDyingProjects$prod_loc_growth_factor,
                       monthlyFactsByYear$prod_loc_growth_factor))

boxPlot(dataToPlot,
        "",
        "Code Growth indexed (CGi)",
        "boxplot-codegrowth-indexed-dying-projects.pdf",
        TRUE,1)

### CODE GROWTH ABSOLUTE FOR DYING PROJECTS
dataToPlot<-data.frame(xData=c(rep("Dying projects",length(yearlyFactsOnDyingProjects$projectName)),
                               rep("Base rate",length(monthlyFactsByYear$projectName))),
                       yData=c(yearlyFactsOnDyingProjects$sum_abs_loc_growth,
                               monthlyFactsByYear$sum_abs_loc_growth))

boxPlot(dataToPlot,
        "",
        "Code Growth absolute (CGa)",
        "boxplot-codegrowth-absolute-dying-projects.pdf",
        TRUE,1)

### TEAM ACTIVITY FOR DYING PROJECTS
dataToPlot<-data.frame(xData=c(rep("Dying projects",length(yearlyFactsOnDyingProjects$projectName)),
                               rep("Base rate",length(monthlyFactsByYear$projectName))),
                       yData=c(yearlyFactsOnDyingProjects$sum_commits,
                               monthlyFactsByYear$sum_commits))

boxPlot(dataToPlot,
        "",
        "Team Activity (TA)",
        "boxplot-team-activity-dying-projects.pdf",
        TRUE,1)



######### EXAMPLES

dataSelection = subset(monthlyFactsByYear,subset=projectName %in% c("apache","firefox","emacs","gcc","mysql") & age > 5)
dataToPlot <- data.frame(xData=dataSelection$projectName,yData=dataSelection$prod_loc_growth_factor)
boxPlot(dataToPlot,
        "",
        "Code Growth indexed (CGi)",
        "boxplot-codegrowth-indexed-examples.pdf",
        FALSE,0)

dataToPlot <- data.frame(xData=dataSelection$projectName,yData=dataSelection$sum_abs_loc_growth)
boxPlot(dataToPlot,
        "",
        "Code Growth absolute (CGa)",
        "boxplot-codegrowth-absolute-examples.pdf",
        TRUE,0)

dataSelection = subset(monthlyFactsByYear,select=c(prod_loc_growth_factor,median_contributors))
dataToPlot <- data.frame(xData=dataSelection$median_contributors,yData=dataSelection$prod_loc_growth_factor)
scatterPlot(dataToPlot,
        "Yearly median contributors",
        "Code Growth indexed (CGi)",
        "scatterplot-contributors-codegrowth-indexed.pdf")

dataSelection = subset(monthlyFactsByYear,select=c(sum_abs_loc_growth,median_contributors))
dataToPlot <- data.frame(xData=dataSelection$median_contributors,yData=dataSelection$sum_abs_loc_growth)
scatterPlot(dataToPlot,
            "Yearly median contributors",
            "Code Growth absolute (CGa)",
            "scatterplot-contributors-codegrowth-absolute.pdf")
