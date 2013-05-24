require(ggplot2)
require(survival)
require(plyr)
options(scipen=100)

analysis_dir <- "~/git/OhlohAnalytics/analysis/esamir/"
output_dir <- "/Users/magielbruntink/Google Drive/UVA/Research/Writing/FLOSS project evolution and failure"

monthlyFactsByYear <- read.csv(file=paste(analysis_dir,"allYearlyFacts.csv",sep="/"),header=TRUE,sep=",")
projectDeath <- read.csv(file=paste(analysis_dir, "projectDeathStatus.csv",sep="/"),header=TRUE,sep=",")

### Boxplots
boxPlot <- function(dataFrame,xLabel,yLabel,fileName,zoom=TRUE,text=TRUE) {
  allWhiskerLimits = ddply(dataFrame, .(xData), summarise, 
                           whiskerLimMin = boxplot.stats(yData)$stats[c(1)], 
                           whiskerLimMax = boxplot.stats(yData)$stats[c(5)])
  allObsCounts     = ddply(dataFrame, .(xData), summarise, obsCount = length(yData))
  allOutlierCounts = ddply(dataFrame, .(xData), summarise, outlierCount = length(boxplot.stats(yData)$out))  
  allMedians       = ddply(dataFrame, .(xData), summarise, medianValue = round(median(yData),digits=3))
  allLabelData     = cbind(allObsCounts,allOutlierCounts,allMedians)
  
  plotObj <- ggplot(dataFrame,aes(x=xData,y=yData)) +
    geom_boxplot() +
    labs (x=xLabel,y=yLabel) +
    geom_hline(aes(yintercept=1.000))
  
  if(text) {
    plotObj = plotObj + 
              geom_text(data=allLabelData,
                        aes(x = xData,
                            y = medianValue,
#                             label = paste("Med. =", medianValue,
#                                           "Obs. =", obsCount,
#                                           "Out. =", outlierCount,
#                                           sep=" ")),
                            label = medianValue),
              size = 3, vjust = -1.5)
  }
  
  if(zoom) {
    whiskerLimits = c(min(allWhiskerLimits$whiskerLimMin),max(allWhiskerLimits$whiskerLimMax))
    plotObj = plotObj + coord_cartesian(ylim = whiskerLimits*1.05)
  }
  
  ggsave(file=paste(output_dir,fileName,sep="/"),plot=plotObj)
}

######### CODE GROWTH

dataToPlot <- data.frame(xData="All project years",yData=monthlyFactsByYear$prod_loc_growth_factor)
boxPlot(dataToPlot,
        "",
        "Code Growth (CG)",
        "boxplot-codegrowth.pdf")

dataToPlot <- data.frame(xData=factor(monthlyFactsByYear$age),yData=monthlyFactsByYear$prod_loc_growth_factor)
boxPlot(dataToPlot,
        "Project age (years)",
        "Code Growth (CG)",
        "boxplots-codegrowth-by-age.pdf",
        TRUE,FALSE)

dataSelection = subset(monthlyFactsByYear,age<=5)
dataToPlot <- data.frame(xData=factor(dataSelection$age),yData=dataSelection$prod_loc_growth_factor)
boxPlot(dataToPlot,
        "Project age (years)",
        "Code Growth (CG)",
        "boxplots-codegrowth-by-age-5-years.pdf",
        TRUE,FALSE)

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
        TRUE,FALSE)

dataSelection = subset(monthlyFactsByYear,age<=5)
dataToPlot <- data.frame(xData=factor(dataSelection$age),yData=dataSelection$sum_commits)
boxPlot(dataToPlot,
        "Project age (years)",
        "Team Activity (TA)",
        "boxplots-teamactivity-by-age-5-years.pdf",
        TRUE,FALSE)

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
        TRUE,FALSE)

dataSelection = subset(monthlyFactsByYear,age<=5)
dataToPlot <- data.frame(xData=factor(dataSelection$age),yData=dataSelection$median_contributors)
boxPlot(dataToPlot,
        "Project age (years)",
        "Team Size (TS)",
        "boxplots-teamsize-by-age-5-years.pdf",
        TRUE,FALSE)

### Plot of project survival curve
survivalCurve <- survfit(Surv(age,status) ~ 1, data=projectDeath)
pdf(file=paste(output_dir,"surv-projects.pdf",sep="/"))
plot(survivalCurve, xlab="Project age in years", ylab="Project survival",
     main=paste("Kaplan-Meier survival of",length(projectDeath$projectName),"projects",sep=" ")
)
dev.off()

### Plot of age at projects deaths
ageAtDeath <- subset(projectDeath, status==2, select=age)
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


