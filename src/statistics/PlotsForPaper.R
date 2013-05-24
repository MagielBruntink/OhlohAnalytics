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
  whiskerLimits    = boxplot.stats(dataFrame$yData)$stats[c(1, 5)]
  allObsCounts     = ddply(dataFrame, .(xData), summarise, obsCount = length(yData))
  allOutlierCounts = ddply(dataFrame, .(xData), summarise, outlierCount = length(boxplot.stats(yData)$out))  
  allMedians       = ddply(dataFrame, .(xData), summarise, medianValue = round(median(yData),digits=3))
  allLabelData     = cbind(allObsCounts,allOutlierCounts,allMedians)
  print (allLabelData)
  
  plotObj <- ggplot(dataFrame,aes(x=xData,y=yData)) +
    geom_boxplot() +
    labs (x=xLabel,y=yLabel) +
    geom_hline(aes(yintercept=1.000))
  
  if(text) {
    plotObj = plotObj + 
              geom_text(data=allLabelData,
                        aes(x = xData,
                            y = medianValue,
                            label = paste("Med. =", medianValue,
                                          "Obs. =", obsCount,
                                          "Out. =", outlierCount,
                                          sep=" ")),
              size = 3, vjust = -1.5)
  }
  
  if(zoom) {
    plotObj = plotObj + coord_cartesian(ylim = whiskerLimits*1.05)
  }
  
  ggsave(file=paste(output_dir,fileName,sep="/"),plot=plotObj)
}

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


### Plot of project ages in 2012
projectAgesIn2012 <- subset(monthlyFactsByYear,year==2012,select=age)
pdf(file=paste(output_dir,"hist-project-age.pdf",sep="/"))
hist(projectAgesIn2012$age, xlab="Project age in years",
     ylab="Number of occurrences", 
     main=paste("Histogram of the age of", length(projectAgesIn2012$age), "projects in 2012", sep=" " ),
     freq=TRUE,
     breaks=40)
dev.off()

### Plot of monthly growth facts aggregated by year
plotObj <- qplot(age,prod_loc_growth_factor,data=monthlyFactsByYear, group=age,geom="boxplot", log="y",
                 xlab="Project age in years",                    
                 ylab="Yearly LOC growth factor",
                 main=paste("Boxplots of yearly growth factor for each project age, for",length(monthlyFactsByYear$projectName),"project years",sep=" "))
ggsave(file=paste(output_dir,"boxplots-codegrowth-by-age.pdf",sep="/"),plot=plotObj)

### Plot of monthly commits summed by year
plotObj <- qplot(age,sum_commits,data=monthlyFactsByYear, group=age,geom="boxplot", log="y",
                 xlab="Project age in years",
                 ylab="Yearly sum of commits",
                 main=paste("Boxplots of yearly number of commits for each project age, for",length(monthlyFactsByYear$projectName),"project years",sep=" "))
ggsave(file=paste(output_dir,"boxplots-commits-by-age.pdf",sep="/"),plot=plotObj)

### Plot of monthly contributors averaged (median) by year for each project age
plotObj <- qplot(age,median_contributors,data=monthlyFactsByYear, group=age,geom="boxplot", log="y",
                 xlab="Project age in years",
                 ylab="Yearly median of contributors",
                 main=paste("Boxplots of yearly average (median) contributors for each project age, for",length(monthlyFactsByYear$projectName),"project years",sep=" "))
ggsave(file=paste(output_dir,"boxplots-contributors-by-age.pdf",sep="/"),plot=plotObj)

### Plot of monthly contributors averaged (median) by year
plotObj <- qplot(year,median_contributors,data=monthlyFactsByYear, group=year,geom="boxplot", log="y",
                 xlab="Year",
                 ylab="Yearly median of contributors",
                 main=paste("Boxplots of yearly average (median) contributors in a calendar year, for",length(monthlyFactsByYear$projectName),"project years",sep=" "))
ggsave(file=paste(output_dir,"boxplots-contributors-by-year.pdf",sep="/"),plot=plotObj)

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


