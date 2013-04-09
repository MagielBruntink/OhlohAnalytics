module exportProjectFacts

import processProjectFacts;
import Logging;
import lang::csv::IO;

public void exportFactsForAllProjects() {
	logToConsole("exportFactsForAllProject", "Getting all facts for all projects from cache...");
	allFacts=getFactsRelFromCache();
	logToConsole("exportFactsForAllProject", "Outputting all facts for all projects...");
	writeFactsToCSV(allFacts,"allFacts.csv");
	
	logToConsole("exportFactsForAllProject", "Calculating monthly growth facts for all projects...");
	monthlyGrowthFacts=getMonthlyGrowthFacts(allFacts);
	logToConsole("exportFactsForAllProject", "Outputting monthly growth facts for all projects...");
	writeFactsToCSV(monthlyGrowthFacts,"monthlyGrowthFacts.csv");
	
	logToConsole("exportFactsForAllProject", "Grouping monthly growth facts by year for all projects...");
	monthlyGrowthFacts=getMonthlyGrowthFactsByYear(monthlyGrowthFacts);
	logToConsole("exportFactsForAllProject", "Outputting grouped monthly growth facts by year for all projects...");
	writeFactsToCSV(monthlyGrowthFacts,"monthlyGrowthFactsByYear.csv");
	
	logToConsole("exportFactsForAllProject", "Calculating age for all projects...");
	agePerProject=getProjectAges(allFacts);
	logToConsole("exportFactsForAllProject", "Outputting age for all projects...");
	writeFactsToCSV(agePerProject,"agePerProject.csv");
	
}

public void writeFactsToCSV(facts,str fileName) {
	writeCSV(facts,
			 OutputFilesDirectory + fileName,
			 ("separator" : ","));
}
