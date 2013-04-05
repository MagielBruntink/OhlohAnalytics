module exportProjectFacts

import processProjectFacts;
import Logging;
import lang::csv::IO;

public loc OutputFilesDirectory = |project://OhlohAnalytics/output|;

public void exportFactsForAllProjects() {
	logToConsole("exportFactsForAllProject", "Calculating raw facts for all projects...");
	rawFacts=convertFactsMapToRel(mergeFactsForAllProjects());
	logToConsole("exportFactsForAllProject", "Outputting raw facts for all projects...");
	writeFactsToCSV(rawFacts,"rawFacts.csv");
	
	logToConsole("exportFactsForAllProject", "Calculating monthly growth facts for all projects...");
	monthlyGrowthFacts=getMonthlyGrowthFacts(rawFacts);
	logToConsole("exportFactsForAllProject", "Outputting monthly growth facts for all projects...");
	writeFactsToCSV(monthlyGrowthFacts,"monthlyGrowthFacts.csv");
	
	logToConsole("exportFactsForAllProject", "Grouping monthly growth facts by year for all projects...");
	monthlyGrowthFacts=getMonthlyGrowthFactsByYear(monthlyGrowthFacts);
	logToConsole("exportFactsForAllProject", "Outputting grouped monthly growth facts by year for all projects...");
	writeFactsToCSV(monthlyGrowthFacts,"monthlyGrowthFactsByYear.csv");
}

public void writeFactsToCSV(facts,str fileName) {
	writeCSV(facts,
			 OutputFilesDirectory + fileName,
			 ("separator" : ","));
}
