module exportProjectFacts

import Prelude;
import processProjectFacts;
import projectFactsRepository;
import Logging;
import lang::csv::IO;
import util::Maybe;
import IO;
import analyzeProjectDeath;
import analyzeProjectMetaData;
import dataValidation;

public loc OutputFilesDirectory = |project://OhlohAnalytics/output|;

// broken
//public void exportFactsForAllProjects() {
//	logToConsole("exportFactsForAllProject", "Getting Ohloh facts for all projects from cache...");
//	monthlyFactsMap OhlohFacts=getOhlohFactsFromCache();
//	
//	logToConsole("exportFactsForAllProject", "Getting monthly growth facts for all projects from cache...");
//	monthlyFactsMap allMonthlyFacts=getMonthlyFactsFromCache(OhlohFacts);
//	logToConsole("exportFactsForAllProject", "Exporting all monthly facts to CSV for all projects: " + "allMonthlyFacts.csv");
//	writeFactsToCSV(convertMonthlyFactsMapToRel(allMonthlyFacts),"allMonthlyFacts.csv");
//	
//	logToConsole("exportFactsForAllProject", "Getting yearly growth facts for all projects from cache...");
//	yearlyFactsMap allYearlyFacts=getYearlyFactsFromCache(allMonthlyFacts);
//	logToConsole("exportFactsForAllProject", "Exporting all yearly facts to CSV for all projects: " + "allYearlyFacts.csv");
//	writeFactsToCSV(convertYearlyFactsMapToRel(allYearlyFacts),"allYearlyFacts.csv");
//	
//	logToConsole("exportFactsForAllProject", "Calculating project activity for all projects...");
//	projectActivityStats=getProjectActivityStatus(allYearlyFacts);
//	logToConsole("exportFactsForAllProject", "Calculating project death events for all projects...");
//	projectDeathStats=getProjectDeathStatus(projectActivityStats, "2012");
//	logToConsole("exportFactsForAllProject", "Exporting project death events CSV for all projects: " + "projectDeathStatus.csv");
//	writeFactsToCSV(projectDeathStats,"projectDeathStatus.csv");
//}

public void validateAndOutputFacts() {
	logToConsole("validateAndOutputFacts", "Validating all data in repository on project level...");
	remainingProjects = validateDataOnProjectLevel();
	logToConsole("validateAndOutputFacts", "Obtaining all merged facts form repository...");
	facts = mergeFactsForProjects(remainingProjects);
	writeFactsMapToCSV(facts, validationResultsDir + "monthlyFactsWithProperEnlistments.csv");
}

public void doProjectMetaDataAnalyses () {
	logToConsole("doProjectMetaDataAnalyses", "Analyzing project tags for all projects...");
	OAL=generateOALForTags(getProjectNamesInRepository());
	writeValueToFile(OAL,"projects-tags.oal");
}

public void exportRepositoryFacts () {
	logToConsole("exportRepositoryFacts", "Exporting repository facts for all projects...");
	repoFacts=getRepositoryFactsForProjects(getProjectNamesInRepository());
	writeFactsToCSV(repoFacts,"projectRepositoryFacts.csv");
}

public void writeValueToFile(v, str fileName) {
	writeFile(OutputFilesDirectory + fileName, v);
}

public void writeFactsMapToCSV (factsMap facts, loc outFile) {
	separator = ",";
	factKeys = identificationFactKeys + activityFactKeys + sizeFactKeys;
	header = "";
	
	for (factKey <- factKeys) {
		header += (factKey + separator);
	};
	header = substring(header, 0, size(header) - 1) + "\n";	
	writeFile(outFile, header);
	
	for (dataPoint <- facts) {
		line = "";
		factsForDataPoint = facts[dataPoint];
		for (factKey <- factKeys) {
			line += (maybeFactToString(factsForDataPoint[factKey]) + separator);
		};
		line = substring(line, 0, size(line) - 1) + "\n";
		appendToFile(outFile, line);
	};
}

private str maybeFactToString(Maybe[value] mv) {
	switch(mv) {
		case nothing(): return "NA";
		case just(value v): return toString(v);
	};
}
