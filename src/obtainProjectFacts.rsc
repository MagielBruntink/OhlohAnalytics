module obtainProjectFacts

import Prelude;
import projectFactsRepository;
import Logging;
import Sleep;
import lang::xml::DOM;
import util::Math;

private loc OhlohAPIKeyFile = |project://OhlohAnalytics/OhlohAPIKey.txt|;
public str OhlohAPIKey = readFile(OhlohAPIKeyFile);
private str OhlohBaseURL = "www.ohloh.net";
private int timeToSleepBetweenQueries = 1000; //milliseconds

public void obtainProjectListFromOhloh(int endAtPage) {
	obtainProjectListFromOhloh(1, endAtPage);
}

@doc{
	int startAtPage : page number to start at, counting from 1. 
	int endAtPage   : page number of last page to obtain.
}
public void obtainProjectListFromOhloh(int startAtPage, int endAtPage) {
	int pageToObtain = startAtPage;

	while (pageToObtain <= endAtPage) {
		loc projectsListURI = |http://<OhlohBaseURL>/projects.xml?api_key=<OhlohAPIKey>&page=<toString(pageToObtain)>|;
		logToConsole("obtainProjectsFromOhloh", "Reading projects from Ohloh, URL: <projectsListURI>");
		addProjectsListToRepository(readFile(projectsListURI));
		pageToObtain += 1;
		sleep(timeToSleepBetweenQueries);
	}
}

@doc{
	int startAtProject : project number (line nr. in project list file) to start at, counting from 1. 
	int endAtProject   : project number (line nr. in project list file) of last project to obtain.
}
public void obtainProjectDataFromOhloh(int startAtProject, int endAtProject) {
	list[str] projectNamesList = getProjectNamesListFromRepository();
	int numberOfProjectNames = size(projectNamesList);
 	
	int projectToObtain = startAtProject;

	while (projectToObtain <= min(endAtProject, numberOfProjectNames)) {
		obtainActivityFactsFromOhloh(projectNamesList[projectToObtain-1]);
		sleep(timeToSleepBetweenQueries);
		obtainSizeFactsFromOhloh(projectNamesList[projectToObtain-1]);
		sleep(timeToSleepBetweenQueries);
		projectToObtain += 1;
	}
}

public void obtainActivityFactsFromOhloh(str projectName) {
	loc projectActivityFactsURI = |http://<OhlohBaseURL>/projects/<projectName>/analyses/latest/activity_facts.xml?api_key=<OhlohAPIKey>|;
	logToConsole("obtainActivityFactsFromOhloh", "Reading activity facts for project <projectName> from Ohloh, URL: <projectActivityFactsURI>");
	try addActivityFactsToRepository(readFile(projectActivityFactsURI), projectName);
	catch: logToConsole("obtainActivityFactsFromOhloh", "WARNING: problem when reading activity facts for project <projectName> from Ohloh.");
}

public void obtainSizeFactsFromOhloh(str projectName) {
	loc projectSizeFactsURI = |http://<OhlohBaseURL>/projects/<projectName>/analyses/latest/size_facts.xml?api_key=<OhlohAPIKey>|;
	logToConsole("obtainSizeFactsFromOhloh", "Reading size facts for project <projectName> from Ohloh, URL: <projectSizeFactsURI>");
	try addSizeFactsToRepository(readFile(projectSizeFactsURI), projectName);
	catch: logToConsole("obtainSizeFactsFromOhloh", "WARNING: problem when reading size facts for project <projectName> from Ohloh.");
}
