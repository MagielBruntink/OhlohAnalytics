module obtainProjectFacts

import Prelude;
import projectFactsRepository;

private loc OhlohAPIKeyFile = |project://OhlohAnalytics/OhlohAPIKey.txt|;
private str OhlohAPIKey = readFile(OhlohAPIKeyFile);
private str OhlohBaseURL = "www.ohloh.net";

@doc{
	Work in progress!
}
public list[str] obtainSomeProjectNamesFromOhloh() {
	list[str] result = [];
	int projectsObtained = 0;

	//https://www.ohloh.net/projects.xml?api_key=
	loc projectsListURI = |http://<OhlohBaseURL>/projects.xml?api_key=<OhlohAPIKey>|;
	println(projectsListURI);
	println(readFile(projectsListURI));
	// now process the resulting XML from Ohloh and get the project names out
	return result;
}

//Activity Facts query
//https://www.ohloh.net/projects/rascal/analyses/latest/activity_facts.xml?api_key=

//Size Facts query
//https://www.ohloh.net/projects/firefox/analyses/latest/size_facts.xml?api_key=

