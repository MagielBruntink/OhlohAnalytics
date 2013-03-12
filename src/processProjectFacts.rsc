module processProjectFacts

import IO;
import lang::xml::DOM;

public loc OhlohProjectsRepository = |project://OhlohAnalytics/Projects|;

public Node getActivityFacts(str Project) {
	loc ActivityFactsFile = OhlohProjectsRepository + Project + "ActivityFacts.xml";
	getXMLContentsDOC(ActivityFactsFile);
}

public Node getSizeFacts(str Project) {
	loc SizeFactsFile = OhlohProjectsRepository + Project + "SizeFacts.xml";
	getXMLContentsDOC(SizeFactsFile);
}

public Node getXMLContentsDOM(loc File) {
	str XMLContentsAsString = readFile(ActivityFactsFile);
	return XMLContentsDOM = parseXMLDOM(XMLContentsAsString);
}