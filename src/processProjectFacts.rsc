module processProjectFacts

import IO;
import lang::xml::DOM;

public loc OhlohProjectsRepository = |project://OhlohAnalytics/data/projects|;

public Node getActivityFacts(str Project) {
	loc ActivityFactsFile = OhlohProjectsRepository + Project + "ActivityFacts.xml";
	return getXMLContentsDOM(ActivityFactsFile);
}

public Node getSizeFacts(str Project) {
	loc SizeFactsFile = OhlohProjectsRepository + Project + "SizeFacts.xml";
	return getXMLContentsDOM(SizeFactsFile);
}

public Node getXMLContentsDOM(loc File) {
	str XMLContentsAsString = readFile(File);
	return XMLContentsDOM = parseXMLDOM(XMLContentsAsString);
}