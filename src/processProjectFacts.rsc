module processProjectFacts

import IO;
import lang::xml::DOM;

public loc OhlohProjectsRepository = |project://OhlohAnalytics/data/projects|;

public set[str] getMonths(Node DOM) {
	set[str] result = {};
	top-down visit(DOM) {
		case element(none(),"month",[charData(str M)]): result += M;
	}
	return result;
}

public Node getActivityFactsDOM(str Project) {
	loc ActivityFactsFile = OhlohProjectsRepository + Project + "ActivityFacts.xml";
	return getXMLContentsDOM(ActivityFactsFile);
}

public Node getSizeFactsDOM(str Project) {
	loc SizeFactsFile = OhlohProjectsRpository + Project + "SizeFacts.xml";
	return getXMLContentsDOM(SizeFactsFile);
}

public Node getXMLContentsDOM(loc File) {
	str XMLContentsAsString = readFile(File);
	return XMLContentsDOM = parseXMLDOMTrim(XMLContentsAsString);
}
