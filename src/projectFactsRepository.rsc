module projectFactsRepository

import Prelude;
import lang::xml::DOM;

public loc LocalOhlohProjectsRepository = |project://OhlohAnalytics/data/projects|;

@doc{
	Returns a relation containing:
		- str: month
		- int: code added
		- int: code deleted
}
public rel[str month,
		   str loc_added,
		   str loc_removed,
		   str commits,
		   str contributors]
	   getActivityFacts(str Project) {
	   
	rel[str,str,str,str,str] result = {};
	top-down visit(getActivityFactsDOM(Project)) {
		case element(none(),"activity_fact",
				[
				 element(_,"month",[charData(str monthAsString)]),
				 element(_,"code_added",[charData(str LOCAddedAsString)]),
				 element(_,"code_removed",[charData(str LOCDeletedAsString)]),
				 Node*,
				 element(_,"commits",[charData(str CommitsAsString)]),
				 element(_,"contributors",[charData(str ContributorsAsString)])
				]):
			 result += {<monthAsString,LOCAddedAsString,LOCDeletedAsString,CommitsAsString,ContributorsAsString>};
	}
	return result;
}

@doc{
	Returns a relation containing:
		- str: month
		- int: code added
		- int: code deleted
}
public rel[str month,
		   str loc_total]
	   getSizeFacts(str Project) {
	   
	rel[str,str] result = {};
	top-down visit(getSizeFactsDOM(Project)) {
		case element(none(),"size_fact",
				[
				 element(_,"month",[charData(str monthAsString)]),
				 element(_,"code",[charData(str LOCTotalAsString)]),
				 Node*
				]):
			 result += {<monthAsString,LOCTotalAsString>};
	}
	return result;
}

private Node getActivityFactsDOM(str Project) {
	loc ActivityFactsFile = LocalOhlohProjectsRepository + Project + "ActivityFacts.xml";
	return getXMLContentsDOM(ActivityFactsFile);
}

private Node getSizeFactsDOM(str Project) {
	loc SizeFactsFile = LocalOhlohProjectsRepository + Project + "SizeFacts.xml";
	return getXMLContentsDOM(SizeFactsFile);
}

private Node getXMLContentsDOM(loc File) {
	str XMLContentsAsString = readFile(File);
	return XMLContentsDOM = parseXMLDOMTrim(XMLContentsAsString);
}

@doc{
	Returns a relation containing:
		- str: month
		- int: code added
		- int: code deleted
}
public rel[str month,
		   str loc_total]
	   getSizeFacts(str Project) {
	   
	rel[str,str] result = {};
	top-down visit(getSizeFactsDOM(Project)) {
		case element(none(),"size_fact",
				[
				 element(_,"month",[charData(str monthAsString)]),
				 element(_,"code",[charData(str LOCTotalAsString)]),
				 Node*
				]):
			 result += {<monthAsString,LOCTotalAsString>};
	}
	return result;
}

private Node getActivityFactsDOM(str Project) {
	loc ActivityFactsFile = LocalOhlohProjectsRepository + Project + "ActivityFacts.xml";
	return getXMLContentsDOM(ActivityFactsFile);
}

private Node getSizeFactsDOM(str Project) {
	loc SizeFactsFile = LocalOhlohProjectsRepository + Project + "SizeFacts.xml";
	return getXMLContentsDOM(SizeFactsFile);
}

private Node getXMLContentsDOM(loc File) {
	str XMLContentsAsString = readFile(File);
	return XMLContentsDOM = parseXMLDOMTrim(XMLContentsAsString);
}