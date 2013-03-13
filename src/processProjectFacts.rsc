module processProjectFacts

import Prelude;
import lang::xml::DOM;

public loc OhlohProjectsRepository = |project://OhlohAnalytics/data/projects|;

public void main() {
	str project = "firefox";
	activityFacts = extractActivityFacts(project);
	sizeFacts = extractSizeFacts(project);
	rel[str project,
	    str month,
		str loc_added,
		str loc_removed,
		str commits,
		str contributors,
		str total_loc] mergedFacts =
	{<project, month> + activityFact + <sizeFact> | month <- activityFacts.month + sizeFacts.month,
	                                                activityFact <- activityFacts[month], sizeFact <- sizeFacts[month]};
	println(mergedFacts<month,contributors>);
}

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
	   extractActivityFacts(str Project) {
	   
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
	   extractSizeFacts(str Project) {
	   
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

public Node getActivityFactsDOM(str Project) {
	loc ActivityFactsFile = OhlohProjectsRepository + Project + "ActivityFacts.xml";
	return getXMLContentsDOM(ActivityFactsFile);
}

public Node getSizeFactsDOM(str Project) {
	loc SizeFactsFile = OhlohProjectsRepository + Project + "SizeFacts.xml";
	return getXMLContentsDOM(SizeFactsFile);
}

public Node getXMLContentsDOM(loc File) {
	str XMLContentsAsString = readFile(File);
	return XMLContentsDOM = parseXMLDOMTrim(XMLContentsAsString);
}
