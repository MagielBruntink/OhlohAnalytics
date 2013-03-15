module projectFactsRepository

import Prelude;
import lang::xml::DOM;

private loc LocalOhlohProjectsRepository = |project://OhlohAnalytics/data|;
private loc ProjectsListFile = LocalOhlohProjectsRepository + "ProjectsList.txt";

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

public void addProjectsListToRepository(list[str] projectList, bool appendToExistingList) {
	str outputString = "";
	for(str projectName <- projectList) {
		outputString += (projectName + "\n");
	}
	if(appendToExistingList) {
		appendToFile(ProjectsListFile, outputString);
	}
	else {
		writeFile(ProjectsListFile, outputString);
	}
}

public list[str] getProjectsListFromRepository() {
	return readFile(ProjectsListFile);
}

private Node getActivityFactsDOM(str Project) {
	loc ActivityFactsFile = LocalOhlohProjectsRepository + "projects" + Project + "ActivityFacts.xml";
	return getXMLContentsDOM(ActivityFactsFile);
}

private Node getSizeFactsDOM(str Project) {
	loc SizeFactsFile = LocalOhlohProjectsRepository + "projects" + Project + "SizeFacts.xml";
	return getXMLContentsDOM(SizeFactsFile);
}

public Node getXMLContentsDOM(loc File) {
	str XMLContentsAsString = readFile(File);
	return XMLContentsDOM = parseXMLDOMTrim(XMLContentsAsString);
}