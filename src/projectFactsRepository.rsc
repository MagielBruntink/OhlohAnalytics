module projectFactsRepository

import Prelude;
import lang::xml::DOM;

private loc LocalOhlohProjectsRepository = |project://OhlohAnalytics/data|;
private loc ProjectNamesListFile = LocalOhlohProjectsRepository + "ProjectNamesList.txt";
private loc ProjectsFile = LocalOhlohProjectsRepository + "Projects.xml";

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

public void addProjectsListToRepository(str projectsListPage) {
	str outputString = "";
	
	list[str] projectList = extractProjectNames(projectsListPage);
	for(str projectName <- projectList) {
		outputString += (projectName + "\n");
	}
	appendToFile(ProjectNamesListFile, outputString);
	appendToFile(ProjectsFile, projectsListPage);
}

public list[str] getProjectNamesListFromRepository() {
	return readFile(ProjectNamesListFile);
}

private list[str] extractProjectNames (str XML) {
	list[str] result = [];
	top-down visit(getXMLContentsDOM(XML)) {
		case element(_,"url_name",[charData(str projectName)]):
			 result += projectName;
	}
	return result;
}

private Node getActivityFactsDOM(str Project) {
	loc ActivityFactsFile = LocalOhlohProjectsRepository + "projects" + Project + "ActivityFacts.xml";
	return getXMLContentsDOM(readFile(ActivityFactsFile));
}

private Node getSizeFactsDOM(str Project) {
	loc SizeFactsFile = LocalOhlohProjectsRepository + "projects" + Project + "SizeFacts.xml";
	return getXMLContentsDOM(readFile(SizeFactsFile));
}

private Node getXMLContentsDOM(str XML) {
	return XMLContentsDOM = parseXMLDOMTrim(XML);
}