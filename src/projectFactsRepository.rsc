module projectFactsRepository

import Prelude;
import lang::xml::DOM;
import Logging;
import Caching;

private loc LocalOhlohProjectsRepository = |project://OhlohAnalytics/data|;
private loc ProjectNamesListFile = LocalOhlohProjectsRepository + "ProjectNamesList.txt";
private loc ProjectsFile = LocalOhlohProjectsRepository + "Projects.xml";
private str MetaDataFileName = "MetaData.xml";
private str EnlistmentsFileName = "Enlistments.xml";
private str SizeFactsFileName = "SizeFacts.xml";
private str ActivityFactsFileName = "ActivityFacts.xml";

private str RepositoryTypeSVN = "SvnRepository";
private str RepositoryTypeSVNSync = "SvnSyncRepository";

public list[str] getProjectNamesInRepository() {
	return listEntries(LocalOhlohProjectsRepository + "projects");
}

public list[str] getProjectNamesOnList() {
	return readFileLines(ProjectNamesListFile);
}

alias repositoriesRel = rel[str projectName, str repositoryType, str repositoryURL];

alias metaDataRel = rel[str projectName, str elementValue];

alias factsKey = tuple[str projectName, str year, str month];

data monthlyFact =
		    loc_added_fact(num i) |
		    loc_deleted_fact(num i) |
		    commits_fact(num i) |
		    contributors_fact(num i) |
		    loc_total_fact(num i) |
		    abs_loc_growth_fact(num i) |
		    loc_growth_factor_fact(num r);
		    
alias monthlyFactsMap = map[factsKey, set[monthlyFact]];

alias activityFactsMap = 
				  map[str, tuple[str projectName,
                      str year,
                      str month,
                      str loc_added,
                      str loc_deleted,
                      str commits,
                      str contributors]];
alias sizeFactsMap = 
				  map[str, tuple[str projectName,
		              str year,
		              str month,
		              str loc_total]];

public monthlyFactsMap mergeFactsForProjects (list[str] projectNames) {
     return (<projectName, year, month> : {
     			loc_added_fact(toInt(loc_added_str)),
     			loc_deleted_fact(toInt(loc_deleted_str)),
     			commits_fact(toInt(commits_str)),
     			contributors_fact(toInt(contributors_str)),
     			loc_total_fact(toInt(loc_total_str))
     		 } |
                   str projectName <- projectNames,
                   !(true := hasInvalidSVNRepositories(projectName) &&
                     logToConsole("mergeFactsForProjects", "WARNING project excluded because its SVN repositories can include tags or branches: <projectName>.")),
                   activityFactsMap activityFacts := getActivityFacts(projectName),
                   sizeFactsMap sizeFacts := getSizeFacts(projectName),
                   str key <- activityFacts,
                   key in sizeFacts,
                   <_, str year, str month, str loc_added_str, str loc_deleted_str,str commits_str, str contributors_str> := activityFacts[key],
                   <_, year, month, str loc_total_str> := sizeFacts[key]
    );
}

public monthlyFactsMap mergeFactsForAllProjects () { 
	return mergeFactsForProjects(getProjectNamesInRepository());
}

public metaDataRel getMetaDataElements(list[str] projectNames, str elementName) {
	return {
		valuesForProject |
		projectName <- projectNames,
		valuesForProject <- getMetaDataElements(projectName, elementName)
	};
}

public metaDataRel getMetaDataElements(str projectName, str elementName) {
	result = {};
		
	top-down visit(getProjectMetaDataDOM(projectName)) {
		case element(_,
					 elementName,
					 [Node*,charData(str elementValue)]):
		{
             result += <projectName,
			 			elementValue>;
		}
	}
	return result;
}

public repositoriesRel getRepositoryFactsForProjects(list[str] projectNames) {
	return {
		<projectName, repositoryType, repositoryURL> |
		projectName <- projectNames,
		<projectName,repositoryType,repositoryURL> <- getRepositoryFacts(projectName)
	};
}

public repositoriesRel findInvalidSVNRepositories(repositoriesRel repositoryFacts) {
	return {
		<projectName, repositoryType, repositoryURL> |
		<str projectName, str repositoryType, str repositoryURL> <- repositoryFacts,
		repositoryType := RepositoryTypeSVN || repositoryType := RepositoryTypeSVNSync,
		!(
		 /.*\/trunk\/?/i      := repositoryURL ||
		 /.*\/head\/?/i       := repositoryURL ||
		 /.*\/sandbox\/?/i    := repositoryURL ||
		 /.*\/site\/?/i       := repositoryURL ||
		 /.*\/branches\/\w+/i := repositoryURL ||
		 /.*\/tags\/\w+/i     := repositoryURL
		)
	};
}

public repositoriesRel findAllInvalidSVNRepositories() {
	return findInvalidSVNRepositories(
	         getRepositoryFactsForProjects(
	           getProjectNamesInRepository()));
}

public bool hasInvalidSVNRepositories(str projectName) {
	return size(findInvalidSVNRepositories(
		          getRepositoryFactsForProjects([projectName])))
		   > 0;
}

public repositoriesRel getRepositoryFacts(str projectName) {	   
	result = {};
	try
		top-down visit(getProjectEnlistmentsDOM(projectName)) {
			case element(_,"repository",
					[
					 _,
					 element(_,"type",[charData(str repositoryType)]),
					 element(_,"url",[charData(str repositoryURL)]),
					 Node*
					]):
			{
	             result += <projectName,
				 			repositoryType,
				 			repositoryURL>;
			}
		}
	catch: logToConsole("getRepositoryFacts", "WARNING error while getting repository facts for project: <projectName>.");
	return result;
}

@doc{
	Returns a relation containing:
		- str: month
		- int: code added
		- int: code deleted
}
public activityFactsMap getActivityFacts(str projectName)
{	   
    result = ();
    try 
    	top-down visit(getActivityFactsDOM(projectName)) {
			case element(none(),"activity_fact",
					[
					 element(_,"month",[charData(str monthAsString)]),
					 element(_,"code_added",[charData(str LOCAddedAsString)]),
					 element(_,"code_removed",[charData(str LOCDeletedAsString)]),
					 Node*,
					 element(_,"commits",[charData(str CommitsAsString)]),
					 element(_,"contributors",[charData(str ContributorsAsString)])
					]):
	        {
				 str year = getYear(monthAsString);
				 str month = getMonth(monthAsString);
				 result += (projectName + "-" + year + "-" + month :
				            <projectName,
	                         year,
				 			 month,
				 			 LOCAddedAsString,
				 			 LOCDeletedAsString,
				 			 CommitsAsString,
				 			 ContributorsAsString>);
	        }
		}
	catch: logToConsole("getActivityFacts", "WARNING error while getting activity facts for project: <projectName>.");
	return result;
}

@doc{
	Returns a relation containing:
		- str: month
		- int: code added
		- int: code deleted
}
public sizeFactsMap getSizeFacts(str projectName) {	   
	result = ();
	try
		top-down visit(getSizeFactsDOM(projectName)) {
			case element(none(),"size_fact",
					[
					 element(_,"month",[charData(str monthAsString)]),
					 element(_,"code",[charData(str LOCTotalAsString)]),
					 Node*
					]):
			{
	             str year = getYear(monthAsString);
	             str month = getMonth(monthAsString);
	             result += (projectName + "-" + year + "-" + month :
				           <projectName,
				 			year,
				 			month,
				 			LOCTotalAsString>);
			}
		}
	catch: logToConsole("getSizeFacts", "WARNING error while getting size facts for project: <projectName>.");
	return validateAndFilterSizeFacts(result);
}

private sizeFactsMap validateAndFilterSizeFacts (sizeFactsMap unfilteredSizeFacts) {
	return (key : <projectName, year, month, loc_total> |
		str key <- unfilteredSizeFacts,
		<str projectName, str year, str month, str loc_total> <- [unfilteredSizeFacts[key]],
		!(toInt(loc_total) < 0 && logToConsole("validateAndFilterSizeFacts", "WARNING data excluded because lines of code count is below 0 for project: <projectName> in year/month: <year>/<month>"))
	);
}

public void addMetaDataToRepository(str metaDataXML, str projectName) {
	addXMLFileToRepository(metaDataXML, projectName, MetaDataFileName);
}

public void addEnlistmentsToRepository(str enlistmentsXML, str projectName) {
	addXMLFileToRepository(enlistmentsXML, projectName, EnlistmentsFileName);
}

public void addActivityFactsToRepository(str activityFactsXML, str projectName) {
	addXMLFileToRepository(activityFactsXML, projectName, ActivityFactsFileName);
}


public void addSizeFactsToRepository(str sizeFactsXML, str projectName) {
	addXMLFileToRepository(sizeFactsXML, projectName, SizeFactsFileName);
}

private void addXMLFileToRepository(str XML, str projectName, str fileName) {
	loc file = LocalOhlohProjectsRepository + "projects" + projectName + fileName;
	if (!validateXML(XML)) throw "addXMLFileToRepository: Validation of XML contents failed while adding <fileName> for project: <projectName>";
	else writeFile(file, XML);
}

public void addProjectsListToRepository(str projectsListPage) {
	str outputString = "";
	
	list[str] projectList = extractProjectNames(projectsListPage);
	for(str projectName <- projectList) {
		outputString += (projectName + "\n");
	}
	appendToFile(ProjectNamesListFile, outputString);
}

public list[str] getProjectNamesListFromRepository() {
	return readFileLines(ProjectNamesListFile);
}

private list[str] extractProjectNames (str XML) {
	list[str] result = [];
	top-down visit(getXMLContentsDOM(XML)) {
		case element(_,"url_name",[charData(str projectName)]):
			 result += projectName;
	}
	return result;
}

public Node getProjectMetaDataDOM(str projectName) {
	return getXMLContentsDOM(projectName, MetaDataFileName);
}

public Node getProjectEnlistmentsDOM(str projectName) {
	return getXMLContentsDOM(projectName, EnlistmentsFileName);
}

private Node getActivityFactsDOM(str projectName) {
	return getXMLContentsDOM(projectName, ActivityFactsFileName);
}

private Node getSizeFactsDOM(str projectName) {
	return getXMLContentsDOM(projectName, SizeFactsFileName);
}

private Node getXMLContentsDOM(str projectName, str fileName) {
	return getValueFromCache("projects" + "/" + projectName + "/" + fileName + ".cache",
		   			         #Node, Node () {return uncachedGetXMLContentsDOM(projectName, fileName);});
}

private Node uncachedGetXMLContentsDOM(str projectName, str fileName) {
	loc file = LocalOhlohProjectsRepository + "projects" + projectName + fileName;
	str XML = readFile(file);
	if (!validateXML(XML)) throw "getXMLContentsDOM: Validation of XML contents failed while reading <fileName> for project: <projectName>";
	XMLContentsDOM = parseXMLDOMTrim(XML);
	return XMLContentsDOM;
}

private str reformatDateTime(str dateTimeString) {
	datetime dt = parseDateTime(dateTimeString,"yyyy-MM-dd\'T\'HH:mm:ss\'");
	return printDate(dt, "yyyy-MM");
}

private str getYear(str dateTimeString) {
	return printDate(getDateTime(dateTimeString), "yyyy");
}

private str getMonth(str dateTimeString) {
	return printDate(getDateTime(dateTimeString), "MM");
}

private datetime getDateTime(str dateTimeString) {
	return parseDateTime(dateTimeString,"yyyy-MM-dd\'T\'HH:mm:ss");
}

private bool validateXML(str input) {
	return (/\s*\<response\>\s*\<status\>\s*success\s*\<\/status\>.*/i := input);
}

