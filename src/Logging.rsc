module Logging

import DateTime;
import ToString;
import IO;

private loc LogsDirectory = |project://OhlohAnalytics/logs|;
private str separator = "   ";
private str extension = ".txt";


public void logToConsole(str identification, str message) {
	println(toString(now()) + separator + identification + separator + message); 
}

public loc openLogFile(str name) {
	return LogsDirectory + (printDateTime(now(),"YYYYMMdd-HHmm") + "-" + name + extension);
}

public void logToFile(loc logFile, str identification, str message) {
	appendToFile(logFile, toString(now()) + separator + identification + separator + message + "\n");
}
