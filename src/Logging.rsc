module Logging

import DateTime;
import ToString;
import IO;

private str separator = "   ";

public void logToConsole(str identification, str message) {
	println(toString(now()) + separator + identification + separator + message); 
}
