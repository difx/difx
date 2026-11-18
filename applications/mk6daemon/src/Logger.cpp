#include <ctime> 
#include <fstream> 
#include <iostream> 
#include <sstream> 

#include <Logger.h> 

using namespace std; 


/**
// Constructor: Opens the log file in append mode 
**/
Logger::Logger(const string& filename, bool toConsole = false)
{ 
    
    m_logFile.open(filename, ios::app); 
    if (!m_logFile.is_open()) { 
        cerr << "Error opening log file: " << filename << endl; 
    } 

    m_toConsole = toConsole;
} 

/**
// Destructor: Closes the log file 
**/
Logger::~Logger()
{ 
    m_logFile.close(); 
} 

  /**
  // Logs a message with a given log level 
  **/ 
void Logger::log(LogLevel level, const string message) 
{ 
    // Get current timestamp 
    time_t now = time(0); 
    tm* timeinfo = localtime(&now); 
    char timestamp[20]; 
    strftime(timestamp, sizeof(timestamp), 
        "%Y-%m-%d %H:%M:%S", timeinfo); 

    // Create log entry 
    ostringstream logEntry; 
    logEntry << "[" << timestamp << "] "
        << levelToString(level) << ": " << message 
        << endl; 

    // Output to console 
    if (m_toConsole)
        cout << logEntry.str(); 

    // Output to log file 
    if (m_logFile.is_open()) { 
      m_logFile << logEntry.str(); 
      m_logFile.flush(); // Ensure immediate write to file 
    } 
} 

  // Converts log level to a string for output 
string Logger::levelToString(LogLevel level) 
{ 
    switch (level) { 
    case VERBOSE: 
      return "VERBOSE"; 
    case INFO: 
      return "INFO"; 
    case WARNING: 
      return "WARNING"; 
    case ERROR: 
      return "ERROR"; 
    case NONE: 
      return "NONE"; 
    default: 
      return "UNKNOWN"; 
    } 
} 


