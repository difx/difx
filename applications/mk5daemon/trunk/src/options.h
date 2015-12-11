/**
 * Storage class for the user options passed to mk5daemon
 **/
class Options
{
public:
	int isMk5;
	int isMk6;
	int isHeadNode;
	int isEmbedded;
	int noSu;
	const char *userID;
	const char *providedHostname;
	const char *logPath;

public:
	Options();
	int validate();
	void setDefaults();

};
