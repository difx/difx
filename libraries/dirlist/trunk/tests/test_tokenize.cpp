#include <iostream>
#include "parse.h"

int main(int argc, char **argv)
{
	std::vector<std::string> tokens;
	std::vector<std::string>::const_iterator it;
	std::string comment;
	bool ok;

	for(int a = 1; a < argc; ++a)
	{
		std::cout << "Input string: " << argv[a] << std::endl;

		ok = tokenize(tokens, comment, argv[a]);
		if(!ok)
		{
			std::cout << "Error tokenizing" << std::endl;
		}
		else
		{
			for(it = tokens.begin(); it != tokens.end(); ++it)
			{
				std::cout << "Token: " << *it << std::endl;
			}
			std::cout << "Comment: " << comment << std::endl;
		}
	}

	return 0;
}
