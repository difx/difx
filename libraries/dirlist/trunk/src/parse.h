#ifndef __PARSE_H__
#define __PARSE_H__

#include <string>
#include <vector>

// inputString here must have already been tokenized
void separateStringList(std::vector<std::string> &outputList, const std::string &inputString);

// For now, this is simple: just look to see if first and last quotes are same, and if so, return 1..-2
// otherwise return 0..-1
std::string unquoteString(const std::string &str);

bool tokenize(std::vector<std::string> &tokens, std::string &comment, const std::string &inputString);


#endif
