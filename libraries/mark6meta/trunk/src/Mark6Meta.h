/* 
 * File:   Mark6Meta.h
 * Author: Helge Rottmann (MPIfR)
 *
 * Created on 30. September 2015, 12:22
 */

#ifndef MARK6META_H
#define	MARK6META_H

#include <string>
#include <vector>
#include <map>
#include "Mark6Module.h"

/**
 * Custom exception class for reporting Mark6 related metadata errors
 */
class Mark6InvalidMetadata : public std::exception 
{
private:
    std::string err_msg;

public:
    Mark6InvalidMetadata(std::string msg) : err_msg(msg) {};
    ~Mark6InvalidMetadata() throw() {};
    const char *what() const throw() { return this->err_msg.c_str(); };
};

class Mark6Meta {
public:
    Mark6Meta();
    Mark6Meta(const Mark6Meta& orig);
    virtual ~Mark6Meta();
    std::string getEMSN() const;
    void parse(std::string);
    void reset();
    const std::map<int, std::string> &getSerials();
    std::vector<std::string> getGroup() const;
  
private:
    std::string eMSN_m;
    std::vector<std::string> group_m;
    std::map<int,std::string> serials_m;
};

#endif	/* MARK6META_H */

