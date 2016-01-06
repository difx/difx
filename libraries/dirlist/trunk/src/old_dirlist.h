#ifndef __OLD_DIRLIST_H__
#define __OLD_DIRLIST_H__

#include <sstream>
#include "dirlist.h"

int loadOldDirList(DirList &D, const char *fileName, std::stringstream &error);

int mark5LegacyLoad(DirList &D, const char *vsn, std::stringstream &error); // looks in $MARK5_DIR_PATH, first for .dirlist, then .dir

#endif
