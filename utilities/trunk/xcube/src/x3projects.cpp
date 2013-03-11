/*
 * x3projects.cpp
 *
 * Chris Phillips
 *
 */
#include <stdio.h>
#include <stdlib.h>

#include <iostream>
#include <string>
#include <vector>

#include "projectManagement/Projects.h"

int main(int argc, char **argv) {
  int status;
  string mnt ("/mnt/disk");
  string base ("/xcube");

  Projects projects(mnt, base, 16);
  vector <PROJECT> prjs = projects.pmGetProjects();

  vector<PROJECT>::iterator itp;

  for (itp = prjs.begin(); itp < prjs.end(); itp++) {
    PROJECT pr = *itp;
    printf("\nProject %s %0.1f GB\n", pr.prjName.c_str(),  pr.prjSize/1e9);
    
    status = projects.pmActivateProject(pr.prjName);
    if (status==-1) {
      printf("Error activating project - one is already open\n");
      break;
    }
    if (status==-2) {
      printf("Error activating project - project not found\n");
      break;
    }
    
    vector<STREAM> strms;
    if (projects.pmGetStreams(strms) != 0) {
      fprintf(stdout, "Error retrieving streams for current project\n");
      return -1;
    }
    
    vector<STREAM>::iterator its;

    for (its = strms.begin(); its < strms.end(); its++) {
      STREAM st = *its;
      printf("  %s %d %llu\n", st.strmName.c_str(), st.strmType, st.strmSize);
    }

    status = projects.pmInactivateProject();
    if (status!=0) {
      printf("Failed to close project\n");
      break;
    }
  }

  return EXIT_SUCCESS;
  
}
