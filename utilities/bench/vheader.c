
#include "vheader.h"

enumstr encodestrs[] = {
  {NONE, "NONE"},
  {AT, "AT"},
  {VLBA, "VLBA"},
  {ENDENUM}
};

enumstr polstrs[] = {
  {NOPOL, "NONE"},
  {R, "R"},
  {L, "L"},
  {X, "X"},
  {Y, "Y"},
  {ENDENUM}
};

enumstr sidestrs[] = {
  {NOSIDE, "NONE"},
  {USB, "USB"},
  {LSB, "LSB"},
  {ENDENUM}
};
 
int resetheader(vhead *header) {
  int i;

  header->headersize = HEADERSIZE;

  header->time[0] = 0;
  header->filetime[0] = 0;
  header->antennaid[0] = ' ';
  header->antennaid[1] = ' ';
  header->antennaname[0] = 0;
  header->experimentid[0] = 0;
  
  header->headerversion = HEADERVERSION;
  header->headersubversion = HEADERSUBVERSION;

  header->recorderversion = 0;
  header->recordersubversion = 0;

  header->numbits = 0;
  header->nchan = 0;
  header->sequence = 0;
  header->bandwidth = 0.0;

  header->encoding = NONE;

  for (i=0; i<MAXCHAN; i++) {
    header->frequency[i] = 0.0;
    header->polarisation[i] = NOPOL;
    header->sideband[i] = NOSIDE;
  }

  return(0);
}

vhead *newheader () {
  int status;
  vhead *header;

  header = malloc(sizeof(vhead));

  if (header==NULL) return NULL;

  status = resetheader(header);

  if (status) {
    free(header);
    return NULL;
  }
  return(header);
}

void destroyheader(vhead *header) {
  
  free(header);

}

int settime(vhead *header, struct tm *date) {

  strftime(header->time, TIMESTR, "%Y%m%d:%H%M%S", date);
  return(0);
}

int settimestr(vhead *header, char *date) {

  if (strlen(date)+1>TIMESTR) return(STRLENERR);

  strcpy(header->time, date);
  return(NOERROR);
}

int setfiletime(vhead *header, struct tm *date) {

  strftime(header->filetime, TIMESTR, "%Y%m%d:%H%M%S", date);
  return(0);
}

int setfiletimestr(vhead *header, char *date) {

  if (strlen(date)+1>TIMESTR) return(STRLENERR);

  strcpy(header->filetime, date);
  return(NOERROR);
}

int setantennaid(vhead *header, const char* antennaid) {
  
  if (strlen(antennaid) != 2) {
    fprintf(stderr, "Antenna ID must be 2 chatacters\n");
    return(-1);
  }

  header->antennaid[0] = antennaid[0];
  header->antennaid[1] = antennaid[1];
  
  return(0);
}

int setantennaname(vhead *header, const char* name) {

  if (strlen(name)+1>ANTENNASTR) return(STRLENERR);

  strcpy(header->antennaname, name);

  return(NOERROR);
}

int setexperimentid(vhead *header, const char* exper) {

  if (strlen(exper)+1>EXPERSTR) return(STRLENERR);

  strcpy(header->experimentid, exper);

  return(NOERROR);
}

int setheaderversion(vhead *header, int major, int minor) {
  header->headerversion = major;
  header->headersubversion = minor;

  return(0);
}

int setrecorderversion(vhead *header, int major, int minor) {
  header->recorderversion = major;
  header->recordersubversion = minor;

  return(0);
}

int setheadersize(vhead *header, int size) {

  header->headersize = HEADERSIZE;
  return(0);
}

int setnumbits(vhead *header, int nbits) {

  header->numbits = nbits;
  return(0);
}

int setnchan(vhead *header, int nchan) {
  
  if (nchan>MAXCHAN) return(NCHAN);

  header->nchan = nchan;
  return(0);
}

int setsequence(vhead *header, int sequence) {
  
  header->sequence = sequence;
  return(0);
}

int setbandwidth(vhead *header, float bandwidth) {

  header->bandwidth = bandwidth;
  return(0);
}

int setencoding(vhead *header, encodingtype encoding) {

  header->encoding = encoding;
  return(NOERROR);
}

int setfrequency(vhead *header, float freq[]) {
  int i;

  if (header->nchan<=0) return(NOCHAN);

  for (i=0; i<header->nchan; i++) 
    header->frequency[i] = freq[i];

  return(NOERROR);
}

int setpolarisation(vhead *header, polarisationtype pol[]) {
  int i;

  if (header->nchan<=0) return(NOCHAN);

  for (i=0; i<header->nchan; i++) 
    header->polarisation[i] = pol[i];

  return(NOERROR);
}

int setsideband(vhead *header, sidebandtype side[]) {
  int i;

  if (header->nchan<=0) return(NOCHAN);

  for (i=0; i<header->nchan; i++) 
    header->sideband[i] = side[i];

  return(NOERROR);
}

int gettime(vhead *header, struct tm *date) {
  int yyyy, mm, dd, hh, min, sec;

  if (sscanf(header->time, "%4d%2d%2d:%2d%2d%2d", 
  	     &yyyy, &mm, &dd, &hh, &min, &sec) == 6) {
    date->tm_sec = sec;
    date->tm_min = min;
    date->tm_hour = hh;
    date->tm_mday = dd;
    date->tm_mon = mm - 1;
    date->tm_year = yyyy - 1900;
    date->tm_isdst = 0;

    return(0);
  } else {
    return(BADTIME);
  }
}

int gettimestr(vhead *header, char *date) {
  date = header->time;
  return(0);
}

int getfiletime(vhead *header, struct tm *date) {
  int yyyy, mm, dd, hh, min, sec;

  if (sscanf(header->filetime, "%4d%2d%2d:%2d%2d%2d", 
  	     &yyyy, &mm, &dd, &hh, &min, &sec) == 6) {
    date->tm_sec = sec;
    date->tm_min = min;
    date->tm_hour = hh;
    date->tm_mday = dd;
    date->tm_mon = mm - 1;
    date->tm_year = yyyy - 1900;
    date->tm_isdst = 0;

    return(0);
  } else {
    return(BADTIME);
  }
}

int getfiletimestr(vhead *header, char *date) {
  date = header->filetime;
  return(0);
}

char* enum2str(int val, enumstr *enumarray) {
  enumstr *enumptr;

  for (enumptr=enumarray; enumptr->val!=ENDENUM; enumptr++) {
    if (enumptr->val==val) return enumptr->str;
  }
  return(NULL);
}

int str2enum(char *str, enumstr *enumarray) {
  enumstr *enumptr;

  for (enumptr=enumarray; enumptr->val!=ENDENUM; enumptr++) {
    if (strcasecmp(str, enumptr->str)==0) return enumptr->val;
  }
  return(0);
}


#define ADDLINE() ({   \
  linesize = strlen(line); \
  if (used+linesize>headsize) { \
    free(headblock); \
    return(HEADSIZE); \
  } \
  memcpy(headblock+used, line, linesize); \
  used += linesize; \
 } )

int writeheader(vhead *header, int file, char **buf) {
  int nwrote;
  char *headblock;
  char line[VSTRSIZE];
  char* enumstr;
  int linesize, headsize, used, i, retval;

  retval = NOERROR;

  headsize = header->headersize;
  used = 0;

  headblock = calloc(headsize, 1);

  sprintf(line, "TIME %s\n", header->time);
  ADDLINE();

  if (header->filetime[0] != 0) {
    sprintf(line, "FILETIME %s\n", header->filetime);
    ADDLINE();
  }

  sprintf(line, "HEADERSIZE %d\n", headsize);
  ADDLINE();

  sprintf(line, "HEADERVERSION %d.%d\n", header->headerversion, 
	  header->headersubversion);
  ADDLINE();

  sprintf(line, "RECORDERVERSION %d.%d\n", header->recorderversion, 
	  header->recordersubversion);
  ADDLINE();

  if (header->antennaid[0] !=' ' ||  header->antennaid[1] != ' ') 
    sprintf(line, "ANTENNAID %c%c\n", header->antennaid[0], header->antennaid[1]);
  else 
    strcpy(line, "ANTENNAID xx\n");
  
  ADDLINE();

  if (strlen(header->antennaname)==0)
    strcpy(line, "ANTENNANAME null\n");
  else
    sprintf(line, "ANTENNANAME %s\n", header->antennaname);
  ADDLINE();

  if (strlen(header->experimentid)==0)
    strcpy(line, "EXPERIMENTID null\n");
  else
    sprintf(line, "EXPERIMENTID %s\n", header->experimentid);
  ADDLINE();

  sprintf(line, "NUMBITS %d\n", header->numbits);
  ADDLINE();

  sprintf(line, "NCHAN %d\n", header->nchan);
  ADDLINE();

  sprintf(line, "SEQUENCE %d\n", header->sequence);
  ADDLINE();

  sprintf(line, "BANDWIDTH %.1f\n", header->bandwidth);
  ADDLINE();

  enumstr = enum2str(header->encoding, encodestrs);
  if (enumstr==NULL) {
    fprintf(stderr, "Failed to decode encoding %d\n", header->encoding);
    strcpy(line, "ENCODING NONE");
  } else {
    sprintf(line, "ENCODING %s\n", enumstr);
  }
  ADDLINE();

  if (header->frequency[0]>0 && header->nchan>0) {
    strcpy(line, "FREQUENCY");
    for (i=0; i<header->nchan; i++) {
      sprintf(line, "%s %.3f", line, header->frequency[i]);
    }
    strcat(line, "\n");
    ADDLINE();
  }

  if (header->polarisation[0]!=NOPOL && header->nchan>0) {
    strcpy(line, "POLARISATION");
    for (i=0; i<header->nchan; i++) {
      enumstr = enum2str(header->polarisation[i], polstrs);
      if (enumstr==NULL) {
	fprintf(stderr, "Failed to decode polarisation %d\n", header->polarisation[i]);
	strcat(line, " NONE");
      } else {
	sprintf(line, "%s %s", line, enumstr);
      }
    }
    strcat(line, "\n");
    ADDLINE();
  }

  if (header->sideband[0]!=NOSIDE && header->nchan>0) {
    strcpy(line, "SIDEBAND");
    for (i=0; i<header->nchan; i++) {
      enumstr = enum2str(header->sideband[i], sidestrs);
      if (enumstr==NULL) {
	fprintf(stderr, "Failed to decode sideband %d\n", header->sideband[i]);
	strcat(line, " NONE");
      } else {
	sprintf(line, "%s %s", line, enumstr);
      }
    }
    strcat(line, "\n");
    ADDLINE();
  }

  strcpy(line, "END\n");
  ADDLINE();

  if (file) {
    nwrote = write(file, headblock, header->headersize);
    if (nwrote==0) 
      retval = FILEWRITEERROR2; 
    else if (nwrote<0) 
      retval = FILEWRITEERROR;
    else if (nwrote<header->headersize) 
      retval = FILEWRITEERROR1;
  }

  if (buf) {
    *buf = headblock;
  } else {
    free(headblock);
  }

  return(retval);
}


int readline(FILE *file, char **buf, char *line) {
  int  len, shift;
  char *char_ptr;

  len = 0;

  while (len==0) {
    if (file!=NULL) {
      /* Read next line from text line and check for end of file */
      if (fgets(line,VSTRSIZE,file)==NULL) {
	return(HEADEOF);
      }
    } else {
      char_ptr = strchr(*buf, '\n');
      if (char_ptr-*buf > VSTRSIZE) {
	return(STRLENERR);
      }
      memcpy(line, *buf, char_ptr-*buf);
      line[char_ptr-*buf] = '\0';
      *buf = char_ptr+1;
    }
     
    len = strlen(line);
    // fgets includes trailing \n character that must be removed if present
    if (line[len-1]=='\n') {
      len--;
      line[len] = '\0';
    }
    if (len==0) continue;
    
    // Strip off any text past the comment character
    char_ptr = strchr(line, COMMENT_CHAR);
    if (char_ptr != NULL) {
      *char_ptr = '\0';
      len = strlen(line);
      if (len==0) continue;
    }
    
    // Remove trailing white space characters
    
    for (char_ptr=&line[len-1];strchr(WHITESPACE,*char_ptr)!=NULL;
	 char_ptr--,len--)
      *char_ptr = '\0';
    
    /* Remove white space prefix */
    for (char_ptr=line;
	 (strchr(WHITESPACE,*char_ptr)!=NULL)&&(*char_ptr!='\0');
	 char_ptr++);
    if (*char_ptr!='\0') { // There must have been white space
      shift = char_ptr - line;
      if (shift>0) {
	len = len - shift;
	memmove(line, line+shift, len);
      }
    }
  }

  if (strcasecmp(line, "END")==0) return(HEADEOF);
  return(NOERROR);
}

#define GETINT(x) \
   header->x = strtol(value, &endptr, 10);\
   if (!(endptr[0]==0)) {		  \
     if (endptr==value) { \
       fprintf(stderr, "Error interpreting %s %s\n", keystr, value); \
       return(BADVALUE); \
     } else { \
     fprintf(stderr, "Warning: Extra characters for %s: %s\n", keystr, endptr); \
     } \
   } 

#define GETFLOAT(x) \
   header->x = (float)strtod(value, &endptr);\
   if (!(endptr[0]==0)) {		     \
     if (endptr==value) { \
       fprintf(stderr, "Error interpreting %s %s\n", keystr, value); \
       return(BADVALUE); \
     } else { \
       fprintf(stderr, "Warning: Extra characters for %s: %s\n", keystr, endptr); \
     } \
   } 

#define GETSTR(x,y) \
   if (strlen(value)>y) { \
     fprintf(stderr, "Warning %s value %s too long\n", keystr, value); \
     return(LONGSTR); \
    } \
    strcpy(header->x, value); 

#define GETVERSION(x,y) \
   status = sscanf(value, "%u.%u", &tmp1, &tmp2); \
   if (status!=2) { \
	fprintf(stderr, "Error interpreting %s %s\n", keystr, value); \
	return(BADVALUE); \
   } \
   header->x = tmp1; \
   header->y = tmp2; 

#define GETENUMARRAY(x, y) \
  endptr = value; \
  for (i=0; i<MAXCHAN; i++) { \
    startptr = strsep(&value, WHITESPACE); \
    if (startptr==NULL) break; \
    header->x[i] = str2enum(startptr, y); \
  } 

int readheader(vhead *header, int file, char *buf) {
  int status, i;
  unsigned int tmp1, tmp2;
  char line[VSTRSIZE], *keystr, *value, *endptr, *startptr, *pbuf;
  size_t sook;

  FILE* filestream = 0;

  if (file!=0) {
    // Create a file stream so we can use fgets
    filestream = fdopen(file, "r");
  } else {
    pbuf = buf;
    filestream = NULL;
  }
  
  while ((status=readline(filestream, &pbuf, line))==NOERROR) {
    value = line;
    keystr = strsep(&value, WHITESPACE);

    if (value==0 || strlen(value)==0) {
      fprintf(stderr, "No value for %s\n", keystr);
      fclose(filestream);
      return(NOVALUE);
    }

    if (strcasecmp(keystr,"TIME")==0) {
      GETSTR(time, TIMESTR);
    
    } else if (strcasecmp(keystr,"FILETIME")==0) {
      GETSTR(filetime, TIMESTR);

    } else if (strcasecmp(keystr,"HEADERSIZE")==0) {
      GETINT(headersize);

    } else if (strcasecmp(keystr,"HEADERVERSION")==0) {
      GETVERSION(headerversion, headersubversion);

    } else if (strcasecmp(keystr,"RECORDERVERSION")==0) {
      GETVERSION(recorderversion, recordersubversion);

    } else if (strcasecmp(keystr,"ANTENNAID")==0) {
      status = sscanf(value, "%c%c", &header->antennaid[0], 
		      &header->antennaid[1]);
      if (status!=2) {
	fprintf(stderr, "Error interpreting %s %s\n", keystr, value);
	fclose(filestream);
	return(BADVALUE);
      }

    } else if (strcasecmp(keystr,"ANTENNANAME")==0) {
      GETSTR(antennaname, ANTENNASTR);

    } else if (strcasecmp(keystr,"EXPERIMENTID")==0) {
      GETSTR(experimentid, EXPERSTR);
      
    } else if (strcasecmp(keystr,"NUMBITS")==0) {
      GETINT(numbits);

    } else if (strcasecmp(keystr,"NCHAN")==0) {
      GETINT(nchan);

    } else if (strcasecmp(keystr,"SEQUENCE")==0) {
      GETINT(sequence);

    } else if (strcasecmp(keystr,"BANDWIDTH")==0) {
      GETFLOAT(bandwidth);

    } else if (strcasecmp(keystr,"ENCODING")==0) {
      header->encoding = str2enum(value, encodestrs);
      //printf("%s: %s\n", keystr, enum2str(header->encoding, encodestrs));

    } else if (strcasecmp(keystr,"FREQUENCY")==0) {
      endptr = value;
      for (i=0; i<MAXCHAN; i++) {
	startptr = endptr;
	header->frequency[i] = (float)strtod(startptr, &endptr);
	if (endptr==startptr) break;
      }
      if (endptr!=NULL && endptr!=startptr) {
	fprintf(stderr, "Warning: Ignoring FREQUENCY %s\n", endptr);
      }

    } else if (strcasecmp(keystr,"POLARISATION")==0) {
      GETENUMARRAY(polarisation, polstrs);

    } else if (strcasecmp(keystr,"SIDEBAND")==0) {
      GETENUMARRAY(sideband, sidestrs);

    } else {
      fprintf(stderr, "Warning: Unknown header keyword %s\n", keystr);
    }
  }

  if (buf==NULL) {
    // Skip to the end of the header section
    sook = lseek(file, header->headersize, SEEK_SET);
    if (sook==-1) {
      perror("Seeking to end of header");
      return(HEADERSEEKERROR);
    } else if (sook!=header->headersize) {
      fprintf(stderr, "Did not seek to end of header\n");
      return(HEADERSEEKERROR);
    }
  }
  return(NOERROR);
}


int readprofile(vhead *header, const char *profilename) {
  /* Read setup info from a text file. Save it directly into the header */
  int status, i;
  char line[VSTRSIZE], *value, *keystr, *endptr, *startptr, msg[VSTRSIZE];
  FILE* file;

  file = fopen(profilename, "r");

  if (file==0) {
    sprintf(msg, "Opening %s", profilename);
    perror(msg);
    return(FILEOPENERROR);
  }

  printf("Reading profile %s\n", profilename);

  while ((status=readline(file, NULL, line))==NOERROR) {
    value = line;
    keystr = strsep(&value,WHITESPACE);

    if (strlen(value)==0) continue; // Ignore these

    if (strcasecmp(keystr,"ANTENNAID")==0) {
      status = sscanf(value, "%c%c", &header->antennaid[0], 
		      &header->antennaid[1]);
      if (status!=2) {
	fprintf(stderr, "Error interpreting %s %s\n", keystr, value);
	return(BADVALUE);
      }

    } else if (strcasecmp(keystr,"ANTENNANAME")==0) {
      GETSTR(antennaname, ANTENNASTR);

    } else if (strcasecmp(keystr,"EXPERIMENTID")==0) {
      GETSTR(experimentid, EXPERSTR);
      
    } else if (strcasecmp(keystr,"NUMBITS")==0) {
      GETINT(numbits);

    } else if (strcasecmp(keystr,"ENCODING")==0) {
      header->encoding = str2enum(value, encodestrs);

    } else if (strcasecmp(keystr,"FREQUENCY")==0) {
      endptr = value;
      for (i=0; i<MAXCHAN; i++) {
	startptr = endptr;
	header->frequency[i] = (float)strtod(startptr, &endptr);
	if (endptr==startptr) break;
      }
      if (endptr!=NULL && endptr!=startptr) {
	fprintf(stderr, "Warning: Ignoring FREQUENCY %s\n", endptr);
      }

    } else if (strcasecmp(keystr,"POLARISATION")==0) {
      GETENUMARRAY(polarisation, polstrs);

    } else if (strcasecmp(keystr,"SIDEBAND")==0) {
      GETENUMARRAY(sideband, sidestrs);

    } 
  }

  fclose(file);

  return(NOERROR);
}

double tm2mjd(struct tm date) {
  int y, c;
  double dayfrac;

  if (date.tm_mon < 2) {
    y = date.tm_mon+1900-1;
  } else {
    y = date.tm_year+1900;
  }

  c = y/100;
  y = y-c*100;

  dayfrac = ((date.tm_hour*60.0+date.tm_min)*60.0+date.tm_sec)
    /(60.0*60.0*24.0);

  return(cal2mjd(date.tm_mday, date.tm_mon+1, date.tm_year+1900)
	 +dayfrac);
}

double cal2mjd(int day, int month, int year) {
  int m, y, c, x1, x2, x3;

  if (month <= 2) {
    m = month+9;
    y = year-1;
  } else {
    m = month-3;
    y = year;
  }

  c = y/100;
  y = y-c*100;

  x1 = 146097*c/4;
  x2 = 1461*y/4;
  x3 = (153*m+2)/5;

  return(x1+x2+x3+day-678882);
}
