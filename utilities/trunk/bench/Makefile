#COMMON = -g -Wall 
COMMON = -Wall -O2
CC=gcc $(COMMON)

vlbi_fake: vlbi_fake.c vheader.c mk5blib.c 
	$(CC) -I$(DIFXROOT)/include -L$(DIFXROOT)/lib -o $@ $^ -lm -lvdifio

