#include "mk5daemon.h"


void Mk5Daemon_startRecord(Mk5Daemon *D)
{
	char options[];
	char command[];
	char message[];

	snprintf(command, CommandLength, "mk5record %c %s", D->activeBank, options);


	/* use thread, not fork! */
	D->record_pid = fork();
	if(D->record_pid)
	{
		snprintf(message, , "Executing through pipe: %s\n", command);
		Logger_logData(D->log, message)
	}
	else
	{
		pin = popen(command, "r");
		while(!feof(pin))
		{
		}
		fclose(pin);
	}
}

void Mk5Daemon_stopRecord(Mk5Daemon *D)
{
	if(D->record_pid)
	{
		kill(D->record_pid, SIG_INT);

		D->record_pid = 0;
	}
}

void Mk5Daemon_setBank(Mk5Daemon *D, int bank)
{
	if(bank < 0 || bank >= N_BANK)
	{
		return;
	}

	D->activeBank = bank;
}

void Mk5Daemon_setProtect(Mk5Daemon *D, enum WriteProtectState state)
{
}
