#include "mk4_data.h"
#include "pass_struct.h"

int
postcorrect (pass)
struct type_pass *pass;
    {
    extern int do_accounting;
    msg ("Postcorrect routine stubbed for now", 0);
    if (do_accounting) account ("PostCorrect Data");
    return (0);
    }
