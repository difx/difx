#ifndef FF_SIGNAL_HANDLER_H__
#define FF_SIGNAL_HANDLER_H__

#include <signal.h>

void
fourfit_signal_handler(int signal_value);

void 
fourfit_register_signal_handler(struct sigaction *handler_action);

#endif /* end of include guard: FF_SIGNAL_HANDLER_H__ */
