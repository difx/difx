/*
 * (c) Massachusetts Institute of Technology, 2013..2023
 * (c) Geoffrey B. Crew, 2013..2023
 *
 * $Id: vdifthr.h 5734 2023-03-15 18:03:05Z gbc $
 *
 * This file does the work of building threads.  Parts of the logic
 * are similar to sequence building, but we need to have a sequence
 * for each thread requested, and allow skipping over parts of fragments.
 */
#ifndef vdifthr_h
#define vdifthr_h

extern int create_vthreads(void);

#endif /* vdifthr_h*/
/*
 * eof
 */
