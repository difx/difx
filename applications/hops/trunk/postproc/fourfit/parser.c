/*******************************************************************************
*                                                                              *
* parser uses a table-driven finite-state-machine to verify the syntactic      *
*        validity of the input token string, and to use the parameters and     *
*        conditions to generate a chained string of control blocks.            *
*                                                                              *
*                                                   rjc  92.12.22              *
*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "parser.h"
#include "control.h"
#include "mk4_sizes.h"

#define FALSE 0
#define TRUE 1
                                    // multiple pols. mapped into index 0 and 1
#define LXH 0
#define RYV 1

int parser ()
   {
   extern struct token_struct *tokens;   /* input struct of tokens & values   */
   extern double *float_values;          /* array of actual fl. pt. values    */
   extern struct fsm_table_entry *fsm_base;             /* start of fsm table */
   extern struct c_block *cb_head;                  /* start of c_block chain */
   extern char *char_values;             /* pointer to array of actual strings*/
   extern int msglev;


   int i,
       nstat,
       ntok,
       next_state,
       found,
       action,
       tnum,
       tval,
       toknum,
       negation,
       nv,
       sideband,
       parsed_scan[2],
       parsed_knot[5],
       ns;

   char parsed_f_group,
        parsed_station,
        parsed_baseline[2],
        parsed_source[32],
        parsed_codes[MAXFREQ],
        *psc,
        chan[2];

   float fval;

   struct c_block *cond_start,
                  *cb_start,      /* start of appplicable blocks in the event
                                                    of a complex IF condition */
                  *cb_ptr,
                  *cb_tail;                 /* points to last cblock in chain */


   msg ("Following parser triples are token_number:state:category", -2);

   ntok = 0;                   /* point to beginning of input token structure */

   negation = FALSE;                     /* start out not negating conditions */

                                        /* find tail of current c_block chain */
   if (cb_head != NULL)
      for (cb_ptr=cb_head; cb_ptr != NULL; cb_ptr=cb_ptr->cb_chain)
         cb_tail = cb_ptr;

                                        /* allocate space for generic c_block */
   if ((cb_ptr =  (struct c_block *) malloc (sizeof (struct c_block))) == NULL)
      {
      msg ("Error allocating c_block memory.",2);
      return (-1);
      }

   nullify_cblock (cb_ptr);                    /* nullify the generic c_block */

   if (cb_head == NULL)
      cb_head = cb_ptr;                             /* this is start of chain */

   else
      {
      cb_tail->cb_chain = cb_ptr;                       /* chain in new block */
      cb_tail = cb_ptr;                                  /* and readjust tail */
      }

   cond_start = cb_ptr;               /* initialize condition block start ptr */

   next_state = BLOCK_INTERIOR;                    /* init. to starting state */

   do
      {
      found = FALSE;

                        /* loop over states to find matching state transition */

      if (msglev <= -2)                    /* replace with new msg if no lf's */
          {
          if (ntok % 7 == 0)
              printf ("\n");          /* print line feed after every 7 tokens */
          printf ("%d:%d:%d  ",ntok,next_state,tokens[ntok].category);
          }

      for (nstat=0; fsm_base[nstat].current_state != 0; nstat++)
         if (fsm_base[nstat].current_state == next_state
          && (   fsm_base[nstat].token_type == tokens[ntok].category
              || fsm_base[nstat].token_type == MATCH_ALL))
            {
            next_state = fsm_base[nstat].next_state;
            action = fsm_base[nstat].action;
            tval = tokens[ntok].value;
            tnum = tokens[ntok].symbol;
            found = TRUE;
            break;
            }

      if (found)
         {
         switch (action)         /* perform action appropriate for this state */
            {
            case CLEAR_FREQS:                /* clear all freq codes to start */
               for (cb_ptr=cond_start; cb_ptr!=NULL; cb_ptr=cb_ptr->cb_chain)
                  for (i=0; i<MAXFREQ; i++)
                     cb_ptr->frequency[i] = 0;
                                                /* and then save token number */
               
            case SAVE_TOKEN_NUM:
               toknum = tnum;
               nv = 0;                          /* clear vector element index */
               break;


            case INSERT_PAR:    /* insert scalar integer and float parameters */
               for (cb_ptr=cond_start; cb_ptr!=NULL; cb_ptr=cb_ptr->cb_chain)
                   if (toknum == X_CRC_)
                       cb_ptr -> x_crc = tval;
                   else if (toknum == Y_CRC_)
                       cb_ptr -> y_crc = tval;
                   else if (toknum == X_SLIP_SYNC_)
                       cb_ptr -> x_slip_sync = tval;
                   else if (toknum == Y_SLIP_SYNC_)
                       cb_ptr -> y_slip_sync = tval;
                   else if (toknum == ADHOC_PHASE_)
                       cb_ptr -> adhoc_phase = tval;
                   else if (toknum == SKIP_)
                       cb_ptr -> skip = TRUE;
                   else if (toknum == MAX_PARITY_)
                       cb_ptr -> max_parity = float_values[tval];
                   else if (toknum == REF_FREQ_)
                       cb_ptr -> ref_freq = float_values[tval];
                   else if (toknum == RA_OFFSET_)
                       cb_ptr -> ra_offset = float_values[tval];
                   else if (toknum == DEC_OFFSET_)
                       cb_ptr -> dec_offset = float_values[tval];
                   else if (toknum == ADHOC_TREF_)
                       cb_ptr -> adhoc_tref = float_values[tval];
                   else if (toknum == ADHOC_PERIOD_)
                       {
                       if (float_values[tval] > 0)
                           cb_ptr -> adhoc_period = float_values[tval];
                       else
                           {
                           msg ("Ad hoc period must be greater than 0!",2);
                           return (-1);
                           }
                       }
                   else if (toknum == ADHOC_AMP_)
                       cb_ptr -> adhoc_amp = float_values[tval];
                   else if (toknum == PC_MODE_)      /* insert phase cal mode */
                       {                             /* into non-wildcard stn */
                       if (cb_ptr -> baseline[0] == WILDCARD)
                           cb_ptr -> pc_mode.rem = tval; 
                       if (cb_ptr -> baseline[1] == WILDCARD)
                           cb_ptr -> pc_mode.ref = tval;
                       }
                   else if (toknum == PC_PERIOD_)    // insert phase cal period
                       {                             // into non-wildcard stn
                       if (cb_ptr -> baseline[0] == WILDCARD)
                           cb_ptr -> pc_period.rem = tval; 
                       if (cb_ptr -> baseline[1] == WILDCARD)
                           cb_ptr -> pc_period.ref = tval;
                       }
                   else if (toknum == LSB_OFFSET_)       /* insert LSB offset */
                       {
                       if (cb_ptr -> baseline[0] == WILDCARD)
                           cb_ptr -> lsb_offset.rem = float_values[tval];
                       if (cb_ptr -> baseline[1] == WILDCARD)
                           cb_ptr -> lsb_offset.ref = float_values[tval];
                       }
                   else if (toknum == START_)
                       cb_ptr -> time_span[0] = tval;
                   else if (toknum == STOP_)
                       cb_ptr -> time_span[1] = tval;
                   else if (toknum == SWITCHED_)
                       cb_ptr -> switched_mode = tval;
                   else if (toknum == PERIOD_)
                       cb_ptr -> switched_period = tval;
                   else if (toknum == USE_SAMPLES_)
                       cb_ptr -> use_samples = tval;
                   else if (toknum == T_COHERE_)
                       cb_ptr -> t_cohere = float_values[tval];
                   else if (toknum == IONOSPHERE_)
                       {
                       if (cb_ptr -> baseline[0] == WILDCARD)
                           cb_ptr -> ionosphere.rem = float_values[tval];
                       if (cb_ptr -> baseline[1] == WILDCARD)
                           cb_ptr -> ionosphere.ref = float_values[tval];
                       }
                   else if (toknum == STATION_DELAY_)
                       {
                       if (cb_ptr -> baseline[0] == WILDCARD)
                           cb_ptr -> station_delay.rem = 1e-9 * float_values[tval];
                       if (cb_ptr -> baseline[1] == WILDCARD)
                           cb_ptr -> station_delay.ref = 1e-9 * float_values[tval];
                       }
                   else if (toknum == PC_DELAY_L_
                         || toknum == PC_DELAY_X_)
                       {
                       if (cb_ptr -> baseline[0] == WILDCARD)
                           cb_ptr -> pc_delay_l.rem = 1e-9 * float_values[tval];
                       if (cb_ptr -> baseline[1] == WILDCARD)
                           cb_ptr -> pc_delay_l.ref = 1e-9 * float_values[tval];
                       }
                   else if (toknum == PC_DELAY_R_ 
                         || toknum == PC_DELAY_Y_)
                       {
                       if (cb_ptr -> baseline[0] == WILDCARD)
                           cb_ptr -> pc_delay_r.rem = 1e-9 * float_values[tval];
                       if (cb_ptr -> baseline[1] == WILDCARD)
                           cb_ptr -> pc_delay_r.ref = 1e-9 * float_values[tval];
                       }
                   else if (toknum == DC_BLOCK_)
                       cb_ptr -> dc_block = tval;
                   else if (toknum == SAMPLERS_)
                       {
                       if (tval >= MAX_SAMP)
                           {
                           msg ("too many samplers specified", 2);
                           return (-1);
                           }
                       cb_ptr -> nsamplers = tval;
                       ns = 0;           // next string encountered will be 0th
                       psc = cb_ptr -> sampler_codes;  // point to beg of array
                       }
                   else if (toknum == OPTIMIZE_CLOSURE_)
                       cb_ptr -> optimize_closure = tval;
                   else if (toknum == ION_NPTS_)
                       cb_ptr -> ion_npts = tval;
                   else if (toknum == INTERPOLATOR_)
                       cb_ptr -> interpolator = tval;
                   else if (toknum == WEAK_CHANNEL_)
                       cb_ptr -> weak_channel = float_values[tval];
                   else if (toknum == PC_AMP_HCODE_)
                       cb_ptr -> pc_amp_hcode = float_values[tval];
                   else if (toknum == FMATCH_BW_PCT_)
                       cb_ptr -> fmatch_bw_pct = float_values[tval];
                   else if (toknum == MBD_ANCHOR_)
                       cb_ptr -> mbd_anchor = tval;
                   else if (toknum == ION_SMOOTH_)
                       cb_ptr -> ion_smooth = tval;
               break;


            case INSERT_V_PAR:
               for (cb_ptr=cond_start; cb_ptr!=NULL; cb_ptr=cb_ptr->cb_chain)
                   if (toknum == INDEX_)
                       {
                       if (nv == 2*MAXFREQ)
                           {
                           msg ("Too many index numbers",2);
                           return (-1);
                           }
                       cb_ptr -> index[nv] = tval;
                       }

                   else if (toknum == PC_PHASE_)  /* is this phase cal phase? */
                       {
                       i = fcode(parsed_codes[nv]);
                       if (i<0 || i>MAXFREQ-1)
                           {
                           msg ("Invalid phase cal frequency code",2);
                           return (-1);
                           }
                                         /* get phases from appropriate place */
                       if (tokens[ntok].category == INTEGER)
                           fval = tval;
                       else
                           fval = float_values[tval];

                                                   /* phase normally gets stored
                                                      for only one station, and
                                                      into correct freq slot.
                                                      If both specified, set remote
                                                      phase cal to value, and zero
                                                      out the reference value. */
                       if (cb_ptr -> baseline[1] == WILDCARD)
                           {        // both polarizations get the same value
                           cb_ptr -> pc_phase[i][LXH].ref = fval;
                           cb_ptr -> pc_phase[i][RYV].ref = fval;
                           }
                       else if (cb_ptr -> baseline[0] == WILDCARD)
                           {
                           cb_ptr -> pc_phase[i][LXH].rem = fval;
                           cb_ptr -> pc_phase[i][RYV].rem = fval;
                           }
                       else 
                           {
                           cb_ptr -> pc_phase[i][LXH].ref = 0.0;
                           cb_ptr -> pc_phase[i][RYV].ref = 0.0;
                           cb_ptr -> pc_phase[i][LXH].rem = fval;
                           cb_ptr -> pc_phase[i][RYV].rem = fval;
                           }
                       }

                   else if (toknum == PC_PHASE_L_
                         || toknum == PC_PHASE_X_)  // is this L/X/H phase cal phase?
                       {
                       i = fcode(parsed_codes[nv]);
                       if (i<0 || i>MAXFREQ-1)
                           {
                           msg ("Invalid phase cal frequency code",2);
                           return (-1);
                           }
                                         /* get phases from appropriate place */
                       if (tokens[ntok].category == INTEGER)
                           fval = tval;
                       else
                           fval = float_values[tval];

                                                   /* phase normally gets stored
                                                      for only one station, and
                                                      into correct freq slot.
                                                      If both specified, set remote
                                                      phase cal to value, and zero
                                                      out the reference value. */
                       if (cb_ptr -> baseline[1] == WILDCARD)
                           cb_ptr -> pc_phase[i][LXH].ref = fval;
                       else if (cb_ptr -> baseline[0] == WILDCARD)
                           cb_ptr -> pc_phase[i][LXH].rem = fval;
                       else 
                           {
                           cb_ptr -> pc_phase[i][LXH].ref = 0.0;
                           cb_ptr -> pc_phase[i][LXH].rem = fval;
                           }
                       }

                   else if (toknum == PC_PHASE_R_
                         || toknum == PC_PHASE_Y_)  // is this R/Y/V phase cal phase?
                       {
                       i = fcode(parsed_codes[nv]);
                       if (i<0 || i>MAXFREQ-1)
                           {
                           msg ("Invalid phase cal frequency code",2);
                           return (-1);
                           }
                                         /* get phases from appropriate place */
                       if (tokens[ntok].category == INTEGER)
                           fval = tval;
                       else
                           fval = float_values[tval];

                                                   /* phase normally gets stored
                                                      for only one station, and
                                                      into correct freq slot.
                                                      If both specified, set remote
                                                      phase cal to value, and zero
                                                      out the reference value. */
                       if (cb_ptr -> baseline[1] == WILDCARD)
                           cb_ptr -> pc_phase[i][RYV].ref = fval;
                       else if (cb_ptr -> baseline[0] == WILDCARD)
                           cb_ptr -> pc_phase[i][RYV].rem = fval;
                       else 
                           {
                           cb_ptr -> pc_phase[i][RYV].ref = 0.0;
                           cb_ptr -> pc_phase[i][RYV].rem = fval;
                           }
                       }

                   else if (toknum == PC_FREQ_)    /* is this phase cal freq? */
                       {
                       i = fcode(parsed_codes[nv]);
                       if (i<0 || i>MAXFREQ-1)
                           {
                           msg ("Invalid phase cal frequency code",2);
                           return (-1);
                           }
                                          /* get freqs from appropriate place */
                       if (tokens[ntok].category == INTEGER)
                           fval = tval;
                       else
                           fval = float_values[tval];
                                                   /* freq normally gets stored
                                                      for only one station, and
                                                      into correct freq slot  */
                       if (cb_ptr -> baseline[1] == WILDCARD)
                           {
                           cb_ptr -> pc_freq[i].ref = fval;
                                          /* if both are wild cards, set both */
                           if (cb_ptr -> baseline[0] == WILDCARD)
                               cb_ptr -> pc_freq[i].rem = fval;
                           }
                       else if (cb_ptr -> baseline[0] == WILDCARD)
                           cb_ptr -> pc_freq[i].rem = fval;
                       else                     /* both stations are specific */
                           {
                           cb_ptr -> pc_freq[i].ref = fval;
                           cb_ptr -> pc_freq[i].rem = fval;
                           }
                       }


                   else if (toknum == PC_TONEMASK_)  // pcal tone exclusion mask?
                       {
                       i = fcode(parsed_codes[nv]);
                       if (i<0 || i>MAXFREQ-1)
                           {
                           msg ("Invalid pcal freq code in tonemask",2);
                           return (-1);
                           }
                                                   // tonemask normally gets stored
                                                   // for only one station, and
                                                   // into correct freq chan slot
                       if (cb_ptr -> baseline[1] == WILDCARD)
                           cb_ptr -> pc_tonemask[i].ref = tval;
                       if (cb_ptr -> baseline[0] == WILDCARD)
                           cb_ptr -> pc_tonemask[i].rem = tval;
                       }


                   else if (toknum == GATES_) /*are these freq. switch gates? */
                       {
                       i = fcode(parsed_codes[nv/2]);
                       if (i<0 || i>MAXFREQ-1)
                           {
                           msg ("Invalid gates frequency code",2);
                           return (-1);
                           }
                                          /* get gates from appropriate place */
                       if (tokens[ntok].category == INTEGER)
                           fval = tval;
                       else
                           fval = float_values[tval];
                       
                       if (nv % 2)                   /* on_delay or duration? */
                           cb_ptr -> gates[i].duration = fval;
                       else
                           cb_ptr -> gates[i].on_delay = fval;
                       }

                   else if (toknum == SB_WIN_)  
                       {
                       if (nv > 1)
                           {
                           msg ("Too many sb_win numbers",2);
                           return (-1);
                           }
                       cb_ptr -> sb_window[nv] = 
                           (tokens[ntok].category == INTEGER) ?  tval : float_values[tval];
                       }

                   else if (toknum == MB_WIN_)
                       {
                       if (nv > 1)
                           {
                           msg ("Too many mb_win numbers",2);
                           return (-1);
                           }
                       cb_ptr -> mb_window[nv] = 
                           (tokens[ntok].category == INTEGER) ?  tval : float_values[tval];
                       }

                   else if (toknum == DR_WIN_)
                       {
                       if (nv > 1)
                           {
                           msg ("Too many dr_win numbers",2);
                           return (-1);
                           }
                       cb_ptr -> dr_window[nv] = 
                           (tokens[ntok].category == INTEGER) ?  tval : float_values[tval];
                       }

                   else if (toknum == ION_WIN_)
                       {
                       if (nv > 1)
                           {
                           msg ("Too many ion_win numbers",2);
                           return (-1);
                           }
                       cb_ptr -> ion_window[nv] = 
                           (tokens[ntok].category == INTEGER) ?  tval : float_values[tval];
                       }

                   else if (toknum == ADHOC_POLY_)
                       {
                       if (nv > 5)
                           {
                           msg ("More than max. of 6 phase polynomial numbers",2);
                           return (-1);
                           }
                                   /* get coefficients from appropriate place */
                       if (tokens[ntok].category == INTEGER)
                           fval = tval;
                       else
                           fval = float_values[tval];

                       cb_ptr -> adhoc_poly[nv] = fval;
                       }

                   else if (toknum == PASSBAND_)
                       {
                       if (nv > 1)
                           {
                           msg ("Too many passband numbers",2);
                           return (-1);
                           }
                       if (tokens[ntok].category == INTEGER)
                           cb_ptr -> passband[nv] = tval;
                       else
                           cb_ptr -> passband[nv] = float_values[tval];
                       }

// ##DELAY_OFFS##  for next clause
                   else if (toknum == DELAY_OFFS_) // is this a channel delay offset?
                       {
                       i = fcode(parsed_codes[nv]);
                       if (i<0 || i>MAXFREQ-1)
                           {
                           msg ("Invalid delay offset frequency code",2);
                           return (-1);
                           }
                                         /* get phases from appropriate place */
                       if (tokens[ntok].category == INTEGER)
                           fval = tval;
                       else
                           fval = float_values[tval];
                                                   /* delays normally get stored
                                                      for only one station, and
                                                      into correct freq slot.
                                                      If both specified, set remote
                                                      delay offset to value, and zero
                                                      out the reference value. */
                       if (cb_ptr -> baseline[1] == WILDCARD)
                           cb_ptr -> delay_offs[i].ref = fval;
                       else if (cb_ptr -> baseline[0] == WILDCARD)
                           cb_ptr -> delay_offs[i].rem = fval;
                       else 
                           {
                           cb_ptr -> delay_offs[i].ref = 0.0;
                           cb_ptr -> delay_offs[i].rem = fval;
                           // Consider making this illegal to avoid pilot error.
                           }
                       }

                   else if (toknum == DELAY_OFFS_L_ || toknum == DELAY_OFFS_X_) 
                       {
                       i = fcode(parsed_codes[nv]);
                       if (i<0 || i>MAXFREQ-1)
                           {
                           msg ("Invalid delay offset frequency code",2);
                           return (-1);
                           }
                                         /* get phases from appropriate place */
                       if (tokens[ntok].category == INTEGER)
                           fval = tval;
                       else
                           fval = float_values[tval];
                                         /* unclear what should happen if specified on a baseline */
                       if (cb_ptr -> baseline[1] == WILDCARD)
                           cb_ptr -> delay_offs_pol[i][LXH].ref = fval;
                       else if (cb_ptr -> baseline[0] == WILDCARD)
                           cb_ptr -> delay_offs_pol[i][LXH].rem = fval;
                       else
                           {
                           msg ("Must specify delay_offs_? on a station, not a baseline",2);
                           return (-1);
                           }
                       }

                   else if (toknum == DELAY_OFFS_R_ || toknum == DELAY_OFFS_Y_) 
                       {
                       i = fcode(parsed_codes[nv]);
                       if (i<0 || i>MAXFREQ-1)
                           {
                           msg ("Invalid delay offset frequency code",2);
                           return (-1);
                           }
                                         /* get phases from appropriate place */
                       if (tokens[ntok].category == INTEGER)
                           fval = tval;
                       else
                           fval = float_values[tval];
                                         /* unclear what should happen if specified on a baseline */
                       if (cb_ptr -> baseline[1] == WILDCARD)
                           cb_ptr -> delay_offs_pol[i][RYV].ref = fval;
                       else if (cb_ptr -> baseline[0] == WILDCARD)
                           cb_ptr -> delay_offs_pol[i][RYV].rem = fval;
                       else
                           {
                           msg ("Can only specify delay_offs_? on a station, not a baseline",2);
                           return (-1);
                           }
                       }

                   else if (toknum == SAMPLER_DELAY_L_ || toknum == SAMPLER_DELAY_X_)
                       {
                       if (nv >= MAX_SAMP)
                           {
                           msg ("Too many (%d) sampler delays specified", 2, nv+1);
                           return (-1);
                           }
                                   /* get coefficients from appropriate place */
                       if (tokens[ntok].category == INTEGER)
                           fval = tval;
                       else
                           fval = float_values[tval];
                       fval *= 1e-9;      // convert ns -> sec
                                          // insert to station, or rem if no wildcard
                       if (cb_ptr -> baseline[1] == WILDCARD)
                           cb_ptr -> sampler_delay[nv][LXH].ref = fval;
                       else if (cb_ptr -> baseline[0] == WILDCARD)
                           cb_ptr -> sampler_delay[nv][LXH].rem = fval;
                       else 
                           {
                           cb_ptr -> sampler_delay[nv][LXH].ref = fval;
                           cb_ptr -> sampler_delay[nv][LXH].rem = fval;
                           }
                       }

                   else if (toknum == SAMPLER_DELAY_R_ || toknum == SAMPLER_DELAY_Y_)
                       {
                       if (nv >= MAX_SAMP)
                           {
                           msg ("Too many (%d) sampler delays specified", 2, nv+1);
                           return (-1);
                           }
                                   /* get coefficients from appropriate place */
                       if (tokens[ntok].category == INTEGER)
                           fval = tval;
                       else
                           fval = float_values[tval];
                       fval *= 1e-9;      // convert ns -> sec
                                          // insert to station, or rem if no wildcard
                       if (cb_ptr -> baseline[1] == WILDCARD)
                           cb_ptr -> sampler_delay[nv][RYV].ref = fval;
                       else if (cb_ptr -> baseline[0] == WILDCARD)
                           cb_ptr -> sampler_delay[nv][RYV].rem = fval;
                       else 
                           {
                           cb_ptr -> sampler_delay[nv][RYV].ref = 0.0;
                           cb_ptr -> sampler_delay[nv][RYV].rem = fval;
                           }
                       }
               nv++;                       /* bump index for next vector parm */
               break;



            case INSERT_V_CHAR:                 /* first figure out side band */
               if (tnum == ONE_CHAR_)
                   {
                   chan[0] = char_values[tval];
                   sideband = DSB;
                   }
               else if (tnum == TWO_CHAR_)
                   {
                   memcpy (chan, char_values+tval, 2);
                   if (chan[1] == '+')
                       sideband = USB;
                   else if (chan[1] == '-')
                       sideband = LSB;
                   else
                       sideband = -1;                        /* denotes error */
                   }
               else                                // must be an integer
                   {
                   if (tval >=0 && tval <= 9)
                       {
                       chan[0] = '0' + tval;
                       sideband = DSB;
                       }
                   else
                       chan[0] = '#';                 // null char will cause error
                       chan[1] = '#';
                   }

               i = fcode(chan[0]);                       /* get freq. array index */
               if (i<0 || i>MAXFREQ-1 || sideband<0) /* trap error conditions */
                   {
                   msg ("Errant freq element: %c%c", 2, chan[0], chan[1]);
                   return (-1);
                   }
                                        /* OK, load into appropriate c_blocks */
               for (cb_ptr=cond_start; cb_ptr!=NULL; cb_ptr=cb_ptr->cb_chain)
                   cb_ptr -> frequency[i] = sideband; 
               break;

           case INSERT_STRING:                   // handle all string arguments
               if (toknum == SAMPLERS_)
                   {               // add string and change concatenation point
                   strcat (psc, char_values+tval);
                   for (cb_ptr=cond_start; cb_ptr!=NULL; cb_ptr=cb_ptr->cb_chain)
                       cb_ptr -> psamplers[ns] = psc;
                   psc += strlen (char_values+tval) + 1;
                   ns++;
                                // transition if we've read all expected strings
                   if (ns == cond_start -> nsamplers)
                       next_state = BLOCK_INTERIOR;
                   }
                                // store adhoc (pcal) file names
               else if (toknum == ADHOC_FILE_)
                   for (cb_ptr=cond_start; cb_ptr!=NULL; cb_ptr=cb_ptr->cb_chain)
                       {
                       if (cb_ptr -> baseline[1] == WILDCARD)      // ref station
                           strncpy (cb_ptr -> adhoc_file[0], char_values+tval, 256);
                       else if (cb_ptr -> baseline[0] == WILDCARD) // rem station
                           strncpy (cb_ptr -> adhoc_file[1], char_values+tval, 256);
                       }
                                // store adhoc (pcal) file channels
               else if (toknum == ADHOC_FILE_CHANS_)
                   for (cb_ptr=cond_start; cb_ptr!=NULL; cb_ptr=cb_ptr->cb_chain)
                       {
                       if (cb_ptr -> baseline[1] == WILDCARD)      // ref station
                           strncpy (cb_ptr -> adhoc_file_chans[0], 
                                    char_values+tval, 128);
                       else if (cb_ptr -> baseline[0] == WILDCARD) // rem station
                           strncpy (cb_ptr -> adhoc_file_chans[1], 
                                    char_values+tval, 128);
                       }
               break;

            case POP_TOKEN:
               ntok--;                                  /* stay on same token */
               break;

            case CLEAR_CONDS:                    /* clear condition variables */
               if (tnum == IF_)
                  cb_start = NULL;       /* range of condition pointers grows */
               else if (tnum == OR_)          /* when they are OR'ed together */
                  cb_start = cond_start;

               parsed_f_group = WILDCARD;
               parsed_station = WILDCARD;
               for (i=0; i<2; i++) 
                  {
                  parsed_baseline[i] = WILDCARD;
                  parsed_scan[i] = NULLINT;
                  }
               for (i=0; i<5; i++) 
                  parsed_knot[i] = FALSE;
               for (i=0; i<32; i++) 
                  parsed_source[i] = WILDCARD;
               break;

            case SAVE_FG:
               if (tnum == WILDCARD_)
                   parsed_f_group = WILDCARD;
               else
                   parsed_f_group = char_values[tval];
               parsed_knot[2] = negation;
               negation = FALSE;
               break;

            case SAVE_STAT:
               if (tnum == WILDCARD_)
                   parsed_station = WILDCARD;
               else
                   parsed_station = char_values[tval];
               parsed_knot[4] = negation;
               negation = FALSE;
               break;

            case SAVE_BASE:
               memcpy (parsed_baseline, char_values + tval, 2);
               parsed_knot[0] = negation;
               negation = FALSE;
               break;

            case SAVE_SCAN:
               parsed_scan[0] = tval;
               parsed_scan[1] = tval;
               parsed_knot[3] = negation;
               negation = FALSE;
               break;

            case SAVE_2ND_SCAN:
               if (toknum == TO_)                /* overwrite end of interval */
                  parsed_scan[1] = tval;

               else if (toknum == LESS_THAN_)
                  {
                  parsed_scan[0] = 0;           /* start at beginning of year */
                  parsed_scan[1] = tval - 1;   /* note: interval is inclusive */
                  }

               else if (toknum == GREATER_THAN_)
                  {
                  parsed_scan[0] = tval + 1;
                  parsed_scan[1] = 99999999;          /* include rest of year */
                  }

               break;

            case SAVE_SOURCE:
               if (tnum != WILDCARD_)
                   {
                   memset (parsed_source,' ',32);           /* pad with blanks */
                   i = strlen (char_values + tval);
                   i = (i > 32) ? 32 : i;               /* move at most 32 chars */
                   memcpy (parsed_source, char_values + tval, i);
                   }
               parsed_knot[1] = negation;
               negation = FALSE;
               break;

            case SAVE_CODES:
               memset (parsed_codes,'\0',MAXFREQ);               /* pad with nulls */
               i = strlen (char_values + tval);
               i = (i > MAXFREQ) ? MAXFREQ : i;      /* move at most MAXFREQ chars */
//               i = (i > 16) ? 16 : i;                /* move at most 16 chars */
               memcpy (parsed_codes, char_values + tval, i);
               break;

            case GEN_CBLOCKS:           /* generate one or more c_blocks that
                                             represent the current conditions */

               i = (parsed_station == WILDCARD)? 1 : 2;
               if (append_cblocks (&cond_start, &cb_tail, i))
                  return (-1);

               cond_start -> f_group = parsed_f_group;
               cb_tail    -> f_group = parsed_f_group;

               for (i=0; i<2; i++)
                  {
                  cond_start -> scan[i] = parsed_scan[i];
                  cb_tail    -> scan[i] = parsed_scan[i];
                  }
               memcpy (cond_start -> baseline, parsed_baseline, 8);
               memcpy (cb_tail    -> baseline, parsed_baseline, 8);

               memcpy (cond_start -> source, parsed_source, 32);
               memcpy (cb_tail    -> source, parsed_source, 32);

               for (i=0; i<4; i++)
                  {
                  cond_start -> knot[i] = parsed_knot[i];
                  cb_tail    -> knot[i] = parsed_knot[i];
                  }
               if (parsed_station != WILDCARD)  /* station overrides baseline */
                  {
                  cond_start -> baseline[0] = parsed_station;
                  cond_start -> baseline[1] = WILDCARD;
                  cb_tail ->    baseline[0] = WILDCARD;
                  cb_tail ->    baseline[1] = parsed_station;
                  cond_start -> knot[0] = parsed_knot[4];
                  cb_tail    -> knot[0] = parsed_knot[4];
                  }

               if (cb_start != NULL)
                  cond_start = cb_start;    /* extended range of active c_b's */
               ntok--;             /* pop token for block interior processing */
               break;

            case NEGATE:             /* get set to negate following condition */
               negation = TRUE;
               break;

            case EOF_CLEANUP:
               break;


            case NO_OP:
            default:
               ;
            }
         ntok++;
         }

      else                 /* no appropriate state transition with this token */
         {
         parsing_error (next_state, ntok);
         return (-1);
         }
      }

   while (next_state != END_STATE);

   return (0);                                          /* successful return! */
   }


/*******************************************************************************
*    append_cblocks appends an arbitrary number (num) of c_blocks to the end   *
*    of the current chain. On entry, cb_end points to the pointer to the tail  *
*    of the current c_block chain. Pointers to the start and end of the new    *
*    section are returned in *cb_start and *cb_end.        rjc  92.2.19        *
*******************************************************************************/

int append_cblocks (cb_start, cb_end, num)
int num;
struct c_block **cb_start,**cb_end;
   {
   int i;
   struct c_block *cb_ptr;

   for (i=0; i<num; i++)
      {
      if ((cb_ptr = (struct c_block *) malloc (sizeof (struct c_block))) == NULL)
         {
         msg ("Error allocating c_block memory.",2);
         return (-1);
         }

      if (i == 0)
         *cb_start = cb_ptr;     /* need to return pointer to first new block */

      nullify_cblock (cb_ptr);                     /* nullify the new c_block */

      (*cb_end) -> cb_chain = cb_ptr;           /* splice into existing chain */
      *cb_end = cb_ptr;
      }
   return (0);
   }



/*******************************************************************************
*     parsing_error reports an error in parsing of the control file            *
*                                                                              *
*     Input:                                                                   *
*       state_num   - number of the current state in the FSM table             *
*       ntok        - token number of the encountered token                    *
*                                                                              *
*       94.1.13  rjc  initial code                                             *
*******************************************************************************/

parsing_error (state_num, ntok)

int state_num,
    ntok;
   {
   extern struct token_struct *tokens;   /* input struct of tokens & values   */
   extern char *token_string[];

   char *state[MAX_STATES];

                                    /* Initialize names of various FSM states */
   state[BLOCK_INTERIOR]    = "BLOCK_INTERIOR";    
   state[NEED_INT]          = "NEED_INT";         
   state[NEED_FLOAT]        = "NEED_FLOAT";      
   state[NEED_TWO_FLOAT_1]  = "NEED_TWO_FLOAT_1";
   state[NEED_TWO_FLOAT_2]  = "NEED_TWO_FLOAT_2";
   state[NEED_VECTOR_INT]   = "NEED_VECTOR_INT";
   state[NEED_VECTOR_FLOAT] = "NEED_VECTOR_FLOAT";
   state[NEED_CONDITION]    = "NEED_CONDITION";  
   state[NEED_F_GROUP]      = "NEED_F_GROUP";   
   state[END_STATE]         = "END_STATE";     
   state[NEED_STATION]      = "NEED_STATION"; 
   state[NEED_SCAN]         = "NEED_SCAN";   
   state[NEED_SOURCE]       = "NEED_SOURCE"; 
   state[NEED_BASELINE]     = "NEED_BASELINE";  
   state[NEED_VECTOR_CHAR]  = "NEED_VECTOR_CHAR";  
   state[NEED_CODES]        = "NEED_CODES";        
   state[NEED_OR]           = "NEED_OR";        
   state[MAY_HAVE_TO]       = "MAY_HAVE_TO";  
   state[NEED_2ND_SCAN]     = "NEED_2ND_SCAN";  

   msg ("Parser semantic error on line %d of control file:",2,tokens[ntok].line);
   msg ("In state %s, encountered illegal token %s", 2,
         state[state_num], token_string[tokens[ntok].symbol]);
   }
