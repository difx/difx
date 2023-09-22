/**------------------------------------------------------
* ALMA - Atacama Large Millimeter Array
* (c) Associated Universities Inc., 2013
* (c) Massachusetts Institute of Technology, 2013
* @author Victor Pankratius, MIT Haystack Observatory
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/

/*
*------------------------------------------------------
*   VEX Grammar for ANTLR 3
*-------------------------------------------------------
* General comments:
* 
*  - "<" will be replaced by "_@1" + warning.
*  - VEX file is appended as XML comment, where all '-' are replaced by '~' to match XML spec.
*/

grammar vexGrammar;


options {
  output = AST;
}

@header{
  import java.io.*;
}

/*
* Inject this code into parser during code generation
* Appends original VEX file as XML comment to XML file
* Because '-' not allowed within <!-- -->, replace by '~'
*/
@members {
  PrintWriter writer;

  //write to files within parser actions
  public vexGrammarParser(TokenStream input, String fileName) {
    super(input);
    try {      
      writer = new PrintWriter(new PrintWriter(fileName), true); //creates printWriter with autoflush
    } catch(Exception e) {
      e.printStackTrace();
    }
  }
  
  //append original vex file as XML comment when done
  public void finalize(String sourceFile){
    
    System.out.println("Appending input file as comment. Replacing '-' by '~'");
    writer.println("<!--");
    
    try{
    BufferedReader in = new BufferedReader(new FileReader(sourceFile));

    while (in.ready()) {
        String s = in.readLine();
        s = s.replaceAll("-", "~");  //because hyphen is not allowed within XML comments
    
        writer.println(s);
        }
    in.close();
    } catch (IOException e) {
       System.err.println("Error: " + e);
    }
    
   writer.println("-->");
  }
  
}


/* start rule */
vex   @init{
        writer.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
        writer.println("<root>");
        writer.println("<vex2xml_rev>1.1</vex2xml_rev>");
    }
      @after{writer.println("</root>");}

:  version block+ 
;


/* version number */
version   @init{writer.println("<vex_rev>");}
          @after{writer.println("</vex_rev>");}
          :  T_VEX_REV '=' T_NAME ';' {writer.println($T_NAME.text);}
;

/* blocks */
block:    global_block
        | station_block 
        | mode_block
        | freq_block  
        | sched_block
        | antenna_block
        | bbc_block
        | clock_block
        | das_block
        | eop_block
        | exper_block
        | head_pos_block
        | if_block
        | pass_order_block
        | phase_cal_detect_block
        | procedures_block
        | roll_block
        | scheduling_params_block
        | sefd_block
        | site_block
        | source_block
        | tapelog_obs_block
        | tracks_block  
;

/* $GLOBAL block -------------------------------------------*/
global_block  @init{writer.println("<GLOBAL>");}
              @after{writer.println("</GLOBAL>");}
              : B_GLOBAL ';' ref+ 
                | B_GLOBAL ';'
;


/* $STATION block -------------------------------------------*/
station_block @init{writer.println("<STATION>");}
              @after{writer.println("</STATION>");}
              :  B_STATION ';' station_def+
                 | B_STATION ';'
;
station_def   @init{writer.println("<def>");}
              @after{writer.println("</def>");}
              :  defname ';' ref+ T_ENDDEF ';'
                 | defname ';' T_ENDDEF ';'
;


/* $MODE block -------------------------------------------*/
mode_block    @init{writer.println("<MODE>");}
              @after{writer.println("</MODE>");}

              : B_MODE ';' mode_def+
                | B_MODE ';'
;

mode_def      @init{writer.println("<def>");}
              @after{writer.println("</def>");}

              : defname ';' qref+ T_ENDDEF ';'
              | defname ';' T_ENDDEF ';'  
            
; 


/* $FREQ block -------------------------------------------*/
freq_block    @init{writer.println("<FREQ>");}
              @after{writer.println("</FREQ>");}
              : B_FREQ ';' freq_def+
                | B_FREQ ';'
;

freq_def      @init{writer.println("<def>");}
              @after{writer.println("</def>");}
              : defname ';' freq_lowl+ T_ENDDEF ';'
                | defname ';' T_ENDDEF ';'
            
;

freq_lowl:  chan_def
    | sample_rate
    | bits_per_sample
    | switching_cycle
    | external_ref
;

chan_def      @init{writer.println("<chan_def>");}
              @after{writer.println("</chan_def>");}

              : T_CHAN_DEF '=' linkedvalue /* band_id */
              ':' unit_value      /* sky frequency */
              ':' value           /* net sb */
              ':' unit_value      /* channel BW */
              ':' linkedvalue     /* chan ID */
              ':' linkedvalue     /* BBC ID */
              ':' linkedvalue';'  /* phase-cal ID */

              | T_CHAN_DEF '=' linkedvalue  /* band_id */
              ':' unit_value      /* sky frequency */
              ':' value           /* net sb */
              ':' unit_value      /* channel BW */
              ':' linkedvalue     /* chan ID */
              ':' linkedvalue     /* BBC ID */
                switch_state+ ';'  /* phase-cal ID */

              | T_CHAN_DEF '='                                 
              emptyvalue          /* NO band_id */
              ':' unit_value      /* sky frequency */
              ':' value           /* net sb */
              ':' unit_value      /* channel BW */
              ':' linkedvalue     /* chan ID */
              ':' linkedvalue     /* BBC ID */
              ':' linkedvalue  ';'/* phase-cal ID */

              | T_CHAN_DEF '='    
              emptyvalue          /* NO band_id */
              ':' unit_value      /* sky frequency */
              ':' value           /* net sb */
              ':' unit_value      /* channel BW */
              ':' linkedvalue     /* chan ID */
              ':' linkedvalue     /* BBC ID */
                switch_state+ ';'  /* phase-cal ID */
;

switch_state    @init{writer.println("<switch_state>");}
                @after{writer.println("</switch_state>");}
                : ':' value 
;

sample_rate     @init{writer.println("<sample_rate>");}
                @after{writer.println("</sample_rate>");}
                :  T_SAMPLE_RATE '=' unit_value ';'
;

bits_per_sample @init{writer.println("<sample_rate>");}
                @after{writer.println("</sample_rate>");}
                : T_BITS_PER_SAMPLE '=' value ';'  
;

switching_cycle @init{writer.println("<switching_cycle>");}
                @after{writer.println("</switching_cycle>");}
                : T_SWITCHING_CYCLE '=' value ':' unit_list ';' 
;



/* $SCHED block -------------------------------------------*/
sched_block   @init{writer.println("<SCHED>");}
              @after{writer.println("</SCHED>");}
              :  B_SCHED ';' sched_def+
                 | B_SCHED ';'
;

sched_def     @init{writer.println("<scan>");}
              @after{writer.println("</scan>");}
              :  T_SCAN T_NAME                            {writer.println("<scanname>"+$T_NAME.text+"</scanname>");}
                      ';' sched_lowl+ T_ENDSCAN ';'
                 | T_SCAN T_NAME ';' T_ENDSCAN ';'        {writer.println("<scanname>"+$T_NAME.text+"</scanname>");}
;

sched_lowl: start
    | mode
    | source
    | station
    | data_transfer
;

start         @init{writer.println("<start>");}
              @after{writer.println("</start>");}
              : T_START '=' value ';'
;

mode          @init{writer.println("<mode>");}
              @after{writer.println("</mode>");}
              : T_MODE '=' value ';'
;

source        @init{writer.println("<source>");}
              @after{writer.println("</source>");}
              : T_SOURCE '=' value ';'
;

station       @init{writer.println("<station>");}
              @after{writer.println("</station>");}

              :  T_STATION '=' value ':'  /* name */
                 unit_value ':'      /* data start */
                 unit_value ':'      /* data stop */
                 start_position ':'
                 pass ':'    
                 sector ':'      /* pointing sector */
                 drives ';'
;

data_transfer @init{writer.println("<data_transfer>");}
              @after{writer.println("</data_transfer>");}
              :  T_DATA_TRANSFER '=' scan_id ':' /* name */
                 method ':'                      /* method disk2file or in2net */
                 destination ':'           /* filename or blank */
                 unit_value2 ':'           /* data start */
                 unit_value2 ':'           /* data stop */
                 opt ';'                       /* future use, empty now */
     
               | T_DATA_TRANSFER '=' scan_id ':' /* name */
                 method ':'                     /* method disk2file or in2net */
                 destination ':'                /* filename or blank */
                 unit_value2 ':'                /* data start */
                 unit_value2 ';'                /* data stop */
;

start_position  @init{writer.println("<start_position>");}
                @after{writer.println("</start_position>");}
                : (emptyvalue|unit_value)
;

pass            @init{writer.println("<pass>");}
                @after{writer.println("</pass>");}
                :   (emptyvalue|value)
;

sector          @init{writer.println("<sector>");}
                @after{writer.println("</sector>");}
                :   (emptyvalue | linkedvalue)
;

drives          @init{writer.println("<drives>");}
                @after{writer.println("</drives>");}
                : (emptyvalue|value | value ':' value)
;

scan_id         @init{writer.println("<scan_id>");}
                @after{writer.println("</scan_id>");}
                : (emptyvalue|value)
;

method          @init{writer.println("<method>");}
                @after{writer.println("</method>");}
                : (emptyvalue|value)
;

destination     @init{writer.println("<destination>");}
                @after{writer.println("</destination>");}
                : (emptyvalue|value)
;

unit_value2:    (emptyvalue|value value)
;

opt:            (emptyvalue| value)
;


/* $ANTENNA block -------------------------------------------*/
antenna_block @init{writer.println("<ANTENNA>");}
              @after{writer.println("</ANTENNA>");}
              :  B_ANTENNA ';' antenna_def+
                 | B_ANTENNA ';'
;

antenna_def:  defname ';' antenna_lowl+ T_ENDDEF ';'
              | defname ';' T_ENDDEF ';'
;

antenna_lowl: antenna_diam
    | axis_type
    | axis_offset
    | antenna_motion
    | pointing_sector
    | external_ref
;

antenna_diam  @init{writer.println("<antenna_diam>");}
              @after{writer.println("</antenna_diam>");}
              : T_ANTENNA_DIAM '=' unit_value ';'
;

axis_type     @init{writer.println("<axis_type>");}
              @after{writer.println("</axis_type>");}
              :  T_AXIS_TYPE '=' value ':' value ';'
;

axis_offset   @init{writer.println("<axis_offset>");}
              @after{writer.println("</axis_offset>");}
              :  T_AXIS_OFFSET '=' unit_value ';'
;

antenna_motion  @init{writer.println("<antenna_motion>");}
                @after{writer.println("</antenna_motion>");}
                : T_ANTENNA_MOTION '=' value ':' unit_value ':' unit_value ';'
;

pointing_sector @init{writer.println("<pointing_sector>");}
                @after{writer.println("</pointing_sector>");}
                :  T_POINTING_SECTOR '=' linkedvalue ':'
                   value ':' unit_value ':' unit_value ':' value ':' unit_value ':' unit_value ';'
;


/* $BBC block -------------------------------------------*/
bbc_block     @init{writer.println("<BBC>");}
              @after{writer.println("</BBC>");}
              :  B_BBC ';' bbc_def+
                | B_BBC ';'
;

bbc_def       @init{writer.println("<def>");}
              @after{writer.println("</def>");}
              :  defname ';' bbc_lowl+ T_ENDDEF ';'
               | defname ';' T_ENDDEF ';'
;

bbc_lowl: bbc_assign
    | external_ref
;

bbc_assign    @init{writer.println("<bbc_assign>");}
              @after{writer.println("</bbc_assign>");}
              : T_BBC_ASSIGN '=' linkedvalue ':' value ':' linkedvalue ';'
;


/* $CLOCK block -------------------------------------------*/
clock_block:  B_CLOCK ';' clock_def+
    | B_CLOCK ';'
;

clock_def:  T_DEF T_NAME ';' clock_lowl+ T_ENDDEF ';'
    | T_DEF T_NAME ';' T_ENDDEF ';'
;

clock_lowl: clock_early
    | external_ref
;

clock_early:  T_CLOCK_EARLY '=' ':' unit_value ';'
    | T_CLOCK_EARLY '=' T_NAME ':' unit_value ';'
    | T_CLOCK_EARLY '=' T_NAME ':' unit_value ':' T_NAME ':' value ';'
    | T_CLOCK_EARLY '=' ':' unit_value ':' T_NAME ':' value ';'
;


/* $DAS block -------------------------------------------*/
das_block     @init{writer.println("<DAS>");}
              @after{writer.println("</DAS>");}
              :  B_DAS ';' das_def+
                 | B_DAS ';'
;

das_def:  defname ';' das_lowl+ T_ENDDEF ';'
          | defname ';' T_ENDDEF ';'
;


das_lowl: record_transport_type
    | electronics_rack_type
    | number_drives
    | headstack
    | record_density
    | tape_length
    | recording_system_id
    | tape_motion
    | tape_control
    | external_ref
;

record_transport_type   @init{writer.println("<record_transport_type>");}
                        @after{writer.println("</record_transport_type>");}
                        :  T_RECORD_TRANSPORT_TYPE '=' value ';'
;

electronics_rack_type   @init{writer.println("<electronics_rack_type>");}
                        @after{writer.println("</electronics_rack_type>");}
                        :  T_ELECTRONICS_RACK_TYPE '=' value ';'
;

number_drives           @init{writer.println("<number_drives>");}
                        @after{writer.println("</number_drives>");}
                        :    T_NUMBER_DRIVES '=' value ';'
;

headstack               @init{writer.println("<headstack>");}
                        @after{writer.println("</headstack>");}
                        :  T_HEADSTACK '=' value ':' value ':' value ';'
                           | T_HEADSTACK '=' value ':' emptyvalue  ':' value ';'
;

record_density          @init{writer.println("<record_density>");}
                        @after{writer.println("</record_density>");}
                        : T_RECORD_DENSITY '=' value value ';'
;

tape_length             @init{writer.println("<tape_length>");}
                        @after{writer.println("</tape_length>");}
                        :  T_TAPE_LENGTH '=' unit_value ';'
                           | T_TAPE_LENGTH '=' unit_value ':' value ':' value ';'
;

recording_system_id     @init{writer.println("<recording_system_id>");}
                        @after{writer.println("</recording_system_id>");}
                        :  T_RECORDING_SYSTEM_ID '=' value ';'
;

tape_motion             @init{writer.println("<tape_motion>");}
                        @after{writer.println("</tape_motion>");}
                        :  T_TAPE_MOTION '=' value ';'
                           | T_TAPE_MOTION '=' value ':' unit_value ';'
                           | T_TAPE_MOTION '=' value ':' unit_value ':' unit_value ':' unit_value ';'
;

tape_control            @init{writer.println("<tape_control>");}
                        @after{writer.println("</tape_control>");}
                        : T_TAPE_CONTROL '=' value ';'
;



/* $EOP block -------------------------------------------*/
eop_block:  B_EOP ';' eop_def+
    | B_EOP ';'
;

eop_def:  T_DEF T_NAME ';' eop_lowl+ T_ENDDEF ';'
    | T_DEF T_NAME ';' T_ENDDEF ';'
;

eop_lowl: tai_utc
    | a1_tai
    | eop_ref_epoch
    | num_eop_points
    | eop_interval
    | ut1_utc
    | x_wobble
    | y_wobble
    | nut_ref_epoch
    | num_nut_points
    | nut_interval
    | delta_psi
    | delta_eps
    | nut_model
    | external_ref
;

tai_utc:  T_TAI_UTC '=' unit_value ';'
;

a1_tai:   T_A1_TAI '=' unit_value ';'
;

eop_ref_epoch:  T_EOP_REF_EPOCH '=' T_NAME ';'
;

num_eop_points: T_NUM_EOP_POINTS '=' value ';'
;

eop_interval: T_EOP_INTERVAL '=' unit_value ';'
;

ut1_utc:  T_UT1_UTC '=' unit_list ';'
    | T_UT1_UTC '=' ';'
;

x_wobble: T_X_WOBBLE '=' unit_list ';'
    | T_X_WOBBLE '=' ';'
;

y_wobble: T_Y_WOBBLE '=' unit_list ';'
    | T_Y_WOBBLE '=' ';'
;

nut_ref_epoch:  T_NUT_REF_EPOCH '=' T_NAME ';'
;

num_nut_points: T_NUM_NUT_POINTS '=' value ';'
;

nut_interval: T_NUT_INTERVAL '=' unit_value ';'
;

delta_psi:      T_DELTA_PSI '=' unit_list ';'
    | T_DELTA_PSI '=' ';'
;

delta_eps:      T_DELTA_EPS '=' unit_list ';'
    | T_DELTA_EPS '=' ';'
;

nut_model:  T_NUT_MODEL '=' T_NAME ';'
;


/* $EXPER block -------------------------------------------*/

exper_block @init{writer.println("<EXPER>");}
            @after{writer.println("</EXPER>");}

            :   B_EXPER ';' exper_def+ 
              | B_EXPER ';'
;

exper_def   @init{writer.println("<def>");}
            @after{writer.println("</def>");}

            :  defname ';'  exper_lowl+ T_ENDDEF ';'  
             | defname ';' T_ENDDEF ';'
;

exper_lowl: exper_num
      | exper_name
      | exper_description
      | exper_nominal_start
      | exper_nominal_stop
      | pi_name
      | pi_email
      | contact_name
      | contact_email
      | scheduler_name
      | scheduler_email
      | target_correlator
      | external_ref
;

exper_num:  T_EXPER_NUM '=' value ';'                       {writer.println("<exper_num>"+$T_EXPER_NUM.text+"</exper_num>");}
;

exper_name: T_EXPER_NAME '=' T_NAME ';'                     {writer.println("<exper_name>"+$T_NAME.text+"</exper_name>");}
;

exper_description:
    T_EXPER_DESCRIPTION '=' T_QUOTESTRING';'
        {writer.println("<exper_description>"+$T_QUOTESTRING.text+"</exper_description>");}
    | T_EXPER_DESCRIPTION '=' T_NAME ';'
        {writer.println("<exper_description>"+$T_NAME.text+"</exper_description>");}
;

exper_nominal_start:  T_EXPER_NOMINAL_START '=' T_NAME ';'  {writer.println("<exper_nominal_start>"+$T_NAME.text+"</exper_nominal_start>");}
;

exper_nominal_stop: T_EXPER_NOMINAL_STOP '=' T_NAME ';'     {writer.println("<exper_nominal_stop>"+$T_NAME.text+"</exper_nominal_stop>");}
;

pi_name:  T_PI_NAME '=' T_QUOTESTRING ';'                        {writer.println("<pi_name>"+$T_QUOTESTRING.text+"</pi_name>");}
;

pi_email: T_PI_EMAIL '=' T_NAME ';'                         {writer.println("<pi_email>"+$T_NAME.text+"</pi_email>");}
;

contact_name: T_CONTACT_NAME '=' T_NAME ';'                 {writer.println("<contact_name>"+$T_NAME.text+"</conact_name>");}
;

contact_email:  T_CONTACT_EMAIL '=' T_NAME ';'              {writer.println("<contact_email>"+$T_NAME.text+"</conact_email>");}
;

scheduler_name: T_SCHEDULER_NAME '=' T_NAME ';'             {writer.println("<scheduler_name>"+$T_NAME.text+"</scheduler_name>");}
;

scheduler_email:  T_SCHEDULER_EMAIL '=' T_NAME ';'          {writer.println("<scheduler_email>"+$T_NAME.text+"</scheduler_email>");}
;

target_correlator:  T_TARGET_CORRELATOR '=' T_NAME ';'      {writer.println("<target_correlator>"+$T_NAME.text+"</target_correlator>");}
;


/* $HEAD_POS block -------------------------------------------*/
  
head_pos_block  @init{writer.println("<HEAD_POS>");}
                @after{writer.println("</HEAD_POS>");}
                : B_HEAD_POS ';' head_pos_def+
                | B_HEAD_POS ';'
;

head_pos_def    @init{writer.println("<def>");}
                @after{writer.println("</def>");}
                : defname ';' head_pos_lowl+ T_ENDDEF ';'
                  | defname ';' T_ENDDEF ';'
;

head_pos_lowl:  headstack_pos
    | external_ref
;

headstack_pos   @init{writer.println("<headstack_pos>");}
                @after{writer.println("</headstack_pos>");}
                :  T_HEADSTACK_POS '=' value ':' unit_list ';'
;


/* $IF block -------------------------------------------*/
if_block    @init{writer.println("<IF>");}
            @after{writer.println("</IF>");}
            : B_IF ';' if_def+
              | B_IF ';'
;

if_def      @init{writer.println("<def>");}
            @after{writer.println("</def>");}
            : defname ';' if_lowl+ T_ENDDEF ';'
              | defname ';' T_ENDDEF ';'
;

if_lowl:  if_def_st
    | external_ref
;

if_def_st  @init{writer.println("<if_def>");} 
           @after{writer.println("</if_def>");}
    : T_IF_DEF '=' linkedvalue ':' value ':' value ':' unit_value ':' value ';'
    | T_IF_DEF '=' linkedvalue ':' value ':' value ':' unit_value ':' value ':' ':' ';'
    | T_IF_DEF '=' linkedvalue ':' value ':' value ':' unit_value ':' value ':' ';'
    | T_IF_DEF '=' linkedvalue ':' value ':' value ':' unit_value ':' value ':' unit_value ';'
    | T_IF_DEF '=' linkedvalue ':' value ':' value ':' unit_value ':' value ':' unit_value ':' ';'
    | T_IF_DEF '=' linkedvalue ':' value ':' value ':' unit_value ':' value ':' unit_value ':' unit_value ';'
;

/* $PASS_ORDER block -------------------------------------------*/
pass_order_block  @init{writer.println("<PASS_ORDER>");}
                  @after{writer.println("</PASS_ORDER>");}
                  : B_PASS_ORDER ';' pass_order_def+
                    | B_PASS_ORDER ';'
;

pass_order_def    @init{writer.println("<def>");}
                  @after{writer.println("</def>");}
                  : defname ';' pass_order_lowl+ T_ENDDEF ';'
                    | defname ';' T_ENDDEF ';'
;

pass_order_lowl:  pass_order
      | s2_group_order
      | external_ref
;

pass_order        @init{writer.println("<pass_order>");}
                  @after{writer.println("</pass_order>");}
                  : T_PASS_ORDER '=' name_list ';'
;

s2_group_order    @init{writer.println("<s2_group_order>");}
                  @after{writer.println("</s2_group_order>");}
                  : T_S2_GROUP_ORDER '=' value_list ';'
;


/* $PHASE_CAL_DETECT block -------------------------------------------*/
phase_cal_detect_block  @init{writer.println("<PHASE_CAL_DETECT>");}
                        @after{writer.println("</PHASE_CAL_DETECT>");}
                        : B_PHASE_CAL_DETECT ';' phase_cal_detect_def+
                        | B_PHASE_CAL_DETECT ';'
;

phase_cal_detect_def    @init{writer.println("<def>");}
                        @after{writer.println("</def>");}
                        : defname ';' phase_cal_detect_lowl+ T_ENDDEF ';'
                          | defname ';' T_ENDDEF ';'
;

phase_cal_detect_lowl:  phase_cal_detect
    | external_ref
;

phase_cal_detect        @init{writer.println("<phase_cal_detect>");}
                        @after{writer.println("</phase_cal_detect>");}
                        : 
                          T_PHASE_CAL_DETECT '=' linkedvalue ';'
                          | T_PHASE_CAL_DETECT '=' linkedvalue ':' value_list ';' 
;


/* $PROCEDURES block -------------------------------------------*/
procedures_block    @init{writer.println("<PROCEDURES>");}
                    @after{writer.println("</PROCEDURES>");}

                    : B_PROCEDURES ';' procedures_def+
                      | B_PROCEDURES ';' 
;

procedures_def      @init{writer.println("<def>");}
                    @after{writer.println("</def>");}

                    : defname ';' procedures_lowl+ T_ENDDEF ';'
                      | defname ';' T_ENDDEF ';'
;

procedures_lowl:  tape_change
      | headstack_motion
      | new_source_command
      | new_tape_setup
      | setup_always
      | parity_check
      | tape_prepass
      | preob_cal
      | midob_cal
      | postob_cal
      | procedure_name_prefix
      | external_ref
;

tape_change         @init{writer.println("<tape_change>");}
                    @after{writer.println("</tape_change>");}
                    :  T_TAPE_CHANGE '=' unit_value ';'
;

headstack_motion    @init{writer.println("<headstack_motion>");}
                    @after{writer.println("</headstack_motion>");}
                    : T_HEADSTACK_MOTION '=' unit_value ';'
;

new_source_command  @init{writer.println("<new_source_command>");}
                    @after{writer.println("</new_source_command>");}
                    : T_NEW_SOURCE_COMMAND '=' unit_value ';'
;

new_tape_setup      @init{writer.println("<new_tape_setup>");}
                    @after{writer.println("</new_tape_setup>");}
                    : T_NEW_TAPE_SETUP '=' unit_value ';'
;

setup_always        @init{writer.println("<setup_always>");}
                    @after{writer.println("</setup_always>");}
                    : T_SETUP_ALWAYS '=' name_value ':' unit_value ';'
;

parity_check        @init{writer.println("<parity_check>");}
                    @after{writer.println("</parity_check>");}
                    : T_PARITY_CHECK '=' name_value ':' unit_value ';'
;

tape_prepass        @init{writer.println("<tape_prepass>");}
                    @after{writer.println("</tape_prepass>");}
                    : T_TAPE_PREPASS '=' name_value ':' unit_value ';'
;

preob_cal           @init{writer.println("<preob_cal>");}
                    @after{writer.println("</preob_cal>");}
                    :  T_PREOB_CAL '=' name_value ':' unit_value ':' name_value ';'
;

midob_cal           @init{writer.println("<midob_cal>");}
                    @after{writer.println("</midob_cal>");}
                    :  T_MIDOB_CAL '=' name_value ':' unit_value ':' name_value ';'
;

postob_cal          @init{writer.println("<postob_cal>");}
                    @after{writer.println("</postob_cal>");}
                    : T_POSTOB_CAL '=' name_value ':' unit_value ':' name_value ';'
;

procedure_name_prefix       @init{writer.println("<procedure_name_prefix>");}
                            @after{writer.println("</procedure_name_prefix>");}
                            :  T_PROCEDURE_NAME_PREFIX '=' T_NAME ';'             {writer.println("<value>"+$T_NAME.text+"</value>");}
;


/* $ROLL block -------------------------------------------*/

roll_block    @init{writer.println("<ROLL>");}
              @after{writer.println("</ROLL>");}
              : B_ROLL ';' roll_def+
                | B_ROLL ';'
;

roll_def      @init{writer.println("<def>");}
              @after{writer.println("</def>");}
              : defname ';' roll_lowl+ T_ENDDEF ';'
                | defname ';' T_ENDDEF ';'
;

roll_lowl:  roll_reinit_period
    | roll_inc_period
    | roll
    | roll_def_st
    | external_ref
;

roll_reinit_period  @init{writer.println("<roll_reinit_period>");}
                    @after{writer.println("</roll_reinit_period>");}
                    : T_ROLL_REINIT_PERIOD '=' unit_value ';'
;

roll_inc_period     @init{writer.println("<roll_inc_period>");}
                    @after{writer.println("</roll_inc_period>");}
                    :  T_ROLL_INC_PERIOD '=' value ';'
;

roll                @init{writer.println("<roll>");}
                    @after{writer.println("</roll>");}
                    :   T_ROLL '=' T_NAME ';'
;

roll_def_st         @init{writer.println("<roll_def_st>");}
                    @after{writer.println("</roll_def_st>");}
                    :  T_ROLL_DEF '=' value_list ';'
;



/* $SCHEDULING_PARAMS block -------------------------------------------*/
 scheduling_params_block: B_SCHEDULING_PARAMS ';' scheduling_params_def+
      | B_SCHEDULING_PARAMS ';'
;

scheduling_params_def:  T_DEF T_NAME ';' scheduling_params_lowl+ T_ENDDEF ';'
    | T_DEF T_NAME ';' T_ENDDEF ';'
;

scheduling_params_lowl: external_ref
      /*|literal*/
;


/* $SEFD block -------------------------------------------*/

sefd_block: B_SEFD ';' sefd_def+
    | B_SEFD ';'
;

sefd_def: T_DEF T_NAME ';' sefd_lowl+ T_ENDDEF ';'
    | T_DEF T_NAME ';' T_ENDDEF ';'
;

sefd_lowl:  sefd_model
    | sefd
    | external_ref
;

sefd_model: T_SEFD_MODEL '=' T_NAME ';'
;

sefd:   T_SEFD '=' linkedvalue ':' unit_value ':' value_list ';'
;


/* $SITE block -------------------------------------------*/

site_block      @init{writer.println("<SITE>");}
                @after{writer.println("</SITE>");}  
                : B_SITE ';' site_def+
                  | B_SITE ';'
;

site_def        @init{writer.println("<def>");}
                @after{writer.println("</def>");}
                : defname ';' site_lowl+ T_ENDDEF ';'
                  | defname ';' T_ENDDEF ';'
;

site_lowl:  site_type
    | site_name
    | site_id
    | site_position
    | site_position_epoch
    | site_position_ref
    | site_velocity
    | horizon_map_az
    | horizon_map_el
    | zen_atmos
    | ocean_load_vert
    | ocean_load_horiz
    | occupation_code
    | inclination
    | eccentricity
    | arg_perigee
    | ascending_node
    | mean_anomaly
    | semi_major_axis
    | mean_motion
    | orbit_epoch
    | external_ref
;

site_type     @init{writer.println("<site_type>");}
              @after{writer.println("</site_type>");}
              :  T_SITE_TYPE '=' value ';'
;

site_name     @init{writer.println("<site_name>");}
              @after{writer.println("</site_name>");}
              :  T_SITE_NAME '=' value ';'
;

site_id       @init{writer.println("<site_id>");}
              @after{writer.println("</site_id>");}
              :  T_SITE_ID '=' value ';'
;

site_position @init{writer.println("<site_position>");}
              @after{writer.println("</site_position>");}
              :  T_SITE_POSITION '=' unit_value ':' unit_value ':' unit_value ';'
;

site_position_epoch @init{writer.println("<site_position_epoch>");}
                    @after{writer.println("</site_position_epoch>");}
                    :  T_SITE_POSITION_EPOCH '=' value ';'
;

site_position_ref   @init{writer.println("<site_position_ref>");}
                    @after{writer.println("</site_position_ref>");}
                    :  T_SITE_POSITION_REF '=' value ';'
;

site_velocity       @init{writer.println("<site_velocity>");}
                    @after{writer.println("</site_velocity>");}
                    :  T_SITE_VELOCITY '=' unit_value ':' unit_value ':' unit_value ';'
;

horizon_map_az      @init{writer.println("<horizon_map_az>");}
                    @after{writer.println("</horizon_map_az>");}
                    : T_HORIZON_MAP_AZ '=' unit_list ';'
;

horizon_map_el      @init{writer.println("<horizon_map_el>");}
                    @after{writer.println("</horizon_map_el>");}
                    : T_HORIZON_MAP_EL '=' unit_list ';'
;

zen_atmos           @init{writer.println("<zen_atmos>");}
                    @after{writer.println("</zen_atmos>");}
                    :  T_ZEN_ATMOS '=' unit_value ';'
;

ocean_load_vert     @init{writer.println("<ocean_load_vert>");}
                    @after{writer.println("</ocean_load_vert>");}
                    :  T_OCEAN_LOAD_VERT '=' unit_value ':' unit_value ';'
;

ocean_load_horiz    @init{writer.println("<ocean_load_horiz>");}
                    @after{writer.println("</ocean_load_horiz>");}
                    : T_OCEAN_LOAD_HORIZ '=' unit_value ':' unit_value ';'
;

occupation_code     @init{writer.println("<occupation_code>");}
                    @after{writer.println("</occupation_code>");}
                    :  T_OCCUPATION_CODE '=' name_value ';'
;

inclination         @init{writer.println("<inclination>");}
                    @after{writer.println("</inclination>");}
                    :  T_INCLINATION '=' unit_value ';'
;

eccentricity        @init{writer.println("<eccentricity>");}
                    @after{writer.println("</eccentricity>");}  
                    : T_ECCENTRICITY '=' value ';'
;

arg_perigee         @init{writer.println("<arg_perigee>");}
                    @after{writer.println("</arg_perigee>");} 
                    :  T_ARG_PERIGEE '=' unit_value ';'
;

ascending_node      @init{writer.println("<ascending_node>");}
                    @after{writer.println("</ascending_node>");} 
                    : T_ASCENDING_NODE '=' unit_value ';'
;

mean_anomaly        @init{writer.println("<mean_anomaly>");}
                    @after{writer.println("</mean_anomaly>");} 
                    : T_MEAN_ANOMALY '=' unit_value ';'
;

semi_major_axis     @init{writer.println("<semi_major_axis>");}
                    @after{writer.println("</semi_major_axis>");} 
                    :  T_SEMI_MAJOR_AXIS '=' unit_value ';'
;

mean_motion         @init{writer.println("<mean_motion>");}
                    @after{writer.println("</mean_motion>");}
                    :  T_MEAN_MOTION '=' value ';'
;

orbit_epoch         @init{writer.println("<orbit_epoch>");}
                    @after{writer.println("</orbit_epoch>");}
                    :  T_ORBIT_EPOCH '=' T_NAME ';'
;


/* $SOURCE block -------------------------------------------*/
source_block        @init{writer.println("<SOURCE>");}
                    @after{writer.println("</SOURCE>");}  
                    : B_SOURCE ';' source_def+
                    | B_SOURCE ';'
;

source_def          @init{writer.println("<def>");}
                    @after{writer.println("</def>");} 
                    : defname ';' source_lowl+ T_ENDDEF ';'
                      | defname ';' T_ENDDEF ';'
;

source_lowl:  source_type
    | source_name
    | iau_name
    | ra
    | dec
    | ref_coord_frame
    | source_position_ref
    | source_position_epoch
    | ra_rate
    | dec_rate
    | velocity_wrt_lsr
    | source_model
    | inclination
    | eccentricity
    | arg_perigee
    | ascending_node
    | mean_anomaly
    | semi_major_axis
    | mean_motion
    | orbit_epoch
    | external_ref
;

source_type         @init{writer.println("<source_type>");}
                    @after{writer.println("</source_type>");}
                    : T_SOURCE_TYPE '=' value ';'
                      | T_SOURCE_TYPE '=' value ':' value ';'
;

source_name         @init{writer.println("<source_name>");}
                    @after{writer.println("</source_name>");}
                    : T_SOURCE_NAME '=' value ';'
;

iau_name            @init{writer.println("<iau_name>");}
                    @after{writer.println("</iau_name>");}
                    : T_IAU_NAME '=' value ';'
;

ra                  @init{writer.println("<ra>");}
                    @after{writer.println("</ra>");}
                    : T_RA '=' value ';'
;

dec                 @init{writer.println("<dec>");}
                    @after{writer.println("</dec>");}
                    : T_DEC '=' value ';'
;

ref_coord_frame     @init{writer.println("<ref_coord_frame>");}
                    @after{writer.println("</ref_coord_frame>");}
                    : T_REF_COORD_FRAME '=' value ';'
;

source_position_ref @init{writer.println("<source_position_ref>");}
                    @after{writer.println("</source_position_ref>");}
                    : T_SOURCE_POSITION_REF '=' value ';'
;

source_position_epoch @init{writer.println("<source_position_epoch>");}
                      @after{writer.println("</source_position_epoch>");}
                      :  T_SOURCE_POSITION_EPOCH '=' value ';'
;

ra_rate             @init{writer.println("<ra_rate>");}
                    @after{writer.println("</ra_rate>");}
                    :  T_RA_RATE '=' unit_value ';'
;

dec_rate            @init{writer.println("<dec_rate>");}
                    @after{writer.println("</dec_rate>");}
                    : T_DEC_RATE '=' unit_value ';'
;

velocity_wrt_lsr    @init{writer.println("<velocity_wrt_lsr>");}
                    @after{writer.println("</velocity_wrt_lsr>");}
                    : T_VELOCITY_WRT_LSR '=' unit_value ';'
;

source_model        @init{writer.println("<source_model>");}
                    @after{writer.println("</source_model>");}
                    : T_SOURCE_MODEL '=' value ':'
                      linkedvalue ':' unit_value ':' unit_value ':' value ':' unit_value ':' unit_value ':' unit_value ';'
;


/* $TAPELOG_OBS block -------------------------------------------*/

tapelog_obs_block:  B_TAPELOG_OBS ';' tapelog_obs_def+
      | B_TAPELOG_OBS ';' 
;

tapelog_obs_def:  T_DEF T_NAME ';' tapelog_obs_lowl+ T_ENDDEF ';'
      | T_DEF T_NAME ';' T_ENDDEF ';'
;

tapelog_obs_lowl: vsn
      | external_ref
;

vsn:    T_VSN '=' value ':' T_NAME ':' T_NAME ':' T_NAME ';'
;



/* $TRACKS -------------------------------------------*/

tracks_block        @init{writer.println("<TRACKS>");}
                    @after{writer.println("</TRACKS>");} 
                    : B_TRACKS ';' tracks_def+
                      | B_TRACKS ';'
;

tracks_def          @init{writer.println("<def>");}
                    @after{writer.println("</def>");} 
                    : defname ';' tracks_lowl+ T_ENDDEF ';'
                      | defname ';' T_ENDDEF ';'
;

tracks_lowl:  fanin_def
    | fanout_def
    | track_frame_format
    | data_modulation
    | vlba_frmtr_sys_trk
    | vlba_trnsprt_sys_trk
    | s2_recording_mode
    | s2_data_source
    | external_ref
;

fanin_def           @init{writer.println("<fanin_def>");}
                    @after{writer.println("</fanin_def>");} 
                    : T_FANIN_DEF '=' value (':' value|linkedvalue)* ';'
;

fanout_def          @init{writer.println("<fanout_def>");}
                    @after{writer.println("</fanout_def>");} 
                    : T_FANOUT_DEF '=' value (':' value|linkedvalue)* ';'
                      | T_FANOUT_DEF '=' emptyvalue ':' (':' value|linkedvalue)* ';'
;

track_frame_format  @init{writer.println("<track_frame_format>");}
                    @after{writer.println("</track_frame_format>");} 
                    : T_TRACK_FRAME_FORMAT '=' value ';'
;

data_modulation     @init{writer.println("<data_modulation>");}
                    @after{writer.println("</data_modulation>");} 
                    :  T_DATA_MODULATION '=' value ';'
;

vlba_frmtr_sys_trk  @init{writer.println("<vlba_frmtr_sys_trk>");}
                    @after{writer.println("</vlba_frmtr_sys_trk>");} 
                    : T_VLBA_FRMTR_SYS_TRK '=' value ':' value ':' value ':' value ';'
                      | T_VLBA_FRMTR_SYS_TRK '=' value ':' value ':' value ';'
;

vlba_trnsprt_sys_trk  @init{writer.println("<vlba_trnsprt_sys_trk>");}
                      @after{writer.println("</vlba_trnsprt_sys_trk>");} 
                      : T_VLBA_TRNSPRT_SYS_TRK '=' value ':' value ';'
;

s2_recording_mode     @init{writer.println("<s2_recording_mode>");}
                      @after{writer.println("</s2_recording_mode>");} 
                      :  T_S2_RECORDING_MODE '=' value ';'
;

s2_data_source        @init{writer.println("<s2_data_source>");}
                      @after{writer.println("</s2_data_source>");} 
                      :   T_S2_DATA_SOURCE '=' value ':' value ':' value ';'
                          | T_S2_DATA_SOURCE '=' value ';'
;



/* refs utility rules -------------------------------------------*/
ref @init{writer.println("<ref>");}
    @after{writer.println("</ref>");}
    :    T_REF primitive '=' T_NAME ';' {writer.println("<value>"+$T_NAME.text+"</value>");}
;

qref  @init{writer.println("<ref>");}
      @after{writer.println("</ref>");}
      :   T_REF primitive '=' T_NAME                  {writer.println("<value>"+$T_NAME.text+"</value>");}
                                      qualifiers ';'
          | T_REF primitive '=' T_NAME ';'            {writer.println("<value>"+$T_NAME.text+"</value>");}
;

external_ref: T_REF T_NAME ':' primitive '=' T_NAME ';'
;

qualifiers: (':' T_NAME                               {writer.println("<value>"+$T_NAME.text+"</value>");}
                        )+
;
  
primitive @init{writer.println("<primitive>");}
          @after{writer.println("</primitive>");}

          :  B_EXPER                {writer.println("EXPER");}
          | B_SCHEDULING_PARAMS     {writer.println("SCHEDULING_PARAMS");}
          | B_PROCEDURES            {writer.println("PROCEDURES");}
          | B_EOP                   {writer.println("EOP");}
          | B_FREQ                  {writer.println("FREQ");}
          | B_ANTENNA               {writer.println("ANTENNA");}
          | B_BBC                   {writer.println("BBC");}
          | B_CLOCK                 {writer.println("CLOCK");}
          | B_CORR                  {writer.println("CORR");}
          | B_DAS                   {writer.println("DAS");}
          | B_HEAD_POS              {writer.println("HEAD_POS");}
          | B_PASS_ORDER            {writer.println("PASS_ORDER");}
          | B_PHASE_CAL_DETECT      {writer.println("B_PHASE_CAL_DETECT");}
          | B_ROLL                  {writer.println("ROLL");}
          | B_IF                    {writer.println("IF");}
          | B_SEFD                  {writer.println("SEFD");}
          | B_SITE                  {writer.println("SITE");}
          | B_SOURCE                {writer.println("SOURCE");}
          | B_TRACKS                {writer.println("TRACKS");}
          | B_TAPELOG_OBS           {writer.println("TAPELOG_OBS");}
;

unit_value  @init{writer.println("<unitvalue>");}
            @after{writer.println("</unitvalue>");}
            : x=T_NAME                {writer.println("<value>"+$x.text+"</value>");}
              y=T_NAME                {writer.println("<unit>"+$y.text+"</unit>");}
;

value:    T_NAME                    {writer.println("<value>"+$T_NAME.text+"</value>");}
;

unit_list:  unit_value  (':' unit_option)*
;

unit_option:  unit_value
    | value
;

name_list:  name_value (':' name_value)*
;

name_value: T_NAME                  {writer.println("<value>"+$T_NAME.text+"</value>");}
;

value_list: value (':' value)*
;


B_ANTENNA:              '$ANTENNA';
B_BBC:                  '$BBC';
B_CLOCK:                '$CLOCK ';
B_CORR:                 '$CORR';
B_DAS:                  '$DAS';
B_EOP:                  '$EOP';
B_EXPER:                '$EXPER';
B_FREQ:                 '$FREQ';
B_GLOBAL:               '$GLOBAL';
B_HEAD_POS:             '$HEAD_POS';
B_IF:                   '$IF';
B_MODE:                 '$MODE';
B_PASS_ORDER:           '$PASS_ORDER';
B_PHASE_CAL_DETECT:     '$PHASE_CAL_DETECT';
B_PROCEDURES:           '$PROCEDURES';
B_ROLL:                 '$ROLL';
B_SCHED:                '$SCHED';
B_SCHEDULING_PARAMS:    '$SCHEDULING_PARAMS ';
B_SEFD:                 '$SEFD';
B_SITE:                 '$SITE';
B_SOURCE:               '$SOURCE';
B_STATION:              '$STATION';
B_TAPELOG_OBS:          '$TAPELOG_OBS';
B_TRACKS:               '$TRACKS';
T_A1_TAI:               'A1-TAI';
T_ANTENNA_DIAM:         'antenna_diam';
T_ANTENNA_MOTION:       'antenna_motion';
T_ARG_PERIGEE:          'arg_perigee';
T_ASCENDING_NODE:       'ascending_node';
T_AXIS_OFFSET:          'axis_offset';
T_AXIS_TYPE:            'axis_type';
T_BBC_ASSIGN:           'BBC_assign';
T_BITS_PER_SAMPLE:      'bits_per_sample';
T_CHAN_DEF:             'chan_def';
T_CLOCK_EARLY:          'clock_early';
T_CONTACT_EMAIL:        'contact_email';
T_CONTACT_NAME:         'contact_name';
T_DATA_MODULATION:      'data_modulation';
T_DATA_TRANSFER:        'data_transfer';
T_DEC_RATE:             'dec_rate';
T_DEC:                  'dec';
T_DEF:                  'def ';
T_DELTA_EPS:            'delta_eps';
T_DELTA_PSI:            'delta_psi';
T_ECCENTRICITY:         'eccentricity';
T_ELECTRONICS_RACK_TYPE:'electronics_rack_type';
T_ENDDEF:               'enddef';
T_ENDSCAN:              'endscan';
T_EOP_INTERVAL:         'eop_interval';
T_EOP_REF_EPOCH:        'eop_ref_epoch';
T_EXPER_DESCRIPTION:    'exper_description';
T_EXPER_NAME:           'exper_name';
T_EXPER_NOMINAL_START:  'exper_nominal_start';
T_EXPER_NOMINAL_STOP:   'exper_nominal_stop';
T_EXPER_NUM:            'exper_num';
T_FANIN_DEF:            'fanin_def';
T_FANOUT_DEF:           'fanout_def';
T_HEADSTACK_MOTION:     'headstack_motion';
T_HEADSTACK_POS:        'headstack_pos';
T_HEADSTACK:            'headstack';
T_HORIZON_MAP_AZ:       'horizon_map_az';
T_HORIZON_MAP_EL:       'horizon_map_el';
T_IAU_NAME:             'IAU_name';
T_IF_DEF:               'if_def';
T_INCLINATION:          'inclination';
T_LINK:                 '&';
T_MEAN_ANOMALY:         'mean_anomaly';
T_MEAN_MOTION:          'mean_motion';
T_MIDOB_CAL:            'midob_cal';
T_MODE:                 'mode';
T_NEW_SOURCE_COMMAND:   'new_source_command';
T_NEW_TAPE_SETUP:       'new_tape_setup';
T_NUM_EOP_POINTS:       'num_eop_points';
T_NUM_NUT_POINTS:       'num_nut_points';
T_NUMBER_DRIVES:        'number_drives';
T_NUT_INTERVAL:         'nut_interval';
T_NUT_MODEL:            'nut_model';
T_NUT_REF_EPOCH:        'nut_ref_epoch';
T_OCCUPATION_CODE:      'occupation_code';
T_OCEAN_LOAD_HORIZ:     'ocean_load_horiz';
T_OCEAN_LOAD_VERT:      'ocean_load_vert';
T_ORBIT_EPOCH:          'orbit_epoch';
T_PARITY_CHECK:         'parity_check';
T_PASS_ORDER:           'pass_order';
T_PHASE_CAL_DETECT:     'phase_cal_detect';
T_PI_EMAIL:             'PI_email';
T_PI_NAME:              'PI_name';
T_POINTING_SECTOR:      'pointing_sector';
T_POSTOB_CAL:           'postob_cal';
T_PREOB_CAL:            'preob_cal';
T_PROCEDURE_NAME_PREFIX:'procedure_name_prefix';
T_RA_RATE:              'ra_rate';
T_RA:                   'ra';
T_RECORD_DENSITY:       'record_density';
T_RECORD_TRANSPORT_TYPE:'record_transport_type';
T_RECORDING_SYSTEM_ID:  'recording_system_ID';
T_REF_COORD_FRAME:      'ref_coord_frame';
T_REF:                  'ref';
T_ROLL_DEF:             'roll_def';
T_ROLL_INC_PERIOD:      'roll_inc_period';
T_ROLL_REINIT_PERIOD:   'roll_reinit_period';
T_ROLL:                 'roll';
T_S2_DATA_SOURCE:       'S2_data_source';
T_S2_GROUP_ORDER:       'S2_group_order';
T_S2_RECORDING_MODE:    'S2_recording_mode';
T_SAMPLE_RATE:          'sample_rate';
T_SCAN:                 'scan';
T_SCHEDULER_EMAIL:      'scheduler_email';
T_SCHEDULER_NAME:       'scheduler_name';
T_SEFD_MODEL:           'sefd_model';
T_SEFD:                 'sefd';
T_SEMI_MAJOR_AXIS:      'semi-major_axis';
T_SETUP_ALWAYS:         'setup_always';
T_SITE_ID:              'site_ID';
T_SITE_NAME:            'site_name';
T_SITE_POSITION_EPOCH:  'site_position_epoch';
T_SITE_POSITION_REF:    'site_position_ref';
T_SITE_POSITION:        'site_position';
T_SITE_TYPE:            'site_type';
T_SITE_VELOCITY:        'site_velocity';
T_SOURCE_MODEL:         'source_model';
T_SOURCE_NAME:          'source_name';
T_SOURCE_POSITION_EPOCH:'source_position_epoch';
T_SOURCE_POSITION_REF:  'source_position_ref';
T_SOURCE_TYPE:          'source_type';
T_SOURCE:               'source';
T_START:                'start';
T_STATION:              'station';
T_SWITCHING_CYCLE:      'switching_cycle';
T_TAI_UTC:              'TAI-UTC';
T_TAPE_CHANGE:          'tape_change';
T_TAPE_CONTROL:         'tape_control';
T_TAPE_LENGTH:          'tape_length';
T_TAPE_MOTION:          'tape_motion';
T_TAPE_PREPASS:         'tape_prepass';
T_TARGET_CORRELATOR:    'target_correlator';
T_TRACK_FRAME_FORMAT:   'track_frame_format';
T_UT1_UTC:              'ut1-utc';
T_VELOCITY_WRT_LSR:     'velocity_wrt_LSR';
T_VEX_REV:              'VEX_rev';
T_VLBA_FRMTR_SYS_TRK:   'VLBA_frmtr_sys_trk';
T_VLBA_TRNSPRT_SYS_TRK: 'VLBA_trnsprt_sys_trk';
T_VSN:                  'VSN';
T_X_WOBBLE:             'x_wobble';
T_Y_WOBBLE:             'y_wobble';
T_ZEN_ATMOS:            'zen_atmos';


linkedvalue: T_LINK T_NAME        {writer.println("<linkedvalue>"+$T_NAME.text+"</linkedvalue>");} ;

fragment defname: T_DEF T_NAME              {writer.println("<defname>"+$T_NAME.text+"</defname>");};
fragment emptyvalue:                        {writer.println("<value></value>");};


fragment LETTERS_NUMBERS: ('a'..'z'|'A'..'Z'|'0'..'9')
;
fragment SYMBOLS: ('_'|'@'|'-'|'.'|'#'|'/'|'+'|'('|')')
;
fragment QUOTES: ('"'|'\'') //"
;

fragment T_NOTALLOWEDINXML : '<'  {System.out.println("Warning: Found '<', but not allowed in XML. Replaced by _@1.");{setText(getText().substring(0, getText().length()-1)+"_@1");} }
;

T_NAME: (LETTERS_NUMBERS | QUOTES | '_'| '-')  ( LETTERS_NUMBERS | QUOTES | SYMBOLS | T_NOTALLOWEDINXML )*
;

T_QUOTESTRING: '"' (LETTERS_NUMBERS | ' ' | ',' | SYMBOLS)* '"'  //allows whitespaces
;




COMMENT:'*'     {$channel=HIDDEN;}
  ;

LINE_COMMENT
  :  '*' ~( '\r' | '\n' )* {$channel=HIDDEN;}
  ;

NEWLINE 
  :  ( '\r'? '\n' | '\r' ) {$channel=HIDDEN;}
  ;

WS  :   (' ') {$channel=HIDDEN;}
    ;

/* eof */
