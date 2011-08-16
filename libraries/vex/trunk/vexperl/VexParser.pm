package Astro::VexParser;

use strict;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK $AUTOLOAD);

require Exporter;
require DynaLoader;
require AutoLoader;

@ISA = qw(Exporter DynaLoader);

@EXPORT = qw( vex_open lowl2int block2int int2lowl int2block print_vex
              print_vex_blocks print_block_name print_qref_block
              print_qualifiers print_lowl print_lowl_st print_svalue
              print_literal_list print_comment print_comment_trailing
              vex_field get_source_def get_source_def_next
              get_mode_def get_mode_def_next get_station_def
              get_station_def_next get_all_lowl get_all_lowl_next
              get_mode_lowl get_mode_lowl_next get_station_lowl
              get_station_lowl_next get_source_lowl
              get_source_lowl_next get_global_lowl
              get_global_lowl_next find_block find_def find_next_def
              find_lowl get_scan_start get_scan_mode get_scan_source
              get_scan_source get_scan_source_next get_scan_station
              get_scan_station_next find_next_scan get_scan get_scan_next
              get_station_scan get_station_scan_next
              B_ANTENNA B_BBC B_CLOCK B_CORR
              B_DAS B_EOP B_EXPER B_FREQ B_GLOBAL B_HEAD_POS B_IF
              B_MODE B_PASS_ORDER B_PHASE_CAL_DETECT B_PROCEDURES
              B_ROLL B_SCHED B_SCHEDULING_PARAMS B_SEFD B_SITE
              B_SOURCE B_STATION B_TAPELOG_OBS B_TRACKS T_A1_TAI
              T_ANGLE T_ANTENNA_DIAM T_ANTENNA_MOTION T_ARG_PERIGEE
              T_ASCENDING_NODE T_AXIS_OFFSET T_AXIS_TYPE T_BBC_ASSIGN
              T_BITS_PER_SAMPLE T_CHAN_DEF T_CLOCK_EARLY T_COMMENT
              T_COMMENT_TRAILING T_CONTACT_EMAIL T_CONTACT_NAME
              T_DATA_MODULATION T_DEC T_DEC_RATE T_DEF T_ECCENTRICITY
              T_ELECTRONICS_RACK_TYPE T_ENDDEF T_ENDSCAN
              T_EOP_INTERVAL T_EOP_REF_EPOCH T_EXPER_DESCRIPTION
              T_EXPER_NAME T_EXPER_NOMINAL_START T_EXPER_NOMINAL_STOP
              T_EXPER_NUM T_FANIN_DEF T_FANOUT_DEF T_HEADSTACK
              T_HEADSTACK_MOTION T_HEADSTACK_POS T_HORIZON_MAP_AZ
              T_HORIZON_MAP_EL T_IAU_NAME T_IF_DEF T_INCLINATION
              T_LINK T_LITERAL T_MEAN_ANOMALY T_MEAN_MOTION
              T_MIDOB_CAL T_MODE T_NAME T_NEW_SOURCE_COMMAND
              T_NEW_TAPE_SETUP T_NUMBER_DRIVES T_NUM_EOP_POINTS
              T_OCCUPATION_CODE T_OCEAN_LOAD_HORIZ T_OCEAN_LOAD_VERT
              T_ORBIT_EPOCH T_PARITY_CHECK T_PASS_ORDER
              T_PHASE_CAL_DETECT T_PI_EMAIL T_PI_NAME
              T_POINTING_SECTOR T_POSTOB_CAL T_PREOB_CAL
              T_PROCEDURE_NAME_PREFIX T_RA T_RA_RATE
              T_RECORDING_SYSTEM_ID T_RECORD_DENSITY
              T_RECORD_TRANSPORT_TYPE T_REF T_REF_COORD_FRAME T_ROLL
              T_ROLL_DEF T_ROLL_INC_PERIOD T_ROLL_REINIT_PERIOD
              T_S2_DATA_SOURCE T_S2_GROUP_ORDER T_S2_RECORDING_MODE
              T_SAMPLE_RATE T_SCAN T_SCHEDULER_EMAIL T_SCHEDULER_NAME
              T_SEFD T_SEFD_MODEL T_SEMI_MAJOR_AXIS T_SETUP_ALWAYS
              T_SITE_ID T_SITE_NAME T_SITE_POSITION
              T_SITE_POSITION_EPOCH T_SITE_POSITION_REF T_SITE_TYPE
              T_SITE_VELOCITY T_SOURCE T_SOURCE_MODEL T_SOURCE_NAME
              T_SOURCE_POSITION_EPOCH T_SOURCE_POSITION_REF
              T_SOURCE_TYPE T_START T_STATION T_SWITCHING_CYCLE
              T_TAI_UTC T_TAPE_CHANGE T_TAPE_CONTROL T_TAPE_LENGTH
              T_TAPE_MOTION T_TAPE_PREPASS T_TARGET_CORRELATOR
              T_TRACK_FRAME_FORMAT T_UT1_UTC T_VELOCITY_WRT_LSR
              T_VEX_REV T_VLBA_FRMTR_SYS_TRK T_VLBA_TRNSPRT_SYS_TRK
              T_VSN T_X_WOBBLE T_Y_WOBBLE T_ZEN_ATMOS);

$VERSION = '0.5';

use Carp;

sub AUTOLOAD {
  # This AUTOLOAD is used to 'autoload' constants from the constant()
  # XS function.  If a constant is not found then control is passed
  # to the AUTOLOAD in AutoLoader.

  my $constname;
  ($constname = $AUTOLOAD) =~ s/.*:://;
  croak "& not defined" if $constname eq 'constant';
  my $val = constant($constname, @_ ? $_[0] : 0);
  if ($! != 0) {
    if ($! =~ /Invalid/) {
      $AutoLoader::AUTOLOAD = $AUTOLOAD;
      goto &AutoLoader::AUTOLOAD;
    }
    else {
      croak "$constname not defined";
    }
  }
  no strict 'refs';
  *$AUTOLOAD = sub { $val };
  goto &$AUTOLOAD;
}

bootstrap Astro::VexParser $VERSION;

1;
__END__

=head1 NAME

Astro::Vex - Perl interface to the Haystack VEX parser

=head1 SYNOPSIS

  use Astro::Vex;

  my $vex = vex_open('experiment.vex');

=head1 DESCRIPTION

Stub documentation for VexPerl was created by h2xs. It looks like the
author of the extension was negligent enough to leave the stub
unedited.



=head1 AUTHOR

Chris Phillips   phillips@jive.nl

=cut
