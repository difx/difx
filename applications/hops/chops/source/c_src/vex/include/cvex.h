#ifndef CVEX_H
#define CVEX_H

/*----------------------------------------------------------------------------
 * The general organization of a binary or parsed cvex:$CORR file:
 *    Cvex_Config      [ 1                                        ]   
 *    Cvex_Mode        [ 1                                        ]
 *    Cvex_BoardParms  [ num defs used from $CORR_BD_PARMS        ]
 *    Cvex_SectionMode [ num defs used from $CORR_BD_SECTION_MODE ]
 *    Cvex_ChipMode    [ num defs used from $CORR_CHIP_MODE       ]
 *    Cvex_BlockMode   [ num defs used from $CORR_CHIP_BLOCK_MODE ]
 */  



/*----------------------------------------------------------------------------
 */

#define MAX_SECTS_P_BRD          16               /* Max sections/board     */
#define MAX_BRDS_P_CORR          16               /* Max boards/correlator  */
#define MAX_XPOL_P_BRD           8                /* Max xpol pairs/board   */
#define MAX_CHIP_INS_P_CHIP      4                /* Max chip inputs/chip   */
#define MAX_CHIPS_P_BRD          32               /* Max chips/board        */
#define MAX_MUXS_P_BLK           4                /* Max multiplexers/block */
#define MAX_BLKS_P_SNAKE_P_CHIP  10               /* Max blocks/snake/chip  */
#define MAX_BLKS_P_CHIP          8                /* Max blocks/chip        */
#define MAX_SNAKES_P_CHIP        8                /* Max snakes/chip        */



/*----------------------------------------------------------------------------
 * $CVEX_REV 
 */



/*----------------------------------------------------------------------------
 * $CORR_CONFIG 
 */

typedef struct Cvex_Config {
   char                rev[MAX_PVALSIZE];
   char                defId[MAX_NAMESIZE];
   struct Cvex_Mode *  mode; 
   short               ignoreChanNames;
   short               auto_corr;
} Cvex_Config;



/*----------------------------------------------------------------------------
 * $CORR_MODE
 */

typedef struct Cvex_Section {
   struct Cvex_SectionMode *  mode;
   short                      phyChipNum;
   char                       refId[MAX_PVALSIZE];
   char                       remId[MAX_PVALSIZE];
} Cvex_Section;

 
typedef struct Cvex_Board {
   char                      segId[MAX_PVALSIZE];
   char                      sliceId[MAX_PVALSIZE];
   struct Cvex_BoardParms *  mode;      
   short                     numOfSections;
   Cvex_Section              section[MAX_SECTS_P_BRD];
} Cvex_Board;


typedef struct Cvex_XPolPair {
   char  chn1Id[MAX_PVALSIZE];
   char  chn2Id[MAX_PVALSIZE];
} Cvex_XPolPair;


typedef struct Cvex_Mode {
   char           defId[MAX_NAMESIZE];
   short          numOfBoards;
   Cvex_Board     board[MAX_BRDS_P_CORR];
   short          numOfXPolPairs;
   Cvex_XPolPair  xPolPair[MAX_XPOL_P_BRD];
} Cvex_Mode;


 
/*----------------------------------------------------------------------------
 * $CORR_BD_PARMS
 */

typedef struct Cvex_BoardParms {
   char   defId[MAX_NAMESIZE];
   short  accumRatio;
   short  bocfShsmpRatio;
   short  dataShsmpRatio;
   short  sampleCntPerLagEnable;
} Cvex_BoardParms;



/*----------------------------------------------------------------------------
 * $CORR_SECT_MODE
 */

typedef struct Cvex_Chip {
   struct Cvex_ChipMode *  mode;
   short                   relChipNum;
   char                    input[MAX_CHIP_INS_P_CHIP][MAX_PVALSIZE];
} Cvex_Chip;


typedef struct Cvex_SectionMode {
   char       defId[MAX_NAMESIZE];
   short      numOfChips;
   Cvex_Chip  chip[MAX_CHIPS_P_BRD];
} Cvex_SectionMode;



/*----------------------------------------------------------------------------
 * $CORR_CHIP_MODE
 */

typedef struct Cvex_Block {
   struct Cvex_BlockMode *  mode;
   char                     blockId[MAX_PVALSIZE];
   short                    mux[MAX_MUXS_P_BLK];
} Cvex_Block;


typedef struct Cvex_Snake {
   char   type[MAX_PVALSIZE];
   char   refId[MAX_PVALSIZE];
   char   remId[MAX_PVALSIZE];
   short  lenOfSnakePath;
   char   path[MAX_BLKS_P_SNAKE_P_CHIP][MAX_PVALSIZE];
} Cvex_Snake;
 

typedef struct Cvex_ChipMode {
   char        defId[ MAX_NAMESIZE ];
   short       numOfBlocks;
   Cvex_Block  block[MAX_BLKS_P_CHIP]; 
   short       numOfSnakes;
   Cvex_Snake  snake[MAX_SNAKES_P_CHIP];
} Cvex_ChipMode;

 

/*----------------------------------------------------------------------------
 * $CORR_BLOCK_MODE
 */

typedef struct Cvex_BlockMode {
   char   defId[MAX_NAMESIZE];
   short  invalidateOnTapMove;
   short  headerMode;
   short  enableTap;
   short  lCellRotrMode;
   short  lCellXDly;
   short  lCellYDly;
   short  rCellRotrMode;
   short  rCellXDly;
   short  rCellYDly;
} Cvex_BlockMode;

#endif
