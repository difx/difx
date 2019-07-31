/************************************************************************/
/*                                                                      */
/* In-memory compression/decompression utility, used for squishing the  */
/* postscript fringe plot in the DFIO library.  This code was taken     */
/* from a distribution of LZRW3-A by Ross Williams, and has had most of */
/* the comments removed.  Extremely verbose comments and algorithm      */
/* descriptions can be found in the original source code file, called   */
/* old_lzrw3-a.c, included in the DFIO source code directory.           */
/*                                                                      */
/* Borrowed November 1 1999 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "mk4_typedefs.h"       /* Simpler for use in DFIO library */
#include "mk4_dfio.h"

#define COMPRESS_ACTION_IDENTITY   0
#define COMPRESS_ACTION_COMPRESS   1
#define COMPRESS_ACTION_DECOMPRESS 2

struct compress_identity
  {
   U32 id;             /* Identifying number of algorithm.            */
   U32 memory;         /* Number of bytes of working memory required. */

   char  *name;        /* Name of algorithm.                          */
   char  *version;     /* Version number.                             */
   char  *date;        /* Date of release of this version.            */
   char  *copyright;   /* Copyright message.                          */

   char  *author;      /* Author of algorithm.                        */
   char  *affiliation; /* Affiliation of author.                      */
   char  *vendor;      /* Where the algorithm can be obtained.        */
  };

void  compress(     /* Single function interface to compression algorithm. */
U16  action,        /* Action to be performed.                             */
U8   *wrk_mem,      /* Working memory temporarily given to routine to use. */
U8   *src_adr,      /* Address of input  data.                             */
U32  src_len,       /* Length  of input  data.                             */
U8   *dst_adr,      /* Address of output data.                             */
U32  *p_dst_len     /* Pointer to a longword where routine will write:     */
                    /*    If action=..IDENTITY   => Adr of id structure.   */
                    /*    If action=..COMPRESS   => Length of output data. */
                    /*    If action=..DECOMPRESS => Length of output data. */
);


#define U(X)            ((U32) X)
#define SIZE_P_BYTE     (U(sizeof(U8 *)))
#define ALIGNMENT_FUDGE (U(16))
#define MEM_REQ ( U(4096)*(SIZE_P_BYTE) + ALIGNMENT_FUDGE )

static struct compress_identity identity =
{
 U(0x01B90B91),                           /* Algorithm identification number. */
 MEM_REQ,                                 /* Working memory (bytes) required. */
 "LZRW3-A",                               /* Name of algorithm.               */
 "1.0",                                   /* Version number of algorithm.     */
 "15-Jul-1990",                           /* Date of algorithm.               */
 "Public Domain",                         /* Copyright notice.                */
 "Ross N. Williams",                      /* Author of algorithm.             */
 "Renaissance Software",                  /* Affiliation of author.           */
 "Public Domain"                          /* Vendor of algorithm.             */
};

void compress_compress  (U8 *,U8 *,U32,U8 *,U32 *);
void compress_decompress(U8 *,U8 *,U32,U8 *,U32 *);

void compress(/* action,wrk_mem,src_adr,src_len,dst_adr,p_dst_len) */
U16     action,      /* Action to be performed.                             */
U8    *wrk_mem,      /* Address of working memory we can use.               */
U8    *src_adr,      /* Address of input data.                              */
U32    src_len,      /* Length  of input data.                              */
U8    *dst_adr,      /* Address to put output data.                         */
U32 *p_dst_len)      /* Address of longword for length of output data.      */
{
 switch (action)
   {
    case COMPRESS_ACTION_IDENTITY:
       // *p_dst_len=(U32) &identity; disabled; apparently unused rjc 2010.3.3
       *p_dst_len = 0;
       printf ("possibly fatal error in compress.c!!\n");
       break;
    case COMPRESS_ACTION_COMPRESS:
       compress_compress(wrk_mem,src_adr,src_len,dst_adr,p_dst_len);
       break;
    case COMPRESS_ACTION_DECOMPRESS:
       compress_decompress(wrk_mem,src_adr,src_len,dst_adr,p_dst_len);
       break;
   }
}

#define UCARD unsigned

#define FLAG_BYTES 4

#define FLAG_COMPRESS 0     /* Signals that output was result of compression. */
#define FLAG_COPY     1     /* Signals that output was simply copied over.    */

// #define U32_ALIGN_UP(X) ((((U32)X)+3)&~3)
//                         assume work size is multiple of 16
//                         ..otherwise code failed on 64 bit arch. rjc 2009.3.11
#define U32_ALIGN_UP(X) (X)

#define MAX_RAW_ITEM (18)

#define MAX_RAW_GROUP (16*MAX_RAW_ITEM)

#define MAX_CMP_GROUP (2+16*2)

#define HASH_TABLE_LENGTH (4096)

#define HASH_TABLE_DEPTH_BITS (3)      /* Must be in range [0,12].            */

#define PARTITION_LENGTH_BITS (12-HASH_TABLE_DEPTH_BITS)
#define PARTITION_LENGTH      (1<<PARTITION_LENGTH_BITS)
#define HASH_TABLE_DEPTH      (1<<HASH_TABLE_DEPTH_BITS )
#define HASH_MASK             (PARTITION_LENGTH-1)
#define DEPTH_MASK            (HASH_TABLE_DEPTH-1)

#define START_STRING_18 ((U8 *) "123456789012345678")

#define HASH(PTR) \
 ( \
     (((40543*(((*(PTR))<<8)^((*((PTR)+1))<<4)^(*((PTR)+2))))>>4) & HASH_MASK) \
  << HASH_TABLE_DEPTH_BITS \
 )

#define UPDATE_P(P_BASE,NEWPTR) \
{(P_BASE)[cycle++]=(NEWPTR); cycle&=DEPTH_MASK;}

#define UPDATE_I(I_BASE,NEWPTR) \
{hash[(I_BASE)+cycle++]=(NEWPTR); cycle&=DEPTH_MASK;}

#define ANY_HASH_INDEX (0)

/******************************************************************************/

void compress_compress
           (p_wrk_mem,p_src_first,src_len,p_dst_first,p_dst_len)
/* Input  : Hand over the required amount of working memory in p_wrk_mem.     */
/* Input  : Specify input block using p_src_first and src_len.                */
/* Input  : Point p_dst_first to the start of the output zone (OZ).           */
/* Input  : Point p_dst_len to a U32 to receive the output length.            */
/* Input  : Input block and output zone must not overlap.                     */
/* Output : Length of output block written to *p_dst_len.                     */
/* Output : Output block in Mem[p_dst_first..p_dst_first+*p_dst_len-1]. May   */
/* Output : write in OZ=Mem[p_dst_first..p_dst_first+src_len+MAX_CMP_GROUP-1].*/
/* Output : Upon completion guaranteed *p_dst_len<=src_len+FLAG_BYTES.        */
U8 *p_wrk_mem;
U8 *p_src_first;
U32  src_len;
U8 *p_dst_first;
U32 *p_dst_len;
{
 /* p_src and p_dst step through the source and destination blocks.           */
 U8 *p_src = p_src_first;
 U8 *p_dst = p_dst_first;

 U8 *p_src_post  = p_src_first+src_len;
 U8 *p_dst_post  = p_dst_first+src_len;
 U8 *p_src_max1  = p_src_first+src_len-MAX_RAW_ITEM;
 U8 *p_src_max16 = p_src_first+src_len-MAX_RAW_ITEM*16;

 #define TOPWORD 0xFFFF0000
 U8 *p_control;
 U32 control=TOPWORD;

 U8 **hash= (U8 **) U32_ALIGN_UP(p_wrk_mem);

 U8 **p_h1=0;
 U8 **p_h2=0;

 UCARD cycle=0;

 *p_dst++=FLAG_COMPRESS;
 {UCARD i; for (i=2;i<=FLAG_BYTES;i++) *p_dst++=0;}

 p_control=p_dst; p_dst+=2;

 {UCARD i; U8 **p_h=hash;
  #define ZH *p_h++=START_STRING_18
  for (i=0;i<256;i++)     /* 256=HASH_TABLE_LENGTH/16. */
    {ZH;ZH;ZH;ZH;
     ZH;ZH;ZH;ZH;
     ZH;ZH;ZH;ZH;
     ZH;ZH;ZH;ZH;}
 }

 while (1)
   {/* Begin main processing loop. */

     U8 *p_ziv;                 /* Points to first byte of current Ziv.       */
     UCARD unroll;              /* Loop counter for unrolled inner loop.      */
     UCARD index;               /* Index of current partition.                */
     U8 **p_h0;                 /* Pointer to current partition.              */
     register UCARD d;          /* Depth looping variable.                    */
     register UCARD bestlen;    /* Holds the best length seen so far.         */
     register UCARD bestpos;    /* Holds number of best pointer seen so far.  */

    /* Test for overrun and jump to overrun code if necessary.                */
    if (p_dst>p_dst_post)
       goto overrun;

    /* The following cascade of if statements efficiently catches and deals   */
    /* with varying degrees of closeness to the end of the input block.       */
    unroll=16;
    if (p_src>p_src_max16)
      {
       unroll=1;
       if (p_src>p_src_max1)
         {
          if (p_src==p_src_post)
             break;
          else
             {p_h0=&hash[ANY_HASH_INDEX]; /* Avoid undefined pointer. */
              goto literal;}
         }
      }


    begin_unrolled_loop:

       p_ziv=p_src;

       index=HASH(p_src);
       p_h0=&hash[index];

       bestlen=0;
       bestpos=0;
       for (d=0;d<HASH_TABLE_DEPTH;d++)
         {
          register U8 *s=p_src;
          register U8 *p=p_h0[d];
          register UCARD len;
          if (s[bestlen] == p[bestlen])
            {
             #define PS *p++!=*s++
             PS || PS || PS || PS || PS || PS || PS || PS || PS ||
             PS || PS || PS || PS || PS || PS || PS || PS || PS || s++;
             len=s-p_src-1;
             if (len>bestlen)
               {
                bestpos=d;
                bestlen=len;
               }
            }
         }

       if (bestlen<3)
         {
          literal: *p_dst++=*p_src++; control&=0xFFFEFFFF;

          if (p_h2!=0)
             {UPDATE_P(p_h2,p_ziv-2);}

          p_h2=p_h1; p_h1=p_h0;

         }
       else
         {
          index+=bestpos;
          *p_dst++=((index&0xF00)>>4)|(bestlen-3);
          *p_dst++=index&0xFF;
          p_src+=bestlen;

          if (p_h1!=0)
            {
             if (p_h2!=0)
               {UPDATE_P(p_h2,p_ziv-2); p_h2=0;}
             UPDATE_P(p_h1,p_ziv-1); p_h1=0;
            }

          UPDATE_P(p_h0,p_ziv);
         }
       control>>=1;

    end_unrolled_loop: if (--unroll) goto begin_unrolled_loop;

    if ((control&TOPWORD)==0)
      {
       *p_control++=  control     &0xFF;
       *p_control  = (control>>8) &0xFF;

       p_control=p_dst; p_dst+=2;

       control=TOPWORD;
      }

   } /* End main processing loop. */

 while(control&TOPWORD) control>>=1;
 *p_control++= control     &0xFF;
 *p_control++=(control>>8) &0xFF;

 if (p_control==p_dst) p_dst-=2;

 *p_dst_len=p_dst-p_dst_first;
 return;

 overrun:
 *p_dst_first=FLAG_COPY;
 memcpy (p_dst_first+FLAG_BYTES, p_src_first, src_len);
 *p_dst_len=src_len+FLAG_BYTES;
}

/******************************************************************************/

void compress_decompress
           (p_wrk_mem,p_src_first,src_len,p_dst_first,p_dst_len)
/* Input  : Hand over the required amount of working memory in p_wrk_mem.     */
/* Input  : Specify input block using p_src_first and src_len.                */
/* Input  : Point p_dst_first to the start of the output zone.                */
/* Input  : Point p_dst_len to a U32 to receive the output length.            */
/* Input  : Input block and output zone must not overlap. User knows          */
/* Input  : upperbound on output block length from earlier compression.       */
/* Input  : In any case, maximum expansion possible is nine times.            */
/* Output : Length of output block written to *p_dst_len.                     */
/* Output : Output block in Mem[p_dst_first..p_dst_first+*p_dst_len-1].       */
/* Output : Writes only  in Mem[p_dst_first..p_dst_first+*p_dst_len-1].       */
U8 *p_wrk_mem;
U8 *p_src_first;
U32  src_len;
U8 *p_dst_first;
U32 *p_dst_len;
{
 /* Byte pointers p_src and p_dst scan through the input and output blocks.   */
 register U8 *p_src = p_src_first+FLAG_BYTES;
 register U8 *p_dst = p_dst_first;

 U8 *p_src_post  = p_src_first+src_len;
 U8 *p_src_max16 = p_src_first+src_len-(MAX_CMP_GROUP-2);

 U8 **hash = (U8 **) U32_ALIGN_UP(p_wrk_mem);

 register U32 control=1;

 register UCARD literals=0;

 UCARD cycle=0;

 if (*p_src_first==FLAG_COPY)
   {
    memcpy (p_dst_first, p_src_first+FLAG_BYTES, src_len-FLAG_BYTES);
    *p_dst_len=src_len-FLAG_BYTES;
    return;
   }

 {UCARD i; U8 **p_h=hash;
  #define ZJ *p_h++=START_STRING_18
  for (i=0;i<256;i++)     /* 256=HASH_TABLE_LENGTH/16. */
    {ZJ;ZJ;ZJ;ZJ;
     ZJ;ZJ;ZJ;ZJ;
     ZJ;ZJ;ZJ;ZJ;
     ZJ;ZJ;ZJ;ZJ;}
 }

 while (p_src!=p_src_post)
   {/* Start of outer loop */

    register UCARD unroll;   /* Counts unrolled loop executions.              */

    if (control==1)
      {
       control=0x10000|*p_src++;
       control|=(*p_src++)<<8;
      }

    unroll= p_src<=p_src_max16 ? 16 : 1;

    while (unroll--)
      { /* Begin unrolled inner loop. */

       if (control&1)
         {

          register U8 *p;              /* Points to place from which to copy. */
          register UCARD lenmt;        /* Length of copy item minus three.    */
          register U8 *p_ziv=p_dst;    /* Pointer to start of current Ziv.    */
          register UCARD index;        /* Index of hash table copy pointer.   */

          lenmt=*p_src++;
          index=((lenmt&0xF0)<<4)|*p_src++;
          p=hash[index];
          lenmt&=0xF;

          *p_dst++=*p++;
          *p_dst++=*p++;
          *p_dst++=*p++;
          while (lenmt--)
             *p_dst++=*p++;

          if (literals>0)
            {
             register U8 *r=p_ziv-literals;;
             UPDATE_I(HASH(r),r);
             if (literals==2)
                {r++; UPDATE_I(HASH(r),r);}
             literals=0;
            }

          UPDATE_I(index&(~DEPTH_MASK),p_ziv);
         }
       else
         {
          *p_dst++=*p_src++;

          if (++literals == 3)
             {register U8 *p=p_dst-3;
              UPDATE_I(HASH(p),p); literals=2;}
         }

       control>>=1;

      } /* End unrolled inner loop. */

   } /* End of outer loop */

 *p_dst_len=p_dst-p_dst_first;
}
