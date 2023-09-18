/**
 * @file IdxFileFormat.h
 *
 * This file provides the definition of the XCube Indexer file format.
 *
 * @warning This file format is subject to change. 
 */
#ifndef __IDXFILEFORMAT__H__
#define __IDXFILEFORMAT__H__

#include "xcTypes.h"

namespace x3c {
    namespace indexer {

        /*
        ** Version info
        */
        const UINT16 INDEX_FILE_VERSION_MAJOR   = 1;
        const UINT16 INDEX_FILE_VERSION_MINOR   = 0;

        /**
         * Constants
         */
        const UINT32 INDEX_MAGIC_COOKIE     = 0xa5b4c3d2;
        const UINT32 TRAILER_MAGIC_COOKIE   = 0xcafebabe;
        const std::string INDEX_FILE_EXT    = "ix1";
        const std::string INDEX_FILE_EXT_WDOT    = ".ix1";

        typedef struct _IndexRecord
        {
            UINT64 timestamp;       /* time stamp  (seconds << 32 + nanoseconds) */
            UINT64 offset;          /* file offset to the x3c header, not the pay load */
            UINT64 packet_number;   /* */
        } IndexRecord;

        typedef struct _IndexHeader
        {
            UINT32 magic_number;       /* magic number            */
            UINT32 header_length;      /* does not include magic_number or header length */
            UINT16 version_major;      /* major version number    */
            UINT16 version_minor;      /* minor version number    */
            UINT64 start_time;         /* seconds << 32 + nanoseconds */
            UINT64 end_time;           /* seconds << 32 + nanoseconds */
            INT32  time_zone;          /* +/- offset from GMT in seconds */
            UINT64 num_index_records;  /* number of index records in this file   */
            UINT64 total_index_count;  /* the index count for the entire stream */
            UINT16 num_files;          /* the number of files that make up the stream */
            UCHAR  reserved[0x1fc2];   /* pad to 8KB - must adjust when adding fields */
            UINT32 trailer_magic;      /* for sanity checking the trailer */

        } IndexHeader;

        typedef struct _IndexTrailer
        {
            UINT64 trailer_length;     /* length of the trailer */
            UINT64 timestamp;          /* trailer timestamp (time witten) */
            UINT64 file_record_count;  /* number of index records in the file */ 
            UINT64 file_offset;        /* Offset */ 
            UINT64 total_packets;      /* */
            UCHAR  reserved[0x1fd4];   /* pad to 8KB - must adjust when adding fields */
            UINT32 magic_number;       /* trailer magic. Should always be the last field */
        } IndexTrailer;

    }   /* namespace indexer */
}  /* namespace x3c */

#endif   /* __IDXFILEFORMAT__H__ */
