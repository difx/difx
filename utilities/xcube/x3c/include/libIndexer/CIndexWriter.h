/**
 * @file CIndexFileWriter.h
 *
 * @author bchaco
 *
 * CIndexWriter class definition 
 */
#ifndef __CINDEXWRITER_H__
#define __CINDEXWRITER_H__

#include <vector>
#include <cassert>
#include "IdxFileFormat.h"
#include "CIndexer.h"

namespace x3c {
    namespace indexer {

        class CIndexWriter
        {
        public:

            /**
             * Destructor
             */
            ~CIndexWriter();

            /**
             * Closes the index file that was opened for either reading or writing.
             */
            void close();

            /**
             * Writes a new index to the index file.
             *
             * @param timestamp The timestamp for the index 
             *       (seconds << 32 + nanoseconds) 
             * @param The file offset for this index from the beggining of the
             *        corresponding stream data file
             */
            void  addIndexRecord(UINT64 timestamp, UINT64 offset);

            /**
             * Writes a new index to the index file.
             *
             * @param record The IndexRecord to add
             */
            void  addIndexRecord(IndexRecord const& record);

            /**
             * Returns the number of records written
             */
            UINT32 numRecords() const;

            /**
             * Write the index file to persistent storage.
             *
             * @returns <c>true</c> if successful, false otherwize.
             */
            bool  flushToDisk();

            /**
             * Returns the filename the writer is managing
             */
            std::string const& filename() const;

       private:

            /**
             * Assignment operator - not supported
             */
            CIndexWriter& operator=(CIndexWriter const&);

            /**
             * Copy assignment - not supported
             */
            CIndexWriter(CIndexWriter const&);

            /**
             * Constructor
             *
             * Construction supported only from CIndexer class
             */
            CIndexWriter(CIndexer<CIndexWriter> const& parent,
            			 UINT32 reservedMem);

            /**
             * Construct a CIndexWriter
             */
            bool create(std::string const& filename);

            /**
             * write the index header to disk
             */
            bool writeHeader();

            /**
             * write the index records to disk
             */
            bool writeIndexRecords();

            /**
             * write the index trailer to disk
             */
            bool writeTrailer();

            /** Full path to the index file on disk */
            std::string         mFilename;

            /** The index file header */
            IndexHeader         mHeader;

            /** Container of index records */
            std::vector<IndexRecord>  mIndexRecords;
            
            /** The index file trailer */
            IndexTrailer        mTrailer;

            /** The "parent" CIndexer instance */
            CIndexer<CIndexWriter> const& mParent;

            /** File descriptor for an open Index file */
            mutable INT32       mFd;

            /** The total number of index records */
            unsigned long long  mNumIndexRecords;

            /** The oldest timestamp found */
            UINT64 mFirstTimestamp;

            /** The youngest timestamp found */
            UINT64 mLastTimestamp;

            /** Allow construction and destruction only 
                from CIndexer<CIndexWriter> */
            friend class CIndexer<CIndexWriter>;

            /** The maximum number of in-memory index records 
             *  before a the records will be flushed to disk */
            static const unsigned int DefaultFlushCount = 100; 
        };

        //======================================================================
        // Inline members
        //

        inline std::string const& CIndexWriter::filename() const
        {
            return mFilename;
        }

        inline UINT32 CIndexWriter::numRecords() const
        {
            return mNumIndexRecords;
        }

    }   /* namespace indexer */
}  /* namespace x3c */

#endif /*  __CINDEXWRITER_H__ */
