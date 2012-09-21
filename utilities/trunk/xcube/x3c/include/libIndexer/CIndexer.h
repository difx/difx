/**
 * @file CIndexer.h
 *
 * @author bchaco
 *
 * Indexer class definition 
 */
#ifndef __CINDEXER_H__
#define __CINDEXER_H__

#include <map>
#include <cassert>
#include <iostream>
#include <cstring>

#include "xcTypes.h"

/*
** Debugging macros
*/

/* Define as 1 to enable debug facilities */
#define DEBUG_ENABLED           0
#define DEBUG_PRINT_ENABLED     0

#if !defined(NDEBUG) && DEBUG_CHECK_ENABLED
#define DEBUG_CHECK(cond)                           \
    assert(cond)

#else
#define DEBUG_CHECK(cond)
#endif

#if !defined(NDEBUG) && DEBUG_PRINT_ENABLED
#define DEBUG_PRINT(str)                            \
    std::cerr << "!!!!ERROR encountered at "        \
    << __FILE__ << ":" << __LINE__                  \
    << "  :  " << (str) << std::endl;
#else
#define DEBUG_PRINT(str)
#endif

namespace x3c {
    namespace indexer {

        const UINT32 THIRTYTWO_MB = 32*1024*1024;

/**
 * Sanity Checking macros
 */
#if !defined(NDEBUG) && DEBUG_ENABLED
#define CHECK_HEADER_LENGTH                        \
        do {                                       \
            assert(0x2000 == sizeof(IndexHeader)); \
        } while (0);

#define CHECK_TRAILER_LENGTH                       \
        do {                                       \
            assert(0x2000 == sizeof(IndexHeader)); \
        } while (0);
#else
#define CHECK_HEADER_LENGTH
#define CHECK_TRAILER_LENGTH
#endif   /* NDEBUG */

        /**
         * @class CIndexer
         *
         * This class provides functionality for writing and reading
         * index files. Index files are used for fast searches over
         * stream data that has been written to disk.
         */
        template<class IndexPolicy>
        class CIndexer
        {
        public:

            //==================================================================
            // typedefs
            //
            typedef std::map<std::string, IndexPolicy*> IndexerMap;
            typedef typename std::map<std::string, IndexPolicy*>::iterator IndexerIter;
            typedef typename std::map<std::string, IndexPolicy*>::const_iterator ConstIndexerIter;

            /**
             * Constructor
             */
            explicit CIndexer(UINT32 numFiles,
                              UINT32 reservedMem = THIRTYTWO_MB);

            /**
             * Destructor. Closes index files and free's the indexers
             */
            ~CIndexer();

            /**
             * Gets an indexer for an index file. Ownership of the retured
             * index is not transfered to the caller.
             *
             * @param filename the full path to the index file to be
             * opened for reading.
             * @return A ponter to the IndexPolicy for the file 
             */
            IndexPolicy* getIndexer(std::string const& filename);

            /**
             * returns the filename and offset for the first index
             * record that is greater than the timestamp 
             *
             * @param ts the timestamp to search for
             * @param offset the address of the buffer to store the
             * file offset into
             *
             * @returns On success a string containing the file name of the file
             * containing the index found. If the timestamp is not found then
             * an empty string is returned and offset will contain -1 
             */
            std::string getOffsetFromTime(UINT64 ts, UINT64 *offset) const;

            /**
             * returns the filename and offset for the first index
             * record that is greater than the timestamp 
             *
             * @param ts the struct timespec timestamp to search for
             * @param offset the address of the buffer to store the
             * file offset into
             *
             * @returns On success a string containing the file name of the file
             * containing the index found. If the timestamp is not found then
             * an empty string is returned and offset will contain -1 
             */
            std::string getOffsetFromTime(struct timespec ts, UINT64 *offset) const;

            /**
             * Closes the index files. Note does not free the
             * indexers.
             */
            void closeIndexFiles();

            /**
             * Returns the number of files the Indexer is managing.
             */
            UINT32 numFiles() const;

            /**
             * Sets the number of files the Indexer is managing.
             */
            void numFiles(UINT32 nFiles);

            /**
             * Set the offset from GMT.
             *
             * @param gmt_offset The offset in seconds (+/-) from GMT 
             */
            void timeZone(INT32 gmt_offset);

            /**
             * Return the offset from GMT for this indexer
             */
            INT32 timeZone() const;

        private:
            /**
             * Assignment operator - not supported
             */
            CIndexer& operator=(CIndexer<IndexPolicy> const&);

            /**
             * Copy assignment - not supported
             */
            CIndexer(CIndexer<IndexPolicy> const&);

            /** The number of files this Indexer is managing */
            UINT32 mNumFiles;

            /** The amount of memory to reserve up front for the
             * index data. Reserving memory helps avoid reallocs
             * as the index data grows */
            UINT32 mReservedMem;

            /** Offset from GMT in seconds */
            INT32  mGmtOffset;

            IndexerMap indexers;
        };

        /*
        ** Inline member function implementations
        */

        template<class IndexPolicy> inline
        void  CIndexer<IndexPolicy>::timeZone(INT32 gmtOffset)
        {
            /** TODO: sanity check gmt_offset? */
            mGmtOffset = gmtOffset;
        }

        template<class IndexPolicy> inline
        INT32 CIndexer<IndexPolicy>::timeZone() const
        {
            return mGmtOffset;
        }
 
        template<class IndexPolicy> inline
        UINT32 CIndexer<IndexPolicy>::numFiles() const
        {
            return mNumFiles;
        }
        
        template<class IndexPolicy> inline
        void CIndexer<IndexPolicy>::numFiles(UINT32 nFiles)
        {
            mNumFiles = nFiles;
        }



        //======================================================================
        // CIndexer class implementation
        //
        template<class IndexPolicy>
        CIndexer<IndexPolicy>::CIndexer(UINT32 numFiles,
                                        UINT32 reserve /* = THIRTYTWO_MB*/)
            : mNumFiles(numFiles), mReservedMem(reserve), mGmtOffset(0),
            indexers()
        {
            CHECK_HEADER_LENGTH;
            CHECK_TRAILER_LENGTH;
        }

        template<class IndexPolicy>
        CIndexer<IndexPolicy>::~CIndexer()
        {
            IndexerIter iter = indexers.begin();
            IndexerIter last = indexers.end();
            while ( iter != last ) {
                IndexPolicy* p = (*iter).second;
                delete p;
                ++iter;
            }

            indexers.clear();
        }

        template<class IndexPolicy>
        IndexPolicy* CIndexer<IndexPolicy>::getIndexer(std::string const& filename)
        {
            IndexPolicy *pIndexer = 0;
            IndexerIter i;

            if ( indexers.end() != (i = indexers.find(filename)) ) {
                pIndexer = i->second;
            } else {
                pIndexer = new IndexPolicy(*this, mReservedMem);
                if (0 != pIndexer) {
                    if (pIndexer->create(filename)) {
                        /* Store a pointer to the reader */
                        indexers[filename] = pIndexer;
                    } else {
                        delete pIndexer;
                        pIndexer = NULL;
                        
                        DEBUG_PRINT("CIndexer::getIndexer: Failed to "
                                    "initialize Indexer");
                    }
                } else {
                    DEBUG_PRINT("CIndexer::getIndexer: Out of Memory");
                }
            }

            DEBUG_CHECK(0 != pIndexer);

            return pIndexer;
        }

        template<class IndexPolicy>
        void CIndexer<IndexPolicy>::closeIndexFiles()
        {
            IndexerIter iter = indexers.begin();
            IndexerIter last = indexers.end();
            while ( iter != last ) {
                IndexPolicy* p = (*iter).second;
                p->close();
                ++iter;
            }
        }

        template<class IndexPolicy> std::string
        CIndexer<IndexPolicy>::getOffsetFromTime(UINT64 ts,
                                                 UINT64 *offset) const
        {
            UINT64 filePos;
            std::string fname;
            fname.clear();

            assert(NULL != offset);

            *offset = -1;

            ConstIndexerIter iter = indexers.begin();
            ConstIndexerIter last = indexers.end();
            while ( iter != last ) {
                IndexPolicy* p = (*iter).second;

                if (p->mHeader.start_time < ts && p->mHeader.end_time > ts) {
//                    if ( -1 != (filePos = p->getFilePosByTime(ts)) )
                    int nRetVal = p->getFilePosByTime(ts, filePos);
                    if (0 == nRetVal)
                    {
                        *offset = filePos;
                        fname = iter->first;
                        break;
                    }
                }
                ++iter;
            }

            return fname;
        }

        template<class IndexPolicy>
        std::string CIndexer<IndexPolicy>::getOffsetFromTime(struct timespec ts,
                                                             UINT64 *offset) const
        {
            const UINT64 timestamp(ts.tv_sec << 32 | ts.tv_nsec);
            return getOffsetFromTime((UINT64)timestamp, offset);
        }

    }  /* namespace indexer */
}    /* namespace x3c */

#endif  /* __CINDEXER_H__ */
