/* Counters for the occurrence of each type of error */
struct type_filter
        {
        int     absent;
        int     emasked;
        int     cf_service;
        int     ap_mid_miss;
        int     tap_overrun;
        int     triple_carries;
        int     suid[2];
        int     chid[2];
        int     chksum[2];
        int     cfnum[2];
        int     badd[2];
        int     zero[2];
        int     link[2];
        int     ib_sync[2];
        int     ndiscard;
        int     xperror;
        int     yperror;
        };

