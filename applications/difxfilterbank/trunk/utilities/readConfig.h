/***

*/

#ifdef __cplusplus
  extern "C" {
#endif


/* public data structures */
typedef struct {
    int n_streams;
    int n_bands;
    int n_chans;
    int n_threads;
    unsigned char ***flags; // 3D array of flags[stream][band][chan]
} FB_Config;

/* public function prototypes */
char *loadConfigFile(char *filename);
char *findKey(char *config, char *key);
void readConfigMissingKeyErr(char *key);
int readFlagsFile(char *filename, FB_Config *fb_config);

#ifdef __cplusplus
}
#endif

