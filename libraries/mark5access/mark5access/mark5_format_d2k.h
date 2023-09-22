
 int findfirstframe(const unsigned char *data, int bytes, unsigned int syncword) ;

 int mark5_stream_frame_num_mark5b(const struct mark5_stream *ms);
 int mark5_stream_frame_time_mark5b(const struct mark5_stream *ms, int *mjd, int *sec, double *ns);
 void mark5_format_mark5b_genheaders(const struct mark5_stream *ms, int n, unsigned char *where);
 int mark5_format_mark5b_fixmjd(struct mark5_stream *ms, int refmjd);

/************************* decode routines **************************/

 int mark5b_decode_1bitstream_1bit_decimation1(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_1bitstream_1bit_decimation2(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_1bitstream_1bit_decimation4(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_1bitstream_1bit_decimation8(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_2bitstream_1bit_decimation1(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_2bitstream_1bit_decimation2(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_2bitstream_1bit_decimation4(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_4bitstream_1bit_decimation1(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_4bitstream_1bit_decimation2(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_4bitstream_1bit_decimation4(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_8bitstream_1bit_decimation1(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_8bitstream_1bit_decimation2(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_8bitstream_1bit_decimation4(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_16bitstream_1bit_decimation1(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_16bitstream_1bit_decimation2(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_16bitstream_1bit_decimation4(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_32bitstream_1bit_decimation1(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_32bitstream_1bit_decimation2(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_32bitstream_1bit_decimation4(struct mark5_stream *ms, int nsamp, float **data);

/************************ 2-bit decoders *********************/

 int mark5b_decode_2bitstream_2bit_decimation1(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_2bitstream_2bit_decimation2(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_2bitstream_2bit_decimation4(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_4bitstream_2bit_decimation1(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_4bitstream_2bit_decimation2(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_4bitstream_2bit_decimation4(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_8bitstream_2bit_decimation1(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_8bitstream_2bit_decimation2(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_8bitstream_2bit_decimation4(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_16bitstream_2bit_decimation1(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_16bitstream_2bit_decimation2(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_16bitstream_2bit_decimation4(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_32bitstream_2bit_decimation1(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_32bitstream_2bit_decimation2(struct mark5_stream *ms, int nsamp, float **data);
 int mark5b_decode_32bitstream_2bit_decimation4(struct mark5_stream *ms, int nsamp, float **data);

/************************ 2-bit counters *********************/

 int mark5b_count_2bitstream_2bit_decimation1(struct mark5_stream *ms, int nsamp, unsigned int *highstates);
 int mark5b_count_2bitstream_2bit_decimation2(struct mark5_stream *ms, int nsamp, unsigned int *highstates);
 int mark5b_count_2bitstream_2bit_decimation4(struct mark5_stream *ms, int nsamp, unsigned int *highstates);
 int mark5b_count_4bitstream_2bit_decimation1(struct mark5_stream *ms, int nsamp, unsigned int *highstates);
 int mark5b_count_4bitstream_2bit_decimation2(struct mark5_stream *ms, int nsamp, unsigned int *highstates);
 int mark5b_count_4bitstream_2bit_decimation4(struct mark5_stream *ms, int nsamp, unsigned int *highstates);
 int mark5b_count_8bitstream_2bit_decimation1(struct mark5_stream *ms, int nsamp, unsigned int *highstates);
 int mark5b_count_8bitstream_2bit_decimation2(struct mark5_stream *ms, int nsamp, unsigned int *highstates);
 int mark5b_count_8bitstream_2bit_decimation4(struct mark5_stream *ms, int nsamp, unsigned int *highstates);
 int mark5b_count_16bitstream_2bit_decimation1(struct mark5_stream *ms, int nsamp, unsigned int *highstates);
 int mark5b_count_16bitstream_2bit_decimation2(struct mark5_stream *ms, int nsamp, unsigned int *highstates);
 int mark5b_count_16bitstream_2bit_decimation4(struct mark5_stream *ms, int nsamp, unsigned int *highstates);
 int mark5b_count_32bitstream_2bit_decimation1(struct mark5_stream *ms, int nsamp, unsigned int *highstates);
 int mark5b_count_32bitstream_2bit_decimation2(struct mark5_stream *ms, int nsamp, unsigned int *highstates);
 int mark5b_count_32bitstream_2bit_decimation4(struct mark5_stream *ms, int nsamp, unsigned int *highstates);

/******************************************************************/

 int mark5_format_mark5b_final(struct mark5_stream *ms);

 int one(const struct mark5_stream *ms);
 int onenc(struct mark5_stream *ms);
