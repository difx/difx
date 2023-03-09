#ifndef __MODEL_H__
#define __MODEL_H__

int getModelIndex(const DifxPolyModel *im, int imSize, int mjd, double sec);

/* return value in us */
double getDelay(const DifxPolyModel *im, int imSize, int mjd, double sec, double tInt);

/* return value in us/s */
double getRate(const DifxPolyModel *im, int imSize, int mjd, double sec);

#endif
