#include "adata.h"

#define SAVE 1
#define RESTORE 2
#define RESTORE_NOFLAG 3

typedef struct {
	int                     order;          /* Sort order index */
        int                     lastorder;      /* Temp storage (sort stabilization) */
        int                     keyval;         /* Temporary value to sort on */
        int                     flag;           /* Is this scan edited out? Why? */
	rootsum			data;		/* Contains actual data */
	} rootarray;

typedef struct {
	int                     order;          /* Sort order index */
        int                     lastorder;      /* Temp storage (sort stabilization) */
        int                     keyval;         /* Temporary value to sort on */
        int                     flag;           /* Is this scan edited out? Why? */
	int			parent_root;	/* Index of parent root file if present */
	corelsum		data;		/* Contains actual data */
	} corelarray;

typedef struct {
	int                     order;          /* Sort order index */
        int                     lastorder;      /* Temp storage (sort stabilization) */
        int                     keyval;         /* Temporary value to sort on */
        int                     flag;           /* Is this scan edited out? Why? */
	int			parent_root;	/* Index of parent root file if present */
	int			parent_corel;	/* Index of parent corel file if present */
        int                     param_ptr;      /* Index into user_param array */
	fringesum		data;		/* Contains actual data */
	} fringearray;

typedef struct {
	int                     order;          /* Sort order index */
        int                     lastorder;      /* Temp storage (sort stabilization) */
        int                     keyval;         /* Temporary value to sort on */
        int                     flag;           /* Is this triangle edited out? Why? */
	int			index[3];	/* Indices of parent fringe lines */
	short			reversed;	/* Low-order 3 bits flag reversals */
	trianglesum		data;		/* Contains actual data */
	} trianglearray;

typedef struct {
	int                     order;          /* Sort order index */
        int                     lastorder;      /* Temp storage (sort stabilization) */
        int                     keyval;         /* Temporary value to sort on */
        int                     flag;           /* Is this quad edited out? Why? */
	int			index[6];	/* Indices of parent fringe lines */
	quadsum			data;		/* Contains actual data */
	} quadarray;

typedef struct {
        rootarray               *rdata;
        corelarray              *cdata;
        fringearray             *fdata;
	trianglearray		*tdata;
	quadarray		*qdata;
} esum;
