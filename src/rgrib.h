#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include "grib_api.h"
#include "R.h"
#include "Rinternals.h"

#define MAX_KEY_LEN 255
#define MAX_VAL_LEN 1024
#define MAX_FILE_NAME 1024
#define MAX_HANDLE 15
#define MAX_INDEX 15

/******************************************************************************/
/* This library links to grib_api (eccodes) and uses grib_handle structures.  */
/* But there is an added complexity due to the R interface. Some handles      */
/* must remain resident in R for analysis, while other are deleted on the fly.*/
/* So we work with an array GRIBhandleList[MAX_HANDLE] of RgribHandle*        */
/* which are structures with a pointer to the original grib_handle plus some  */
/* extra stuff to keep R happy.                                               */
/******************************************************************************/

typedef struct {
    int *id ;
    grib_index *h;
    SEXP ext_ptr;
} RgribIndex;

typedef struct {
    int *id ;
    grib_handle *h;
    void *ext_ptr;
} RgribHandle;

// extern RgribHandle* GRIBhandleList[MAX_HANDLE];

/* prototypes */

/***************/
/* bookkeeping */
/***************/
void Rgrib_init_check();

RgribHandle* Rgrib_create_handle();
void Rgrib_GRIBhandle_destroy(int i);
void Rgrib_clear_all_handles();
long Rgrib_count_handles_C();
SEXP Rgrib_count_handles();
SEXP Rgrib_list_handles();
SEXP Rgrib_clear_handle(SEXP gribhandle);
void Rgrib_handleFinalizer(SEXP gribhandle);

RgribIndex* Rgrib_create_index();
void Rgrib_GRIBindex_destroy(int i);
void Rgrib_clear_all_indices();
long Rgrib_count_indices_C();
SEXP Rgrib_count_indices();
SEXP Rgrib_list_indices();
SEXP Rgrib_clear_index(SEXP gribindex);
void Rgrib_indexFinalizer(SEXP gribindex);

/*************************/
/* BASIC FILE OPERATIONS */
/*************************/

void Rgrib_count_messages(char** filename, int* nfields,int* multi);
SEXP Rgrib_parse_file(SEXP filename,
        SEXP IntPar, SEXP DblPar, SEXP StrPar, SEXP rec, SEXP multi);
SEXP Rgrib_index_file(SEXP filename, SEXP nmsg) ;



/***************************/
/*  creating GRIBindex's   */
/***************************/

SEXP Rgrib_index_new_file(SEXP filename, SEXP keylist, SEXP multi); 


/**************************/
/*  creating GRIBhandles  */
/**************************/

SEXP Rgrib_handle_new_file(SEXP filename, SEXP message,SEXP multi);
SEXP Rgrib_handle_new_file2(SEXP filename, SEXP loc, SEXP message,SEXP multi);
SEXP Rgrib_handle_new_sample(SEXP sample);
SEXP Rgrib_handle_new_msg(SEXP msg, SEXP msglen);

/***************************/
/* DECODING HANDLES & INFO */
/***************************/

SEXP Rgrib_handle_parse_all(SEXP gribhandle);
SEXP Rgrib_handle_info(SEXP gribhandle,SEXP StrPar, SEXP IntPar, SEXP DblPar);
SEXP Rgrib_handle_decode(SEXP gribhandle);

/************/
/* ENCODING */
/************/

SEXP Rgrib_handle_mod(SEXP gribhandle,SEXP StrPar, SEXP IntPar, SEXP DblPar);
SEXP Rgrib_handle_enc(SEXP gribhandle,SEXP fieldvalues);
SEXP Rgrib_handle_write(SEXP gribhandle,SEXP filename,SEXP filemode);


