/*
 * Copyright 2014 Alex Deckmyn
 * 
 * This software is licensed under the GPL licence v3
 */
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include "grib_api.h"
#include "R.h"
#include "Rinternals.h"

#define MAX_KEY_LEN 255
#define MAX_VAL_LEN 1024

/******************************************************************************/
/* This library links to grib_api. It uses the grib_handle structure.         */
/* But there is an added complexity due to the R interface. Some handles      */
/* must remain resident in R for analysis, while other are deleted on the fly.*/
/* So we work with an array GRIBhandleList[MAX_HANDLE] of RgribHandle*        */
/* which are structures with a pointer to the original grib_handle plus some  */
/* extra stuff to keep R happy.                                               */





/* prototypes */
static void Rgrib_handleFinalizer(SEXP);

/***************/
/* bookkeeping */
/***************/
#define MAX_HANDLE 15

static int RgribInitialised=0;

typedef struct {
    int *id ;
    grib_handle *h;
    void *ext_ptr;
  } RgribHandle;

static RgribHandle* GRIBhandleList[MAX_HANDLE];

void Rgrib_init(){
    int i;
    for(i=0;i<MAX_HANDLE;i++) {
      GRIBhandleList[i]=NULL;
    }
    RgribInitialised=1;
}

RgribHandle* Rgrib_create_handle(){
  int j,id;
  if(!RgribInitialised) Rgrib_init();
/* Call garbage collection to free any deleted GRIBhandles */
  R_gc();
  for(j=0;GRIBhandleList[j] && j<MAX_HANDLE; j++){}
  id=j;
  if(j>=MAX_HANDLE) {
    Rprintf("Reached maximum open grib handles: %d\n",MAX_HANDLE);
    return(NULL);
  }
  GRIBhandleList[id]=(RgribHandle*) malloc(sizeof(RgribHandle*));
  GRIBhandleList[id]->id = malloc(sizeof(int));
  *GRIBhandleList[id]->id = id;
  GRIBhandleList[id]->ext_ptr = NULL;
/*  GRIBhandleList[id]->h = h;*/
  return(GRIBhandleList[id]);
}

void Rgrib_GRIBhandle_destroy(int i){
  if(i<0 || i>=MAX_HANDLE) return;
  if(!GRIBhandleList[i]) return;

  if(GRIBhandleList[i]->h) {
    grib_handle_delete(GRIBhandleList[i]->h);
    GRIBhandleList[i]->h=NULL;
  }
  else warning("Inexplicably, a GRIBhandle was found without a valid message pointer\n");
  if(GRIBhandleList[i]->id){
    free(GRIBhandleList[i]->id);
    GRIBhandleList[i]->id=NULL;
  }
  else warning("Inexplicably, a GRIBhandle was found without a valid ID pointer\n");
  if(GRIBhandleList[i]->ext_ptr){
    R_ClearExternalPtr(GRIBhandleList[i]->ext_ptr);
  }
  else warning("Inexplicably, a GRIBhandle was found without a valid EXT pointer\n");

  free(GRIBhandleList[i]);
  GRIBhandleList[i]=NULL;
  return;
}

void Rgrib_clear_all_handles(){
  int i;
  if(!RgribInitialised) return;
  for(i=0;i<MAX_HANDLE;i++) Rgrib_GRIBhandle_destroy(i);
  return;
}

long Rgrib_count_handles_C(){
  int i;
  long result=0;
  if(!RgribInitialised) Rgrib_init();
  for(i=0;i<MAX_HANDLE;i++) if(GRIBhandleList[i]) result++;
  return(result);
}

SEXP Rgrib_count_handles(){
  SEXP result;
  PROTECT(result=allocVector(INTSXP,1));
  INTEGER(result)[0]= Rgrib_count_handles_C();
  UNPROTECT(1);
  return(result);
}

SEXP Rgrib_list_handles(){
  int i,j;
  long cl;
  SEXP result;
  cl= Rgrib_count_handles_C();
  if (cl==0){
    Rprintf("There are no open GRIBhandles.\n");
    return(R_NilValue);
  }

  PROTECT(result=allocVector(INTSXP,cl));
  j=0;
  for(i=0;i<MAX_HANDLE && j<cl ;i++) if(GRIBhandleList[i]) {
    INTEGER(result)[j++]= i;
  }
  UNPROTECT(1);
  return(result);
}

SEXP Rgrib_clear_handle(SEXP gribhandle){
  int* id;
  int iid;

  if(!RgribInitialised) error("GRIBhandleList not initialised!\n");

  if( !(id=R_ExternalPtrAddr(gribhandle)) ) error("This is not an open GRIBhandle.\n");

  iid=*id;

  Rgrib_GRIBhandle_destroy(iid);
  R_ClearExternalPtr(gribhandle); /* should be taken care of by the destroy function */
  return(R_NilValue);
}

static void Rgrib_handleFinalizer(SEXP gribhandle)
{
  int *id;
  int iid;

  id=R_ExternalPtrAddr(gribhandle);
  if(!id) return;
  iid=*id;
  Rgrib_GRIBhandle_destroy(iid);

  R_ClearExternalPtr(gribhandle); /* it may already be clear */
}

/*************************/
/* BASIC FILE OPERATIONS */
/*************************/

void Rgrib_count_messages(char** filename, int* nfields,int* multi)  {
  FILE* infile ;
  int err,nmess ;
  grib_handle *h;

  if( !(infile = fopen(*filename,"r")) ) {
    Rprintf("Could not open file %s\n",*filename);
    *nfields=NA_INTEGER;
    return;
  }

  err=grib_count_in_file(0,infile,&nmess);

  if(*multi){
    grib_multi_support_on(0);
    err=grib_count_in_file(0,infile,&nmess);
    *nfields=0;
    while( (h=grib_handle_new_from_file(0,infile,&err)) != NULL){
      (*nfields)++;
      grib_handle_delete(h);
    }
    Rprintf("MULTI: found %d message(s) and %d field(s).\n",nmess,*nfields);
  }
  else{
    *nfields=nmess;
  }
    
  if(err!=0) {
    Rprintf("An error occured when counting fields in %s\n. Not a GRIB file?\n",*filename);
    *nfields=NA_INTEGER;
  }
  fclose(infile);
  return;;
}

/* Get parameter values for a list of records in a file */
/* This is the main "info" routine */
SEXP Rgrib_parse(SEXP filename,
        SEXP IntPar, SEXP DblPar, SEXP StrPar, SEXP rec, SEXP multi)  {
  FILE* infile ;
  int nmesg,i,j,irec,err,nIntPar,nDblPar,nStrPar,nrec,npar,imulti ;
  grib_handle *h;
  char Str_value[MAX_VAL_LEN];
  size_t vlen=MAX_VAL_LEN;
  double Dbl_value;
  long Long_value;
  SEXP result,parvec;
  char   ***StrAns;
  long   **IntAns;
  double **DblAns;

  nIntPar=length(IntPar);
  nDblPar=length(DblPar);
  nStrPar=length(StrPar);
  npar=nStrPar+nDblPar+nIntPar;
/*  Rprintf("Asking for %d int, %d double, %d char.\n",nIntPar,nDblPar,nStrPar);
*/

  imulti = asLogical(multi);
  if(imulti == NA_LOGICAL) error("'multi' must be TRUE or FALSE");

  if(imulti){
    grib_multi_support_on(0);
    warning("MULTI is still buggy!\n");
  }
  else grib_multi_support_off(0);

  nrec=length(rec) ;
  if( !(infile = fopen(CHAR(STRING_ELT(filename,0)),"r")) ) {
    Rprintf("Could not open file %s\n",CHAR(STRING_ELT(filename,0)));
    return(R_NilValue);
  }
  err=grib_count_in_file(0,infile,&nmesg);
  if(err) {
    Rprintf("Problem opening file %s\n",CHAR(STRING_ELT(filename,0)));
    return(R_NilValue);
  }

/* WITH MULTI, THIS NO LONGER APPLIES:
  if(INTEGER(rec)[nrec-1]>nmesg)  {
    Rprintf("Only %d GRIB messages in file %s !\n",nmesg,CHAR(STRING_ELT(filename,0)));
    return(R_NilValue);
  }
*/
/*  Rprintf("There are %d GRIB messages in file %s !\n",nmesg,CHAR(STRING_ELT(filename,0)));
*/

/*  allocate the R lists, initialise to NA/NULL */
  StrAns=(char***) R_alloc(nStrPar,sizeof(char**));
  for(i=0;i<nStrPar;i++){
    StrAns[i]=(char**) R_alloc(nrec,sizeof(char*));
    for(j=0;j<nrec;j++) StrAns[i][j]=NULL;
  }

  IntAns=(long**) R_alloc(nIntPar,sizeof(long*));
  for(i=0;i<nIntPar;i++){
    IntAns[i]=(long*) R_alloc(nrec,sizeof(long));
    for(j=0;j<nrec;j++) IntAns[i][j]=NA_INTEGER;
  }

  DblAns=(double**) R_alloc(nDblPar,sizeof(double*));
  for(i=0;i<nDblPar;i++){
    DblAns[i]=(double*) R_alloc(nrec,sizeof(double));
    for(j=0;j<nrec;j++) DblAns[i][j]=NA_REAL;
  }

  irec=0;
  for(i=0;irec<nrec && (h = grib_handle_new_from_file(0,infile,&err))!=NULL;i++) {

    if(i==INTEGER(rec)[irec]-1){
      for(j=0;j<nStrPar;j++) {
        vlen=MAX_VAL_LEN;
        err=grib_get_string(h,CHAR(STRING_ELT(StrPar,j)),Str_value,&vlen);
/* some error checking! but GRIB_CHECK exits R itself, so avoid using it */
        if (err) {
/* ==NA_STRING is not a string but type SEXP, so we can not use it here yet! */
          Rprintf("Problem: length is %d\n",vlen);
        }
        else {
          StrAns[j][irec]=R_alloc(vlen,sizeof(char));
          strncpy(StrAns[j][irec],Str_value,vlen);
        }
      }
      for(j=0;j<nIntPar;j++) {
        err=grib_get_long(h,CHAR(STRING_ELT(IntPar,j)),&Long_value);
        if (!err) IntAns[j][irec] = Long_value;
      }
      for(j=0;j<nDblPar;j++) {
        err=grib_get_double(h,CHAR(STRING_ELT(DblPar,j)),&Dbl_value);
        if (!err) DblAns[j][irec]=Dbl_value;
      }

      irec+=1;
    }
    grib_handle_delete(h);/* for memory leakage & MULTI : better to delete every time ! */
  }
/*  if(h!=NULL) grib_handle_delete(h);  */
  fclose(infile);

/* now we create the R objects */

  PROTECT(result=allocVector(VECSXP,npar));
  for(i=0;i<nStrPar;i++){
    PROTECT(parvec=allocVector(STRSXP,nrec));
    for(j=0;j<nrec;j++) {
      if( StrAns[i][j]==NULL /* strcmp(StrAns[i][j],"NA_STRING\0")==0 */ ) SET_STRING_ELT(parvec,j,NA_STRING) ;
      else SET_STRING_ELT(parvec,j,mkChar(StrAns[i][j]));
    }
    SET_VECTOR_ELT(result,i,parvec);
    UNPROTECT(1);
  }
  for(i=0;i<nDblPar;i++){
    PROTECT(parvec=allocVector(REALSXP,nrec));
    for(j=0;j<nrec;j++) REAL(parvec)[j]=DblAns[i][j];
    SET_VECTOR_ELT(result,nStrPar+i,parvec);
    UNPROTECT(1);
  }
  for(i=0;i<nIntPar;i++){
    PROTECT(parvec=allocVector(INTSXP,nrec));
    for(j=0;j<nrec;j++) INTEGER(parvec)[j]=(long)IntAns[i][j];
    SET_VECTOR_ELT(result,nStrPar+nDblPar+i,parvec);
    UNPROTECT(1);
  }

/*  namesgets(result,param);*/
  UNPROTECT(1);
  return(result);
}

/***********************************************/
/* Work with external pointers to grib_handles */
/***********************************************/

SEXP Rgrib_handle_new_file(SEXP filename, SEXP message,SEXP multi){
  FILE* infile ;
  int nmesg,i,err,imulti;
  int *id;
  long int ttt;
  grib_handle *h=NULL,*g=NULL;
  RgribHandle *newhandle;
  SEXP output;

  imulti = asLogical(multi);
  if(imulti == NA_LOGICAL) error("'multi' must be TRUE or FALSE");
  if(imulti) grib_multi_support_on(0);
  else grib_multi_support_off(0);

  if( !(infile = fopen(CHAR(STRING_ELT(filename,0)),"r")) ) {
    Rprintf("Problem opening file %s\n",CHAR(STRING_ELT(filename,0)));
    return(R_NilValue);
  }
  err=grib_count_in_file(0,infile,&nmesg);
  if(err) {
    Rprintf("Problem reading file %s. Is it a GRIB file?\n",CHAR(STRING_ELT(filename,0)));
    fclose(infile);
    return(R_NilValue);
  }

  if(INTEGER(message)[0]>nmesg && !imulti)  {
    Rprintf("Only %d GRIB messages in file %s. Maybe set multi=TRUE?\n",nmesg,CHAR(STRING_ELT(filename,0)));
    fclose(infile);
    return(R_NilValue);
  }

  if(!(newhandle=Rgrib_create_handle()) )  {
    Rprintf("Error creating the GRIBhandle.\n");
    fclose(infile);
    return(R_NilValue);
  }

  i=0;
  while( (h = grib_handle_new_from_file(0,infile,&err))!=NULL && ++i<INTEGER(message)[0] ){
/* should I delete the handle at every iteration to stop memory leakage? */
/* It isn't done in the examples, but I think it is necessary. */

    GRIB_CHECK(grib_get_long(h,"validityTime",&ttt),0);

    grib_handle_delete(h);

  }
/*
  for(i=1;i<INTEGER(message)[0];i++){
    if((h = grib_handle_new_from_file(0,infile,&err))==NULL) break; 
    else grib_handle_delete(h);
  }
*/
  if(h==NULL) {
    Rprintf("Error: reached end of file.\n");
    fclose(infile);
    return(R_NilValue);
  }

  id = newhandle->id;
  newhandle->h = grib_handle_clone(h);
  grib_handle_delete(h);
  newhandle->ext_ptr = R_MakeExternalPtr(id, install("GRIBhandle"), R_NilValue);

  R_RegisterCFinalizerEx(newhandle->ext_ptr, Rgrib_handleFinalizer, TRUE);

  R_RegisterCFinalizerEx(newhandle->ext_ptr, Rgrib_handleFinalizer, TRUE);
  PROTECT(output=allocVector(INTSXP,1));
  INTEGER(output)[0]= (long) *id;
  setAttrib(output, install("filename"), filename);
  setAttrib(output,install("message"),message);
  setAttrib(output, install("gribhandle_ptr"), newhandle->ext_ptr);
  UNPROTECT(1);
  fclose(infile);
  return(output);
}

SEXP Rgrib_handle_new_sample(SEXP sample){
  grib_handle *h;
  int* id;
  SEXP output;
  RgribHandle *newhandle;

  if(!(newhandle=Rgrib_create_handle()) )  {
    Rprintf("Error creating the GRIBhandle.\n");
    return(R_NilValue);
  }

  h = grib_handle_new_from_samples(NULL,CHAR(STRING_ELT(sample,0)));
  id = newhandle->id;
  newhandle->h = h;

  newhandle->ext_ptr = R_MakeExternalPtr(id, install("GRIBhandle"), R_NilValue);
  R_RegisterCFinalizerEx(newhandle->ext_ptr, Rgrib_handleFinalizer, TRUE);
  PROTECT(output=allocVector(INTSXP,1));
  INTEGER(output)[0]=(long) *id;
  setAttrib(output,install("sample"),sample);
  setAttrib(output, install("gribhandle_ptr"), newhandle->ext_ptr);
  UNPROTECT(1);
  return(output);
}

/***************************/
/* DECODING HANDLES & INFO */
/***************************/

SEXP Rgrib_handle_info(SEXP gribhandle,SEXP StrPar, SEXP IntPar, SEXP DblPar){
  grib_handle *h;
  int i,irec,err,nIntPar,nDblPar,nStrPar,npar ;
  char   **StrAns;
  size_t vlen=MAX_VAL_LEN;
  char Str_value[MAX_VAL_LEN];
  double *DblAns;
  long *IntAns;
  SEXP result,parvec;
  int *id;


  id=R_ExternalPtrAddr(gribhandle);
  if(!id) error("Not a valid GRIBhandle.\n");
  h=GRIBhandleList[*id]->h;
  if(!h) error("Not a registered GRIBhandle.\n");

  nIntPar=length(IntPar);
  nDblPar=length(DblPar);
  nStrPar=length(StrPar);
  npar=nStrPar+nDblPar+nIntPar;

  StrAns=(char**) R_alloc(nStrPar,sizeof(char*));
  IntAns=(long*) R_alloc(nIntPar,sizeof(long));
  DblAns=(double*) R_alloc(nDblPar,sizeof(double));

  for(i=0;i<nStrPar;i++) {
    vlen=MAX_VAL_LEN;
    err=grib_get_string(h,CHAR(STRING_ELT(StrPar,i)),Str_value,&vlen);
/* some error checking! but GRIB_CHECK exits R itself */
    if (err) {
      StrAns[irec]=R_alloc(10,sizeof(char));
      strncpy(StrAns[irec],"NA_STRING\0",10);
    }
    else {
      StrAns[i]=R_alloc(vlen,sizeof(char));
      strncpy(StrAns[i],Str_value,vlen);
    }
  }

  for(i=0;i<nIntPar;i++) {
     err=grib_get_long(h,CHAR(STRING_ELT(IntPar,i)),IntAns+i);
     if (err) {
       IntAns[i] = NA_INTEGER;
     }
   }

  for(i=0;i<nDblPar;i++) {
    err=grib_get_double(h,CHAR(STRING_ELT(DblPar,i)),DblAns+i);
    if (err) {
       DblAns[i] = NA_REAL;
    }
  }

/* create the result object */

  PROTECT(result=allocVector(VECSXP,npar));
  for(i=0;i<nStrPar;i++){
    PROTECT(parvec=allocVector(STRSXP,1));
    if( strcmp(StrAns[i],"NA_STRING\0")==0) SET_STRING_ELT(parvec,0,NA_STRING) ;
    else SET_STRING_ELT(parvec,0,mkChar(StrAns[i]));
    SET_VECTOR_ELT(result,i,parvec);
    UNPROTECT(1);
  }
  for(i=0;i<nDblPar;i++){
    PROTECT(parvec=allocVector(REALSXP,1));
    REAL(parvec)[0]=DblAns[i];
    SET_VECTOR_ELT(result,nStrPar+i,parvec);
    UNPROTECT(1);
  }
  for(i=0;i<nIntPar;i++){
    PROTECT(parvec=allocVector(INTSXP,1));
    INTEGER(parvec)[0]=IntAns[i];
    SET_VECTOR_ELT(result,nStrPar+nDblPar+i,parvec);
    UNPROTECT(1);
  }
  UNPROTECT(1);
  return(result);

}

SEXP Rgrib_handle_decode(SEXP gribhandle)
{
  size_t dlen;
  grib_handle *h;
  double *dres;
/*  long nx,ny ;
*/
  SEXP result;
  int *id;

  id=R_ExternalPtrAddr(gribhandle);
  if(!id) error("Not a valid GRIBhandle.\n");
  h=GRIBhandleList[*id]->h;
  if(!h) error("Not a registered GRIBhandle.\n");

/* Nx=-1 for gaussian. numberOfCodedValues or numberOfDataPoints may be better? */
  grib_get_long(h,"numberOfValues",&dlen);     
  PROTECT(result=allocVector(REALSXP,dlen));
  dres=REAL(result);
  grib_get_double_array(h,"values",dres,&dlen);

  UNPROTECT(1);
  return(result);
}

/************/
/* ENCODING */
/************/

SEXP Rgrib_handle_mod(SEXP gribhandle,SEXP StrPar, SEXP IntPar, SEXP DblPar){
  int i,nIntPar,nDblPar,nStrPar,err ;
  grib_handle *h;
  size_t vlen;
  int *id;

  id=R_ExternalPtrAddr(gribhandle);
  if(!id) error("Not a valid GRIBhandle.\n");
  h=GRIBhandleList[*id]->h;
  if(!h) error("Not a registered GRIBhandle.\n");

  nIntPar=length(IntPar);
  nDblPar=length(DblPar);
  nStrPar=length(StrPar);

  for(i=0;i<nStrPar;i++) {
     vlen=MAX_VAL_LEN;
     err=grib_set_string(h,CHAR(STRING_ELT(getAttrib(StrPar, R_NamesSymbol),i)),
                           CHAR(STRING_ELT(VECTOR_ELT(StrPar,i),0)),&vlen);
   }

   for(i=0;i<nIntPar;i++) {
     err=grib_set_long(h,CHAR(STRING_ELT(getAttrib(IntPar, R_NamesSymbol),i)),
                         INTEGER(VECTOR_ELT(IntPar,i))[0]);
   }

   for(i=0;i<nDblPar;i++) {
     err=grib_set_double(h,CHAR(STRING_ELT(getAttrib(DblPar, R_NamesSymbol),i)),
                           REAL(VECTOR_ELT(DblPar,i))[0]);
   }

  return(R_NilValue);
}

SEXP Rgrib_handle_enc(SEXP gribhandle,SEXP fieldvalues){
  double * values;
  size_t values_len;
  grib_handle *h;
  int *id;

  id=R_ExternalPtrAddr(gribhandle);
  if(!id) error("Not a valid GRIBhandle.\n");
  h=GRIBhandleList[*id]->h;
  if(!h) error("Not a registered GRIBhandle.\n");

  grib_get_size(h,"values",&values_len);
  if(values_len != length(fieldvalues)) {
    Rprintf("The number of values in the array doesn\'t fit: %d vs %d \n",(int)length(fieldvalues),(int)values_len);
    return(R_NilValue);
  }
  values=REAL(fieldvalues);

  grib_set_double_array(h,"values",values,values_len);
  return(R_NilValue);
}

SEXP Rgrib_handle_write(SEXP gribhandle,SEXP filename,SEXP filemode){
  grib_handle *h;
  FILE* outfile ;
  const void* buffer = NULL;
  size_t size;
  int *id;

  id=R_ExternalPtrAddr(gribhandle);
  if(!id) error("Not a valid GRIBhandle.\n");
  h=GRIBhandleList[*id]->h;
  if(!h) error("Not a registered GRIBhandle.\n");

  if( !(outfile = fopen(CHAR(STRING_ELT(filename,0)),CHAR(STRING_ELT(filemode,0)))) ) {
    Rprintf("Problem opening file %s\n",CHAR(STRING_ELT(filename,0)));
    return(R_NilValue);
  }
  grib_get_message(h,&buffer,&size);
  if(fwrite(buffer,1,size,outfile) != size)
  {
    Rprintf("oops\n");
  }
  fclose(outfile);
  return(R_NilValue);
}


/*### GRIB PARSER: all keys*/
SEXP Rgrib_handle_parse(SEXP gribhandle)
{
/* To skip read only and not coded keys
unsigned long key_iterator_filter_flags=GRIB_KEYS_ITERATOR_SKIP_READ_ONLY ||
GRIB_KEYS_ITERATOR_SKIP_COMPUTED;
*/
  grib_handle *h;
  unsigned long key_iterator_filter_flags=GRIB_KEYS_ITERATOR_ALL_KEYS;
/* valid name_spaces are ls and mars */
  char* name_space=NULL;
/* name_space=NULL to get all the keys */
/* char* name_space=0; */
  grib_keys_iterator* kiter=NULL;
  char value[MAX_VAL_LEN];
  size_t vlen=MAX_VAL_LEN;
  int *id;

  id=R_ExternalPtrAddr(gribhandle);
  if(!id) error("Not a valid GRIBhandle.\n");
  h=GRIBhandleList[*id]->h;
  if(!h) error("Not a registered GRIBhandle.\n");

  kiter=grib_keys_iterator_new(h,key_iterator_filter_flags,name_space);
  if (!kiter) {
    Rprintf("ERROR: Unable to create keys iterator\n");
    return(R_NilValue) ;
  }
  while(grib_keys_iterator_next(kiter))
  {
    const char* name = grib_keys_iterator_get_name(kiter);
    vlen=MAX_VAL_LEN;
    GRIB_CHECK(grib_get_string(h,name,value,&vlen),name);
    Rprintf("%s = %s %d\n",name,value,vlen);
  }
  grib_keys_iterator_delete(kiter);

  return(R_NilValue) ;
}

