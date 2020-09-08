#include "rgrib.h"

extern RgribHandle* GRIBhandleList[MAX_HANDLE];
RgribIndex* GRIBindexList[MAX_INDEX];

RgribIndex* Rgrib_create_index(){
  int j,id;
  Rgrib_init_check();
/* Call garbage collection to free any deleted GRIBindex */
  R_gc();
  for (j=0;GRIBindexList[j] && j<MAX_INDEX; j++) {}
  id=j;
  if (j>=MAX_INDEX) {
    Rprintf("Reached maximum open grib indices: %d\n",MAX_INDEX);
    return(NULL);
  }
  GRIBindexList[id]=(RgribIndex*) malloc(sizeof(RgribIndex));
  GRIBindexList[id]->id = malloc(sizeof(int));
  *GRIBindexList[id]->id = id;
  GRIBindexList[id]->ext_ptr = NULL;
/*  GRIBindexList[id]->h = h;*/
  return(GRIBindexList[id]);
}

void Rgrib_GRIBindex_destroy(int i){
  Rgrib_init_check();
  if (i<0 || i>=MAX_INDEX) return;
  if (!GRIBindexList[i]) return;

  if (GRIBindexList[i]->h) {
    grib_index_delete(GRIBindexList[i]->h);
    GRIBindexList[i]->h=NULL;
  }
  else warning("Inexplicably, a GRIBindex was found without a valid message pointer\n");

  if (GRIBindexList[i]->id) {
    free(GRIBindexList[i]->id);
    GRIBindexList[i]->id=NULL;
  }
  else warning("Inexplicably, a GRIBindex was found without a valid ID pointer\n");

  if (GRIBindexList[i]->ext_ptr) {
    R_ClearExternalPtr(GRIBindexList[i]->ext_ptr);
  }
  else warning("Inexplicably, a GRIBindex was found without a valid EXT pointer\n");

  free(GRIBindexList[i]);
  GRIBindexList[i]=NULL;
  return;
}

void Rgrib_clear_all_indices(){
  int i;
  for (i=0;i<MAX_INDEX;i++) Rgrib_GRIBindex_destroy(i);
  return;
}

long Rgrib_count_indices_C(){
  int i;
  long result=0;
  Rgrib_init_check();
  for (i=0;i<MAX_INDEX;i++) if (GRIBindexList[i]) result++;
  return(result);
}

SEXP Rgrib_count_indices(){
  SEXP result;
  PROTECT(result=allocVector(INTSXP,1));
  INTEGER(result)[0] = Rgrib_count_indices_C();
  UNPROTECT(1);
  return(result);
}

SEXP Rgrib_list_indices(){
  int i, j;
  long cl;
  SEXP result;
  cl= Rgrib_count_indices_C();
  if (cl==0) {
    Rprintf("There are no open GRIBindices.\n");
    return(R_NilValue);
  }

  PROTECT(result=allocVector(INTSXP,cl));
  j=0;
  for (i=0;i<MAX_INDEX && j<cl ;i++) if(GRIBindexList[i]) {
    INTEGER(result)[j++]= i;
  }
  UNPROTECT(1);
  return(result);
}

SEXP Rgrib_clear_index(SEXP gribindex){
  int* id;
  int iid;

  if (!(id=R_ExternalPtrAddr(gribindex))) error("This is not an open GRIBindex.\n");

  iid=*id;

  Rgrib_GRIBindex_destroy(iid);
  R_ClearExternalPtr(gribindex); /* should be taken care of by the destroy function */
  return(R_NilValue);
}

void Rgrib_indexFinalizer(SEXP gribindex)
{
  int *id;
  int iid;

  id=R_ExternalPtrAddr(gribindex);
  if(!id) return;
  iid=*id;
  Rgrib_GRIBindex_destroy(iid);

  R_ClearExternalPtr(gribindex); /* it may already be clear */
}

SEXP Rgrib_index_from_file(SEXP filename, SEXP keylist, SEXP multi) {

  FILE* infile ;
  grib_index *gi=NULL;
  RgribIndex *newindex;
  int err, *id, imulti;
  SEXP output;
  char str_val[MAX_FILE_NAME];

//  Rprintf("%s\n%s\n", CHAR(STRING_ELT(filename,0)), CHAR(STRING_ELT(keylist,0)));
  imulti = asLogical(multi);
  if (imulti == NA_INTEGER) error("'multi' must be TRUE or FALSE");
  if (imulti) grib_multi_support_on(0);
  else grib_multi_support_off(0);

  if(!(newindex=Rgrib_create_index()) )  {
    Rprintf("Error creating the GRIBindex.\n");
    fclose(infile);
    return(R_NilValue);
  }

  //comma separated list of keys for the index. The type of the key can be explicitly declared appending :l for long, (or alternatively :i) :d for double, :s for string to the key name. If the type is not declared explicitly, the native type is assumed.
  id = newindex->id;
//  Rprintf("Creating new index %i : %li.\n", *id, (long int) id);
  // NOTE: index is created using a file name (char*), not a FILE object!
  // to avoid a warning, we copy it to a local (non-const) string
  strncpy(str_val, CHAR(STRING_ELT(filename,0)), MAX_FILE_NAME);
  gi = grib_index_new_from_file(NULL, str_val,
      CHAR(STRING_ELT(keylist,0)), &err);
  newindex->h = gi;
  newindex->ext_ptr = R_MakeExternalPtr(id, install("GRIBindex"), R_NilValue);

  R_RegisterCFinalizerEx(newindex->ext_ptr, Rgrib_indexFinalizer, TRUE);

  PROTECT(output=allocVector(INTSXP,1));
  INTEGER(output)[0]= (long) *id;
  setAttrib(output, install("filename"), filename);
  setAttrib(output, install("keylist"), keylist);
  setAttrib(output, install("multi"), multi);
  setAttrib(output, install("gribindex_ptr"), newindex->ext_ptr);
  UNPROTECT(1);
  return(output);

}

// set (all!) keys of an index and return handle for first message
// NOTE: if all keys are already set, you can iterate over messages
//       by calling this function with keys=list() (empty list)
void Rgrib_index_select(SEXP gribindex, SEXP keylist){

  grib_index *gi=NULL;
  int err, *id, i, nkeys;
  SEXP key_names = getAttrib(keylist, R_NamesSymbol);
  SEXP key_val;
  char key_name[MAX_KEY_LEN];
  size_t vlen=MAX_VAL_LEN;
  char str_val[MAX_VAL_LEN];
  double dbl_val;
  long int int_val;

  id = (int*) R_ExternalPtrAddr(gribindex);
//  Rprintf("Got index pointer %li.\n", (long int) id);
  if (!id) error("Not a valid GRIBindex.\n");
//  Rprintf("id=%i\n", *id);
  gi=GRIBindexList[*id]->h;
  if(!gi) error("Not a registered GRIBindex.\n");

  
  nkeys = length(keylist);
//  Rprintf("Setting %i keys.\n", nkeys);

  for (i=0 ; i<nkeys ; i++) {
    strncpy(key_name, CHAR(STRING_ELT(key_names, i)), MAX_KEY_LEN);
    key_val = VECTOR_ELT(keylist, i);
    if (isReal(key_val)) {
      dbl_val = REAL(key_val)[0];
//      Rprintf("Setting %s = (real) %lf\n", key_name, dbl_val);
      grib_index_select_double(gi, key_name, dbl_val);
    }
    else if (isInteger(key_val)) {
      int_val = (long) INTEGER(key_val)[0];
//      Rprintf("Setting %s = (int) %li\n", key_name, int_val);
      grib_index_select_long(gi, key_name, int_val);
    }
    else if (isString(key_val)) {
      strncpy(str_val, CHAR(STRING_ELT(key_val, 0)), MAX_VAL_LEN);
//      Rprintf("Setting %s = (string) %s\n", key_name, str_val);
      grib_index_select_string(gi, key_name, str_val);
    }
    else Rprintf("Warning: key %s has an unknown type.\n", key_name);
  }
  // No output
}

SEXP Rgrib_handle_from_index(SEXP gribindex){
  grib_index *gi=NULL;
  grib_handle *h;
  int err, *id;
  SEXP output;
  RgribHandle *newhandle;

  id = (int*) R_ExternalPtrAddr(gribindex);
//  Rprintf("Got index pointer %li.\n", (long int) id);
  if (!id) error("Not a valid GRIBindex.\n");
//  Rprintf("id=%i\n", *id);
  gi=GRIBindexList[*id]->h;
  if (!gi) error("Not a registered GRIBindex.\n");

  h = grib_handle_new_from_index(gi, &err);
  if (!h) error("Could not retrieve a GRIBhandle from the index...\n");
// NOTE: don't create the new handle before you are sure to use it!
//
  if(!(newhandle=Rgrib_create_handle()) )  {
    Rprintf("Error creating the GRIBhandle.\n");
    return(R_NilValue);
  }

  id = newhandle->id;
//  Rprintf("newhandle id=%i\n", *id);
  newhandle->h = h;

  newhandle->ext_ptr = R_MakeExternalPtr(id, install("GRIBhandle"), R_NilValue);
  R_RegisterCFinalizerEx(newhandle->ext_ptr, Rgrib_handleFinalizer, TRUE);
  PROTECT(output=allocVector(INTSXP,1));
  INTEGER(output)[0]=(long) *id;
  setAttrib(output, install("gribhandle_ptr"), newhandle->ext_ptr);
  UNPROTECT(1);
  return(output);
}

// ========================================
// return key values for all (selected) messages in an index
// PROBLEM: there is no simple way to know the number of messages beforehand
//          there is no function for that in eccodes. Annoying.
/*
SEXP Rgrib_index_info(SEXP gribindex,
        SEXP IntPar, SEXP DblPar, SEXP StrPar) {
  grib_index *gi=NULL;
  int i, j, err, *id, nIntPar, nDblPar, nStrPar, npar, nrec ;
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
//  Rprintf("Asking for %d int, %d double, %d char.\n",nIntPar,nDblPar,nStrPar);

  id = (int*) R_ExternalPtrAddr(gribindex);
  if (!id) error("Not a valid GRIBindex.\n");
  gi=GRIBindexList[*id]->h;
  if(!gi) error("Not a registered GRIBindex.\n");


  // FIXME: there is no simple way to know how many messages are available

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
  for(i=0;irec<nrec && (h = grib_handle_new_from_index(gi, &err))!=NULL;i++) {

    if(i==INTEGER(rec)[irec]-1){
      for(j=0;j<nStrPar;j++) {
        vlen=MAX_VAL_LEN;
        err=grib_get_string(h,CHAR(STRING_ELT(StrPar,j)),Str_value,&vlen);
        if (err) {
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
    grib_handle_delete(h);  }


  PROTECT(result=allocVector(VECSXP,npar));
  for(i=0;i<nStrPar;i++){
    PROTECT(parvec=allocVector(STRSXP,nrec));
    for(j=0;j<nrec;j++) {
      if( StrAns[i][j]==NULL  ) SET_STRING_ELT(parvec,j,NA_STRING) ;
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

  UNPROTECT(1);
  return(result);
}

// get all available values for a set of keys
SEXP Rgrib_index_get(SEXP gribindex, SEXP keylist) {

}
*/
