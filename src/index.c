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
SEXP Rgrib_handle_from_index(SEXP gribindex, SEXP keys, SEXP multi){

  grib_index *gi=NULL;
  grib_handle *h;
  int err, *id, imulti, count, i, nkeys;
  SEXP output;
  SEXP key_names = getAttrib(keys, R_NamesSymbol);
  SEXP key_val;
  char key_name[MAX_KEY_LEN];
  RgribHandle *newhandle;
  size_t vlen=MAX_VAL_LEN;
  char str_val[MAX_VAL_LEN];
  double dbl_val;
  long int int_val;

  imulti = asLogical(multi);
  if (imulti == NA_INTEGER) error("'multi' must be TRUE or FALSE");
  if (imulti) grib_multi_support_on(0);
  else grib_multi_support_off(0);

  id = (int*) R_ExternalPtrAddr(gribindex);
//  Rprintf("Got index pointer %li.\n", (long int) id);
  if (!id) error("Not a valid GRIBindex.\n");
//  Rprintf("id=%i\n", *id);
  gi=GRIBindexList[*id]->h;
  if(!gi) error("Not a registered GRIBindex.\n");

  if(!(newhandle=Rgrib_create_handle()) )  {
    Rprintf("Error creating the GRIBhandle.\n");
    return(R_NilValue);
  }
  
  nkeys = length(keys);
//  Rprintf("Setting %i keys.\n", nkeys);

  for (i=0 ; i<nkeys ; i++) {
    strncpy(key_name, CHAR(STRING_ELT(key_names, i)), MAX_KEY_LEN);
    key_val = VECTOR_ELT(keys, i);
    Rprintf("Setting %s\n", key_name);
    if (isReal(key_val)) {
      dbl_val = REAL(key_val)[0];
      grib_index_select_double(gi, key_name, dbl_val);
    }
    else if (isInteger(key_val)) {
      int_val = INTEGER(key_val)[0];
      grib_index_select_long(gi, key_name, int_val);
    }
    else if (isString(key_val)) {
      strncpy(str_val, CHAR(STRING_ELT(key_val, 0)), MAX_VAL_LEN);
      grib_index_select_string(gi, key_name, str_val);
    }
    else Rprintf("Warning: key %s has an unknown type.\n", key_name);
  }
    
  h = grib_handle_new_from_index (gi, &err);
  id = newhandle->id;
  newhandle->h = h;

  newhandle->ext_ptr = R_MakeExternalPtr(id, install("GRIBhandle"), R_NilValue);
  R_RegisterCFinalizerEx(newhandle->ext_ptr, Rgrib_handleFinalizer, TRUE);
  PROTECT(output=allocVector(INTSXP,1));
  INTEGER(output)[0]=(long) *id;
/*  setAttrib(output,install("sample"),sample); */
  setAttrib(output, install("gribhandle_ptr"), newhandle->ext_ptr);
  UNPROTECT(1);
  return(output);
}
