#include "rgrib.h"

RgribHandle* GRIBhandleList[MAX_HANDLE];
extern RgribIndex* GRIBindexList[MAX_INDEX];

void Rgrib_init_check() {
  static int is_initialised=0;
  int i;

  if (!is_initialised) {
    for (i=0;i<MAX_HANDLE;i++) GRIBhandleList[i]=NULL;
    for (i=0;i<MAX_INDEX;i++) GRIBindexList[i]=NULL;
    is_initialised=1;
  }
}

RgribHandle* Rgrib_create_handle(){
  int j,id;
  Rgrib_init_check();
/* Call garbage collection to free any deleted GRIBhandles */
  R_gc();
  for (j=0;GRIBhandleList[j] && j<MAX_HANDLE; j++) {}
  id=j;
  if (j>=MAX_HANDLE) {
    Rprintf("Reached maximum open grib handles: %d\n",MAX_HANDLE);
    return(NULL);
  }
  GRIBhandleList[id]=(RgribHandle*) malloc(sizeof(RgribHandle));
  GRIBhandleList[id]->id = malloc(sizeof(int));
  *GRIBhandleList[id]->id = id;
  GRIBhandleList[id]->ext_ptr = NULL;
/*  GRIBhandleList[id]->h = h;*/
  return(GRIBhandleList[id]);
}

void Rgrib_GRIBhandle_destroy(int i){
  Rgrib_init_check();
  if (i<0 || i>=MAX_HANDLE) return;
  if (!GRIBhandleList[i]) return;

  if (GRIBhandleList[i]->h) {
    grib_handle_delete(GRIBhandleList[i]->h);
    GRIBhandleList[i]->h=NULL;
  }
  else warning("Inexplicably, a GRIBhandle was found without a valid message pointer\n");

  if (GRIBhandleList[i]->id) {
    free(GRIBhandleList[i]->id);
    GRIBhandleList[i]->id=NULL;
  }
  else warning("Inexplicably, a GRIBhandle was found without a valid ID pointer\n");

  if (GRIBhandleList[i]->ext_ptr) {
    R_ClearExternalPtr(GRIBhandleList[i]->ext_ptr);
  }
  else warning("Inexplicably, a GRIBhandle was found without a valid EXT pointer\n");

  free(GRIBhandleList[i]);
  GRIBhandleList[i]=NULL;
  return;
}

void Rgrib_clear_all_handles(){
  int i;
  for (i=0;i<MAX_HANDLE;i++) Rgrib_GRIBhandle_destroy(i);
  return;
}

long Rgrib_count_handles_C(){
  int i;
  long result=0;
  Rgrib_init_check();
  for (i=0;i<MAX_HANDLE;i++) if(GRIBhandleList[i]) result++;
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
  for (i=0;i<MAX_HANDLE && j<cl ;i++) if(GRIBhandleList[i]) {
    INTEGER(result)[j++]= i;
  }
  UNPROTECT(1);
  return(result);
}

SEXP Rgrib_clear_handle(SEXP gribhandle){
  int* id;
  int iid;

  if (!(id=R_ExternalPtrAddr(gribhandle))) error("This is not an open GRIBhandle.\n");

  iid=*id;

  Rgrib_GRIBhandle_destroy(iid);
  R_ClearExternalPtr(gribhandle); /* should be taken care of by the destroy function */
  return(R_NilValue);
}

void Rgrib_handleFinalizer(SEXP gribhandle)
{
  int *id;
  int iid;

  id=R_ExternalPtrAddr(gribhandle);
  if(!id) return;
  iid=*id;
  Rgrib_GRIBhandle_destroy(iid);

  R_ClearExternalPtr(gribhandle); /* it may already be clear */
}


