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


