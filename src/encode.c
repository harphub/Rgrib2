#include "rgrib.h"

extern RgribHandle* GRIBhandleList[MAX_HANDLE];

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


