#include "rgrib.h"

// parse a file and locate the first grib message that fits all keys
//   multi_message flag should be OK
//   return gribhandle
SEXP Rgrib_fast_find(SEXP filename, SEXP keys, SEXP multi) {
  FILE* infile;
  grib_handle *h;
  int nkeys, i, j, OK, err, imulti, found, *id, count=0;
  size_t vlen=MAX_VAL_LEN;
  SEXP output;
  SEXP key_names = getAttrib(keys, R_NamesSymbol);
  SEXP key_val;
  char key_name[MAX_KEY_LEN]; // max key length?
  double DblAns;
  long IntAns;
  char StrAns[MAX_VAL_LEN];
  RgribHandle *newhandle;

  if(!(newhandle=Rgrib_create_handle()) )  {
    Rprintf("Error creating the GRIBhandle.\n");
    return(R_NilValue);
  }

  imulti = asLogical(multi);
  if(imulti == NA_LOGICAL) error("'multi' must be TRUE or FALSE");
  if(imulti) grib_multi_support_on(0);
  else grib_multi_support_off(0);

  if( !(infile = fopen(CHAR(STRING_ELT(filename,0)),"r")) ) {
    Rprintf("Could not open file %s\n",CHAR(STRING_ELT(filename,0)));
    return(R_NilValue);
  }

  nkeys = length(keys);
  found = 0;
  while ( (h = grib_handle_new_from_file(0,infile,&err))!=NULL ){
    count++;
//    Rprintf("message %i\n", count);
    OK = 1;
    // check all keys for this message, untill it fails
    for (i=0; i<nkeys; i++) {
      // get key name and find out what kind of value is expected
      // key_name = CHAR(STRING_ELT(key_names, i)) ;
      strncpy(key_name, CHAR(STRING_ELT(key_names, i)), MAX_KEY_LEN);
      key_val = VECTOR_ELT(keys, i);
      if (isReal(key_val)) {
//        Rprintf("%s : %lf\n", key_name, REAL(key_val)[0]);
        err=grib_get_double(h, key_name, &DblAns);
        if (err) {
          DblAns = NA_REAL;
        }
        if (DblAns != REAL(key_val)[0]) {
          OK = 0;
          break ;
        }
      } else if  (isInteger(key_val)) {
//        Rprintf("%s : %li\n", key_name, INTEGER(key_val)[0]);
        err=grib_get_long(h, key_name, &IntAns);
        if (err) {
          IntAns = NA_INTEGER;
        }
        if (IntAns != INTEGER(key_val)[0]) {
          OK = 0;
          break ;
        }
      } else if  (isString(key_val)) {
//        Rprintf("%s : %s\n", key_name, CHAR(STRING_ELT(key_val,0)));
        vlen=MAX_VAL_LEN;

        err=grib_get_string(h, key_name, StrAns, &vlen);
        if (err) {
          strncpy(StrAns,"NA_STRING\0",10);
        }
        if (strcmp(StrAns, CHAR(STRING_ELT(key_val,0)))) {
          OK = 0;
          break ;
        }
      }
    }
    // did all the keys match?
    if (OK) {
      found = 1;
      break;
    }
    grib_handle_delete(h);
  }

  if (!found) {
    Rprintf("Error: reached end of file.\n");
    fclose(infile);
    return(R_NilValue);
  }

  id = newhandle->id;
  newhandle->h = grib_handle_clone(h);
  grib_handle_delete(h);
  newhandle->ext_ptr = R_MakeExternalPtr(id, install("GRIBhandle"), R_NilValue);

  R_RegisterCFinalizerEx(newhandle->ext_ptr, Rgrib_handleFinalizer, TRUE);
  PROTECT(output=allocVector(INTSXP,1));
  INTEGER(output)[0]= (long) *id;
  setAttrib(output, install("filename"), filename);
//  setAttrib(output, install("message"), count); // turn count into a SEXP !
//  can I set the class to "GRIBhandle" ?
  setAttrib(output, install("gribhandle_ptr"), newhandle->ext_ptr);
  UNPROTECT(1);
  fclose(infile);
  return(output);

}


