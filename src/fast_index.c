#include "rgrib.h"
#define BUFLEN 100
// parse a file and locate all grib messages
//   return all byte addresses as double (int is only 32bit)
//   also check GRIB version (1 or 2)
//   multi_message flag would be useful
SEXP Rgrib_fast_index(SEXP filename, SEXP nmsg) {
  FILE* infile;
  int *len, *edition;
  int i, n, found, nread, max_msg;
  int n_len, k;
  unsigned char buf1[BUFLEN], *b_len;
  double *pos;
  long int loc;
  SEXP result, parvec;

  if( !(infile = fopen(CHAR(STRING_ELT(filename,0)),"r")) ) {
    Rprintf("Could not open file %s\n",CHAR(STRING_ELT(filename,0)));
    return(R_NilValue);
  }
  max_msg = *INTEGER(nmsg);
//  Rprintf("Max msg= %i\n", max_msg);
  pos = (double*) R_alloc(max_msg, sizeof(double));
  len = (int *) R_alloc(max_msg, sizeof(int));
  edition = (int *) R_alloc(max_msg, sizeof(int));

  n = 0;
  loc = 0;
  while(1) {
    fseek(infile, loc, SEEK_SET); 
    nread = fread(buf1, 1, BUFLEN, infile);
    if (nread == 0) break;
    found=0;
    for (i=0 ; i < nread-8 ; i++) {
      if (strstr(buf1+i, "GRIB")) {
        found=1;
        break;
      }
    }  
    if (found) {
      pos[n] = loc + i;
      edition[n] = buf1[i+7];
      if (edition[n] == 1) {
        // message length is last 3 bytes of 8.
        b_len = buf1 + i + 4;
        n_len = 3;
//        len[n] = buf1[i+4]*256*256 + buf1[i+5]*256 + buf1[i+6];
      } else {
        // message length is last 8 bytes of 16.
        b_len = buf1 + i + 8;
        n_len = 8;
      }
      len[n] = 0;
      for (k = 0; k < n_len; k++) len[n] = 256*len[n] + *(b_len++);
//      Rprintf("%i : loc=%ld len=%i\n", n, (long int) pos[n], len[n]);
      // TODO: read other meta data ? Search for sub-messages?
      // jump to end of message. check for '7777'.
      fseek(infile, loc + i + len[n] - 4, SEEK_SET);
      fread(buf1, 1, 4, infile);
      if (!strstr(buf1, "7777")) {
        Rprintf("Inconsistency in GRIB message %i \n", n+1);
        break;
      }
      loc += i + len[n];

      n++;
      if (n >= max_msg) {
        Rprintf("More than %i GRIB messages!\n", max_msg);
        break;
      }
    }
    else {
      loc += BUFLEN - 8;
    }
  }

// now prepare the output : a list of loc, len, edition
  if (n==0) {
    Rprintf("No GRIB messages found.\n");
    return(R_NilValue);
  }

  PROTECT(result=allocVector(VECSXP, 3));

  PROTECT(parvec=allocVector(REALSXP, n));
  for (i=0; i<n; i++) REAL(parvec)[i] = pos[i];
  SET_VECTOR_ELT(result, 0, parvec);
  UNPROTECT(1);

  PROTECT(parvec=allocVector(REALSXP, n));
  for (i=0; i<n; i++) REAL(parvec)[i] = len[i];
  SET_VECTOR_ELT(result, 1, parvec);
  UNPROTECT(1);

  PROTECT(parvec=allocVector(REALSXP, n));
  for (i=0; i<n; i++) REAL(parvec)[i] = edition[i];
  SET_VECTOR_ELT(result, 2, parvec);
  UNPROTECT(1);

// finished
  UNPROTECT(1);
  return(result);

}


