#include "rgrib.h"
#define BUFLEN 100
// parse a file and locate all grib messages
//   return all byte addresses as double (int is only 32bit)
//   also check GRIB version (1 or 2)
//   multi_message flag would be useful
#define int3(buf) ((buf)==NULL ? 0 : (int) ((buf[0]<<16)+(buf[1]<<8)+buf[2]))

SEXP Rgrib_position_index(SEXP filename, SEXP nmsg) {
  FILE* infile;
  int *len, *edition;
  int i, n, found, nread, max_msg;
  int n_len, k;
  int len_pds, len_gds, len_bms, len_bds, lgds, lbms ;
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
    for (i=0 ; i < nread-16 ; i++) { // for GRIB2: need at least 16 bytes
      // buf1 = strstr(buf1, "GRIB");
      // that would be safe as long as buf1 ends with \0
      if (buf1[i]=='G' && buf1[i+1]=='R' && buf1[i+2]=='I' && buf1[i+3]=='B') {
        found=1;
        break;
      }
    }  
    if (found) {
      pos[n] = loc + i;
      edition[n] = buf1[i+7];
      if (edition[n] == 1) {
        // message length is in bytes 5-7
        //b_len = buf1 + i + 4;
        n_len = 3;
        len[n] = int3( (buf1+i+4) ) ;
//        len[n] = buf1[i+4]*256*256 + buf1[i+5]*256 + buf1[i+6];
      } else {
        // message length is last 8 bytes of 16.
        // So you *should* store it in a long int !!!
        // But for now lets pretend the grib record is < 2GB
        b_len = buf1 + i + 8;
        n_len = 8;
        len[n] = 0;
        for (k = 0; k < n_len; k++) len[n] = (len[n]<<8) + *(b_len++);
      }
//      Rprintf("%i : loc=%ld len=%i\n", n, (long int) pos[n], len[n]);
      // TODO: read other meta data ? Search for sub-messages?
      // jump to end of message. check for '7777'.
      // FIX: ECMWF has modified behaviour for very large data sections in grib-1
      //  ref. wgrib.c, find "echack"
      if (edition[n] == 1 && len[n] >=  2^23) {  // if highest bit set
        lgds = buf1[i+7] & 128 ;
        lbms = buf1[i+7] & 64 ;
        //b_len = buf1 + 8;
        len_pds = int3((buf1+8)) ;
        if (lgds) {
          fseek(infile, loc + i + len_pds, SEEK_SET) ;
          fread(buf1, 1, 4, infile) ;
          len_gds = int3(buf1) ;
        } else {
          len_gds = 0 ;
        }
        if (lbms) {
          fseek(infile, loc + i + len_pds + len_gds, SEEK_SET) ;
          fread(buf1, 1, 4, infile) ;
          len_bms = int3(buf1) ;
        } else {
          len_bms = 0 ;
        }
        fseek(infile, loc + i + len_pds + len_gds + len_bms, SEEK_SET) ;
        fread(buf1, 1, 4, infile) ;
        len_bds = int3(buf1) ;
        if (len_bds < 120) {
          len[n] = (len[n] & 0x7fffff)*120 - len_bds + 4 ;
        }
      }
      fseek(infile, loc + i + len[n] - 4, SEEK_SET);
      fread(buf1, 1, 4, infile);
      if (buf1[0] != '7' || buf1[1] != '7' || buf1[2] != '7' || buf1[3] != '7') {
        Rprintf("Inconsistency in GRIB message %i \n", n+1);
        Rprintf("%i : ed=%i loc=%ld len=%i\n", n, edition[n], (long int) pos[n], len[n]);
        Rprintf("%s\n", buf1);
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
      loc += BUFLEN - 16;
    }
  }
// at this point, we have finished with the file itself
  fclose(infile);

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


