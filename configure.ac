AC_INIT(Rgrib2, 1.4.0, alex.deckmyn@meteo.be)

# find R home and set correct compiler + flags
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  AC_MSG_ERROR([cannot determine R_HOME. Make sure you use R CMD INSTALL!])
  exit 1
fi

# pick all flags for testing from R (e.g. use ~/.R/Makevars)
RBIN="${R_HOME}/bin/R"
CC=`"${RBIN}" CMD config CC`
CFLAGS=`"${RBIN}" CMD config CFLAGS`
CPPFLAGS=`"${RBIN}" CMD config CPPFLAGS`
LDFLAGS=`"${RBIN}" CMD config LDFLAGS`
AC_PROG_CC
AC_PROG_CPP

# check for user-specified ECCODES
AC_ARG_WITH([eccodes-include],
            AC_HELP_STRING([--with-eccodes-include=INCLUDE_PATH],
                           [the location of the eccodes header files]),
            [eccodes_include_path=$withval])
ECCODES_CPPFLAGS="-I."
if test [ -n "$eccodes_include_path" ] ; then
  ECCODES_CPPFLAGS="-I. -I${eccodes_include_path}"
else
  if test [ -n "$ECCODES_INCLUDE" ] ; then
    ECCODES_CPPFLAGS="-I. -I${ECCODES_INCLUDE}"
  fi
fi

AC_ARG_WITH([eccodes-lib],
            AC_HELP_STRING([--with-eccodes-lib=LIB_PATH],
                           [the location of the eccodes libraries]),
            [eccodes_lib_path=$withval])
if test [ -n "$eccodes_lib_path" ] ; then
  LIBS="-L${eccodes_lib_path} -Wl,-rpath,${eccodes_lib_path} ${LIBS}"
else
  if test [ -n "${ECCODES_LIBS}" ] ; then
    LIBS="-L${ECCODES_LIBS} -Wl,-rpath,${ECCODES_LIBS} ${LIBS}"
  fi
fi

# both combined in a single --with-eccodes statement:
AC_ARG_WITH([eccodes],
            AC_HELP_STRING([--with-eccodes=PROJ_PATH],
                           [the location of eccodes]),
            [eccodes_path=$withval])
if test [ -n "$eccodes_path" ] ; then
  LIBS="-L${eccodes_path}/lib -Wl,-rpath,${eccodes_path}/lib ${LIBS}"
  ECCODES_CPPFLAGS="-I. -I${eccodes_path}/include"
else
  if test [ -n "${ECCODES_DIR}" ] ; then
    LIBS="-L${ECCODES_DIR}/lib -Wl,-rpath,${ECCODES_DIR}/lib ${LIBS}"
    ECCODES_CPPFLAGS="-I. -I${ECCODES_DIR}/include"
  fi
fi

# you need jasper and/or openjp2, unless eccodes was compiled without
AC_CHECK_LIB(jasper, jas_stream_memopen)
AC_CHECK_LIB(openjp2, opj_image_create)

# look for eccodes
AC_CHECK_LIB(eccodes, grib_count_in_file, [],
              [AC_MSG_ERROR([eccodes not found.
*** Install ECCODES or add --with-eccodes=<path/to/eccodes>
*** or --with-eccodes-include=<path/to/eccodes/include> --with-eccodes-lib=<path/to/eccodes/lib>
*** or set environment variables ECCODES_DIR or ECCODES_INCLUDE and ECCODES_LIBS]) ])
CPPFLAGS="${CPPFLAGS} ${ECCODES_CPPFLAGS}"
AC_CONFIG_FILES(src/Makevars)
AC_SUBST(ECCODES_CPPFLAGS)
AC_SUBST(LIBS)
AC_OUTPUT


