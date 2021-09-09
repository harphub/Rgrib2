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
# FIRST: both combined in a single --with-eccodes statement:
AC_ARG_WITH([eccodes],
            AC_HELP_STRING([--with-eccodes=PROJ_PATH],
                           [the location of eccodes]),
            [eccodes_path=$withval])
if test [ -n "$eccodes_path" ] ; then
  AC_MSG_NOTICE([Using eccodes_path=${eccodes_path}])
  LIBS="-L${eccodes_path}/lib -Wl,-rpath,${eccodes_path}/lib ${LIBS}"
  ECCODES_CPPFLAGS="-I. -I${eccodes_path}/include"
else
  if test [ -n "${ECCODES_DIR}" ] ; then
    AC_MSG_NOTICE([Using ECCODES_DIR=${ECCODES_DIR}])
  # as defined in the ECMWF modules
  # TODO: should we also check for ".../lib64" ?
    LIBS="-L${ECCODES_DIR}/lib -Wl,-rpath,${ECCODES_DIR}/lib ${LIBS}"
    ECCODES_CPPFLAGS="-I. -I${ECCODES_DIR}/include"
  else
    # separate --with-eccodes-include (or ECCODES_INCLUDE)
    AC_ARG_WITH([eccodes-include],
                AC_HELP_STRING([--with-eccodes-include=INCLUDE_PATH],
                               [the location of the eccodes header files]),
                [eccodes_include_path=$withval])
    ECCODES_CPPFLAGS="-I."
    if test [ -n "$eccodes_include_path" ] ; then
      AC_MSG_NOTICE([Using eccodes_include_path=${eccodes_include_path}])
      ECCODES_CPPFLAGS="-I. -I${eccodes_include_path}"
    else
      if test [ -n "$ECCODES_INCLUDE" ] ; then
        AC_MSG_NOTICE([Using ECCODES_INCLUDE=${ECCODES_INCLUDE}])
        # NOTE: ECCODES_INCLUDE already contains the "-I"
        ECCODES_CPPFLAGS="-I. ${ECCODES_INCLUDE}"
      fi
    fi

    # separate --with-eccodes-lib (or ECCODES_LIB)
    AC_ARG_WITH([eccodes-lib],
                AC_HELP_STRING([--with-eccodes-lib=LIB_PATH],
                               [the location of the eccodes libraries]),
                [eccodes_lib_path=$withval])
    if test [ -n "$eccodes_lib_path" ] ; then
      AC_MSG_NOTICE([Using eccodes_lib_path=${eccodes_lib_path}])
      LIBS="-L${eccodes_lib_path} -Wl,-rpath,${eccodes_lib_path} ${LIBS}"
    else
      if test [ -n "${ECCODES_LIB}" ] ; then
        AC_MSG_NOTICE([Using ECCODES_LIB=${ECCODES_LIB}])
        # NOTE: ECCODES_LIB includes the "-Wl,-rpath,....." part!
        LIBS="${ECCODES_LIB} ${LIBS}"
      fi
    fi

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
*** or set environment variables eccodes_path, eccodes_lib_path & eccodes_include_path
*** or (like at ECMWF) ECCODES_DIR or ECCODES_INCLUDE and ECCODES_LIB]) ])
CPPFLAGS="${CPPFLAGS} ${ECCODES_CPPFLAGS}"
AC_CONFIG_FILES(src/Makevars)
AC_SUBST(ECCODES_CPPFLAGS)
AC_SUBST(LIBS)
AC_OUTPUT


