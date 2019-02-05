AC_INIT(Rgrib2, 1.3.0, alex.deckmyn@meteo.be)
## don't give an error if eccodes isn't found: on ecgate that's normal
## installation will work anyway
AC_SEARCH_LIBS([grib_count_in_file],[eccodes grib_api],[],
              [AC_MSG_WARN([Neither eccodes nor grib_api found.])])
AC_CHECK_LIB(jasper,  jas_stream_memopen)
AC_CHECK_LIB(openjp2, opj_image_create)
AC_CONFIG_FILES(src/Makevars)
AC_OUTPUT


