#Rgrib2
### a R package to decode and encode GRIB data

This R package is an api to the *grib_api* library developed by ECMWF, with several functionalities added.

The data fields are coded as matrices with attributes to describe projection and grid specifications.

##C CODE

For GRIB fiels, this package requires the package grib_api (versions <=2012 are GPL, most recent version Apache license) developped by ECMWF and available from
https://software.ecmwf.int/wiki/display/GRIB/Home

Note that this library must be compiled as a shared library on linux systems, which is not the default (and probably requires the -fpic flag to be added).

This package also usually requires the PROJ.4 library for projections.

#TO DO



