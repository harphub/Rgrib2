#Rgrib2
### a R package to decode and encode GRIB data

This R package is an api to the *grib_api* library developed by ECMWF, with several functionalities added.

The data fields are coded as matrices with attributes to describe projection and grid specifications.

Usually, this package will be used together with the companion package *geogrid*.

##C CODE

This package requires the library *grib_api* (versions <=2012 are GPL, more recent versions use the Apache license) developped by ECMWF and available from https://software.ecmwf.int/wiki/display/GRIB/Home

Note that this library must be compiled as a shared library on linux systems, which is not the default (and probably requires the -fpic flag to be added).

This package also usually requires the PROJ.4 library for projections.

#EARTH RADIUS
In GRIB-1 files, the Earth radius is not defined explicitely. However, the WMO standard value differs from that used at ECMWF and in the ALADIN consortium. This code uses our default value!!!

### ATTENTION: the WMO standard for GRIB-1 files has earth radius a=6367470.0
###            On the other hand, NCEP uses a=6371200.0 (also hardcoded in grads)
###            and the ALADIN model a=6371229.0
###            In GRIB-1 it is not possible to pass the Earth radius.
###            In GRIB-2, there is a parameter 'earth shape'. Value 0 corresponds
###            to the grib-1 sphere, while value 6 corresponds to the sphere as uses by ECMWF and ALADIN.
### strangely, grib_api returns shape="6" and radius 6367470.0 for grib-1 files. This is inconsistent!

##License
Copyright 2003-2016, Alex Deckmyn, Royal Meteorological Institute of Belgium
alex.deckmyn@meteo.be

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
inst/COPYING


