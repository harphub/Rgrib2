".onLoad" <-
  function (lib, pkg)
{
  if (.Machine$integer.max != 2147483647)
    stop("Current Rgrib implementation assumes 32-bit integers.")
#  require(geogrid)
#  library.dynam("Rgrib2", pkg, lib)
}
####################################
".Last.lib" <- function (path){
  GhandleFreeAll();
#  library.dynam.unload(paste(path,"/libs/","Rgrib2",".so",sep=""))
}


