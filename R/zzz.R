".onLoad" <-
  function (lib, pkg)
{
  if (.Machine$integer.max != 2147483647)
    stop("Current Rgrib implementation assumes 32-bit integers.")
}
####################################
".onUnload" <- function (libpath){
  GhandleFreeAll();
  GindexFreeAll();
  library.dynam.unload("Rgrib2",libpath)
}


