".onLoad" <-
  function (lib, pkg)
{
  if (.Machine$integer.max != 2147483647)
    stop("Current Rgrib implementation assumes 32-bit integers.")
#  Additional definitions for eccodes (probably belong outside Rgrib2)
  Sys.setenv("ECCODES_DEFINITION_PATH" = paste(
            Sys.getenv("ECCODES_DEFINITION_PATH"),
            paste(lib,pkg,"definitions", sep="/"), sep=":"))

}
####################################
".onUnload" <- function (libpath){
  GhandleFreeAll();
  GindexFreeAll();
  library.dynam.unload("Rgrib2",libpath)
}


