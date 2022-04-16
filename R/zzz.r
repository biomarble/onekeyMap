##' @importFrom utils packageDescription
.onAttach <- function(libname, pkgname="mapChina") {
    pkgVersion <- packageDescription(pkgname, fields="Version")
    author<-packageDescription(pkgname,fields="Maintainer")
    msg <- paste0(pkgname, " v", pkgVersion, " load successfully!\n",
		  "Author:",author,"\n"
               ,   "More Packages: https://blog.ugeneyun.cn/software\n"
    )
    packageStartupMessage(msg)
}
