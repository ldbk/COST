.onLoad <- function(lib,pkg) {
	cat("-------------------------------------------------------------------\n")
	pkg.info <- drop(read.dcf(file=system.file("DESCRIPTION", package="COSTcore"), fields=c("Version", "Built")))
	cat(" COSTcore - \"The DCF Statistical Suite\"\n")
#	cat(" WARNING: this is a development version, use at your own risk !!\n")
	cat(paste(" (Version: ", pkg.info["Version"], ". Built on: ", pkg.info["Built"], ")\n", sep=""))
	cat("-------------------------------------------------------------------\n")
}
