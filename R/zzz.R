.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "FishDiveR version ", utils::packageVersion(pkgname),
    ". Facilitating classification of aquatic animal behaviours from vertical movement data.\n Type 'citation(\"FishDiveR\")' for citing this R package in publications."
  )
}
