#' write baselers data to file
#'
#' @param nsim number of simulations
#' @param seed seed
#'
#' @export
#'
#' @importFrom readr write_csv
write_basel <- function(nsim = 10000, seed = 1) {

  basel <- simulate_basel(nsim = nsim, seed = seed)

  # if(file.exists("inst/extdata/baselers.txt")) {file.remove("inst/extdata/baselers.txt")}

  readr::write_csv(x = basel, path = "inst/extdata/basel.txt")

  # if(file.exists("data/baselers.RData")) {file.remove("data/baselers.RData")}

  save(basel, file = "data/basel.RData")

  message("data/basel.RData and inst/extdata/basel.txt saved!")

}
