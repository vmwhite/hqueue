#' Under the WAIT discipline, calculate the utilization of the primary server
#'
#' @param lambda := the arrival rate of customers (number of customers per unit time)
#' @param E := the average total service time
#'
#' @return rho_p := the utilization of the primary server
#' @export
#'
#' @examples
#' lambda <- 2
#' E <- .2
#' rho_p <- calc_rho_p (lambda,E)
calc_rho_p <- function(lambda, E){
  val <- lambda *E
  return(val)
}
