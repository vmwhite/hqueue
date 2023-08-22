#' Under the WAIT discipline, calculate the average number of customers
#'
#' @param rho_p := the utilization of the primary server
#' @param lambda := the arrival rate of customers (number of customers per unit time)
#' @param sigma_sqr := the service time variance for a all customers
#'
#' @return L_p := the average number of customers in the system
#' @export
#'
#' @examples
#' rho_p <- .6
#' lambda <- 3
#' sigma_sqr <- .4
calc_L_p <- function(rho_p, lambda, sigma_sqr){
  val <- (rho_p^2) + ((lambda^2)*(sigma_sqr))
  val <- val / (2*(1-rho_p))
  val <- rho_p + val
  return(val)
}
