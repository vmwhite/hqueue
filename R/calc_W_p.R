#' Under the WAIT discipline, calculate the average primary service wait time
#'
#' @param L_p :=  the average number of customers in the system
#' @param lambda := the arrival rate of customers (number of customers per unit time)
#'
#' @return W_p := the average primary service wait time (in unit time)
#' @export
#'
#' @examples
#' lambda <- 2
#' L_p <- 4
#' W_p <- calc_W_p(L_p, lambda)

calc_W_p <- function(L_p, lambda){
  val <- (L_p / lambda)
  return(val)
}
