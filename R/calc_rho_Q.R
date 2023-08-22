#' under the WAIT discipline, calculating the secondary server utilization
#'
#' @param N :=the number of primary servers. No more than N requests for secondary service can exist simultaneously
#' @param P_1 := the probability a call requires  secondary service
#' @param lambda := the customer arrival rate
#' @param mu_2 := the secondary service rate
#'
#' @return $$rho_Q$$ := utilization of a secondary server. Note: this is rho_s in the original text
#' @export
#'
#' @examples
#' N <- 5
#' P_1 <- .2
#' lambda <- 3
#' mu_2 <- 4
#' rho_Q <- calc_rho_Q(N, P_1, lambda, mu_2)
calc_rho_Q <- function(N, P_1,lambda,mu_2){
  val <- (N * P_1 * lambda) / mu_2
  return(val)
}
