#' under the WAIT discipline, calculating the probability a customer has to wait for secondary service
#'
#' @param N := the number of primary servers
#' @param rho_Q := the utilization of secondary servers
#'
#' @return $$prob_delay$$ := the probability a customer has to wait for secondary service
#' @export
#'
#' @examples
#' N <- 5
#' P_1 <- .2
#' lambda <- 3
#' mu_2 <- 4
#' rho_Q <- calc_rho_Q(N, P_1, lambda, mu_2)
#' prob_delay <- prob_delay(N, rho_Q)
prob_delay <- function(N, rho_Q){
  val <- 0
  theta <- calc_theta(rho_Q,N)
  for (i in 2:N){
    val <- val + ((((1-rho_Q)*(theta^i))/facto(i)))
  }
  return(val)
}
