#' under the WAIT discipline, calculating he average number of secondary customers in the system
#'
#' @param N := the number of primary servers
#' @param rho_Q := the utilization of secondary servers
#'
#' @return $$L_Q$$ := the average number of secondary customers in the system
#' @export
#'
#' @examples
#' N <- 5
#' P_1 <- .2
#' lambda <- 3
#' mu_2 <- 4
#' rho_Q <- calc_rho_Q(N, P_1, lambda, mu_2)
#' L_Q <- calc_L_Q(N, rho_Q)
calc_L_Q <- function(N, rho_Q){
  val <- 0
  theta <- calc_theta(rho_Q,N)
  for (i in 0:N){
    val <- val + i*((((1-rho_Q)*(theta^i))/facto(i)))
  }
  return(val)
}
