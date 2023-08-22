#' Under the WAIT discipline, the probability distribution of the number of requests for secondary service
#'
#' @param n := the number to evaluate the probability distribution at. 0<= n<= N, where no more than N requests can exist simultaneously
#' @param rho_Q := utilization of the secondary server. Note: this is rho_s in the original paper.
#' @param theta := the singular positive root of the polynomial \eqn{\sum_{n=0}^{n=N} \frac{(1-\rho_q)*\theta^n}{n!} = 1}
#'
#' @return pr(n) := the probability of n requests for secondary service
#' @export
#'
#' @examples
#' n <- 2
#' N <- 5
#' mu_q  <- 3
#' P_1 <- .2
#' lambda <- 1
#' mu_2 <- mu_q
#' rho_Q <- calc_rho_Q(N, P_1, lambda, mu_2)
#' theta <- calc_theta(rho_Q, N)
#' prob_n <- pr(n,rho_Q,theta)
pr <- function(n, rho_Q, theta){
  val <- ((1 - rho_Q)*(theta^n))/(hqueue::facto(n))
  return(val)
}
