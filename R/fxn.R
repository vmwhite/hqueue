#' Under the WAIT discipline, intermediate function for calculating \eqn{\sigma_{Q_q}^2}
#'
#' @param n := the number to evaluate the probability distribution at. 0<= n<= N, where no more than N requests can exist simultaneously
#' @param N := the number of primary servers. No more than N requests for secondary service can exist simultaneously
#' @param rho_Q := secondary server utilization
#' @param theta := the singular positive root of the polynomial \eqn{\sum_{n=0}^{n=N} \frac{(1-\rho_q)*\theta^n}{n!} = 1}
#'
#' @return intermediate value in calculating \eqn{\sigma_{Q_q}^2}
#' @export
#'
#' @examples
#' i <- 1
#' N <- 5
#' mu_q  <- 3
#' P_1 <- .2
#' lambda <- 1
#' mu_2 <- mu_q
#' rho_Q <- calc_rho_Q(N, P_1, lambda, mu_2)
#' theta <- calc_theta(rho_Q,N)
#' fxn(i,N, rho_Q, theta)
fxn<- function(n,N,rho_Q, theta){
 val <- pr(n,rho_Q, theta) / (1-pr(N,rho_Q, theta))
  return(val)
}
