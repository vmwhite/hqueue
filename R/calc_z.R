#' Under the WAIT discipline, intermediate function for calculating \eqn{\sigma_{Q_q}^2}
#'
#' @param N  := the number of primary servers. No more than N requests for secondary service can exist simultaneously
#' @param rho_Q := utilization of the secondary server. Note: this is rho_s in the original paper.
#' @param theta := the singular positive root of the polynomial \eqn{\sum_{n=0}^{n=N} \frac{(1-\rho_q)*\theta^n}{n!} = 1}
#'
#' @return z:= intermediate value in calculating \eqn{\sigma_{Q_q}^2}. Note this is q in the original paper.
#' @export
#'
#' @examples
#' N <- 5
#' P_1 <- .2
#' lambda <- 3
#' mu_2 <- 4
#' rho_Q <- calc_rho_Q(N, P_1, lambda, mu_2)
#' theta <- calc_theta(rho_Q,N)
#' z <- calc_z(N, rho_Q, theta)
calc_z<- function(N,rho_Q, theta){
  val <- 0
  for(i in 0:(N-1)){
    val<- val + (i *fxn(i,N,rho_Q, theta))
  }
  return(val)
}
