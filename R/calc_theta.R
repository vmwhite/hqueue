#' Under the WAIT discipline, calculate \eqn{\theta}
#'
#' @param rho_Q :=  utilization of the secondary server. Note: this is rho_s in the original paper.
#' @param N  := the number of primary servers. No more than N requests for secondary service can exist simultaneously
#'
#' @return \eqn{\theta} :=the singular positive root of the polynomial \eqn{\sum_{n=0}^{n=N} \frac{(1-\rho_q)*\theta^n}{n!} = 1}
#' @export
#'
#' @examples
#' N <- 5
#' P_1 <- .2
#' lambda <- 3
#' mu_2 <- 4
#' rho_Q <- calc_rho_Q(N, P_1, lambda, mu_2)
#' theta <- calc_theta(rho_Q,N)
calc_theta <- function(rho_Q,N){
  z<- c()
  z <- append(z,-1+ ((1-rho_Q)/(hqueue::facto(0))))
  for (i in 1:N){
    z <- append(z,(1-rho_Q)/(hqueue::facto(i)))
  }
  roots <- polyroot(z)
  theta <- c()
  for (j in 1:length(roots)){
    if (Im(zapsmall(roots[j]))==0) theta <- append(theta, as.numeric(Re(roots[j])))
  }
  theta <- theta[which( theta > 0)]
  return(theta)
}
