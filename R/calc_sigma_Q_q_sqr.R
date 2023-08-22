#' Under the WAIT discipline, intermediate function for calculating \eqn{\sigma_q^2}
#'
#' @param N := the number of primary servers. No more than N requests for secondary service can exist simultaneously
#' @param sigma_2_sqr := the variance of secondary service.
#' @param mu_q := the secondary service rate
#' @param rho_Q := secondary server utilization
#' @param theta := the singular positive root of the polynomial \eqn{\sum_{n=0}^{n=N} \frac{(1-\rho_q)*\theta^n}{n!} = 1}
#'
#' @return \egn{sigma_{Qq}^2} := the variance of the total wait time for secondary service. Note: this is defined as \eqn{\sigma_s^2} in the original text
#' @export
#'
#' @examples
#' N <- 5
#' mu_q  <- 3
#' P_1 <- .2
#' lambda <- 1
#' mu_2 <- mu_q
#' sigma_2_sqr  <- sqrt(1/mu_2^2)
#' rho_Q <- calc_rho_Q(N, P_1, lambda, mu_2)
#' theta <- calc_theta(rho_Q,N)
#' calc_sigma_Q_q_sqr(N,sigma_2_sqr, mu_q,rho_Q, theta)
calc_sigma_Q_q_sqr <- function(N,sigma_2_sqr, mu_q,rho_Q, theta){
  z <- calc_z(N,rho_Q, theta)
  val <- ((1+z)*sigma_2_sqr) + ((1/mu_q)* calc_sigma_z_sqr(N,z,rho_Q, theta))
  return(val)
}
