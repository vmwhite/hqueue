#' Under the WAIT discipline, calculating  the variance of the total queue
#'
#' @param W_q  := the expected value of a secondary customer's service wait time from requesting secondary service until completion of service. Note this is W_s in the original paper.
#' @param P_1  := the probability the customer needs secondary service
#' @param sigma_Q_q_sqr := The conditional variance of the wait for secondary service. Note: this is (sigma_s)^2 in the original paper
#' @param sigma_p_sqr := The variance of the primary service time. Note: this is (sigma_1)^2 in the original paper
#'
#' @return (sigma)^2 := the service time variance
#' @export
#'
#' @examples
#' N <- 5
#' P_1 <- .2
#' lambda <- 3
#' mu_1 <- 6
#' mu_2 <- 4
#' mu_q <- mu_2
#' sigma_2_sqr  <- .04
#' rho_Q <- calc_rho_Q(N, P_1, lambda, mu_2)
#' L_Q <- calc_L_Q(N, rho_Q)
#' theta <- calc_theta(rho_Q,N)
#' W_q <- calc_W_Q(L_Q, N, P_1, lambda)
#' sigma_Q_q_sqr <- calc_sigma_Q_q_sqr(N,sigma_2_sqr, mu_q,rho_Q, theta)
#' sigma_p_sqr <- 1/(mu_1^2)
calc_sigma_sqr<- function(W_q, P_1, sigma_Q_q_sqr,sigma_p_sqr ){
  val <- (P_1)*(1-P_1)*(W_q^2)
  val <- val + (P_1* sigma_Q_q_sqr)
  val <- val + sigma_p_sqr
  return(val)
}
