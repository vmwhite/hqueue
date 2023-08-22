#' under the WAIT discipline, calculating the the average delay form the time of requesting secondary service until completion of secondary service
#'
#' @param W_Q := the average total wait time form the time of requesting secondary service until completion of secondary service
#' @param mu_Q := secondary service rate
#'
#' @return \eqn{W_{Q_q}}:= the average delay from the time of requesting secondary service until completion of secondary service. Note: this is D_s in the text.
#' @export
#'
#' @examples
#' N <- 5
#' P_1 <- .2
#' lambda <- 3
#' mu_2 <- 4
#' rho_Q <- calc_rho_Q(N, P_1, lambda, mu_2)
#' L_Q <- calc_L_Q(N, rho_Q)
#' W_Q <- calc_W_Q(L_Q,N,P_1,lambda)
#' mu_Q <- mu_2
#' W_Q_q <- calc_W_Q_q(W_Q, mu_Q)
calc_W_Q_q <- function(W_Q, mu_Q){
  val <- (W_Q - (1 / mu_Q))
  return(val)
}
