#' under the WAIT discipline, calculating the average total wait time form the time of requesting secondary service until completion of secondary service
#'
#' @param L_Q := the average number of secondary customers in the system
#' @param N   := the number of primary servers. No more than N requests for secondary service can exist simultaneously
#' @param P_1 := the probability a call requires  secondary service
#' @param lambda := the customer arrival rate
#'
#' @return $$W_q$$ := the average total wait time form the time of requesting secondary service until completion of secondary service
#' @export
#'
#' @examples
#' N <- 5
#' P_1 <- .2
#' lambda <- 3
#' mu_2 <- 4
#' rho_Q <- calc_rho_Q(N, P_1, lambda, mu_2)
#' L_Q <- calc_L_Q(N, rho_Q)
#' W_q <- calc_W_Q(L_Q, N, P_1, lambda)
calc_W_Q <- function(L_Q, N, P_1, lambda){
  val <- (L_Q / (N*P_1*lambda))
  return(val)
}
