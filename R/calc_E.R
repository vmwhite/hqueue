#' Under the WAIT discipline, calculating the average total service time
#'
#' @param mu_p := the service time of the primary queue
#' @param P_1  := the probability the customer needs secondary service
#' @param W_q  := the expected value of a secondary customer's service wait time from requesting secondary service until completion of service

#'
#' @return E := the average total service time
#' @export
#'
#' @examples
#' mu_p <-5
#' P_1 <- .3
#' W_q <- 10
#' E <- calc_E(mu_p,P_1,W_q)
calc_E <- function(mu_p, P_1,W_q){
  val <- (1/mu_p) + (P_1*W_q)
  return(val)
}
