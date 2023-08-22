
#' Under the WAIT discipline, Calculate the expected delay for the primary service
#'
#' @param W_p := the average primary service wait time
#' @param E := the average total service time
#'
#' @return W_p_q := the expected delay for the primary service. Note: this is D_p in the original paper
#' @export
#'
#' @examples
#' W_p <- 12
#' E <- 50
#' W_p_q <- calc_W_p_q(W_p,E)
calc_W_p_q <- function(W_p, E){
  val <- W_p - E
  return(val)
}
