#' Check to make sure the inputs are valid
#'
#' @param rho_p utilization of primary servers
#' @param rho_q utilization of secondary servers
#' @return text reporting success of test
#' @export
#'
#' @examples
#' rho_q <- .4
#' rho_p <- .6
#' check_system(rho_q, rho_p)
check_system<- function(rho_q, rho_p){
  if (rho_q > 1){
    paste0("ERROR - utilization of secondary servers is ", rho_q, " must be below 1. please adjust N, P_1, or lambda lower and/or mu_2 higher.")
  }else if (rho_p > 1){
    paste0("ERROR - utilization of primary servers is ", rho_p, " must be below 1. please adjust lambda lower and/or N or mu_2 higher.")
  }else{
    paste0("Calculating Metrics - model inputs are valid.")
  }
}
