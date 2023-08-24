#' Check to make sure the inputs are valid
#'
#' @param lambda := customer arrival rate.
#' @param P_1 := probability a customer requires secondary service.
#' @param mu_2 := secondary service rate.
#' @param N := the number of primary servers.
#' @param E := average total service time of a customer
#' @return text reporting success of test
#' @export
#'
#' @examples
#' lambda <- 2
#' P_1 <- .2
#' mu_2 <- 4
#' E <- 3
#' N <- 5
#' check_system(lambda, P_1,mu_2,N,E)
check_system<- function(lambda,P_1, mu_2, N, E){
  # N <= N_check
  #N_check = (lambda)/(P_1*mu_2)
  rho_q_check = (N*P_1*lambda)/mu_2
  rho_p_check = (lambda*E)
  #if (N_check < N){
    #paste0("ERROR - N must be less than: ", N_check)
  if (rho_q_check > 1){
    paste0("ERROR - utilization of secondary servers is ", rho_q_check, " must be below 1. please adjust N, P_1, or lambda lower and/or mu_2 higher.")
  }else if (rho_p_check > 1){
    paste0("ERROR - utilization of primary servers is ", rho_p_check, " must be below 1. please adjust lambda lower and/or N or mu_2 higher.")
  }else{
    paste0("Calculating Metrics - model inputs are valid.")
  }
}
