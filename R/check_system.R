#' Check to make sure the inputs are valid
#'
#' @param lambda := customer arrival rate.
#' @param P_1 := probability a customer requires secondary service.
#' @param mu_2 := secondary service rate.
#' @param N := the number of primary servers.
#'
#' @return text reporting success of test
#' @export
#'
#' @examples
#' lambda <- 2
#' P_1 <- .2
#' mu_2 <- 4
#' N <- 5
#' check_system(lambda, P_1,mu_2,N)
check_system<- function(lambda,P_1, mu_2, N){
  # N <= N_check
  N_check = (lambda)/(P_1*mu_2)
  if (N_check < N){
    paste0("ERROR - N must be less than: ", N_check)
  }else{
    paste0("Calculating Metrics - model inputs are valid.")
  }

}
