#' Calculation of Hierarchical Queue outputs from Willemain (1974).
#'
#' @param N := number of primary servers
#' @param lambda :customer arrival rate
#' @param P_1 := percentage of customers that rwuqire secondary service
#' @param mu_1 := primary service rate
#' @param mu_2 := secondary service rate
#' @param sigma_1 := standard deviation of primary service rate, default is for exponential distribution
#' @param sigma_2 := standard deviation of secondary service rate, default is for exponential distribution
#'
#' @return A table of the queue outputs in a dataframe
#' @export
#'
#' @examples
#' lambda <- 2
#' P_1 <- .2
#' mu_1 <- 4
#' sigma_1 <- 1
#' mu_2 <- 4
#' sigma_2 <- 1
#' N <- 5
#' results <- h_queue(N,lambda, P_1,mu_1, mu_2, sigma_1, sigma_2)
h_queue <- function(N,lambda, P_1,mu_1, mu_2, sigma_1= sqrt(1/(mu_1^2)), sigma_2 =sqrt(1/(mu_2^2)) ){
  #store input metrics
  metrics <- list("S","lambda", "P_1", "mu_1", "mu_2", "sigma_1", "sigma_2")
  results <- list(N,lambda, P_1,mu_1, mu_2, sigma_1, sigma_2)

  #check the system is valid
  ##solve for rho_Q, utilization of secondary server
  rho_Q <- calc_rho_Q(N,P_1,lambda,mu_2)
  ##solve for L_Q, number of secondary customers in the system
  L_Q <- calc_L_Q(N,rho_Q)
  ##solve for W_q, secondary customer average time in the system
  W_Q <-  calc_W_Q(L_Q,N,P_1,lambda)
  ##solve for rho_P, utilization of primary servers
  E <- calc_E(mu_1,P_1,W_Q)
  rho_P <- calc_rho_p(lambda,E)
  skip<- FALSE
  if (grepl(  "ERROR",print(check_system(rho_Q, rho_P)), fixed = TRUE) == TRUE){
    skip <- TRUE
  }
    if (skip == FALSE){
    #Solve for metrics
    ##solve for C, system capacity
    results <- append(results, calc_C(N,P_1,mu_1,mu_2))
    ##solve for rho_Q, utilization of secondary server
    rho_Q <- calc_rho_Q(N,P_1,lambda,mu_2)
    results <- append(results, rho_Q)
    ##solve for L_Q, number of secondary customers in the system
    L_Q <- calc_L_Q(N,rho_Q)
    results <- append(results, L_Q)
    ##solve for W_q, secondary customer average time in the system
    W_Q <-  calc_W_Q(L_Q,N,P_1,lambda)
    results <- append(results, W_Q)
    ##solve for rho_P, utilization of primary servers
    E <- calc_E(mu_1,P_1,W_Q)
    rho_P <- calc_rho_p(lambda,E)
    results <- append(results, rho_P )
    ##solve for L_p, number of primary customers in the system
    theta <- calc_theta(rho_Q,N)
    sigma_Q_q_sqr <- calc_sigma_Q_q_sqr(N,sigma_2^2,mu_2,rho_Q, theta)
    simga_p_sqr <- (sigma_1^2)
    sigma_sqr <- calc_sigma_sqr(W_Q,P_1,sigma_Q_q_sqr,simga_p_sqr)
    L_p <- calc_L_p(rho_P,lambda,sigma_sqr)
    results <- append(results, L_p)
    ##solve for W_p, average time in system of primary customers
    W_p <- calc_W_p(L_p,lambda)
    results <- append(results, W_p)
    ##solve for D_p, average time in queue (i.e., delay) for primary customers
    results <- append(results, calc_W_p_q(W_p,E))
    ##solve for D_q, average time in queue (i.e., delay) for secondary customers
    results <- append(results, calc_W_Q_q(W_Q,mu_2))
    ## solve for probabability of delay
    results <- append(results, prob_delay(N,rho_Q))
  }else{
      for (i in 1:10){
        results <- append(results, "unstable")
      }
  }
  #reformat results
  #create DF of results
  DF <- data.frame(results)
  # set column names
  metrics <- append(metrics, "C" )
  metrics <- append(metrics, "rho_Q" )
  metrics <- append(metrics, "L_Q" )
  metrics <- append(metrics, "W_Q" )
  metrics <- append(metrics, "rho_P" )
  metrics <- append(metrics, "L_p" )
  metrics <- append(metrics, "W_p" )
  metrics <- append(metrics, "W_P_q" )
  metrics <- append(metrics, "W_Q_q" )
  metrics <- append(metrics, "prob_delay")
  colnames(DF) <- metrics
  return(DF)

}
