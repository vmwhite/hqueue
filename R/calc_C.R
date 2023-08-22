#' under WAIT discipline calculating system capacity
#'
#' @param N   := the number of primary servers. No more than N requests for secondary service can exist simultaneously
#' @param P_1 := the probability a call requires  secondary service
#' @param mu_1 := the primary service rate
#' @param mu_2 := the secondary service rate
#'
#' @return C := system capacity (customers/time)
#' @export
#'
#' @examples
#' N <- 5
#' P_1 <- .3
#' mu_1 <- 2
#' mu_2 <- 4
#' C <- calc_C(N,P_1, mu_1,mu_2)
calc_C <- function(N,P_1, mu_1,mu_2){
    lambda_1<- mu_2 / (N*P_1)
    coef <- c(-1,-(1/mu_1),1)
    roots <- polyroot(coef)
    lambda_2 <- c()
    for (j in 1:length(roots)){
      if (Im(zapsmall(roots[j]))==0) lambda_2<- append(lambda_2, as.numeric(Re(roots[j])))
    }
    lambda_2 <- lambda_2[which( lambda_2 > 0)]
    lambda_max <- max(lambda_1, lambda_2)
    C <- N*lambda_max
    return(C)
}
