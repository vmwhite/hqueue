% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prob_delay.R
\name{prob_delay}
\alias{prob_delay}
\title{under the WAIT discipline, calculating the probability a customer has to wait for service}
\usage{
prob_delay(N, rho_Q)
}
\arguments{
\item{N}{:= the number of primary servers}

\item{rho_Q}{:= the utilization of secondary servers}
}
\value{
$$prob_delay$$ := the probability a customer has to wait for service
}
\description{
under the WAIT discipline, calculating the probability a customer has to wait for service
}
\examples{
N <- 5
P_1 <- .2
lambda <- 3
mu_2 <- 4
rho_Q <- calc_rho_Q(N, P_1, lambda, mu_2)
prob_delay <- prob_delay(N, rho_Q)
}
