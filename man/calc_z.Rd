% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_z.R
\name{calc_z}
\alias{calc_z}
\title{Under the WAIT discipline, intermediate function for calculating \eqn{\sigma_{Q_q}^2}}
\usage{
calc_z(N, rho_Q, theta)
}
\arguments{
\item{N}{:= the number of primary servers. No more than N requests for secondary service can exist simultaneously}

\item{rho_Q}{:= utilization of the secondary server. Note: this is rho_s in the original paper.}

\item{theta}{:= the singular positive root of the polynomial \eqn{\sum_{n=0}^{n=N} \frac{(1-\rho_q)*\theta^n}{n!} = 1}}
}
\value{
z:= intermediate value in calculating \eqn{\sigma_{Q_q}^2}. Note this is q in the original paper.
}
\description{
Under the WAIT discipline, intermediate function for calculating \eqn{\sigma_{Q_q}^2}
}
\examples{
N <- 5
P_1 <- .2
lambda <- 3
mu_2 <- 4
rho_Q <- calc_rho_Q(N, P_1, lambda, mu_2)
theta <- calc_theta(rho_Q,N)
z <- calc_z(N, rho_Q, theta)
}
