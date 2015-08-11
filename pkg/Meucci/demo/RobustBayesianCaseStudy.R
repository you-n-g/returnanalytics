# Example from Meucci's MATLAB script:  S_SnPCaseStudy.m
# See MATLAB package "Meucci_RobustBayesian" for original MATLAB
# source on www.symmys.com

p_m <- .1 # robustness parameter location
p_s <- .1 # robustness parameter scatter
data(SectorsSnP500)

################################################################################
# compute weekly returns
Ps <- sectorsSnP500$P[seq(1, nrow(sectorsSnP500$P), 5), ]
R <- Ps[2:nrow(Ps), ] / Ps[1:(nrow(Ps) - 1), ] - 1
Dates_P <- sectorsSnP500$DP[seq(1, length(sectorsSnP500$DP), 5)]
Dates_R <- Dates_P[-1]
Ttot <- nrow(R)
N <- ncol(R)

################################################################################
# estimation
W <- 52 # rolling estimation period

NumPortf <- 10
Ret_hat <- c()
Ret_rB <- c()
Dates <- c()
for (t in (W + 1):(Ttot - 1)) {
    Rets <- R[(t - W):t, ]

    # sample estimate
    m_hat <- colMeans(Rets)
    S_hat <- cov(Rets)
    EF <-  efficientFrontier(NumPortf, S_hat, m_hat)
    de_hat <- EF$returns
    ds_hat <- EF$volatility
    w_hat <- EF$weights
    # Bayesian prior
    S0 <- diag(diag(S_hat))
    m0 <- .5 * S0 %*% array(1, N) / N
    T <- nrow(Rets)
    T0 <- 2 * T
    nu0 <- 2 * T

    # Bayesian posterior parameters
    T1 <- T + T0
    m1 <- 1 / T1 * (m_hat * T + m0 * T0)
    nu1 <- T + nu0
    S1 <- 1 / nu1 * ( S_hat * T + S0 * nu0 + (m_hat - m0) %*% t(m_hat - m0) /
         (1 / T + 1 / T0))
    w1  <-  efficientFrontier(NumPortf, S1, m1)$weights

    # robustness parameters
    q_m2 <- chi2inv(p_m,N)
    g_m <- sqrt(q_m2 / T1 * nu1 / (nu1 - 2))
    q_s2 <- chi2inv(p_s, N * (N + 1) / 2)
    PickVol <- round(.8 * NumPortf)
    v <- (ds_hat[PickVol]) ^ 2
    g_s <- v / (  nu1 / (nu1 + N + 1) + sqrt( 2 * nu1 * nu1 * q_s2 /
           ((nu1 + N + 1) ^ 3)))
    Target <- c()

    wu <- w_hat[PickVol, ]
    Ret_hat <- c(Ret_hat, R[t + 1, ] %*% wu)

    for (k in 1:(NumPortf - 1)) {
        NewTarget <- -(10 ^ 10)
        if (t(wu) %*% S1 %*% wu <=  g_s)
            NewTarget  <-  t(m1) %*% wu - g_m * sqrt(t(wu) %*% S1 %*% wu)
        Target <- c(Target, NewTarget)
    }

    k <- which.max(Target)
    wu <- w1[k, ]
    Ret_rB <- c(Ret_rB, R[t + 1, ] %*% wu)
    Dates <- c(Dates, Dates_R[t + 1])
}

NAV_hat <- cumprod(1 + Ret_hat)
NAV_rB <- cumprod(1 + Ret_rB)

################################################################################
# plots

dev.new()
par(mfrow = c(2, 1))
plot(Dates, Ret_hat, "l", xlab = "", ylab = "")
plot(Dates, Ret_rB, "l", xlab = "", ylab = "")

dev.new()
par(mfrow = c(2, 1))
plot(Dates,NAV_hat, "l", xlab = "", ylab = "")
plot(Dates,NAV_rB, "l", xlab = "", ylab = "")
