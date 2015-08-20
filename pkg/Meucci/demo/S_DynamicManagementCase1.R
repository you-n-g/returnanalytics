#' This script is the first of two case studies using the techniques described
#' in A. Meucci, "Dynamic Portfolio Management with Views at Multiple Horizons"
#' It considers only one risk driver (the 10 year rate) and only one view: that
#' the expected value of the 10-year rate will be the actual value minus
#' 50 basis points at t^view = 1 year from today.
#'
#' @references
#' A. Meucci - "Dynamic Portfolio Management with Views at Multiple Horizons"
#'' \url{http://symmys.com/node/831}
#'
#' See Meucci's script "S_CaseStudy1.m"
#
#' @author Xavier Valls \email{xaviervallspla@@gmail.com}

#load the Calibrated Parameters
data(dynamicManagement)

mu <- dynMan$calibratedParameters$mu[1]
theta <- dynMan$calibratedParameters$theta[1, 1]
sig2 <- dynMan$calibratedParameters$sig2[1, 1]
mu_LT <- solve(theta) %*% mu

#load the simulated path for the 10y Government rate (this is equal to its
# shadow rate)
x0 <- dynMan$pathDaily$X.path[1]

################################################################################
#set the trading

# trading frequency (daily)
tau <- 1 / 252
# effective future portfolio horizon (in years)
T_Hor <- 0.5
# effective number of future tradings at any point in time
tau_ <- length(seq( 0, T_Hor, tau))
# number of risk drivers
n_ <- 1

################################################################################
#set the view at time 0
t_view0 <- 1                            # time of the view at time 0 (in years)
mu_x10y <- x0 - 0.5 / 100                  # view on the 10y rate at time 0
t <- matrix(c(seq(0,T_Hor, tau), t_view0)) # monitoring times
t_ <- length(t)                            # number of monitoring times 

################################################################################
#set the parameters for optimization
gamma <- 10^-2                     # risk aversion parameter 
eta <- 0.5                         # weight of the market impact of transaction
lambda <- log(2) / 20              # discount (half life 20*tau)
b_legacy <- 0                      # legacy portfolio

################################################################################
#compute the prior at time 0
Prior0 <- MVOU_Prior(t, x0, theta, sig2, mu)
#matrix of market impact
c2 <- Prior0$cov[(n_ + 1):(2 * n_), (n_ + 1):(2 * n_)]

###LLEVAR UNA VEGADA COMPROVAT
x <- dynMan$pathDaily$X.path
t_view <- t_view0
view <- mu_x10y
################################################################################
################################################################################
# Prior and Posterior Optimal exposure with Market Impcat of transaction.
# SOLVING THE BELLMAN EQUATION

b_MI_Bellman <- BellmanEq_CS1(eta, gamma, lambda, tau, theta, mu, sig2, c2,
                            b_legacy, dynMan$pathDaily$X.path, t_view0, mu_x10y)

################################################################################
################################################################################
#CALCULUS OF VARIATION

################################################################################
#initialize variables

#prior
#optimal 1-period myopic prior solution
b_NoMI_prior <- matrix(NaN, tau_-1, n_)
#optimal prior solution with MI (Calculus of Variation)
b_MI_Vc_prior <- matrix(NaN, tau_-1, n_)
b_legacy_prior <- b_legacy
b_legacy_prior_LR <- b_legacy
b_legacy_prior_xt <- b_legacy

#posterior
#optimal 1-period myopic posterior solution
b_NoMI_post <- matrix(NaN, tau_-1, n_)
#Decomposition of the optimal solution in absence of market impact
b_NoMI_LongTerm <- matrix(NaN, tau_-1, n_)
#Decomposition of the optimal solution in absence of market impact
b_NoMI_viewMean <- matrix(NaN, tau_-1, n_)
#optimal posterior solution with MI (Calculus of Variation)
b_MI_Vc_post <- matrix(NaN, tau_-1, n_)
b_legacy_post <- b_legacy
b_legacy_post_LR <- b_legacy
b_legacy_post_xt <- b_legacy
X_path <- dynMan$pathDaily$X.path
################################################################################
for(i in 1:(tau_-1)) {
    t_roll <- cbind( seq(0, T_Hor, tau), (t_view0 - tau * (i - 1)))

    #######################################################################
    ##The Prior
    #######################################################################
    #compute the prior
    Prior <- MVOU_Prior(t_roll, matrix(X_path[i, ]), theta, sig2, mu) 
    Mean_prior <- t(matrix(Prior$mean[1:(t_ * n_)], nrow = n_, ncol = t_))        

    ############################################################################
    ## No Market impact myopic 1 period solution
    ER_prior <- Mean_prior[2, 1:n_] - Mean_prior[1, 1:n_]
    sig2_1 <- Prior$cov[(n_ + 1):(2 * n_),(n_ + 1):(2 * n_)]
    b_NoMI_prior[i, 1:n_] <- solve(gamma * sig2_1) %*% t(ER_prior[1:n_])   

    ############################################################################
    ## Market impact: calculus of variation
    l_t <- - exp(-lambda %*% t(seq(0, tau_ - 2, 1))) *
             diff(Mean_prior[1:(length(Mean_prior) - 1)])
    l_t[1] <- l_t[1] - eta * c2 * b_legacy_prior
    q_t <- QuadraticMat_Vc(lambda, gamma, eta, Prior$cov, c2, tau_ - 1, n_)
    b_MI_Vc_prior_tmp <- solve(2 * q_t) %*% l_t
    b_MI_Vc_prior[i, 1:n_] <- b_MI_Vc_prior_tmp[1:n_]
    b_legacy_prior <- b_MI_Vc_prior[i, 1:n_]

    #######################################################################
    ##The Posterior
    #######################################################################

    #######################################################################
    #set the rolling view 
    grid <- meshgrid(t_roll, 1:n_)
    T <- grid$X
    N <- grid$Y
    N_Meanviews <- 1  #Number of views on expectations
    v_tmp <- array(0, dim = c(N_Meanviews, n_, T_))
    v_tmp[1, 1, length(v_tmp)] <- 1
    mu_view <- view[1]
    v_mu <- matrix(v_tmp, nrow = nrow(v_tmp))
    views <- list()
    views$N_Meanviews <-  N_Meanviews
    views$N_Covviews <- c()
    views$dimension <- array(N)
    views$monitoring_time <- array(T)
    views$v_mu <- v_mu
    views$v_sig <- NaN
    views$mu_view <- mu_view
    views$sig2_view <- c()
    ############################################################################
    # compute the posterior moments
    Posterior <- MVOU_Posterior(Prior, views)
    Mean_post <- t(matrix(Posterior$mean[1:(t_ * n_)], nrow = n_, ncol = t_))

    ############################################################################
    ## No Market impact myopic 1 period solution
    ER_post = Mean_post[2, 1:n_] - Mean_post[1, 1:n_]
    sig2_1 = Posterior$cov(n_+1:2*n_,n_+1:2*n_)
    b_NoMI_post[i, 1:n_] = solve(gamma*sig2_1) %*% t(ER_post[1:n_])

    ############################################################################
    ## Market impact: calculus of variation
    l_t = -exp(t(-lambda * t(seq(0, tau_ - 2, 1)))) *
          diff(Mean_post[1:(length(Mean_post) - 1)])
    l_t[1] = l_t[1] - eta * c2 * b_legacy_post
    q_t = QuadraticMat_Vc(lambda, gamma, eta, Posterior$cov, c2, tau_-1, n_)
    b_MI_Vc_post_tmp = solve(2 * q_t) %*% l_t
    b_MI_Vc_post[i, 1:n_] = b_MI_Vc_post_tmp[1:n_]
    b_legacy_post = b_MI_Vc_post[i, 1:n_]
   
    ############################################################################
    ## LongTerm and viewMean contributions of the optimal solution with views in
    ## absence of market impact of transactions 
    b_NoMI_LongTerm[i, 1:n_] = 2 * theta / (gamma * sig2) / 
                               (1 + exp(-theta * tau)) * 
                               (1 - (1 + exp(theta * tau)) /
                               (1 + exp(theta * (t_roll[length(t_roll)])))) *
                               (mu / theta - dynMan$pathDaily$X.path[i, 1])
    b_NoMI_viewMean[i, 1:n_] = 2 * theta / (gamma * sig2) * exp(theta*tau) / 
                               (exp(theta * t_roll[length(t_roll)]) - 
                               exp(-theta * t_roll[length(t_roll)])) * 
                               (mu_view - dynMan$pathDaily$X.path[i, 1])
}

################################################################################
################################################################################

# save ('data\CaseStudy1.mat','X_path','t','mu_LT','mu_view','b_NoMI_prior',
#    'b_NoMI_post',...
#    'b_MI_Vc_prior','b_MI_Vc_post','b_MI_Bellman_prior','b_MI_Bellman_post',
#    'b_NoMI_LongTerm','b_NoMI_viewMean','tau_');

