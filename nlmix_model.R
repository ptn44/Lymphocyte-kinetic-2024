library(nlmixr2)
library(xpose)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(deSolve)
library(broom.mixed)


#B-cells----
d3=read.csv(file = 'Bcells_data.csv')
wbc <- function() {
  ini({
    ## Note that the UI can take expressions
    ## Also note that these initial estimates should be provided on the log-scale
    log_CIRC0 <- log(1.00735)
    log_KTR <- log(0.19747)
    
    ## Initial estimates should be high for SAEM ETAs
    eta.CIRC0  ~ .1
    eta.KTR  ~ .1
    
    ##  Also true for additive error (also ignored in SAEM)
    prop.err <- .1
  })
  model({
    CIRC0 = exp(log_CIRC0 + eta.CIRC0)
    KTR =  exp(log_KTR + eta.KTR)
    
    FDBK = (CIRC0 / A_circ);
    
    CIRC = A_circ;
    
    A_prol(0) = D3/D3;
    A_circ(0) = D3;
    
    d/dt(A_prol) = KTR * A_prol * FDBK - KTR * A_prol;
    d/dt(A_circ) = KTR * A_prol - KTR*A_circ;
    
    CIRC ~ prop(prop.err)
  })
}
fit.S0.2 <- nlmixr(wbc, d3, est="saem", list(print=0), table=list(cwres=TRUE, npde=TRUE))

xpdb2 = xpose_data_nlmixr(fit.S0.2)
plt2 <- dv_vs_ipred(xpdb2)
plt2+xlab('Individual prediction')+ylab('Individual observation')
p2 <- nlmixr2::vpcPlot(fit.S0.2, show=list(obs_dv=TRUE));
p2 <- p2+ ylab('Relative concentration to baseline')+xlab('Day')
p2

#T-cells----
d3=read.csv(file = 'Tcells_data.csv')
wbc <- function() {
  ini({
    ## Note that the UI can take expressions
    ## Also note that these initial estimates should be provided on the log-scale
    log_CIRC0 <- log(1.00735)
    log_KTR <- log(0.19747)
    
    ## Initial estimates should be high for SAEM ETAs
    eta.CIRC0  ~ .1
    eta.KTR  ~ .1
    
    ##  Also true for additive error (also ignored in SAEM)
    prop.err <- .1
  })
  model({
    CIRC0 = exp(log_CIRC0 + eta.CIRC0)
    KTR =  exp(log_KTR + eta.KTR)
    
    FDBK = (CIRC0 / A_circ);
    
    CIRC = A_circ;
    
    A_prol(0) = D3/D3;
    A_tr(0) = D3/D3;
    A_circ(0) = D3;
    
    d/dt(A_prol) = KTR * A_prol * FDBK - KTR * A_prol;
    d/dt(A_tr) = KTR * A_prol - KTR * A_tr;
    d/dt(A_circ) = KTR * A_tr - KTR*A_circ;
    
    CIRC ~ prop(prop.err)
  })
}
fit.S1 <- nlmixr(wbc, d3, est="saem", list(print=0), table=list(cwres=TRUE, npde=TRUE))
xpdb = xpose_data_nlmixr(fit.S1)
plt <- dv_vs_ipred(xpdb)
plt
p2 <- nlmixr2::vpcPlot(fit.S1, show=list(obs_dv=TRUE));
p2 <- p2+ ylab('Relative concentration to baseline')+xlab('Day+4')
p2

