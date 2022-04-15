## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = TRUE,
  warning = FALSE
)

## ----mm, message = FALSE, include=FALSE---------------------------------------
library(crossnma)

## -----------------------------------------------------------------------------
head(ipddata)

## -----------------------------------------------------------------------------
head(stddata)

## -----------------------------------------------------------------------------
# jags model: code+data
mod1 <- crossnma.model(treat, id, relapse, n, design,
  prt.data = ipddata, std.data = stddata,
  reference = NULL, trt.effect = "random",
  #---------- bias adjustment ----------
  method.bias = "naive",
  #---------- assign a prior ----------
  prior = list(tau.trt='dunif(0,3)')
  )

## ---- fig.width=4.5, fig.height=5,fig.show='hold',fig.align='center'----------
netgraph(mod1)

## -----------------------------------------------------------------------------
# run jags
jagsfit1 <- crossnma(mod1,
  n.adapt = 500, n.iter = 5000, n.burnin = 2000,
  thin = 1, n.chains = 2)

## -----------------------------------------------------------------------------
knitr::kable(summary(jagsfit1, exp = FALSE), digits = 3)

## -----------------------------------------------------------------------------
oldpar <- par(mar = rep(2, 4), mfrow = c(2, 3))
plot(jagsfit1)

## -----------------------------------------------------------------------------
# jags model: code+data
mod2 <- crossnma.model(treat, id, relapse, n, design,
  prt.data = ipddata, std.data = stddata,
  reference = "A", trt.effect = "random",
  #---------- bias adjustment ----------
  method.bias = "naive",
  #----------  meta-regression ----------
  cov1 = age,
  split.regcoef = FALSE
  )

## -----------------------------------------------------------------------------
# run jags
jagsfit2 <- crossnma(mod2,
  n.adapt = 500, n.iter = 5000, n.burnin = 2000,
  thin = 1, n.chains = 2)

## -----------------------------------------------------------------------------
knitr::kable(summary(jagsfit2, exp = FALSE), digits = 3)

## ---- fig.width=6, fig.height=5,fig.show='hold',fig.align='center'------------
league(jagsfit2, exp = TRUE, cov1.value = 38)

## ---- fig.width=6, fig.height=5,fig.show='hold',fig.align='center'------------
league(jagsfit2, exp = TRUE, cov1.value = 38, direction = "long")

## -----------------------------------------------------------------------------
# jags model: code+data
mod3 <- crossnma.model(treat, id, relapse, n, design,
  prt.data = ipddata, std.data = stddata,
  reference = "D", trt.effect = "random",
  #----------  meta-regression ----------
  cov1 = age,
  split.regcoef = FALSE,
  #---------- bias adjustment ----------
  method.bias = "prior",
  run.nrs =
    list(trt.effect = "common",
         var.infl = 0.6, mean.shift = 0,
         n.adapt = 500, n.iter = 10000, n.burnin = 4000,
         thin = 1, n.chains = 2)
  )

## -----------------------------------------------------------------------------
# run jags
jagsfit3 <- crossnma(mod3,
  n.adapt = 500, n.iter = 5000, n.burnin = 2000,
  thin = 1, n.chains = 2)

## ---- fig.width=6, fig.height=5,fig.show='hold',fig.align='center'------------
heatplot(jagsfit3, exp = TRUE, cov1.value = 38,
  size = 6, size.trt = 20, size.axis = 12)

## -----------------------------------------------------------------------------
# jags model: code+data
mod4 <- crossnma.model(treat, id, relapse, n, design,
  prt.data = ipddata, std.data = stddata,
  reference = "A", trt.effect = "random",
  #---------- bias adjustment ----------
  method.bias = 'adjust1',
  bias.type = 'add',
  bias.effect = 'common',
  bias = rob,
  unfav = unfavored,
  bias.group = bias.group,
  bias.covariate = year
)

## -----------------------------------------------------------------------------
# run jags
jagsfit4 <- crossnma(mod4,
  n.adapt = 500, n.iter = 5000, n.burnin = 2000,
  thin = 1, n.chains = 2)

## -----------------------------------------------------------------------------
knitr::kable(summary(jagsfit4, exp = FALSE), digits = 3)

## -----------------------------------------------------------------------------
# jags model: code+data
mod5 <- crossnma.model(treat, id, relapse, n, design,
  prt.data = ipddata, std.data = stddata,
  reference = "A", trt.effect = "random",
  #---------- bias adjustment ----------
  method.bias = 'adjust2',
  bias.type = 'add',
  bias = rob,
  unfav = unfavored,
  bias.group = bias.group,
)

## -----------------------------------------------------------------------------
# run jags
jagsfit5 <- crossnma(mod5,
  n.adapt = 500, n.iter = 5000, n.burnin = 2000,
  thin = 1, n.chains = 2)

## -----------------------------------------------------------------------------
knitr::kable(summary(jagsfit5, exp = FALSE), digits = 3)

## -----------------------------------------------------------------------------
par(oldpar)

