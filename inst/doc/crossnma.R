## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = TRUE,
  warning = FALSE
  )
options(knitr.kable.NA = ".")

## ----setup, message = FALSE, echo = FALSE-------------------------------------
library("crossnma")
set.seed(1910)
settings.meta(digits = 3)
cilayout("(", " to ")

## -----------------------------------------------------------------------------
dim(ipddata)
head(ipddata)

## -----------------------------------------------------------------------------
stddata

## -----------------------------------------------------------------------------
# JAGS model: code + data
mod1 <- crossnma.model(treat, id, relapse, n, design,
  prt.data = ipddata, std.data = stddata,
  #---------- bias adjustment ----------
  method.bias = "naive",
  #---------- assign a prior ----------
  prior.tau.trt = "dunif(0, 3)",
  #---------- SUCRA ----------
  sucra = TRUE, small.values = "desirable"
  )

## ----fig.width=4.5, fig.height=5,fig.show='hold',fig.align='center'-----------
netgraph(mod1, cex.points = n.trts, adj = 0.5, plastic = FALSE,
 number = TRUE, pos.number.of.studies = c(0.5, 0.4, 0.5, 0.5, 0.6, 0.5))

## -----------------------------------------------------------------------------
# Run JAGS
jagsfit1 <- crossnma(mod1, n.iter = 5000, n.burnin = 2000, thin = 1)
jagsfit1

## ----echo = FALSE-------------------------------------------------------------
knitr::kable(summary(jagsfit1, backtransf = FALSE), digits = 3)

## -----------------------------------------------------------------------------
par(mar = rep(2, 4), mfrow = c(2, 3))
plot(jagsfit1)

## -----------------------------------------------------------------------------
# JAGS model: code + data
mod2 <- crossnma.model(treat, id, relapse, n, design,
  prt.data = ipddata, std.data = stddata,
  #---------- bias adjustment ----------
  method.bias = "naive",
  #----------  meta-regression ----------
  cov1 = age,
  split.regcoef = FALSE
  )

## -----------------------------------------------------------------------------
# Run JAGS
jagsfit2 <- crossnma(mod2, n.iter = 5000, n.burnin = 2000, thin = 1)

## ----echo = FALSE-------------------------------------------------------------
knitr::kable(summary(jagsfit2, backtransf = FALSE), digits = 3)

## ----fig.width=6, fig.height=5,fig.show='hold',fig.align='center'-------------
league(jagsfit2, cov1.value = 38, digits = 2)

## ----fig.width=6, fig.height=5,fig.show='hold',fig.align='center'-------------
league(jagsfit2, cov1.value = 38, digits = 2, direction = "long")

## -----------------------------------------------------------------------------
# JAGS model: code + data
mod3 <- crossnma.model(treat, id, relapse, n, design,
  prt.data = ipddata, std.data = stddata,
  reference = "D",
  #----------  meta-regression ----------
  cov1 = age,
  split.regcoef = FALSE,
  #---------- bias adjustment ----------
  method.bias = "prior",
  run.nrs.trt.effect= "common",
  run.nrs.var.infl = 0.6, run.nrs.mean.shift = 0,
  run.nrs.n.iter = 10000, run.nrs.n.burnin = 4000,
  run.nrs.thin = 1, run.nrs.n.chains = 2
  )

## -----------------------------------------------------------------------------
# Run JAGS
jagsfit3 <- crossnma(mod3, n.iter = 5000, n.burnin = 2000, thin = 1)

## ----fig.width=6, fig.height=5,fig.show='hold',fig.align='center'-------------
heatplot(jagsfit3, cov1.value = 38,
  size = 6, size.trt = 20, size.axis = 12)

## -----------------------------------------------------------------------------
# JAGS model: code + data
mod4 <- crossnma.model(treat, id, relapse, n, design,
  prt.data = ipddata, std.data = stddata,
  #---------- bias adjustment ----------
  method.bias = "adjust1",
  bias.type = "add",
  bias.effect = "common",
  bias = rob,
  unfav = unfavored,
  bias.group = bias.group,
  bias.covariate = year
)

## -----------------------------------------------------------------------------
# Run JAGS
jagsfit4 <- crossnma(mod4, n.iter = 5000, n.burnin = 2000, thin = 1)

## ----echo = FALSE-------------------------------------------------------------
knitr::kable(summary(jagsfit4, backtransf = FALSE), digits = 3)

## -----------------------------------------------------------------------------
# JAGS model: code + data
mod5 <- crossnma.model(treat, id, relapse, n, design,
  prt.data = ipddata, std.data = stddata,
  #---------- bias adjustment ----------
  method.bias = "adjust2",
  bias.type = "add",
  bias = rob,
  unfav = unfavored,
  bias.group = bias.group
)

## -----------------------------------------------------------------------------
# Run JAGS
jagsfit5 <- crossnma(mod5, n.iter = 5000, n.burnin = 2000, thin = 1)

## ----echo = FALSE-------------------------------------------------------------
knitr::kable(summary(jagsfit5, backtransf = FALSE), digits = 3)

## ----echo = FALSE, message = FALSE--------------------------------------------
tools::compactPDF(path = ".", gs_quality = "ebook")

