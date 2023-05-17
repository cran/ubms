## ---- echo=FALSE--------------------------------------------------------------
knitr::knit_hooks$set(time_it = local({
  now <- NULL
  function(before, options) {
    if (before) {
      # record the current time before each chunk
      now <<- Sys.time()
    } else {
      # calculate the time difference after a chunk
      res <- difftime(Sys.time(), now)
      # return a character string to show the time
      paste("Time for this code chunk to run:", res)
    }
  }
}))
knitr::opts_chunk$set(message=FALSE, warning=FALSE)

## -----------------------------------------------------------------------------
library(unmarked)
data(crossbill)

## -----------------------------------------------------------------------------
dim(crossbill)
names(crossbill)

## -----------------------------------------------------------------------------
site_covs <- crossbill[,c("id", "ele", "forest")]

## -----------------------------------------------------------------------------
y <- crossbill[,c("det991","det992","det993")]
head(y)

## -----------------------------------------------------------------------------
date <- crossbill[,c("date991","date992","date993")]

## -----------------------------------------------------------------------------
umf <- unmarkedFrameOccu(y=y, siteCovs=site_covs, obsCovs=list(date=date))
head(umf)

## -----------------------------------------------------------------------------
(fit_unm <- occu(~1~1, data=umf))

## ---- eval=FALSE--------------------------------------------------------------
#  library(ubms)
#  
#  (fit_stan <- stan_occu(~1~1, data=umf, chains=3, iter=500, cores=3, seed=123))

## ---- echo=FALSE--------------------------------------------------------------
library(ubms)
(fit_stan <- stan_occu(~1~1, data=umf, chains=3, iter=500, refresh=0, seed=123))

## -----------------------------------------------------------------------------
cbind(unmarked=coef(fit_unm), stan=coef(fit_stan))

## -----------------------------------------------------------------------------
fit_stan

## -----------------------------------------------------------------------------
sum_tab <- summary(fit_stan, "state")
sum_tab$mean[1]

## -----------------------------------------------------------------------------
names(fit_stan)
occ_intercept <- extract(fit_stan, "beta_state[(Intercept)]")[[1]]
hist(occ_intercept, freq=FALSE)
lines(density(occ_intercept), col='red', lwd=2)

## ---- eval=FALSE--------------------------------------------------------------
#  fit_null <- fit_stan
#  
#  fit_global <- stan_occu(~scale(date)~scale(forest)+scale(ele), data=umf,
#                          chains=3, iter=500, seed=123)

## ---- echo=FALSE--------------------------------------------------------------
fit_null <- fit_stan

## ---- warning=TRUE, echo=FALSE------------------------------------------------
fit_global <- stan_occu(~scale(date)~scale(forest)+scale(ele), data=umf,
                        chains=3, iter=500, refresh=0, seed=123)

## -----------------------------------------------------------------------------
mods <- fitList(fit_null, fit_global)

## -----------------------------------------------------------------------------
round(modSel(mods), 3)

## -----------------------------------------------------------------------------
loo(fit_global)

## -----------------------------------------------------------------------------
waic(fit_global)

## -----------------------------------------------------------------------------
fit_top <- fit_global
fit_top

## -----------------------------------------------------------------------------
traceplot(fit_top, pars=c("beta_state", "beta_det"))

## -----------------------------------------------------------------------------
plot_residuals(fit_top, submodel="state")

## -----------------------------------------------------------------------------
plot_residuals(fit_top, submodel="state", covariate="forest")

## -----------------------------------------------------------------------------
(fit_top_gof <- gof(fit_top, draws=100, quiet=TRUE))
plot(fit_top_gof)

## -----------------------------------------------------------------------------
sim_y <- posterior_predict(fit_top, "y", draws=100)
dim(sim_y)

## -----------------------------------------------------------------------------
prop0 <- apply(sim_y, 1, function(x) mean(x==0, na.rm=TRUE))

## -----------------------------------------------------------------------------
actual_prop0 <- mean(getY(fit_top) == 0, na.rm=TRUE)

#Compare
hist(prop0, col='gray')
abline(v=actual_prop0, col='red', lwd=2)

## -----------------------------------------------------------------------------
plot_effects(fit_top, "state")
plot_effects(fit_top, "det")

## -----------------------------------------------------------------------------
head(predict(fit_top, submodel="state"))

## -----------------------------------------------------------------------------
nd <- data.frame(ele=100, forest=25)
predict(fit_top, submodel="state", newdata=nd)

## -----------------------------------------------------------------------------
zpost <- posterior_predict(fit_top, "z", draws=100)
dim(zpost)

## -----------------------------------------------------------------------------
group1 <- rowMeans(zpost[,1:50], na.rm=TRUE)
group2 <- rowMeans(zpost[,51:100], na.rm=TRUE)

plot_dat <- rbind(data.frame(group="group1", occ=mean(group1),
                             lower=quantile(group1, 0.025),
                             upper=quantile(group1, 0.975)),
                  data.frame(group="group2", occ=mean(group2),
                             lower=quantile(group2, 0.025),
                             upper=quantile(group2, 0.975)))


## -----------------------------------------------------------------------------
library(ggplot2)

ggplot(plot_dat, aes(x=group, y=occ)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2) +
  geom_point(size=3) +
  ylim(0,1) +
  labs(x="Group", y="Occupancy + 95% UI") +
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        axis.text=element_text(size=12), axis.title=element_text(size=14))

