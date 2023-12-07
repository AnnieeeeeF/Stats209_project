# Computes the AIPW estimator for the ATE on input dataset.
compute_aipw <- function(dat) {
    # Create estimators
    mu1 <- lm(SEGRADES ~ SCSELF + RCNOW + CPSNOW + PAAHOME + as.factor(SEX) + AGE2004 + as.factor(RACEETHN) + as.factor(MOMEDUC) + as.factor(MOMSTAT) + as.factor(ZIPURB) + HGOVCUR, data = dat, subset = HGOVCUR==1)

    mu0 <- lm(SEGRADES ~ SCSELF + RCNOW + CPSNOW + PAAHOME + as.factor(SEX) + AGE2004 + as.factor(RACEETHN) + as.factor(MOMEDUC) + as.factor(MOMSTAT) + as.factor(ZIPURB) + HGOVCUR, data = dat, subset = HGOVCUR==0)

    dat$mu1pred <- predict(mu1, dat)
    dat$mu0pred <- predict(mu0, dat)

    # Compute AIPW ATE estimator
    mu1_dr <- mean(dat$HGOVCUR * (dat$SEGRADES - dat$mu1pred) / dat$prop + dat$mu1pred)

    mu0_dr <- mean((1 - dat$HGOVCUR) * (dat$SEGRADES - dat$mu0pred) / dat$prop + dat$mu0pred)

    return(mu1_dr - mu0_dr)
}

# Executes Augmented Inverse Probability Weighting analysis.
# Returns ATE and variance estimator.
# If bootstrap_variance is FALSE, variance estimator is set to 0.
# compute_prop should be set to FALSE if input dat has prop variable.
aipw_analysis <- function(dat, compute_prop = FALSE, bootstrap_variance = FALSE) {
    if (compute_prop) {
        # Compute propensity score
        dat$prop <- glm(HGOVCUR ~ SCSELF + RCNOW + CPSNOW + PAAHOME + as.factor(SEX) + AGE2004 + as.factor(RACEETHN) + as.factor(MOMEDUC) + as.factor(MOMSTAT) + as.factor(ZIPURB), data=dat, family = "binomial")$fitted.values
    }

    ate <- compute_aipw(dat)
    variance <- 0

    return(c(ate, variance))
}