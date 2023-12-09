# Computes the AIPW estimator for the ATE on input dataset.
compute_aipw <- function(dat) {
    # Create estimators
    mu1 <- lm(SEGRADES ~ SCSELF + RCNOW + CPSNOW + PAAHOME + as.factor(SEX) + AGE2004 + as.factor(RACEETHN) + as.factor(MOMEDUC) + as.factor(MOMSTAT) + as.factor(ZIPURB),
              data = dat,
              subset = HGOVCUR == 1)

    mu0 <- lm(SEGRADES ~ SCSELF + RCNOW + CPSNOW + PAAHOME + as.factor(SEX) + AGE2004 + as.factor(RACEETHN) + as.factor(MOMEDUC) + as.factor(MOMSTAT) + as.factor(ZIPURB),
              data = dat,
              subset = HGOVCUR == 0)

    mu1pred <- predict(mu1, dat)
    mu0pred <- predict(mu0, dat)

    # Compute AIPW ATE estimator
    mu1_dr <- mean(dat$HGOVCUR * (dat$SEGRADES - mu1pred) / dat$prop + mu1pred)

    mu0_dr <- mean((1 - dat$HGOVCUR) * (dat$SEGRADES - mu0pred) / dat$prop + mu0pred)

    return(mu1_dr - mu0_dr)
}

# Executes Augmented Inverse Probability Weighting analysis.
# Returns ATE and variance estimator.
# If n_bootstrap is 0, variance estimator is set to -1.
aipw_analysis <- function(dat, n_bootstrap = 0) {
    ate <- compute_aipw(dat)
    variance <- -1

    if (n_bootstrap > 0) {
        treated_pop <- subset(dat, HGOVCUR == 1)
        control_pop <- subset(dat, HGOVCUR == 0)
        n1 <- nrow(treated_pop)
        n0 <- nrow(control_pop)

        bootstrapped_estimates <- numeric(n_bootstrap)
        for (i in 1:n_bootstrap) {
            treated_boot <- treated_pop[sample(n1, n1, replace = TRUE), ]
            control_boot <- control_pop[sample(n0, n0, replace = TRUE), ]
            dat_boot <- rbind(treated_boot, control_boot)

            bootstrapped_estimates[i] <- compute_aipw(dat_boot)
        }
        variance <- var(bootstrapped_estimates)
    }

    return(c(ate, variance))
}

# Performs AIPW variance analysis with sampling for the bootstrap stratified along MOMEDUC
# No longer necessary
aipw_analysis_with_strat <- function(dat, n_bootstrap = 0) {

    ate <- compute_aipw(dat)
    variance <- -1

    if (n_bootstrap > 0) {
        treated_pop <- list()
        control_pop <- list()
        for (i in 1:3){
            treated_pop <- c(treated_pop, list(data_frame_element = data.frame(subset(dat, HGOVCUR == 1 & MOMEDUC == i))))
            control_pop <- c(control_pop, list(data_frame_element = data.frame(subset(dat, HGOVCUR == 0 & MOMEDUC == i))))
        }

        bootstrapped_estimates <- numeric(n_bootstrap)
        for (i in 1:n_bootstrap) {
            treated_boot <- list()
            control_boot <- list()
            for (k in 1:3) {
                pop1 <- data.frame(treated_pop[k])
                treated_boot <- rbind(treated_boot, pop1[sample(nrow(pop1), nrow(pop1), replace = TRUE), ])
                pop0 <- data.frame(control_pop[k])
                control_boot <- rbind(control_boot, pop0[sample(nrow(pop0), nrow(pop0), replace = TRUE), ])
            }
            dat_boot <- rbind(treated_boot, control_boot)
            colnames(dat_boot) <- colnames(dat)
            bootstrapped_estimates[i] <- compute_aipw(dat_boot)
        }
        variance <- var(bootstrapped_estimates)
    }

    return(c(ate, variance))
}