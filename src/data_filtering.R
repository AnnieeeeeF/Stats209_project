filter_and_clean <- function(full_data, verbose = 0) {
    dat <- full_data

    # Exclude homeschooled children
    dat <- subset(dat, HOMESCHL != 1)
    if (verbose)
        print(sprintf("%i units left after selecting children who were not homeschooled.", nrow(dat)))

    # Select low-income families only
    # Exclude families from group 14
    dat <- subset(dat, HINCOME <= 13)
    income_medians <- c(2500, 7500, 12500, 17500, 22500, 27500, 32500, 37500, 42500, 47500, 55000, 67500, 87500)
    threshold <- c(9310 + seq(0, 10) * 3180)

    dat <- subset(dat, income_medians[HINCOME] < 2 * threshold[HHTOTAL])
    if (verbose)
        print(sprintf("%i units left after selecting low-income families.", nrow(dat)))

    # Select households with employed mothers
    dat <- subset(dat, MOMEMPLD == 1)
    # dat <- subset(dat, MOMFTFY == 1)
    if (verbose)
        print(sprintf("%i units left after selecting households with employed mothers.", nrow(dat)))

    # Select children that receive some type of after school care
    dat <- subset(dat, RCNOW == 1 | CPSNOW == 1 | SCSELF == 1 | PAAHOME == 1)
    if (verbose)
        print(sprintf("%i units left after filtering out children who were not receiving any type of after school care.", nrow(dat)))

    # Drop children from ethnicities other than Black / Hispanic / White
    dat <- subset(dat, RACEETHN < 4)
    if (verbose)
        print(sprintf("%i units left after filtering children from ethnicities other than Black/Hispanic/White.", nrow(dat)))

    # Cleanup treatment variable
    dat <- subset(dat, HGOVCUR >= 0)
    if (verbose)
        print(sprintf("%i units left after selecting units for which treatment variable is defined.", nrow(dat)))
    dat$HGOVCUR <- ifelse(dat$HGOVCUR == 1, 1, 0)

    return(dat)
}

perform_sanity_checks <- function(dat) {
    print(sprintf("The observed proportion of children receiving care from a relative other than a parent on a regular basis after school, for example, from grandparents, brothers or sisters, or any other relatives is %f, when in the article it is closer to 0.262.", nrow(subset(dat, RCNOW == 1)) / nrow(dat)))

    # # receiving care in your home or another home on a regular basis after school from someone who is not related to them
    # nrow(subset(dat, NCNOW == 1)) / nrow(dat)

    print(sprintf("The observed proportion of children now attending an after- school program at a school or in a center, either on a scheduled or a drop-in basis is %f, when in the article it is closer to 0.171.", nrow(subset(dat, CPSNOW == 1)) / nrow(dat)))

    print(sprintf("The observed proportion of children responsible for themselves after school on a regular basis is %f, when in the article it is closer to 0.132.", nrow(subset(dat, SCSELF == 1)) / nrow(dat)))

    print(sprintf("The observed proportion of children who have their parents at home is %f, when in the article it is closer to 0.36.", nrow(subset(dat, PAAHOME == 1)) / nrow(dat)))

    print(sprintf("The observed proportion of children who are white is %f, when in the article it is closer to 0.339.", nrow(subset(dat, RACEETHN == 1)) / nrow(dat)))
    print(sprintf("The observed proportion of children who are black is %f, when in the article it is closer to 0.23.", nrow(subset(dat, RACEETHN == 2)) / nrow(dat)))
    print(sprintf("The observed proportion of children who are hispanic is %f, when in the article it is closer to 0.431.", nrow(subset(dat, RACEETHN == 3)) / nrow(dat)))

    print(sprintf("The observed proportion of children who are receiving treatment is %f, when in the article it is closer to 0.164.", nrow(subset(dat, HGOVCUR == 1)) / nrow(dat)))
}