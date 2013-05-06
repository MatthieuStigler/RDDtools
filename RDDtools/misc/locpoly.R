locpoly2 <- function(x, y, drv = 0L, degree, kernel = "normal",
                    bandwidth, gridsize = 401L, bwdisc = 25, range.x,
                    binned = FALSE, truncate = TRUE)

{
    ## Install safeguard against non-positive bandwidths:
    if (!missing(bandwidth) && bandwidth <= 0)
        stop("'bandwidth' must be strictly positive")

    drv <- as.integer(drv)
    if (missing(degree)) degree <- drv + 1L
    else degree <- as.integer(degree)

    if (missing(range.x) && !binned)
        if (missing(y)) {
            extra <- 0.05*(max(x) - min(x))
            range.x <- c(min(x)-extra,  max(x)+extra)
        } else range.x <- c(min(x), max(x))

    ## Rename common variables
    M <- gridsize
    Q <- as.integer(bwdisc)
    a <- range.x[1L]
    b <- range.x[2L]
    pp <- degree + 1L
    ppp <- 2L*degree + 1L
    tau <- 4

    ## Decide whether a density estimate or regressionestimate is required.

    if (missing(y))  {  # obtain density estimate
        y <- NULL
        n <- length(x)
        gpoints <- seq(a, b, length = M)
        xcounts <- linbin(x, gpoints, truncate)
        ycounts <- (M-1)*xcounts/(n*(b-a))
        xcounts <- rep(1, M)
    } else {            # obtain regression estimate
        ## Bin the data if not already binned
        if (!binned) {
            gpoints <- seq(a, b, length = M)
            out <- rlbin(x, y, gpoints, truncate)
            xcounts <- out$xcounts
            ycounts <- out$ycounts
        } else {
            xcounts <- x
            ycounts <- y
            M <- length(xcounts)
            gpoints <- seq(a, b, length = M)
        }
    }

    ## Set the bin width
    delta <- (b-a)/(M-1L)

    ## Discretise the bandwidths
    if (length(bandwidth) == M) {
        hlow <- sort(bandwidth)[1L]
        hupp <- sort(bandwidth)[M]
        hdisc <- exp(seq(log(hlow), log(hupp), length = Q))

        ## Determine value of L for each member of "hdisc"
        Lvec <- floor(tau*hdisc/delta)

        ## Determine index of closest entry of "hdisc"
        ## to each member of "bandwidth"
        indic <- if (Q > 1L) {
            lhdisc <- log(hdisc)
            gap <- (lhdisc[Q]-lhdisc[1L])/(Q-1)
            if (gap == 0) rep(1, M)
            else round(((log(bandwidth) - log(sort(bandwidth)[1L]))/gap) + 1)
        } else rep(1, M)
    } else if (length(bandwidth) == 1L) {
        indic <- rep(1, M)
        Q <- 1L
        Lvec <- rep(floor(tau*bandwidth/delta), Q)
        hdisc <- rep(bandwidth, Q)
    } else
        stop("'bandwidth' must be a scalar or an array of length 'gridsize'")

    if (min(Lvec) == 0)
        stop("Binning grid too coarse for current (small) bandwidth: consider increasing 'gridsize'")

    ## Allocate space for the kernel vector and final estimate

    dimfkap <- 2L * sum(Lvec) + Q
    fkap <- rep(0, dimfkap)
    curvest <- rep(0, M)
    midpts <- rep(0, Q)
    ss <- matrix(0, M, ppp)
    tt <- matrix(0, M, pp)
    Smat <- matrix(0, pp, pp)
    Tvec <- rep(0, pp)
    ipvt <- rep(0, pp)


print(Q)
print(Lvec)
print(indic)
print(dimfkap)
    ## Call FORTRAN routine "locpol"

    out <- .Fortran(F_locpol, as.double(xcounts), as.double(ycounts),
                    as.integer(drv), as.double(delta), as.double(hdisc),
                    as.integer(Lvec), as.integer(indic), as.integer(midpts),
                    as.integer(M), as.integer(Q), as.double(fkap), as.integer(pp),
                    as.integer(ppp), as.double(ss), as.double(tt),
                    as.double(Smat), as.double(Tvec), as.integer(ipvt),
                    as.double(curvest))

# print(out)
    curvest <- gamma(drv+1) * out[[19L]]

    list(x = gpoints, y = curvest)
}


environment(locpoly2) <- environment(locpoly)
y <- geyser$waiting
plot(x, y)
fit <- locpoly2(x, y, bandwidth = 0.25)
# fit <- locpoly2(x, y, bandwidth = rep(0.25, 401))
lines(fit)