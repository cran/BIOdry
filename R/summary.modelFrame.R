summary.modelFrame <- structure(function #summarize a modelFrame object
### A summary of a \code{\link{modelFrame}} object is obtained.
(
    object, ##<< an object inheriting from class
            ##\code{\link{modelFrame}}.
    ... ##<< additional optional arguments passed to
        ##\code{\link{summary.lme}} method.
    
) {
        summary(object$'model', ...)
        ## A summary model.
} , ex=function() {
    ## An object from class \code{\link{summary.lme}}.
    data(Pchron,Pradii03,envir = environment())
    ## Tree-ring width fluctuations:
    trwf <- modelFrame(Pchron,
                       sc.c = Pradii03,
                       rf.t = 2003,
                       log.t = TRUE)
    summary(trwf)
})
