slitFrame <- structure(function #Multilevel splitting
### This function splits a Multilevel data frame into factor levels.
(
    rd, ##<<\code{data.frame} object with factor-level columns.
    lv = cClass(rd,'factor') ##<< \code{Numeric} or
                             ##\code{character}. Position number in
                             ##the factor-level columns of \code{rd},
                             ##or correspondant column name to split
                             ##the data. If the spliting column is not
                             ##a factor, the character name of the
                             ##column should be used.
) {
    rd11 <- rd[lv]
    if(is.null(lv)){
        rd1 <- Filter(is.factor, rd)
        rd11 <- rd1[1:length(rd1)]
    }
    lrd <- split(rd, rd11, drop = TRUE)
    spn <- data.frame(
        do.call(rbind,
                strsplit(names(lrd), '\\.')))
    nmr <- apply(spn[rev(names(spn))], 1,
                 paste, collapse = '.')
    options(warn=-1)
    anm <- all(is.na(as.numeric(as.character(nmr))))
    options(warn=0)
    if(anm)
        names(lrd) <- nmr
    lrd <- lrd[order(names(lrd))]
        return(lrd)
### \code{list} of \code{data.frame} objects.
},
ex=function() {
    ##Ring data frame:
    ##Multilevel data frame of tree-ring widths:
    data(Prings05, envir = environment())
    data(PTclim05, envir = environment())
    ## split multilevel data into its second factor-level column:
    spl <- slitFrame(Prings05)
    str(spl)
    ## split the data into the factor-level: 'year':
    spl <- slitFrame(Prings05,'year')
    str(spl)
    spl <- slitFrame(PTclim05,'year')
    str(spl)

})
