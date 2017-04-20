levexp <- structure(function# Vector releveling
### Expansion or reduction of a numeric vector by matching its level names
### with the ecological factors of a multilevel ecological data
### series.
(
    x, ##<<\code{numeric} vector with names of the vector representing
       ##the levels to be matched.
    levels ##<<\code{data.frame}. Multilevel ecological data series,
           ##or \code{character} vector of levels.
) {
    tx <- split(x,names(x))
    if(is.character(levels))
        dsp <- split(levels,levels)
    if(is.data.frame(levels))
        dsp <- slitFrame(levels)
    nam <- lapply(seq_len(length(tx)),
                  function(i)paste("\\b",
                                   names(tx[i]),"\\b",sep = ""))
    nms <- lapply(seq_len(length(tx)),
                  function(i)grep(nam[[i]],
                                  names(dsp),value = TRUE))
    names(nms) <- names(tx)
    nnms <- lapply(nms,length)
    nms1 <- lapply(seq_len(length(tx)),
                   function(i)rep(tx[[i]],nnms[[i]]))
    nm <- lapply(seq_len(length(tx)),
                 function(i)data.frame(nms[[i]],nms1[[i]]))
    nmd <- do.call(rbind,nm)
    nmd1 <- nmd[,2]
    names(nmd1) <- nmd[,1]
    
    nmd1 <- nmd1[names(dsp)]
    nmd1 <- nmd1[!is.na(nmd1)]
    return(nmd1)
### numeric vector with expanded/reduced levels.
} , ex=function(){
    ##Multilevel ecological data series of tree-ring widths:
    data(Prings05,envir = environment())
    ## tree radii measured at 2003:
    data(Pradii03,envir = environment())    
    
    ## Releveling the tree radii
    refs <- levexp(Pradii03,Prings05)
    refs
})
