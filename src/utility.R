## code taken from "stats209_section_matching.Rmd"
## puts the results of a pair match in a nice form
## Usage: summarize.match(dataset,pairmatch_output)
summarize.match <- function(dat, ms, ps.name="prop", keep.mset=FALSE) {
  adat <- dat
  adat$mset <- ms
  adat <- adat[!is.na(adat$mset),]
  adat.treat <- adat[adat$HGOVCUR==1, ]
  adat.ctrl <- adat[adat$HGOVCUR==0, ]
  
  adat.m <- merge(adat.treat, adat.ctrl, by="mset", suffixes=c(".1", ".0"))
  
  if(!keep.mset) {
    adat.m <- adat.m[, -which(names(adat.m) %in% c("HGOVCUR.1", "HGOVCUR.0", "mset"))]
  } else {
    adat.m <- adat.m[, -which(names(adat.m) %in% c("HGOVCUR.1", "HGOVCUR.0"))]        
  }
  adat.m <- adat.m[, sort(names(adat.m), index.return=TRUE)$ix]
  
  p0.name <- paste0(ps.name,".", 0)
  p1.name <- paste0(ps.name,".",1)
  
  adat.m.tmp.1 <- adat.m[, -which(names(adat.m) %in% c(p0.name, p1.name))]
  adat.m.tmp.2 <- adat.m[, c(p0.name, p1.name)]
  
  adat.m <- cbind(adat.m.tmp.1, adat.m.tmp.2)
  
  return(adat.m)
}

ms.transform <- function(dat.arg, ms.rcbal) {
    ctrl <- seq(sum(dat.arg$HGOVCUR==0))
    matched.ctrl <- ms.rcbal
    unmatched.ctrl <- setdiff(ctrl,ms.rcbal)
    
    dat.tmp <- dat.arg
    dat.tmp$foo <- NA
    dat.tmp$foo[dat.tmp$HGOVCUR==1] <- matched.ctrl
    dat.tmp$foo[dat.tmp$HGOVCUR==0][matched.ctrl] <- matched.ctrl

    return(dat.tmp$foo)    
}