filter_mdb <- function(x, dots){
   fkFilter <- function(toRet, tn){
      fkf <- fk %>% filter(from==tn)
      fkt <- fk %>% filter(to==tn)
      fk <<- fk %>% filter(
         !paste(from, to, sep="--") %in% paste(!!fkf$from, !!fkf$to, sep="--"),
         !paste(from, to, sep="--") %in% paste(!!fkt$from, !!fkt$to, sep="--")
      )
      fkl <- bind_rows(
         fkf,
         fkt %>% rename("from"="to", "ff"="tf", "to"="from", "tf"="ff")
      )
      if(nrow(fkl)>0){
         for(i in 1:nrow(fkl)){
            ntn <- fkl$to[i]
            if(ntn %in% names(toRet)){
               toRet[[ntn]] <- toRet[[ntn]] %>%
                  filter(
                     do.call(
                        paste,
                        c(
                           (!!toRet[[ntn]][, fkl$tf[[i]], drop=FALSE]),
                           list(sep="_")
                        )
                     ) %in%
                        do.call(
                           paste,
                           c(
                              (!!toRet[[tn]][, fkl$ff[[i]], drop=FALSE]),
                              list(sep="_")
                           )
                        )
                  )
            }else{
               tv <- x[[ntn]]
               toRet[[ntn]] <- tv %>%
                  filter(
                     do.call(
                        paste,
                        c(
                           (!!tv[, fkl$tf[[i]], drop=FALSE]),
                           list(sep="_")
                        )
                     ) %in%
                        do.call(
                           paste,
                           c(
                              (!!toRet[[tn]][, fkl$ff[[i]], drop=FALSE]),
                              list(sep="_")
                           )
                        )
                  )
            }
            toRet <- fkFilter(toRet, ntn)
         }
      }
      return(toRet)
   }
   toRet <- list()
   fk <- get_foreign_keys(dataModel(x)) %>%
      select(from, ff, to, tf)
   for(tn in names(dots)){
      toRet[[tn]] <- filter(x[[tn]], eval(!!dots[[tn]])) ### !! EVAL EXPRESSION --> not nice !!!
      toRet <- fkFilter(toRet, tn)
   }
   return(internalMDB(
      dataModel=dataModel(x)[names(toRet)],
      dbTables=toRet,
      colMembers=collectionMembers(x) %>% filter(table %in% names(x)),
      dbInfo=dbInfo(x)
   ))
}
