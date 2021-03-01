function(
   x, y,
   orthologs=FALSE,
   restricted=FALSE,
   ...
){
   if(!requireNamespace("BED")){
      stop("The BED package is required")
   }
   if(!BED::checkBedConn()){
      stop(
         "You need to connect to a BED database using",
         " the BED::connectToBed() function"
      )
   }
   
   if(!"organism" %in% colnames(x)){
      d <- x
      scopes <- dplyr::distinct(d, be, source)
      nd <- c()
      for(i in 1:nrow(scopes)){
         be <- scopes$be[i]
         source <- scopes$source[i]
         toadd <- d %>% dplyr::filter(be==be, source==source)
         organism <- BED::guessIdScope(
            toadd$identifier, be=be, source=source, tcLim=Inf
         ) %>%
            attr("details") %>% 
            filter(be==!!be & source==!!source) %>% 
            pull(organism) %>% 
            unique()
         toadd <- merge(toadd, tibble(organism=organism))
         nd <- bind_rows(nd, toadd)
      }
      x <- nd %>% mutate(organism_type="Scientific name")
   }
   if(!"organism" %in% colnames(y)){
      d <- y
      scopes <- dplyr::distinct(d, be, source)
      nd <- c()
      for(i in 1:nrow(scopes)){
         be <- scopes$be[i]
         source <- scopes$source[i]
         toadd <- d %>% dplyr::filter(be==be, source==source)
         organism <- BED::guessIdScope(
            toadd$identifier, be=be, source=source, tcLim=Inf
         ) %>%
            attr("details") %>% 
            filter(be==!!be & source==!!source) %>% 
            pull(organism) %>% 
            unique()
         toadd <- merge(toadd, tibble(organism=organism))
         nd <- bind_rows(nd, toadd)
      }
      y <- nd %>% mutate(organism_type="Scientific name")
   }
   
   xscopes <- dplyr::distinct(x, be, source, organism, organism_type)
   yscopes <- dplyr::distinct(y, be, source, organism, organism_type)
   
   toRet <- NULL
   for(i in 1:nrow(xscopes)){
      xscope <- xscopes[i,]
      if(any(apply(xscope, 2, is.na))){
         next()
      }
      xi <- dplyr::right_join(
         x, xscope,
         by=c("be", "source", "organism", "organism_type")
      )
      xorg <- ifelse(
         xscope$organism_type=="NCBI taxon identifier",
         BED::getOrgNames(xscope$organism) %>%
            dplyr::filter(nameClass=="scientific name") %>% 
            dplyr::pull(name),
         xscope$organism
      )
      for(j in 1:nrow(yscopes)){
         yscope <- yscopes[j,]
         if(any(apply(yscope, 2, is.na))){
            next()
         }
         yi <- dplyr::right_join(
            y, yscope, by=c("be", "source", "organism", "organism_type")
         )
         yorg <- ifelse(
            yscope$organism_type=="NCBI taxon identifier",
            BED::getOrgNames(yscope$organism) %>%
               dplyr::filter(nameClass=="scientific name") %>% 
               dplyr::pull(name),
            yscope$organism
         )
         if(xorg==yorg || orthologs){
            xy <- BED::convBeIds(
               ids=xi$identifier,
               from=xscope$be,
               from.source=xscope$source,
               from.org=xorg,
               to=yscope$be,
               to.source=yscope$source,
               to.org=yorg,
               restricted=restricted
            ) %>%
               dplyr::as_tibble() %>% 
               dplyr::select(from, to)
            if(restricted){
               xy <- dplyr::bind_rows(
                  xy,
                  BED::convBeIds(
                     ids=yi$identifier,
                     from=yscope$be,
                     from.source=yscope$source,
                     from.org=yorg,
                     to=xscope$be,
                     to.source=xscope$source,
                     to.org=xorg,
                     restricted=restricted
                  ) %>%
                     dplyr::as_tibble() %>% 
                     dplyr::select(to=from, from=to)
               )
            }
            xy <- xy %>% 
               dplyr::rename("identifier_x"="from", "identifier_y"="to") %>% 
               dplyr::mutate(
                  be_x=xscope$be,
                  source_x=xscope$source, organism_x=xscope$organism,
                  be_y=yscope$be,
                  source_y=yscope$source, organism_y=yscope$organism
               )
            toRet <- dplyr::bind_rows(toRet, xy)
         }
      }
   }
   toRet <- dplyr::distinct(toRet)
   return(toRet)
}
