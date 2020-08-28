function(
   x, y,
   deprecated=FALSE,
   ...
){
   if(!requireNamespace("DODO")){
      stop("The DODO package is required")
   }
   if(!DODO::check_dodo_connection()){
      stop(
         "You need to connect to a DODO database using",
         " the DODO::connect_to_dodo() function"
      )
   }
   x$lid <- paste(x$source, x$identifier, sep=":")
   y$lid <- paste(y$source, y$identifier, sep=":")
   convTable <- dplyr::tibble()
   for(xcondition in unique(x$condition)){
      for(ycondition in unique(y$condition)){
         if(ycondition==xcondition){
            convTable <- dplyr::bind_rows(convTable, DODO::convert_concept(
               from=setdiff(union(x$lid, y$lid), NA),
               from.concept=xcondition,
               to.concept=xcondition,
               deprecated=deprecated
            ) %>%
               dplyr::select(-"deprecated") %>% 
               dplyr::mutate(
                  fcondition=xcondition,
                  tcondition=xcondition
               )
            )
         }else{
            convTable <- dplyr::bind_rows(convTable, DODO::convert_concept(
               from=setdiff(x$lid, NA),
               from.concept=xcondition,
               to.concept=ycondition,
               deprecated=deprecated
            ) %>%
               dplyr::select(-"deprecated") %>% 
               dplyr::mutate(
                  fcondition=xcondition,
                  tcondition=ycondition
               )
            )
            convTable <- dplyr::bind_rows(convTable, DODO::convert_concept(
               from=setdiff(y$lid, NA),
               from.concept=ycondition,
               to.concept=xcondition,
               deprecated=deprecated
            ) %>%
               dplyr::select(-"deprecated") %>% 
               dplyr::mutate(
                  fcondition=ycondition,
                  tcondition=xcondition
               )
            )
         }
      }
   }
   toRet <- dplyr::bind_rows(
      dplyr::left_join(
         x, convTable, by=c("lid"="from", "condition"="fcondition")
      ) %>% 
         dplyr::rename(
            "lid_x"="lid", "lid_y"="to",
            "condition_x"="condition", "condition_y"="tcondition"
         ),
      dplyr::left_join(
         x, convTable, by=c("lid"="to", "condition"="tcondition")
      ) %>% 
         dplyr::rename(
            "lid_x"="lid", "lid_y"="from",
            "condition_x"="condition", "condition_y"="fcondition"
         )
   )
   toRet <- dplyr::right_join(
      toRet, y,
      by=c("lid_y"="lid", "condition_y"="condition"),
      suffix=c("_x", "_y")
   ) %>% 
      dplyr::select(-lid_x, -lid_y) %>% 
      dplyr::distinct()
   return(toRet)
   
}
