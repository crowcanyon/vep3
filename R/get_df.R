get_df <- function(x){
  out <- cbind(xyFromCell(x, seq_len(ncell(x))),
               tibble::tibble(ID = getValues(x))) %>%
    tibble::as_tibble()
  
  if(is.factor(x)){
    
    levels <- levels(x)[[1]] %>%
      dplyr::mutate_all(.funs = list(ordered)) %>%
      tibble::as_tibble()
    
    fact <- out$ID %>%
      ordered(levels = levels(levels$ID))
    
    out %<>%
      dplyr::mutate(ID = fact) %>%
      dplyr::left_join(levels)
  }
  
  return(out)
}
