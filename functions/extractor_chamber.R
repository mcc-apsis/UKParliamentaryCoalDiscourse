
extractor_chamber <- function(x){
  
  chamber <- ifelse(
    is.na(x %>% html_node(xpath='div[@class="date"]/a') %>% html_text()), 
    NA,
    x %>% html_node(xpath='div[@class="date"]/a') %>% html_text()
  )
  
  return(chamber)
}
