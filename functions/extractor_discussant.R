extractor_discussant <- function(x){
  
  discussant <- ifelse(
    is.na(x %>% html_node(xpath='cite/a') %>% html_text()), 
    NA,
    x %>% html_node(xpath='cite/a') %>% html_text()
  )
  
  if (!is.na(discussant)) {
    url = x %>% html_node(xpath='cite/a') %>% html_attr("href")
  } else {
    url = NA
  }
  
  return(data.frame(name=discussant, link=url, stringsAsFactors = FALSE))
}
