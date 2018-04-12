
extractor_docTitle <- function(x){
  
  title <- ifelse(
    is.na(x %>% html_node(xpath='h4[@class="result-title"]/a') %>% html_text()), 
    NA,
    x %>% html_node(xpath='h4[@class="result-title"]/a') %>% html_text()
  )
  
  if (!is.na(title)) {
    url = x %>% html_node(xpath='h4[@class="result-title"]/a') %>% html_attr("href")
  } else {
    url = NA
  }
  
  return(data.frame(title=title, link=url, stringsAsFactors = FALSE))
}