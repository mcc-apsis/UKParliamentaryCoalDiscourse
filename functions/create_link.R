create_link <- function(x) {
  
  if (x %>% html_name() == "a" && !grepl("permalink", x %>% html_attr("class"))) {
    
    if (grepl("offical report", tolower(x %>% html_text()))) {
      type = "Official Report"
    } else {
      type = x %>% html_attr("href") %>% dirname() %>% substr(1, nchar(.)-1)
    }
    
    out <- list(
      type  = type,
      href  = x %>% html_attr("href"),
      text  = x %>% html_text()
    )
  } else {
    stop()
  }
  
  return(out)
}