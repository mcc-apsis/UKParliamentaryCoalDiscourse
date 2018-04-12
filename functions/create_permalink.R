create_permalink <- function(x) {
  
  if (grepl("permalink", x %>% html_attr("class"))) {
    out <- list(
      id    = x %>% html_attr("id"),
      title = x %>% html_attr("title"),
      name  = x %>% html_attr("name"),
      href  = x %>% html_attr("href"),
      rel   = x %>% html_attr("rel")
    )
  } else {
    stop()
  }
  
  return(out)
}