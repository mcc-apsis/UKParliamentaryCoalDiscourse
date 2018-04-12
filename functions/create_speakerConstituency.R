create_speakerConstituency <- function(x) {
  out <- list(
    title = x %>% html_node(xpath="./a") %>% html_attr("title"),
    href  = x %>% html_node(xpath="./a") %>% html_attr("href"),
    text  = x %>% html_node(xpath="./a") %>% html_text()
  )
  
  return(out)
}