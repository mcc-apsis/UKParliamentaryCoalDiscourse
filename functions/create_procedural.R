
create_procedural <- function(x, DP=NULL) {
  out <- list()
  
  out[["id"]] <- x %>% html_attr("id")
  
  if (!is.null(DP)) out[["datePublished"]] <- DP
  
  out[["link"]] <- list()
  cnt_link <- 1
  
  out[["permalink"]] <- list()
  cnt_permalink <- 1
  
  # Get childrens
  items <- x %>% html_children()
  
  # Get member name and permalinks
  if (length(items) != 0) {
    for (ki in 1:length(items)) {
      treated <- FALSE
      # Sup
      if (items[[ki]] %>% html_name() == "sup") {
        out[["sup"]] <- items[[ki]] %>% html_text()
        treated <- TRUE
      }
      # Member
      if (items[[ki]] %>% html_name() == "span" && items[[ki]] %>% html_attr("class") == "member") {
        out[["member"]] <- items[[ki]] %>% html_text()
        treated <- TRUE
      }
      # Permalinks
      if (grepl("permalink", items[[ki]] %>% html_attr("class"))) {
        out[["permalink"]][[cnt_permalink]] <- create_permalink(items[[ki]])
        cnt_permalink <- cnt_permalink + 1
        treated <- TRUE
      }
      # Links
      if (items[[ki]] %>% html_name() == "a" && !grepl("permalink", items[[ki]] %>% html_attr("class"))) {
        out[["link"]][[cnt_link]] <- create_link(items[[ki]])
        cnt_link <- cnt_link + 1
        treated <- TRUE
      }
      # Permalinks in tags <q>
      if (items[[ki]] %>% html_name() == "q") {
        subitems <- items[[ki]] %>% html_children()
        if (length(subitems) != 0) {
          for (ksi in 1:length(subitems)) {
            if (grepl("permalink", subitems[[ksi]] %>% html_attr("class"))) {
              out[["permalink"]][[cnt_permalink]] <- create_permalink(subitems[[ksi]])
              cnt_permalink <- cnt_permalink + 1
            }
            if (subitems[[ksi]] %>% html_name() == "a" && !grepl("permalink", subitems[[ksi]] %>% html_attr("class"))) {
              out[["link"]][[cnt_link]] <- create_link(subitems[[ksi]])
              cnt_link <- cnt_link + 1
            }
          }
        }
        treated <- TRUE
      }
      if (!treated) print(paste0("[WARNING] procedural -> ", items[[ki]] %>% as.character()))
    }
  }
  
  # Extract text
  out[["text"]]  <- x %>% as.character() %>% remove_html_from_text()
  
  return(out)
}