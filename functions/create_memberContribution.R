create_memberContribution <- function(x, DP=NULL) {
  out <- list()
  
  out[["id"]] <- x %>% html_attr("id")
  
  if (!is.null(DP)) out[["datePublished"]] <- DP
  
  out[["permalink"]] <- list()
  cnt_permalink <- 1
  
  out[["link"]] <- list()
  cnt_link <- 1
  
  # Get blockquote's children
  items <- x %>% html_node(xpath='./blockquote') %>% html_children()
  
  # Get member name and permalinks
  if (length(items) != 0) {
    for (ki in 1:length(items)) {
      treated = FALSE
      # Prefix
      if (items[[ki]] %>% html_name() == "span" && items[[ki]] %>% html_attr("class") == "prefix") {
        out[["prefix"]] <- items[[ki]] %>% html_text()
        treated = TRUE
      }
      # Member
      if (items[[ki]] %>% html_name() == "cite" && grepl("member", items[[ki]] %>% html_attr("class"))) {
        out[["member"]] <- create_speaker(items[[ki]])
        treated = TRUE
      }
      # Member consituency
      if (items[[ki]] %>% html_name() == "span" && items[[ki]] %>% html_attr("class") == "member_constituency") {
        out[["member_constituency"]] <- create_speakerConstituency(items[[ki]])
        treated = TRUE
      }
      # Permalinks
      if (grepl("permalink", items[[ki]] %>% html_attr("class"))) {
        out[["permalink"]][[cnt_permalink]] <- create_permalink(items[[ki]])
        cnt_permalink <- cnt_permalink + 1
        treated = TRUE
      }
      # Links
      if (items[[ki]] %>% html_name() == "a" && !grepl("permalink", items[[ki]] %>% html_attr("class"))) {
        out[["link"]][[cnt_link]] <- create_link(items[[ki]])
        cnt_link <- cnt_link + 1
        treated = TRUE
      }
      # Permalinks in tags <p>
      if (items[[ki]] %>% html_name() == "p") {
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
        treated = TRUE
      }
      if (!treated) print(paste0("[WARNING] memberContribution -> ", items[[ki]] %>% as.character()))
    }
  }
  
  # Extract text
  out[["text"]] <- items %>% as.character() %>% remove_html_from_text() %>% remove_empty_and_NA()
  
  return(out)
}