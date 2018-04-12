# Get path to file containing the session list 
v_sessionList_file <- ifelse(u_sessionList_file == "", paste0("../data/data_sessionList_", u_searchTerm, ".RData"), u_sessionList_file)

if (file.exists(v_sessionList_file)) {
  cat("Loading existing session list file...")
  load(v_sessionList_file)
} else {
  cat("Scraping Hansard UK web site...")
  #== Initialise ========================================================
  data_sessionList <- list()
  
  # Get web page
  wp <- read_html(v_url)
  
  #== Scrape HANSARD list of documents ==================================
  # Get number of results for requested search term
  tmp <- wp %>% 
    html_nodes(xpath='//*[@id="content"]/div[@id="search-results"]/h3[@id="results-header"]') %>% 
    html_text()
  nb_results <- as.numeric(
    gsub(",", "", 
         trimws(
           substr((tmp %>% strsplit("\n"))[[1]][2], 
                  as.numeric(regexpr("of", (tmp %>% strsplit("\n"))[[1]][2]))+2, 
                  nchar((tmp %>% strsplit("\n"))[[1]][2])))))
  cat(paste0("Total number of results (unfiltered): ", nb_results, "\n"))
  
  # Get results of page 1 
  tmp <- wp %>% 
    html_nodes(xpath='//*[@id="content"]/div[@class="search-result"]')
  
  # Get document titles and links
  tmp2 = do.call('rbind', tmp %>% lapply(extractor_docTitle))
  data_sessionList[[1]] <- data.frame(
    doc_title = tmp2$title,
    doc_link  = tmp2$link)
  # Add date
  data_sessionList[[1]]$date <- unlist(lapply(strsplit(tmp2$link, "/", fixed=TRUE), function(x){ paste0(x[3:5], collapse="-")}))
  
  # Get discussants
  tmp2 = do.call('rbind', tmp %>% lapply(extractor_discussant))
  data_sessionList[[1]]$discussant      = tmp2$name
  data_sessionList[[1]]$discussant_link = tmp2$link
  
  # Get chamber
  tmp2 = do.call('rbind', tmp %>% lapply(extractor_chamber))
  data_sessionList[[1]]$chamber = tmp2
  
  # Loop over pages
  if (ceiling(nb_results/10) >= 2) {
    for (k_page in 2:ceiling(nb_results/10)) {
      
      cat(paste0("Page: ", k_page, "\n"))
      
      wp <- read_html(paste0(v_url, "?page=", paste(k_page)))
      
      tmp = wp %>% 
        html_nodes(xpath='//*[@id="content"]/div[@class="search-result"]')
      
      # Get document titles and links
      tmp2 = do.call('rbind', tmp %>% lapply(extractor_docTitle))
      data_sessionList[[k_page]] <- data.frame(
        doc_title = tmp2$title,
        doc_link  = tmp2$link)
      # Add date
      data_sessionList[[k_page]]$date <- unlist(lapply(strsplit(tmp2$link, "/", fixed=TRUE), function(x){ paste0(x[3:5], collapse="-")}))
      
      # Get discussants
      tmp2 = do.call('rbind', tmp %>% lapply(extractor_discussant))
      data_sessionList[[k_page]]$discussant      = tmp2$name
      data_sessionList[[k_page]]$discussant_link = tmp2$link
      
      # Get chamber
      tmp2 = do.call('rbind', tmp %>% lapply(extractor_chamber))
      data_sessionList[[k_page]]$chamber = tmp2
      
    } 
  }
  
  # Merge list elements (i.e. pages)
  data_sessionList <- do.call("rbind", data_sessionList)
  
  save(data_sessionList, file = paste0("../data/data_sessionList_", u_searchTerm, ".RData"), precheck = TRUE)
}

