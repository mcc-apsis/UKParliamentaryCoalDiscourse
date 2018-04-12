# Get path to file containing the discussions
v_discussions_file <- ifelse(u_discussions_file == "", paste0("../data/data_discussions_", u_searchTerm, ".RData"), u_discussions_file)

if (file.exists(v_discussions_file)) {
  cat("Loading existing discussion file...")
  load(v_discussions_file)
} else {
  cat("Scraping Hansard UK web site...")

  #url = "http://hansard.millbanksystems.com/commons/1871/may/04/ways-and-means-report#S3V0206P0_18710504_HOC_37"
  #== Initialise ===============
  data_discussions <- list()
  
  #== Lopp over filtered session list
  for (kr in 1:length(urls_sessionsToScrape)) {
    
    id_match  <- which(grepl(substr(urls_sessionsToScrape[kr], 35, nchar(urls_sessionsToScrape[kr])), data_sessionList$doc_link, fixed = TRUE))[1]
    cur_title <- data_sessionList$doc_title[id_match]
    
    cat(paste0("Processing [", kr,"]: ", paste0(substr(cur_title, 1, 50), "[...]"), "\n"))
    
    # Initialise list
    data_discussions[[kr]] <- list()
    
    # Get web page
    url <- urls_sessionsToScrape[kr]
    wp  <- read_html(url)
    
    data_discussions[[kr]][["title"]] <- wp %>% html_node(xpath='//div[@id="header"]/h1[@class="title"]') %>% html_text()
    data_discussions[[kr]][["url"]]   <- url
    
    cnt_procedural <- 1
    cnt_memberContribution <- 1
    cnt_division <- 1
    cur_DP <- NULL
    
    tmp = wp %>% html_nodes(xpath='//div[@id="content"]') %>% html_children()
    for (ks in 1:length(tmp)) {
      if (!is.na(tmp[[ks]] %>% html_attr("class"))) {
        
        treated = FALSE
        
        if (tmp[[ks]] %>% html_attr("class") == "section") {
          data_discussions[[kr]][["section"]]             <- tmp[[ks]] %>% html_text()
          treated <- TRUE
        }
        if (tmp[[ks]] %>% html_attr("class") == "permalink column-permalink") {
          data_discussions[[kr]][["permalink"]]           <- create_permalink(tmp[[ks]])
          treated <- TRUE
        }
        if (tmp[[ks]] %>% html_attr("class") == "procedural") {
          data_discussions[[kr]][[paste0("procedural_", cnt_procedural)]] <- create_procedural(tmp[[ks]], DP=cur_DP)
          cnt_procedural = cnt_procedural+1
          treated <- TRUE
        }
        if (tmp[[ks]] %>% html_attr("class") == "hentry member_contribution") {
          data_discussions[[kr]][[paste0("memberContribution_", cnt_memberContribution)]] <- create_memberContribution(tmp[[ks]], DP=cur_DP)
          cnt_memberContribution = cnt_memberContribution+1
          treated <- TRUE
        }
        if (tmp[[ks]] %>% html_attr("class") == "division")  {
          print("TODO: division")
          data_discussions[[kr]][[paste0("division_", cnt_division)]] <- create_division(tmp[[ks]], DP=cur_DP)
          cnt_division <- cnt_division + 1
          treated <- TRUE
        }
        if (tmp[[ks]] %>% html_attr("class") == "unparsed_division") {
          print("TODO: unparsed_division")
          treated <- TRUE
        }
        
        if (tmp[[ks]] %>% html_attr("class") == "time published") {
          cur_DP <- list(
            abbr = tmp[[ks]] %>% html_node(xpath="./a/abbr") %>% html_text(),
            date = tmp[[ks]] %>% html_node(xpath="./a/abbr") %>% html_attr("title"))
          treated <- TRUE
        } else {
          cur_DP <- NULL
        }
        
        if (tmp[[ks]] %>% html_attr("class") == "table") {
          print("TODO: table")
          print(tmp[[ks]] %>% as.character())
          treated <- TRUE
        }
        if (tmp[[ks]] %>% html_attr("class") == "xoxo") {
          print("TODO: xoxo")
          if (tmp[[ks]] %>% html_text != "") {
            print(tmp[[ks]] %>% as.character())
          }
          treated <- TRUE
        }
        
        if (!treated) print(paste0("unknown section : ", tmp[[ks]] %>% as.character()))
      }
    }
  }
  
  # Save discussions data
  save(data_discussions, file=v_discussions_file)
}


