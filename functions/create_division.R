create_division <- function(x, DP=NULL) {
  out <- list()
  
  out[["id"]] <- x %>% html_attr("id")
  
  if (!is.null(DP)) out[["datePublished"]] <- DP
  
  # Get tables
  items  <- x %>% html_nodes(xpath="./table")
  tables <- list()
  for (kt in 1:length(items)) {
    if (items[[kt]] %>% html_text() != "") {
      tables[[kt]] <- items[[kt]] %>% html_table(fill=TRUE)
    }
  }
  tables <- remove_empty_and_NA(do.call("c", sapply(tables, function(x) unlist(c(x), use.names = FALSE))))
  if (length(which(grepl("division", tolower(tables)))) !=0) tables <- tables[-which(grepl("division", tolower(tables)))]
  
  nb_elements <- length(tables)
  
  # AYES. / NOES. format
  id_ayes <- which(grepl("^AYES", tables))
  id_noes <- which(grepl("^NOES", tables))
  if (length(id_ayes) != 0 && length(id_noes) != 0) {
    
    out[["type"]] <- "Ayes/Noes"
    
    id_tellersAyes <- which(grepl("^TELLERS FOR THE AYES", tables))
    id_tellersNoes <- which(grepl("^TELLERS FOR THE NOES", tables))
    
    if (length(id_tellersAyes) != 0 && length(id_tellersNoes) != 0) {
      
      out[["Yes"]] <- tables[1:(id_noes-1)][-id_ayes]
      out[["No"]]  <- tables[(id_noes+1):nb_elements]
      
      out[["tellers"]] <- list()
      if (id_tellersAyes == (id_noes-1)) {
        out[["tellers"]][["Yes"]] <- substr(paste(tables[id_tellersAyes]), 22, nchar(paste(tables[id_tellersAyes])))
      } else {
        tmp <- c()
        for (k in id_tellersAyes:(id_noes-1)) {
          if (k == id_tellersAyes) {
            tmp <- c(tmp, substr(paste(tables[k]), 22, nchar(paste(tables[k]))))
          } else {
            tmp <- c(tmp, paste(tables[k]))
          }
        }
        out[["tellers"]][["Yes"]] <- unlist(sapply(tmp, function(x) strsplit(gsub("\\.$", "", gsub("^—|^–", "", gsub("\\sand|\\sand\\s|and\\s", "\\/", paste0(x, collapse="")))), "\\/")[[1]]), use.names = FALSE)
      }
      out[["Yes"]] <- out[["Yes"]][-seq(length(out[["Yes"]])-((id_noes-1)-id_tellersAyes), length(out[["Yes"]]))]
      
      if (id_tellersNoes == nb_rows) {
        out[["tellers"]][["No"]] <- substr(paste(tables[id_tellersNoes, nb_cols]), 22, nchar(paste(tables[id_tellersNoes, nb_cols])))
      } else {
        tmp <- c()
        for (k in id_tellersNoes:nb_rows) {
          if (k == id_tellersNoes) {
            tmp <- c(tmp, substr(paste(tables[k]), 22, nchar(paste(tables[k]))))
          } else {
            tmp <- c(tmp, paste(tables[k]))
          }
        }
        #gsub("[\u2013:\u2016]", "", s)
        out[["tellers"]][["No"]] <- unlist(sapply(tmp, function(x) strsplit(gsub("\\.$", "", gsub("^—|^–", "", gsub("\\sand|\\sand\\s|and\\s", "\\/", paste0(x, collapse="")))), "\\/")[[1]]), use.names = FALSE)
      }
      out[["No"]] <- out[["No"]][-seq(length(out[["No"]])-(nb_rows-id_tellersNoes-1), length(out[["No"]]))]
      
    } else {
      out[["Yes"]] <- c()
      out[["No"]]  <- c()
      
      for (k in 1:nb_cols) {
        out[["Yes"]] <- c(out[["Yes"]], tables[1:(id_noes-1),   k])
        out[["No"]]  <- c(out[["No"]],  tables[id_noes:nb_rows, k])
      }
      out[["Yes"]] <- remove_empty_and_NA(out[["Yes"]])[-1]
      out[["No"]]  <- remove_empty_and_NA(out[["No"]])[-1]
      
      out[["tellers"]] <- list()
      out[["tellers"]][["Yes"]] <- NULL
      out[["tellers"]][["No"]]  <- NULL
    }
  }
  
  # CONTENTS / NON-CONTENTS
  id_content    <- which(tables$X1 == "CONTENTS")
  id_notcontent <- which(tables$X1 == "NOT-CONTENTS")
  if (length(id_content) != 0 && length(id_notcontent) != 0) {
    out[["type"]] <- "Content/Not-Content"
    
    out[["Yes"]] <- c()
    out[["No"]]  <- c()
    for (k in 1:nb_cols) {
      out[["Yes"]] <- c(out[["Yes"]], tables[(id_content+1):(id_notcontent-1), k])
      out[["No"]]  <- c(out[["No"]],  tables[(id_notcontent+1):nb_rows,        k])
    }
    out[["Yes"]] <- remove_empty_and_NA(out[["Yes"]])
    out[["No"]]  <- remove_empty_and_NA(out[["No"]])
    
    # Find Tellers
    out[["tellers"]] <- list()
    out[["tellers"]][["Yes"]] <- NULL
    out[["tellers"]][["No"]]  <- NULL
    id_tellerYes = which(out[["Yes"]] == "[Teller.]")
    if(length(id_tellerYes) != 0) {
      out[["tellers"]][["Yes"]] <- out[["Yes"]][id_tellerYes-1]
      out[["Yes"]] <- out[["Yes"]][-id_tellerYes]
    }
    id_tellerNo = which(out[["No"]] == "[Teller.]")
    if(length(id_tellerNo) != 0) {
      out[["tellers"]][["No"]] <- out[["No"]][id_tellerNo-1]
      out[["No"]] <- out[["No"]][-id_tellerNo]
    }
    
    # Find Lord Chancellor
    out[["LordChancellor"]] <- list()
    out[["LordChancellor"]][["Yes"]] <- NULL
    out[["LordChancellor"]][["No"]]  <- NULL
    
    id_lordChancellorYes = grep("\\[Lord Chancellor\\]", out[["Yes"]])
    if (length(id_lordChancellorYes) != 0) {
      out[["LordChancellor"]][["Yes"]] <- substr(out[["Yes"]][id_lordChancellorYes], 1, nchar(out[["Yes"]][id_lordChancellorYes])-19)
      out[["Yes"]][id_lordChancellorYes] <- substr(out[["Yes"]][id_lordChancellorYes], 1, nchar(out[["Yes"]][id_lordChancellorYes])-19)
    } else {
      id_lordChancellorYes1 = grep("\\[Lord", out[["Yes"]])
      id_lordChancellorYes2 = grep("Chancellor.\\]", out[["Yes"]])
      if (length(id_lordChancellorYes1) != 0 && length(id_lordChancellorYes2) != 0) {
        out[["LordChancellor"]][["Yes"]] <- substr(out[["Yes"]][id_lordChancellorYes1], 1, nchar(out[["Yes"]][id_lordChancellorYes1])-6)
        out[["Yes"]][id_lordChancellorYes1] <- substr(out[["Yes"]][id_lordChancellorYes1], 1, nchar(out[["Yes"]][id_lordChancellorYes1])-6)
        out[["Yes"]] <- out[["Yes"]][-id_lordChancellorYes2]
      } else {
        print("Couldn't find Lord Chancellor in list")
      }
    }
    id_lordChancellorNo = grep("\\[Lord Chancellor.\\]", out[["No"]])
    if (length(id_lordChancellorNo) != 0) {
      out[["LordChancellor"]][["No"]] <- substr(out[["No"]][id_lordChancellorNo], 1, nchar(out[["No"]][id_lordChancellorNo])-19)
      out[["No"]][id_lordChancellorNo] <- substr(out[["No"]][id_lordChancellorNo], 1, nchar(out[["No"]][id_lordChancellorNo])-19)
    } else {
      id_lordChancellorNo1 = grep("\\[Lord", out[["No"]])
      id_lordChancellorNo2 = grep("Chancellor.\\]", out[["No"]])
      if (length(id_lordChancellorNo1) != 0 && length(id_lordChancellorNo2) != 0) {
        out[["LordChancellor"]][["No"]] <- substr(out[["No"]][id_lordChancellorNo1], 1, nchar(out[["No"]][id_lordChancellorNo1])-6)
        out[["No"]][id_lordChancellorNo1] <- substr(out[["No"]][id_lordChancellorNo1], 1, nchar(out[["No"]][id_lordChancellorNo1])-6)
        out[["No"]] <- out[["No"]][-id_lordChancellorNo2]
      } else {
        print("Couldn't find Lord Chancellor in list")
      }
    }
  }
  
  return(out)
}