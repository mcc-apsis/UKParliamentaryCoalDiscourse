# Get unique sessions
urls_uniqueSessions <- paste0("http://hansard.millbanksystems.com", unique(sapply(paste(data_sessionList$doc_link), function(x) strsplit(x, "#", fixed=TRUE)[[1]][1])))
nb_uniqueSessions   <- length(urls_uniqueSessions)
freq_uniqueSessions <- sapply(urls_uniqueSessions, function(x) length(which(grepl(substr(x, 35, nchar(x)), data_sessionList$doc_link, fixed = TRUE))))

# Plot result of filtering
plot(
  sort(unique(freq_uniqueSessions)), 
  sapply(sort(unique(freq_uniqueSessions)), function(x) length(which(freq_uniqueSessions == x))),
  xlab="Number of occurences featuring the word coal per session", ylab="Number of Sessions")
lines(c(u_freq_limit, u_freq_limit), c(0, max(sapply(sort(unique(freq_uniqueSessions)), function(x) length(which(freq_uniqueSessions == x))))), col="red")
plot(
  sort(unique(freq_uniqueSessions)), 
  cumsum(sapply(sort(unique(freq_uniqueSessions)), function(x) length(which(freq_uniqueSessions == x)))),
  xlab="Number of occurences featuring the word coal per session", ylab="Cumulated number of sessions")
lines(c(u_freq_limit, u_freq_limit), c(0, max(cumsum(sapply(sort(unique(freq_uniqueSessions)), function(x) length(which(freq_uniqueSessions == x)))))), col="red")
plot(
  sort(unique(freq_uniqueSessions)), 
  nb_uniqueSessions-cumsum(sapply(sort(unique(freq_uniqueSessions)), function(x) length(which(freq_uniqueSessions == x)))),
  xlab="Number of occurences featuring the word coal per session", ylab="Cumulated number of sessions",
  ylim=c(0,500))
lines(c(u_freq_limit, u_freq_limit), c(0, max(cumsum(sapply(sort(unique(freq_uniqueSessions)), function(x) length(which(freq_uniqueSessions == x)))))), col="red")

# Define list of sessions to be scraped
urls_sessionsToScrape <- urls_uniqueSessions[which(freq_uniqueSessions > u_freq_limit)]
#urls_sessionsToScrape <- urls_uniqueSessions[which(freq_uniqueSessions == max(freq_uniqueSessions))]
