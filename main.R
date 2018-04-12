#== USER SECTION ==========================
u_searchTerm       = "coal"    # Term(s) processed by HANSARD UK search query engine
u_sessionList_file = ""        # If empty the data will be saved by default in data/data_sessionList_[u_searchTerm].RData
u_discussions_file = ""        # If empty the data will be saved by default in data/data_discussions_[u_searchTerm].RData
u_freq_limit       = 50        # Minimum number of occurences of the search term (e.g. coal) in a parliamentary session


#== INITIALISE ============================
source("scripts/initialise.R")
v_url <- paste0("http://hansard.millbanksystems.com/search/", u_searchTerm)


#== GET/SCRAPE HANSARD SESSION LIST =======
source("scripts/get_sessionList.R")


#== FILTER SESSION LIST ===================
source("scripts/filter_sessionList.R")


#== GET DISCUSSIONS/SCRAPE SESSIONS =======
source("scripts/get_discussions.R")


#== DISCOURSE STATISTICS ==================
#TODO


#== TOPIC MODELLING =======================
#TODO