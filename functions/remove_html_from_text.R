remove_html_from_text <- function(x) {
  out <-  x %>% 
    gsub("<p.*?>","", .) %>% gsub("</p>","", .) %>%                   # Remove p
    gsub("<blockquote.*?>","", .) %>% gsub("</blockquote>","", .) %>% # Remove blockquote
    gsub("<cite.*?>","", .) %>% gsub("</cite>","", .) %>%             # Remove cite
    #gsub("<a.*?>.*?</a>","", .) %>%                                   # Remove link (bills, acts ...)
    gsub("<a.*permalink.*?>.</a>","", .) %>%                                      # Remove Permalink (article)
    gsub("<a.*>[0-9]*</a>","", .) %>%                                 # Remove Permalink (column)
    gsub("<a.*people.*?>","", .) %>%  gsub("</a>","", .) %>%          # Remove link (people)
    gsub("<span class=\".*\">","", .) %>% gsub("</span>","", .) %>%   # Remove span
    gsub("<q>","", .) %>% gsub("</q>","", .) %>%                      # Remove q
    gsub("<ul>","", .) %>% gsub("</ul>","", .) %>%                    # Remove ul
    gsub("<li>","", .) %>% gsub("</li>","", .) %>%                    # Remove li
    gsub("(\n[[:blank:]]*)", "", .)
  
  return(out)
}