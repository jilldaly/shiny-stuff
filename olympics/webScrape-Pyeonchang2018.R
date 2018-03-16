##############################################################################
#                         WebScrape Pyeonchange 2018                         #
#                                 Jill Daly                                  #
#                                  22/3/18                                   #
##############################################################################

##############################################################################
#                   Install and import relevant packages                     #
##############################################################################
for (package in c("rvest", "magrittr", "lubridate")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
  }
  library(package, character.only=T)
}


##############################################################################
#                         Strings Used                                       #
##############################################################################
data_folder <- "olympics2018"
medal_detail_URL <- "https://www.pyeongchang2018.com/en/game-time/results/OWG2018/en/general/detailed-medal-standings-.htm"
medallists_URL <- "https://www.pyeongchang2018.com/en/game-time/results/OWG2018/en/general/daily-medallists-date=2018-02-day.htm"
medals_filename <- paste(data_folder, "medals_table.csv", sep = "/")
medallists_filename <- paste(data_folder, "medallists.csv", sep = "/")


##############################################################################
#         Check if we need to create a dir for the csv files                 #
##############################################################################
if (!dir.exists(data_folder)){
  dir.create(data_folder)
}


##############################################################################
#         Function: Scrape Detailed Medal Table and Export data              #
##############################################################################
scrape.export.medals <- function() {
  medals <- read_html(medal_detail_URL)
  medals_table <- html_table(html_nodes(medals, "table")[[1]], fill = TRUE)
  write.csv(medals_table, file = medals_filename)
}


##############################################################################
#       Function: Daily Medal Standings (from 10th - 25th)                   #
##############################################################################
scrape.medallists <- function(day, m_df) {
  medallists <- read_html(sub("day", day, medallists_URL))
  me_tble <- html_table(html_nodes(medallists, "table")[[1]], fill = TRUE)

  # The webpage has two Medal columns, we need to drop the jpg column 
  me_tble <- me_tble[, -c(4)] %>%
              mutate(Date=dmy(sub('day', day, "day-Feb-2018"))) %>% # Add Date Column
              separate(Name, c("NOC", "Name"), "   ", extra = "merge") # Sepatate Country and Name

  # Add this days medallists to the main df
  rbind(m_df,  me_tble) 
} 


##############################################################################
#                           Main - Call Functions                            #
##############################################################################
# scrape and export medals detail table data
scrape.export.medals()

# scrape and export medallist data for the days 10th Feb - 25th Feb, and append to medallist tibble
medallist_df <- data_frame()
medallist_df <- do.call(rbind, lapply(c(10:25), scrape.medallists, medallist_df))

# export medallist df
write.csv(medallist_df, file = medallists_filename)


##############################################################################
#                           Clean up the R Environment                       #
##############################################################################
print('Scrape and Export of Pyeongchang 2018 medals and medallists complete')
print(paste(medals_filename, 'and', medallists_filename, 'have been created', sep = " "))
print('Removing variables, removing variables from this script from your R Environment ')
rm(medal_detail_URL, medallists_URL, medallist_df, package, 
   scrape.export.medals, scrape.medallists, data_folder,
   medals_filename, medallists_filename)

