##############################################################################
#                         Pyeongchang 2018 WebScraper                        #
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
olympics_folder <- "olympics_vis"
medal_detail_URL <- "https://www.pyeongchang2018.com/en/game-time/results/OWG2018/en/general/detailed-medal-standings-.htm"
medallists_URL <- "https://www.pyeongchang2018.com/en/game-time/results/OWG2018/en/general/daily-medallists-date=2018-02-day.htm"
medals_filename <- paste(olympics_folder, "pyeongchang_medals_table.csv", sep = "/")
medallists_filename <- paste(olympics_folder, "pyeongchang_medallists.csv", sep = "/")


##############################################################################
#         Check if we need to create a dir for the csv files                 #
##############################################################################
if (!dir.exists(olympics_folder)){
  dir.create(olympics_folder)
}


##############################################################################
#         Function: Scrape Detailed Medal Table and Export data              #
##############################################################################
scrape.export.medals <- function() {
  medals <- read_html(medal_detail_URL)
  medals_table <- html_table(html_nodes(medals, "table")[[1]], fill = TRUE)
  
  # Rename Columns so they're unique
  names(medals_table) <- c("Rank","City",
                           "Gold_M","Gold_F","Gold_X","Gold_T",
                           "Silver_M","Silver_F","Silver_X","Silver_T",
                           "Bronze_M","Bronze_F","Bronze_X","Bronze_T",
                           "Total_M","Total_F","Total_X","Total_T",
                           "RankbyTotal")
  
  # Add year column 
  medals_table %>% mutate(Year = '2018')
  
  # Remove the first row as it's a duplicate header     
  medals_table <- medals_table[-1,]

  # Write the df to csv  
  write.csv(medals_table, file = medals_filename, row.names = FALSE)
}


##############################################################################
#       Function: Daily Medal Standings (from 10th - 25th)                   #
##############################################################################
scrape.medallists <- function(day, m_df) {
  medallists <- read_html(sub("day", day, medallists_URL))
  me_tble <- html_table(html_nodes(medallists, "table")[[1]], fill = TRUE)

  # The webpage has two Medal columns, we need to drop the jpg column 
  me_tble <- me_tble[, -c(4)] %>%
              mutate(Date=dmy(sub('day', day, "day-Feb-2018")),
                     City='Pyeongchang') %>% # Add Date Column
              separate(Name, c("Country", "Athlete"), "   ", extra = "merge") %>% # Separate Country and Name
              mutate(Event = str_replace(Event, "Ladies'", "Women's")) %>%
              separate(Event, c("Gender", "Event"), "'s ", fill = "left" , extra = "merge" ) %>%
              mutate(Year = "2018")
    
  # Any NAs for Gender are due to being Mixed Team Sports
  me_tble$Gender[is.na(me_tble$Gender)]  <- "Mixed" 
  
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
write.csv(medallist_df, file = medallists_filename, row.names = FALSE)


##############################################################################
#                           Clean up the R Environment                       #
##############################################################################
print('Scrape and Export of Pyeongchang 2018 medals and medallists complete')
print(paste(medals_filename, 'and', medallists_filename, 'have been created', sep = " "))
print('Removing variables, removing variables from this script from your R Environment ')
rm(medal_detail_URL, medallists_URL, medallist_df, package, 
   scrape.export.medals, scrape.medallists, olympics_folder,
   medals_filename, medallists_filename)
