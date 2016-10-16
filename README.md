# Natural-Language-Processing

library("rvest")
library("stringr")
library("tidyr")
url <- 'http://espn.go.com/nfl/superbowl/history/winners'
webpage <- read_html(url)
sb_table <- html_nodes(webpage, 'table') #extracting contents of table tab from html web page
sb <- html_table(sb_table)[[1]] #creating a data frame of the contents that were extracted above
head(sb)
sb <- sb[-(1:2),] #removing first 2 rows 
names(sb) <- c("number", "date", "site", "result") #giving labels to each column
head(sb)
sb$number <- 1:50 #converting roman numbers to actual numbers
head(sb)
sb$date <- as.Date(sb$date, "%B.%d,%Y") #converting dates to conventional form
head(sb)
sb <- separate(sb, result, c("winner", "loser"), sep = ",", remove = TRUE) #separating results column as winner and loser
head(sb)
pattern <- "\\d+$" #for space between 2 elements, $ begins extracting from end
sb$winnerscore <- as.numeric(str_extract(sb$winner,pattern)) #extracting winner score from winner column
sb$loserscore <- as.numeric(str_extract(sb$loser, pattern)) #extracting loser score from loser column
sb$winner <- gsub(pattern, "", sb$winner) #gsub modifies winner column by finally removing the scores that are now stored in winner scores column 
sb$loser <- gsub(pattern, "", sb$loser) #same as above for loser column
head(sb)
write.csv(sb,"Superbowl",row.names = FALSE)
