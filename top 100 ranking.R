library(rvest)
library(dplyr)

url <- 'https://www.esportsearnings.com/players'
webpage <- read_html(url)


rank <- webpage %>% html_nodes("tr td:nth-child(1)") %>% html_text(trim = TRUE)
player_id <- webpage %>% html_nodes("tr td:nth-child(2)") %>% html_text(trim = TRUE)
player_name <- webpage %>% html_nodes("tr td:nth-child(3)") %>% html_text(trim = TRUE)
total_earnings <- webpage %>% html_nodes("tr td:nth-child(4)") %>% html_text(trim = TRUE)
highest_paying_game <- webpage %>% html_nodes("tr td:nth-child(5)") %>% html_text(trim = TRUE)
total_game_earnings <- webpage %>% html_nodes("tr td:nth-child(6)") %>% html_text(trim = TRUE)
percent_of_total <- webpage %>% html_nodes("tr td:nth-child(7)") %>% html_text(trim = TRUE)


data <- data.frame(
  Rank = rank,
  Player_ID = player_id,
  Player_Name = player_name,
  Total_Earnings = total_earnings,
  Highest_Paying_Game = highest_paying_game,
  Total_Game_Earnings = total_game_earnings,
  Percent_of_Total = percent_of_total,
  stringsAsFactors = FALSE
)

print(head(data, 20))