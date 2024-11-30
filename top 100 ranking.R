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


data_overall <- data.frame(
  Rank = rank,
  Player_ID = player_id,
  Player_Name = player_name,
  Total_Earnings = total_earnings,
  Highest_Paying_Game = highest_paying_game,
  Total_Game_Earnings = total_game_earnings,
  Percent_of_Total = percent_of_total,
  stringsAsFactors = FALSE
)
data_overall$Rank <- as.numeric(gsub("\\.", "", data_overall$Rank))
print(head(data_overall, 20))

write.csv(data_overall, "esports_earnings_top100_overall.csv", row.names = FALSE)

url <- 'https://www.esportsearnings.com/countries'
webpage <- read_html(url)

# 抓取所有行
rows <- webpage %>% html_nodes("tr")

# 初始化空的数据框
data <- data.frame(
  Rank = character(),
  Country = character(),
  Total_Earnings = character(),
  Number_of_Players = character(),
  Top_Game = character(),
  Earnings_From_Top_Game = character(),
  Percent_From_Top_Game = character(),
  stringsAsFactors = FALSE
)

# 遍历每一行并提取列数据
for (row in rows) {
  columns <- row %>% html_nodes("td") %>% html_text(trim = TRUE)
  
  # 跳过空行或列数不足的行
  if (length(columns) == 7 && all(columns != "")) {
    data <- bind_rows(data, as.data.frame(t(columns), stringsAsFactors = FALSE))
  }
}

# 重命名列
colnames(data) <- c("Rank", "Country", "Total_Earnings", "Number_of_Players", "Top_Game", "Earnings_From_Top_Game", "Percent_From_Top_Game")

# 打印前几行数据
print(head(data, 10))

library(rvest)
library(dplyr)

library(RSelenium)

library(RSelenium)

# 启动 Selenium 服务
rD <- rsDriver(browser = "chrome", chromever = "latest")
remDr <- rD$client

# 确保 URL 是正确格式的单一字符串
url <- 'https://escharts.com/players?game=lol'

# 打开网页
remDr$navigate(url)

# 检查是否导航成功
Sys.sleep(5)

# 抓取页面源代码
page_source <- remDr$getPageSource()[[1]]

# 使用 rvest 解析页面
library(rvest)
webpage <- read_html(page_source)

# 在这里继续抓取数据...

# 关闭 Selenium
remDr$close()
rD$server$stop()
