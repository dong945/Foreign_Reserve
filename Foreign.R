# 載入套件 ----
library(rvest)
library(lubridate)
library(scales)
library(ggplot2)
# 中央銀行網址, nowPage=1&pagesize=20, 代表第1頁, 抓20筆
ur1 <- "http://www.cbc.gov.tw/lp.asp?CtNode=644&CtUnit=307&BaseDSD=32&mp=1&nowPage=1&pagesize=20"
page.source <- read_html(ur1)
DataTable2 <- html_nodes(page.source, ".DataTable2")
table1 <- html_table(DataTable2)
table1 <- as.data.frame(table1)
colnames(table1) <- c("Date","US.Billion")
table1
# 中央銀行網址, nowPage=2&pagesize=20, 代表第2頁, 抓20筆
ur2 <- "http://www.cbc.gov.tw/lp.asp?CtNode=644&CtUnit=307&BaseDSD=32&mp=1&nowPage=2&pagesize=20"
page.source <- read_html(ur2)
DataTable2 <- html_nodes(page.source, ".DataTable2")
table2 <- html_table(DataTable2)
table2 <- as.data.frame(table2)
colnames(table2) <- c("Date","US.Billion")
table2
# 將讀入的資料合併為一個data.frame, 取2015/01~2017/07 ----
DataTable2 <- rbind(table1, table2)
DataTable2 <- subset(DataTable2, DataTable2$Date > 2014.12)
# 日期格式轉換Function ----
CNV_DATE <- function(x){
  tmp <- as.character(x + 0.001)
  Y <- substr(tmp, 1, 4)
  M <- substr(tmp, 6, 7)
  paste(Y, M, "01", sep = "-")
}
# 以年、月為分群條件，待會畫面用 ----
DataTable2$Date <- CNV_DATE(DataTable2$Date)
DataTable2$year <- year(DataTable2$Date)
DataTable2$month <- month(DataTable2$Date, label = TRUE)
# ggplot2畫圖 ----
g <- ggplot(DataTable2, aes(x=month, y=US.Billion))
g <- g + geom_line(aes(color=factor(year), group = year))
g <- g + scale_color_discrete(name="Year")
g <- g + scale_y_continuous(labels = comma)
g <- g + labs(title="外匯存底", x="Month", y="Billion")
g
# 外資在那裏? ----
F2017 <- subset(DataTable2, DataTable2$Date > "2016-12-01")
# 外資佔外匯存底的比率
F2017$Rate <- c(0.86, 0.85, 0.83, 0.81, 0.79, 0.77, 0.73)
F2017$FI <- F2017$US.Billion*F2017$Rate
