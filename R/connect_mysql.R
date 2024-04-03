
# 连接到MySQL数据库
library(RMySQL)
con <- dbConnect(MySQL(),
                 user = "root",
                 password = "123234345",
                 dbname = "soyplant",
                 host = "localhost")

query<-"SELECT * FROM combination"
result <- dbGetQuery(con, query)

query <- "UPDATE combination SET f = 0 WHERE ma='五星1'"
dbSendQuery(con, query)




dbDisconnect(con)
