
 # con<-function(){
 #  con1 <- dbConnect(RSQLite::SQLite(), system.file("extdata", "ChinaSeedSoyDatabase.sqlite", package = "soyplant"))
 #  return(con1)
 #   }
 #
 #
library(openxlsx)
write.table(read.xlsx("temp_qr_trait.xlsx"),"data/qr_trait.txt",row.names=FALSE)
write.table(read.xlsx("temp_soy_traits.xlsx"),"data/soy_traits.txt",row.names=FALSE)
