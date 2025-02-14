 #
 # con<-function(){
 #  con1 <- dbConnect(RSQLite::SQLite(), system.file("extdata", "ChinaSeedSoyDatabase.sqlite", package = "soyplant"))
 #  return(con1)
 #   }
 #

library(openxlsx)
{#write.table(read.xlsx("temp_traits_qr.xlsx"),"data/qr_trait.txt",row.names=FALSE)
write.table(read.xlsx("temp_traits.xlsx"),"data/soy_traits.txt",row.names=FALSE)
write.table(read.xlsx("temp_traits_baiaoyun.xlsx",startRow = 2),"data/baiaoyun_traits.txt",row.names=FALSE)
write.table(read.xlsx("temp_field.xlsx"),"data/field.txt",row.names=FALSE)
write.table(read.xlsx("temp_soy_mapa.xlsx"),"data/mapa.txt",row.names=FALSE)
# 加载所需的包
library(tidyr)
library(dplyr)
# 使用separate_rows函数按照_分割level_C字段，并展开成多行，其他字段保持不变
data <- read.xlsx("temp_traits.xlsx") %>%
  separate_rows(level_C, sep = "_")
# 使用mutate函数新增level_code字段，并使用gsub函数去除level_C中的非数字部分
new_data <- data %>%
  mutate(level_code = gsub("[^0-9-]", "", level_C))
# 使用mutate函数将name_code和level_code字段用-连接，并更新level_code字段的值
new_data <- new_data %>%
  mutate(level_code = paste(name_code, level_code, sep = "-"))
write.table(new_data,"data/qr_trait.txt",row.names=FALSE)
}

download_model<-function(){
  COMBI<-get_combination(mapa)
  POP<-get_population(COMBI)
  PLANT<-get_plant(POP)
  LINE<-get_line(PLANT)
  PRI<-get_primary(LINE)
  ADV<-get_advanced(PRI)
  PLANTING<-planting(ADV,ck="冀豆12")
  #
  library(openxlsx)
  wb<-createWorkbook()
  addWorksheet(wb,"mapa")
  addWorksheet(wb,"combination")
  addWorksheet(wb,"population")
  addWorksheet(wb,"plant")
  addWorksheet(wb,"line")
  addWorksheet(wb,"primary")
  addWorksheet(wb,"advanced")
  addWorksheet(wb,"planting")

  writeDataTable(wb,"mapa",mapa)
  writeDataTable(wb,"combination",COMBI)
  writeDataTable(wb,"population",POP)
  writeDataTable(wb,"plant",PLANT)
  writeDataTable(wb,"line",LINE)
  writeDataTable(wb,"primary",PRI)
  writeDataTable(wb,"advanced",ADV)
  writeDataTable(wb,"planting",PLANTING)
  mydir<-paste(getwd(),"/","soyplant_model.xlsx",sep="")
  saveWorkbook(wb,mydir,overwrite = TRUE)
}
