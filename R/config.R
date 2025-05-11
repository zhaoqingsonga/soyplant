 #
 # con<-function(){
 #  con1 <- dbConnect(RSQLite::SQLite(), system.file("extdata", "ChinaSeedSoyDatabase.sqlite", package = "soyplant"))
 #  return(con1)
 #   }
library(dplyr)
library(stringr)
library(reticulate)
library(tidyr)
library(openxlsx)
use_python("c:/ProgramData/Anaconda3")
#py_config()
pyf<- py_run_file("inst/python/pyfunction.py")
bay<-read.xlsx("temp_traits_baiaoyun.xlsx",startRow = 2)
level_C <- str_replace_all(bay$列表值LIST_VALUES,",","_")
minmax <- paste(bay$最小值MIN_VALUE,bay$最大值MAX_VALUE,sep="_")
minmax[minmax=="NA_NA"]<-NA
level_C <-coalesce(level_C,minmax)
level_E=pyf$chinese_to_pinyin(level_C)
level_E[level_E=="Na"]<-NA

soy_traits<-data.frame(
  ID=1:nrow(bay),
  name_C = bay$性状名称TRAIT_NAME,
  name_lib = bay$名称缩写ABBR_NAME,
  level_C = level_C,
  order = bay$排序ORDER,
  name_E=NA,
  level_E=level_E,
  species="soybean",
  field_type=recode(bay$性状类型TRAIT_TYPE, "1" = "N","2" = "D","3" = "C","4" = "any"),
  name_code=bay$编码TRAIT_CODE,
  name_baiaoyun=bay$性状名称TRAIT_NAME,
  class_standard=0
)

#write.table(read.xlsx("temp_traits_qr.xlsx"),"data/qr_trait.txt",row.names=FALSE)
#write.table(read.xlsx("temp_traits.xlsx"),"data/soy_traits.txt",row.names=FALSE)
write.table(soy_traits,"data/soy_traits.txt",row.names=FALSE)
write.table(bay,"data/baiaoyun_traits.txt",row.names=FALSE)
write.table(read.xlsx("temp_field.xlsx"),"data/field.txt",row.names=FALSE)
write.table(read.xlsx("temp_soy_mapa.xlsx"),"data/mapa.txt",row.names=FALSE)




# 使用separate_rows函数按照_分割level_C字段，并展开成多行，其他字段保持不变
data <- soy_traits %>%
  separate_rows(level_C, sep = "_")
# 使用mutate函数新增level_code字段，并使用gsub函数去除level_C中的非数字部分
new_data <- data %>%
  mutate(level_code = gsub("[^0-9-]", "", level_C))
# 使用mutate函数将name_code和level_code字段用-连接，并更新level_code字段的值
new_data <- new_data %>%
  mutate(level_code = paste(name_code, level_code, sep = "-"))
write.table(new_data,"data/qr_trait.txt",row.names=FALSE)




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

#
#
#




