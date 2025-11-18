 #
 # con<-function(){
 #  con1 <- dbConnect(RSQLite::SQLite(), system.file("extdata", "ChinaSeedSoyDatabase.sqlite", package = "soyplant"))
 #  return(con1)
 #   }
#说明：不能利用python函数，关于reticulate,use_python,pyf,level_E等
#关于拼音转写的部分注释掉
library(dplyr)
library(stringr)
#library(reticulate)
library(tidyr)
library(openxlsx)
#use_python("c:/ProgramData/Anaconda3")
#use_python("C:/Users/zhaoq/anaconda3/python.exe")

#py_config()
#pyf<- py_run_file("inst/python/pyfunction.py")
bay<-read.xlsx("inst/extdata/temp_traits_baiaoyun.xlsx",startRow = 2)
level_C <- str_replace_all(bay$列表值LIST_VALUES,",","_")
minmax <- paste(bay$最小值MIN_VALUE,bay$最大值MAX_VALUE,sep="_")
minmax[minmax=="NA_NA"]<-NA
level_C <-coalesce(level_C,minmax)
# level_E=pyf$chinese_to_pinyin(level_C)
# level_E[level_E=="Na"]<-NA

soy_traits<-data.frame(
  ID=1:nrow(bay),
  name_C = bay$性状名称TRAIT_NAME,
  name_lib = bay$名称缩写ABBR_NAME,
  level_C = level_C,
  order = bay$排序ORDER,
  name_E=NA,
  #level_E=level_E,
  species="soybean",
  field_type=recode(bay$性状类型TRAIT_TYPE, "1" = "N","2" = "D","3" = "C","4" = "any"),
  name_code=bay$编码TRAIT_CODE,
  name_baiaoyun=bay$性状名称TRAIT_NAME,
  class_standard=0
)

#write.table(read.xlsx("temp_traits_qr.xlsx"),"data/qr_trait.txt",row.names=FALSE)
#write.table(read.xlsx("temp_traits.xlsx"),"data/soy_traits.txt",row.names=FALSE)
#write.table(soy_traits,"data/soy_traits.txt",row.names=FALSE)
write.table(bay,"data/baiaoyun_traits.txt",row.names=FALSE)
write.table(read.xlsx("inst/extdata/temp_field.xlsx"),"data/field.txt",row.names=FALSE)
write.table(read.xlsx("inst/extdata/temp_soy_mapa.xlsx"),"data/mapa.txt",row.names=FALSE)




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



#' 下载模型数据并导出为 Excel 文件
#'
#' 此函数依次从 mapa 中提取组合、群体、单株、系、初选、高代和种植数据，
#' 并将这些数据写入 Excel 文件。支持自定义文件名与保存目录。
#'
#' @param file_name 字符串，导出的 Excel 文件名。默认为 `"soyplant_model.xlsx"`。
#' @param dir_path 字符串，保存文件的目标目录。默认为当前工作目录。
#'
#' @return 无返回值。函数执行后将在指定目录生成 Excel 文件。
#' @importFrom openxlsx createWorkbook addWorksheet writeDataTable saveWorkbook
#' @export
download_model <- function(file_name = "soyplant_model.xlsx",
                           dir_path = getwd()) {
  # 提取数据各阶段信息
  combi <- get_combination(mapa)
  pop <- get_population(combi)
  plant <- get_plant(pop)
  line <- get_line(plant)
  pri <- get_primary(line)
  adv <- get_advanced(pri)
  planting <- planting(adv, ck = "冀豆12")

  # 创建 Excel 工作簿并添加工作表
  wb <- openxlsx::createWorkbook()
  sheets <- c(
    "mapa", "combination", "population", "plant",
    "line", "primary", "advanced", "planting"
  )
  lapply(sheets, openxlsx::addWorksheet, wb = wb)

  # 写入数据表
  openxlsx::writeDataTable(wb, "mapa", mapa)
  openxlsx::writeDataTable(wb, "combination", combi)
  openxlsx::writeDataTable(wb, "population", pop)
  openxlsx::writeDataTable(wb, "plant", plant)
  openxlsx::writeDataTable(wb, "line", line)
  openxlsx::writeDataTable(wb, "primary", pri)
  openxlsx::writeDataTable(wb, "advanced", adv)
  openxlsx::writeDataTable(wb, "planting", planting)

  # 构造完整路径并保存
  output_path <- file.path(dir_path, file_name)
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
}

#
#
#




