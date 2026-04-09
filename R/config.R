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

# Only write data files if they don't exist (use lazy-loaded data in production)
if (!file.exists("data/baiaoyun_traits.txt")) {
  write.table(bay, "data/baiaoyun_traits.txt", row.names = FALSE)
}
if (!file.exists("data/field.txt")) {
  write.table(read.xlsx("inst/extdata/temp_field.xlsx"), "data/field.txt", row.names = FALSE)
}
if (!file.exists("data/mapa.txt")) {
  write.table(read.xlsx("inst/extdata/temp_soy_mapa.xlsx"), "data/mapa.txt", row.names = FALSE)
}




# 使用separate_rows函数按照_分割level_C字段，并展开成多行，其他字段保持不变
data <- soy_traits %>%
  separate_rows(level_C, sep = "_")
# 使用mutate函数新增level_code字段，并使用gsub函数去除level_C中的非数字部分
new_data <- data %>%
  mutate(level_code = gsub("[^0-9-]", "", level_C))
# 使用mutate函数将name_code和level_code字段用-连接，并更新level_code字段的值
new_data <- new_data %>%
  mutate(level_code = paste(name_code, level_code, sep = "-"))
if (!file.exists("data/qr_trait.txt")) {
  write.table(new_data, "data/qr_trait.txt", row.names = FALSE)
}



#' 生成模型各阶段数据
#'
#' 此函数依次从 mapa 中提取组合、群体、单株、系、初选、高代和种植数据，
#' 返回包含所有阶段数据的列表。
#'
#' @return 列表，包含 mapa, combination, population, plant, line, primary, advanced, planting 八个数据框。
#' @export
generate_model_data <- function() {
  combi <- get_combination(mapa)
  pop <- get_population(combi)
  plant <- get_plant(pop)
  line <- get_line(plant)
  pri <- get_primary(line, next_stage = "初级产比", target_stage = "高级产比")
  adv <- get_primary(pri, next_stage = "高级产比", target_stage = "多点鉴定")
  planting <- planting(adv, ck = "冀豆12")

  list(
    mapa = mapa,
    combination = combi,
    population = pop,
    plant = plant,
    line = line,
    primary = pri,
    advanced = adv,
    planting = planting
  )
}

#' 将模型数据写入 Excel 文件
#'
#' @param data 列表，由 `generate_model_data()` 生成的数据列表。
#' @param file_name 字符串，导出的 Excel 文件名。默认为 `"soyplant_model.xlsx"`。
#' @param dir_path 字符串，保存文件的目标目录。默认为当前工作目录。
#'
#' @return 字符型，文件保存路径。
#' @importFrom openxlsx createWorkbook addWorksheet writeDataTable saveWorkbook
#' @export
write_model_excel <- function(data,
                               file_name = "soyplant_model.xlsx",
                               dir_path = getwd()) {
  wb <- openxlsx::createWorkbook()
  sheets <- c("mapa", "combination", "population", "plant",
              "line", "primary", "advanced", "planting")
  lapply(sheets, openxlsx::addWorksheet, wb = wb)

  Map(function(sheet, df) openxlsx::writeDataTable(wb, sheet, df),
      sheets, data[sheets])

  output_path <- file.path(dir_path, file_name)
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  output_path
}

#' 下载模型数据并导出为 Excel 文件（兼容旧接口）
#'
#' @inheritParams generate_model_data
#' @inheritParams write_model_excel
#' @return 无返回值。函数执行后将在指定目录生成 Excel 文件。
#' @export
download_model <- function(file_name = "soyplant_model.xlsx",
                           dir_path = getwd()) {
  data <- generate_model_data()
  write_model_excel(data, file_name, dir_path)
}

#
#
#




