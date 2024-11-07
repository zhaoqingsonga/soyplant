
 # con<-function(){
 #  con1 <- dbConnect(RSQLite::SQLite(), system.file("extdata", "ChinaSeedSoyDatabase.sqlite", package = "soyplant"))
 #  return(con1)
 #   }
 #

library(openxlsx)
<<<<<<< Updated upstream
write.table(read.xlsx("temp_traits_qr.xlsx"),"data/qr_trait.txt",row.names=FALSE)
write.table(read.xlsx("temp_traits.xlsx"),"data/soy_traits.txt",row.names=FALSE)
write.table(read.xlsx("temp_traits_baiaoyun.xlsx",startRow = 2),"data/baiaoyun_traits.txt",row.names=FALSE)
write.table(read.xlsx("temp_field.xlsx"),"data/field.txt",row.names=FALSE)
write.table(read.xlsx("temp_soy_mapa.xlsx"),"data/mapa.txt",row.names=FALSE)


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
=======
write.table(read.xlsx("temp_qr_trait.xlsx"),"data/qr_trait.txt",row.names=FALSE)
write.table(read.xlsx("temp_soy_traits.xlsx"),"data/soy_traits.txt",row.names=FALSE)
write.table(read.xlsx("temp_stage_prefix.xlsx"),"data/stage_prefix.txt",row.names=FALSE)
write.table(read.xlsx("temp_ex_type.xlsx"),"data/ex_type.txt",row.names=FALSE)
>>>>>>> Stashed changes
