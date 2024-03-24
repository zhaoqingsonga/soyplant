transname <- function(code=c("fieldid","id","user","stageid","ma"),toenglish=TRUE) {
  mydata <- data.frame(en = NA, ch = NA)
  mydata[1, 1] <- "id"
  mydata[1, 2] <- "材料序号"

  mydata[2, 1] <- "user"
  mydata[2, 2] <- "用户"

  mydata[3, 1] <- "stageid"
  mydata[3, 2] <- "阶段名称"

  mydata[4, 1] <- "name"
  mydata[4, 2] <- "名称"

  mydata[5, 1] <- "f"
  mydata[5, 2] <- "世代"

  mydata[6, 1] <- "ma"
  mydata[6, 2] <- "母本"

  mydata[7, 1] <- "pa"
  mydata[7, 2] <- "父本"

  mydata[8, 1] <- "mapa"
  mydata[8, 2] <- "亲本"

  mydata[9, 1] <- "memo"
  mydata[9, 2] <- "备注"

  mydata[10, 1] <- "stage"
  mydata[10, 2] <- "阶段"

  mydata[11, 1] <- "next_stage"
  mydata[11, 2] <- "下阶段"

  mydata[12, 1] <- "process"
  mydata[12, 2] <- "过程序号"

  mydata[13, 1] <- "path"
  mydata[13, 2] <- "选育过程"

  mydata[14, 1] <- "source"
  mydata[14, 2] <- "来源"

  mydata[15, 1] <- "sele"
  mydata[15, 2] <- "选株数"

  mydata[16, 1] <- "place"
  mydata[16, 2] <- "地点"

  mydata[17, 1] <- "fieldid"
  mydata[17, 2] <- "田间序号"

  mydata[18, 1] <- "code"
  mydata[18, 2] <- "代号"


  if(toenglish){
    re_v<-NULL
    for(i in 1:length(code))
      re_v<-c(re_v,mydata$ch[which(code[i]==mydata$en)])
  }else{
    re_v<-NULL
    for(i in 1:length(code))
      re_v<-c(re_v,mydata$en[which(code[i]==mydata$ch)])
  }
  return(re_v)
}
