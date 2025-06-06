#' getWaterLaneProtected
#'
#' 解析描述水沟、道路和保护行序列的字符串。
#'
#' @param blockStructure 描述地块特征的字符串。默认值为 "w/23/2r/w/2p/w/2p/23/w"。
#' 字符串可以包含以下元素：
#'   - 'w': 水沟
#'   - 'r': 道路
#'   - 'p': 保护行
#'   - 数字可跟在 'r' 或 'p' 后表示多个道路或保护行。
#'
#' @return 一个包含以下内容的列表：
#'   - totalColumns: 总列数，包括水沟、道路和保护行。
#'   - waterColumns: 水沟列的索引。
#'   - laneColumns: 道路列的索引。
#'   - protectedColumns: 保护行列的索引。
#'
#' @examples
#' getWaterLaneProtected("w/23/2r/w/2p/w/2p/23/w")
#' getWaterLaneProtected("5/2r/3p/w")
getWaterLaneProtected <- function(blockStructure = "w/23/2r/w/2p/w/2p/23/w") {

  # 将输入字符串按 '/' 分割成独立的元素
  blockStructure <- unlist(strsplit(blockStructure, "/"))

  re <- NULL  # 初始化一个空向量用于存储序列

  # 遍历分割后的每个元素
  for (i in seq_along(blockStructure)) {
    # 检查是否为水沟
    if (blockStructure[i] == 'w') {
      re <- c(re, -77)
    }
    # 检查是否为单个道路
    else if (blockStructure[i] == 'r') {
      re <- c(re, -111)
    }
    # 检查是否为多个道路
    else if (grepl('r', blockStructure[i])) {
      re <- c(re, rep(-111, as.numeric(sub('r', '', blockStructure[i]))))
    }
    # 检查是否为单个保护行
    else if (blockStructure[i] == 'p') {
      re <- c(re, -9)
    }
    # 检查是否为多个保护行
    else if (grepl('p', blockStructure[i])) {
      re <- c(re, rep(-9, as.numeric(sub('p', '', blockStructure[i]))))
    }
    # 处理普通列
    else {
      re <- c(re, rep(1, as.numeric(blockStructure[i])))
    }
  }

  totalColumns <- length(re)  # 计算序列的总长度
  waterColumns <- which(re == -77)  # 获取水沟索引
  laneColumns <- which(re == -111)  # 获取道路索引
  protectedColumns <- which(re == -9)  # 获取保护行索引

  # 创建一个包含结果的列表
  mylist <- list(
    totalColumns = totalColumns,
    waterColumns = waterColumns,
    laneColumns = laneColumns,
    protectedColumns = protectedColumns
  )

  return(mylist)
}

modibridges <- function(bridges) {
  # 将逗号分隔的字符串拆分成向量
  ori <- unlist(strsplit(bridges, ","))
  lengthori <- length(ori)

  # 预分配列表以存储中间结果
  bri_list <- vector("list", lengthori)

  for (i in seq_len(lengthori)) {
    # 将每个部分进一步拆分成数字向量
    unit1 <- as.numeric(unlist(strsplit(ori[i], "/")))

    # 根据拆分结果的长度决定如何处理
    bri_list[[i]] <- if (length(unit1) == 1) {
      unit1  # 如果只有一个元素，直接添加
    } else {
      rep(unit1[1], unit1[2])  # 否则，重复第一个元素指定次数
    }
  }

  # 将列表折叠成一个向量
  bri <- unlist(bri_list)
  return(bri)
}



#' 根据子组标记矩阵
#'
#' 将输入矩阵按行分组，并根据条件对组内元素进行标记。
#'
#' @param ma 输入矩阵。
#' @param subg 子组的大小。默认值为3。
#' @param mysign 用于标记的符号。默认值为-6。
#'
#' @return 返回标记后的矩阵。
#' @export
#'
#' @examples
#' ma <- matrix(rep(0, 200), nrow = 10, ncol = 20)
#' ma[, 3] <- -77
#' ma[, 18] <- -77
#' result <- plantByGroup(ma, subg = 6)
#' print(result)
plantByGroup <- function(ma=matrix(rep(0, 200), nrow = 10, ncol = 20), subg = 3, mysign = -6) {
  n_rows <- nrow(ma)  # 获取矩阵的行数
  n_cols <- ncol(ma)  # 获取矩阵的列数

  for (i in 1:n_rows) {
    j <- 1
    while (j <= n_cols) {  # 必须使用 while 循环，for 循环不适用
      # 获取当前子组
      current_subgroup <- ma[i, j:min(j + subg - 1, n_cols)]

      # 检查子组是否全为零
      if (length(current_subgroup) == subg && all(current_subgroup == 0)) {
        j <- j + subg  # 如果是，将索引跳过子组长度
      } else {
        if (ma[i, j] == 0) {
          ma[i, j] <- mysign  # 否则，将当前元素标记
        }
        j <- j + 1  # 处理下一个元素
      }
    }
  }

  return(ma)
}


#' 设计地块布局
#'
#' 设计一个包含水沟、道路和保护行的地块布局。
#'
#' @param blocks 地块数。默认值为20。
#' @param y 每块地的列数。默认值为10。
#' @param bridges 每块地的宽度。默认值为c(9, rep(6, 10), rep(3, 9))。
#' @param distanceBetweenBlocks1 宽条间距，默认值为1。
#' @param distanceBetweenBlocks2 窄条间距，默认值为0.2。
#' @param protectedColumns 保护行所在的列，默认值为c(3)。
#' @param protectedBlocks 保护行所在的块，默认值为c(1, 3, 4, 6)。
#' @param waterColumns 水沟所在的列，默认值为c(5)。
#' @param laneColumns 道路所在的列，默认值为c(9)。
#' @param protectedArea 植被区域，默认值为c(56, 65, 6, 8, 34, 38, 2, 5)。
#' @param designFromLeft 是否从左侧设计布局，默认值为TRUE。
#' @param plantFromLeft 是否从左侧种植，默认值为TRUE。
#' @param subg 子组，默认值为3。
#'
#' @return 返回设计的地块布局矩阵。
#'
#@examples
#designPlot()
#designPlot(blocks=15, y=8, waterColumns=c(4, 8))
designPlot <- function(y = 10,
                       bridges = c(9, rep(6, 10), rep(3, 9)),
                       distanceBetweenBlocks1 = 1,
                       distanceBetweenBlocks2 = 0.2,
                       protectedColumns = c(3),
                       protectedBlocks = c(1, 3, 4, 6),
                       waterColumns = c(5),
                       laneColumns = c(9),
                       protectedArea = c(56, 65, 6, 8, 34, 38, 2, 5),
                       designFromLeft = TRUE,
                       plantFromLeft = TRUE,
                       subg = 3) {

  blocks = length(bridges)
  # 如果不从左侧设计布局，则调整保护行、水沟和道路的列
  if (!designFromLeft) {
    protectedColumns <- (1 + y) - protectedColumns
    waterColumns <- (1 + y) - waterColumns
    laneColumns <- (1 + y) - laneColumns
    if (!is.null(protectedArea)) {
      sele <- c((0:(length(protectedArea) / 4 - 1)) * 4 + 3,
                (0:(length(protectedArea) / 4 - 1)) * 4 + 4)
      protectedArea[sele] <- (1 + y) - protectedArea[sele]
    }
  }

  # 确定种植标记
  marker1 <- if (plantFromLeft) 1 else 0

  x <- blocks * 2  # 计算总行数
  protected_rows <- (blocks - protectedBlocks + 1) * 2

  # 确保桥的长度与块数一致
  if (length(bridges) == 1) {
    bridges <- rep(bridges, blocks)
  }
  bridges <- rev(c(bridges, rep(0, blocks - length(bridges))))

  # 初始化布局矩阵
  fi <- matrix(0, nrow = x, ncol = y + 1)
  fi[, waterColumns] <- -77  # 标记水沟列

  # 交替标记道路宽度
  seq1 <- seq(1, x, 4)
  seq3 <- seq(3, x, 4)
  if ((x / 2) %% 2 == 0) {
    fi[seq1, ] <- -1
    fi[seq3, ] <- -11
    fi[seq1, y + 1] <- distanceBetweenBlocks2
    fi[seq3, y + 1] <- distanceBetweenBlocks1
  } else {
    fi[seq1, ] <- -11
    fi[seq3, ] <- -1
    fi[seq1, y + 1] <- distanceBetweenBlocks1
    fi[seq3, y + 1] <- distanceBetweenBlocks2
  }

  fi[, laneColumns] <- -111  # 标记道路列

  # 将桥的宽度应用到布局矩阵
  fi[seq(2, x, 2), y + 1] <- bridges
  # 确保 protectedColumns 是列名或列号向量
  for (col in protectedColumns) {
    fi[fi[, col] == 0, col] <- -9
  }
  #fi[fi[, protectedColumns] == 0, protectedColumns] <- -9
  # 更新指定行和列中的值
  for (col in 1:y) {
    fi[protected_rows, col][fi[protected_rows, col] == 0] <- -99
  }
  #fi[protected_rows, 1:y][fi[protected_rows, 1:y] == 0] <- -99


  # 计算并添加累积列
  fi <- cbind(fi, rev(cumsum(rev(fi[, y + 1]))))
  fi <- cbind(fi, rep(0, x))
  fi[, y + 3][seq(2, x, 2)] <- blocks:1
  fi <- cbind(fi, 1:x)

  mycu <- rev(cumsum(rev(fi[, y + 1])))
  mycu1 <- c(mycu[-1], 0)
  mycu2 <- cbind(mycu1, mycu)

  # 标记植被区域
  if (!is.null(protectedArea)) {
    myma <- matrix(protectedArea, ncol = 4, byrow = TRUE)
    for (mi in seq_len(nrow(myma))) {
      se_li <- (mycu2[, 2] > myma[mi, 1] & myma[mi, 1] > mycu2[, 1]) |
               (mycu2[, 2] > myma[mi, 2] & myma[mi, 2] > mycu2[, 1]) |
               (myma[mi, 2] > mycu2[, 1] & mycu2[, 1] > myma[mi, 1]) |
               (myma[mi, 2] > mycu2[, 2] & mycu2[, 2] > myma[mi, 1])
      fi[se_li, myma[mi, 3]:myma[mi, 4]] <- -8
    }
  }

  # 调用 plantByGroup 函数进行分组种植
  fi <- plantByGroup(fi, subg, -6)

  # 将特定列设置为 NA 或 0
  fi[, y + 3][fi[, y + 3] == -6] <- NA
  fi[, y + 1][fi[, y + 1] == -6] <- 0

  # 遍历地块进行编号
  p1<-1
  num<-1
  for(i in rev(seq(2,x,2))){
    if(p1%%2==marker1){
      pp1<-0
      for(j in 1:y){
        if(fi[i,j]==0){fi[i,j]<-num; num<-num+1;pp1<-pp1+1}
      }
      if(pp1!=0){p1<-p1+1}
    }else{
      pp1<-0
      for(j in y:1){
        if(fi[i,j]==0){fi[i,j]<-num; num<-num+1;pp1<-pp1+1}
      }
      if(pp1!=0){p1<-p1+1}
    }
  }
  colnames(fi)<-c(paste("L",1:(ncol(fi)-4),sep=""),c("stock_length(m)","cumulative_length(m)","block_name","number"))
  return(fi)
}


myAny<-function(vec1){
  return(any(vec1%in%c(-77,-111,-9,-8,-6)))
}

#select special columns

selectedCol<-function(fi=designPlot()){
  nc<-ncol(fi)
  result<-which(apply(fi,2,myAny)==TRUE)
  result1<-result+1
  result_1<-result-1
  result2<-sort(c(1,result,result1,result_1,(nc-4):nc))
  #del 0
  result2<-unique(result2[result2!=0])
  return(fi[,result2])
}




#' 获取地块的统计信息
#'
#' 对设计布局进行统计分析，计算每个地块的相关指标。
#'
#' @param fi 设计布局矩阵，默认使用 designPlot 函数生成的矩阵。
#'
#' @return 返回统计信息，包括每个地块的名称、道路总数、平均道路宽度以及总数。
#' @export
#'
#' @examples
#' fi <- designPlot()
#' stats <- getSta(fi)
#' print(stats)
getSta <- function(fi = designPlot()) {
  nc <- ncol(fi)
  nr <- nrow(fi)

  # 仅保留偶数行
  fi <- fi[seq(2, nr, 2), ]

  # 提取除最后四列以外的数据
  fi2 <- fi[, 1:(nc - 4)]

  # 将负值转换为0，正值转换为1
  fi2[fi2 < 0] <- 0
  fi2[fi2 > 0] <- 1

  # 统计每行中大于0的元素个数
  fi3 <- cbind(fi[, (nc - 3)], apply(fi2, 1, sum))

  # 按地块名称对数据进行汇总
  fi4 <- aggregate(fi3, by = list(name = fi3[, 1]), FUN = sum)

  # 计算平均道路宽度
  fi4$V1 <- fi4[, 2] / fi4[, 1]

  # 添加总计行
  total <- apply(fi4, 2, sum)
  total[1] <- 'total'
  fi5 <- rbind(fi4, total)
  colnames(fi5)<-c("类型","条数","行数")
  return(fi5)
}

#' 创建种子列表
#'
#' 根据输入的数据框，生成包含指定重复次数的元素的列表。
#'
#' @param myd 输入的数据框，默认包含三列（编号、名称、重复次数）。
#'
#' @return 返回一个数据框，其中包含编号和重复元素的列表。
#' @export
#'
#' @examples
#' data <- data.frame(num = 1:3, name = c('JD12', 'JD17', '五星1'), re = c(3, 10, 6))
#' result <- makelist(data)
#' print(result)
makelist <- function(myd = data.frame(num = 1:3, name = c('JD12', 'JD17', '五星1'), rp = c(3, 10, 6))) {
  result <- NULL
  number <- NULL
  for (i in 1:nrow(myd)) {
    result <- c(result, as.character(rep(myd[i, 2], myd[i, 3])))
    number <- c(number, 1:myd[i, 3])
  }
  return(data.frame(result,number))
}

#' 创建种子列表
#'
#' 根据输入的数据框，生成包含指定重复次数的元素的列表。
#'
#' @param myd 输入的数据框，默认包含三列（编号、名称、重复次数）。
#'
#' @return 返回一个列表，其中包含指定重复次数的元素。
#' @export
#'
#' @examples
#' data <- data.frame(num = 1:3, name = c('JD12', 'JD17', '五星1'), re = c(3, 10, 6))
#' result <- makelist2(data)
#' print(result)
makelist2 <- function(myd = data.frame(num = 1:3, name = c('JD12', 'JD17', '五星1'), re = c(3, 10, 6))) {
  result <- NULL
  for (i in 1:nrow(myd)) {
    result <- c(result, as.character(rep(myd[i, 2], myd[i, 3])))
  }
  return(result)
}


#' 根据指定列的值复制数据框行并添加序列号
#'
#' 该函数根据数据框中指定列的值复制每一行，并为每组复制添加序列号列。
#'
#' @param df 要扩展的数据框（data.frame）
#' @param replicate_col 字符型，指定包含复制次数的列名，默认为"rows"
#' @param sequence_col 字符型，指定新序列号列的列名，默认为"seq_id"
#'
#' @return 返回一个新的数据框，其中：
#' \itemize{
#'   \item 包含原始数据框的所有列
#'   \item 新增一个序列号列，显示每组复制中的序号
#' }
#'
#' @examples
#' # 示例数据
#' sample_data <- data.frame(
#'   id = c(1, 2, 3),
#'   name = c("A", "B", "C"),
#'   times = c(2, 3, 1)
#' )
#'
#' # 使用'times'列扩展行
#' expanded_data <- expand_rows(sample_data, "times", "copy_num")
#' print(expanded_data)
#'
#' # 输出结果：
#' #   id name times copy_num
#' # 1  1    A     2        1
#' # 2  1    A     2        2
#' # 3  2    B     3        1
#' # 4  2    B     3        2
#' # 5  2    B     3        3
#' # 6  3    C     1        1
#'
#' @export
expand_rows <- function(df, replicate_col = "rows", sequence_col = "seq_id") {
  # 检查数据框是否包含指定的复制列
  if (!replicate_col %in% colnames(df)) {
    stop(paste0("数据框中必须包含'", replicate_col, "'列"))
  }

  # 确保复制列是数值型
  if (!is.numeric(df[[replicate_col]])) {
    stop(paste0("'", replicate_col, "'列必须是数值型"))
  }

  # 检查序号列名是否已存在
  if (sequence_col %in% colnames(df)) {
    stop(paste0("列名'", sequence_col, "'已存在于数据框中，请指定其他列名"))
  }

  # 创建一个空列表来存储扩展后的数据
  expanded_data <- list()

  # 遍历数据框的每一行
  for (i in 1:nrow(df)) {
    current_row <- df[i, , drop = FALSE]  # 保持数据框结构
    n <- current_row[[replicate_col]]

    # 如果复制值 <= 0，跳过该行
    if (n <= 0) {
      next
    }

    # 复制当前行n次
    replicated_rows <- current_row[rep(1, n), , drop = FALSE]

    # 添加序号列（从1到n）
    replicated_rows[[sequence_col]] <- 1:n

    # 添加到列表中
    expanded_data[[i]] <- replicated_rows
  }

  # 合并所有行
  result <- do.call(rbind, expanded_data)

  # 重置行名
  rownames(result) <- NULL

  return(result)
}

#' 创建重复名称的数据框
#'
#' 该函数根据给定的名称和重复次数，生成一个包含重复记录的数据框。
#'
#' @param name 字符型，要重复的名称（长度为1）
#' @param rp 整型，重复次数（必须大于等于1）
#'
#' @return 返回一个数据框，包含三列：
#' \describe{
#'   \item{stageid}{与输入name相同的内容}
#'   \item{name}{与输入name相同的内容}
#'   \item{line_number}{格式为"1-总重复次数"}
#' }
#'
#' @examples
#' # 基本用法
#' create_repeated_dataframe("试验材料A", 3)
#'
#' # 结果示例
#' #   stageid     name line_number
#' # 1 试验材料A 试验材料A        1-3
#' # 2 试验材料A 试验材料A        1-3
#' # 3 试验材料A 试验材料A        1-3
#'
#' @export
create_repeated_dataframe <- function(name, rp) {
  # 检查参数有效性
  if (!is.character(name) || length(name) != 1) {
    stop("name参数必须是一个长度为1的字符向量")
  }
  if (!is.numeric(rp) || length(rp) != 1 || rp < 1) {
    stop("rp参数必须是一个大于等于1的数值")
  }

  # 创建数据框
  result <- data.frame(
    stageid = rep(name, rp),
    name = rep(name, rp),
    line_number = rep(paste0("1-", rp), rp),  # 修改为固定格式"1-总重复次数"
    stringsAsFactors = FALSE
  )

  return(result)
}




# 加载必要的包
library(dplyr)

#' 根据最后一列的值复制数据框的行
#'
#' 该函数根据数据框最后一列中的整数值来复制行。每一行将根据对应的
#' 最后一列的值重复相应的次数。
#'
#' @param df 一个数据框，其中最后一列包含指示每行复制次数的整数值。
#'
#' @return 一个根据最后一列的值复制行的新数据框。
#'
#' @examples
#' df <- data.frame(
#'   name = c("A", "B", "C"),
#'   value = c(10, 20, 30),
#'   times = c(1, 3, 2)
#' )
#' replicate_rows(df)
#'
#' @export
replicate_rows <- function(df) {
  # 确定最后一列为复制次数
  rep_times <- df[[ncol(df)]]

  # 检查rep_times是否为正整数
  if (!all(rep_times == floor(rep_times) & rep_times > 0)) {
    stop("最后一列中的所有值必须为正整数")
  }


  # 通过行号进行复制
  replicated_df <- df[rep(seq_len(nrow(df)), rep_times), ]

  return(replicated_df)
}




#' 获取子组
#'
#' 从向量中提取子组。
#'
#' @param j 子组的起始索引。
#' @param vec 输入的向量。
#' @param subg 子组的大小。
#'
#' @return 返回提取的子组。
#' @export
#'
#' @examples
#' vec <- c(0, 0, 0, -77, 0, 0, 0, 0, -77, 0, 0, 0, 0, 0, 0, 0, -9, 0, 0)
#' sub_vec <- getSub(3, vec, 3)
#' print(sub_vec)
getSub <- function(j = 1, vec = c(0, 0, -77, 0, 0, 0), subg = 3) {
  len1 <- length(vec)
  if ((j + subg - 1) <= len1) {
    re <- vec[j:(j + subg - 1)]
  } else if (j <= len1) {
    re <- vec[j:len1]
  } else {
    re <- vec[len1]
  }
  return(re)
}

#' 在设计图中种植种子
#'
#' 该函数用于在设计图中根据种子列表进行种植。根据种子列表中的信息，
#' 将设计图中相应的位置替换为种子的详细信息。
#'
#' @param fi 矩阵，设计图矩阵，默认使用 \code{designPlot()} 生成。
#' @param seed 矩阵，种子列表，默认使用 \code{makelist()} 生成。
#' @return 矩阵，返回处理后的设计图矩阵。
#' @examples
#' plant()
#' @export
plant <- function(fi = designPlot(), seed = makelist(),returnfi=TRUE) {
  # 获取设计图的列数和行数，最后四列不属于字段
  y<-ncol(fi) - 4
  plantedfi<-fi[,1:y]
  plantedseed<-seed
  plantedseed$row<-NULL
  plantedseed$col<-NULL
  plantedseed$total_line<-NULL

  #
  tiao<-fi[,ncol(fi)-1]
  # 遍历种子列表的每一行
  for (j in 1:nrow(seed)) {
    # 使用 paste 函数构建种子信息字符串，并替换设计图中的相应位置
    plantedfi[plantedfi == j] <- paste(seed[j, 1], '|', seed[j, 2], '|', j, sep = "")
   #
    plantedseed$row[j]<-tiao[which(fi[,1:y]==j,arr.ind=TRUE)[1]]
    plantedseed$col[j]<-which(fi[,1:y]==j,arr.ind=TRUE)[2]
    #
    plantedseed$total_line[j]<-j
    }
  plantedfi<-cbind(plantedfi,fi[,(ncol(fi)-3):ncol(fi)])
  # 返回处理后的设计图矩阵
  if(returnfi){
    return(plantedfi)
  }else{
    return(plantedseed)
  }
}



