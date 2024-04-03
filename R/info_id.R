
#
ID_prefix <- function(planting = FALSE) {
  onlyID <- format(Sys.time(), "%y%m%d%H%M%S")
  if (planting) {
    onlyID <- paste("f", onlyID, sep = "")
  } else{
    onlyID <- paste("g", onlyID, sep = "")
  }
  return(onlyID)
}
#
ID_suffix5 <- function(n1 = 1, n2 = 99999) {
  id1 <- paste("0000", 1:9, sep = "")
  id2 <- paste("000", 10:99, sep = "")
  id3 <- paste("00", 100:999, sep = "")
  id4 <- paste("0", 1000:9999, sep = "")
  id5 <- 10000:99999
  alln <- c(id1, id2, id3, id4, id5)
  if (is.numeric(n1) & is.numeric(n2) & n2 >= n1 & n1 >= 1 &
      n2 <= 99999)
  {
    re_v <- alln[n1:n2]
    return(re_v)
  } else{
    return(NA)
  }
}
#
ID_suffix4 <- function(n1 = 1, n2 = 9999) {
  id1 <- paste("000", 1:9, sep = "")
  id2 <- paste("00", 10:99, sep = "")
  id3 <- paste("0", 100:999, sep = "")
  id4 <- 1000:9999
  alln <- c(id1, id2, id3, id4)
  if (is.numeric(n1) &
      is.numeric(n2) & n2 >= n1 & n1 >= 1 & n2 <= 9999)
  {
    re_v <- alln[n1:n2]
    return(re_v)
  } else{
    return(NA)
  }
}



ID_suffix3 <- function(n1 = 1, n2 = 999) {
  id1 <- paste("00", 1:9, sep = "")
  id2 <- paste("0", 10:99, sep = "")
  id3 <- 100:999
  alln <- c(id1, id2, id3)
  if (is.numeric(n1) &
      is.numeric(n2) & n2 >= n1 & n1 >= 1 & n2 <= 999)
  {
    re_v <- alln[n1:n2]
    return(re_v)
  } else{
    return(NA)
  }
}

get_ID <- function(n1 = 1,
                   n2 = 6,
                   planting = FALSE,
                   id_prefix=NULL) {
  my_id_prefix <- ID_prefix(planting)
  if(!is.null(id_prefix)) my_id_prefix <-id_prefix
  my_id_sufix <- ID_suffix5(n1, n2)
  re_v <- paste(my_id_prefix, my_id_sufix, sep = "")
  return(re_v)
}


#paste(ID_prefix(),ID_suffix(1,9),sep="")
#获得电脑nodename
get_computer_nodename <- function() {
  return(Sys.info()["nodename"])
}

#获得prefix_linename
get_prefix_linename <- function(prefix = "ZJ",
                                n1 = 1,
                                n2 = 6,
                                digits = 3) {
  if (digits == 3) {
    re_v <- paste(prefix, ID_suffix3(n1, n2), sep = "")
  }
  else if (digits == 4) {
    re_v <- paste(prefix, ID_suffix4(n1, n2), sep = "")
  }
  else{
    re_v <- paste(prefix, ID_suffix3(n1, n2), sep = "")
  }
  return(re_v)
}

get_next_id<-function(my_primary){
  mystr<-last(my_primary)[1,1]
  id_p<-substr(mystr,1,13)
  id_n<-as.numeric(substr(mystr,14,18))+1
  return(list(id_n=id_n,id_p=id_p))
}


