planting_design<-function(){
  planted<-planting(origin,
                    ck, interval,s_prefix,rp,
                    treatment,place,
                    ckfixed, digits,rows,restartfid)
  #注意，参数顺序不能错
  savewb(origin,
         planted,
         planted[FIELDS],
         NA,
         filename,
         overwrite)
}
