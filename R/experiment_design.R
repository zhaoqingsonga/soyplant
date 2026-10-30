#' 生成试验小区设计
#'
#' 生成完整的田间试验小区设计，包括小区数、过道与保护行等布局参数。
#'
#' @return 数据框，小区设计结果
#' @examples
#' \dontrun{
#' planting_design()
#' }
#'
#' @export
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
