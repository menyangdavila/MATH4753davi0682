#' my ddt plot function
#'
#' @param df variable containing dataset
#' @param cond variable containing condition to filter the dataset
#'
#' @importFrom stats filter
#' @importFrom utils write.csv
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth ggtitle
#' @return plot and table
#' @export
#'
#' @examples
#' \dontrun{myddt(df = ddt, SPECIES = "CCATFISH")}

myddt=function(df, cond){
  tab_river=table(with(df, RIVER))/length(with(df, RIVER))

  df_sub=df %>% filter({{cond}})

  species=with(df_sub,SPECIES)
  filename=unique(species)
  write.csv(df_sub,file=paste0("LvsWfor", filename,".csv"),row.names=FALSE)

  river=with(df_sub,RIVER)
  weight=with(df_sub,WEIGHT)
  length=with(df_sub,LENGTH)
  g=ggplot(df_sub, aes(x=length,y=weight)) + geom_point(aes(colour=river)) +
    geom_smooth( method = "lm", formula = y ~ x + I(x^2)) + ggtitle("Mengyang Davila")

  print(g)

  list_data <- list(df, df_sub,tab_river)
  names(list_data) <- c("Full Sample DDT", "Sub Sample DDT","Relative Frequency")

  print(list_data)
}
