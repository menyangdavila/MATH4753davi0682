#' my ddt plot function
#'
#' @param df variable containing data set
#' @param SPECIES containing value of variable
#'
#' @importFrom dplyr filter
#' @importFrom utils write.csv
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth ggtitle
#' @return plot and table
#' @export
#'
#' @examples
#' \dontrun{myddt(df = ddt, SPECIES = "CCATFISH")}

myddt=function(df, SPECIES){
  tab_river=table(with(df, RIVER))/length(with(df, RIVER)) # create the relative frequency table of RIVER before sub-setting

  df_sub=df %>% filter(SPECIES=={{SPECIES}}) # sub-set the data frame by specific species

  filename=unique(df_sub$SPECIES) # take the value of "species"
  write.csv(df_sub,file=paste0("LvsWfor", filename,".csv"),row.names=FALSE) # write csv file with name containing the specific species

  river=with(df_sub,RIVER) # take the column of RIVER as a vector
  weight=with(df_sub,WEIGHT) # take the column of WEIGHT as a vector
  length=with(df_sub,LENGTH) # take the column of LENGTH as a vector
  g=ggplot(df_sub, aes(x=length,y=weight)) + geom_point(aes(colour= river)) +
    geom_smooth( method = "lm", formula = y ~ x + I(x^2)) + ggtitle("Mengyang Davila") # plot with quadratic curve and title

  print(g)

  list_data <- list(df, df_sub,tab_river)
  names(list_data) <- c("Full Sample DDT", "Sub Sample DDT","Relative Frequency") # name data sets

  print(list_data)
}
