na_process <- function(df,r_index){
  if(r_index == 1){return(mean(na.omit(df[1:4,2])))}
  else if(r_index == 2){return(mean(na.omit(df[1:5,2])))}
  else if(r_index == 3){return(mean(na.omit(df[1:6,2])))}
  else if(r_index == 46){return(mean(na.omit(df[43:46,2])))}
  else if(r_index == 45){return(mean(na.omit(df[42:46,2])))}
  else if(r_index == 44){return(mean(na.omit(df[41:46,2])))}
  else{return(mean(na.omit(df[(r_index-3):(r_index+3),2])))}
}

new_df <- function(df){
  for (i in which(is.na(df)[,2])){
    df[i,2] = na_process(df, i)
  }
  return(df)
}