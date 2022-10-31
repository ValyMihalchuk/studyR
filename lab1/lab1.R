fix_data <- function(df) {
  p = lapply(df,function(x) lapply(x, function(y) if (!grepl("[A-Za-z]", y)) {y <- as.double(gsub(" ", "", y))
} else {y<-y} ))
  dd  <-  as.data.frame(matrix(unlist(p), nrow=length(unlist(p[1]))))
  return(dd)
}

get_id <- function(lst) {
  df <- do.call("rbind", lst)
  frame_mean <- aggregate(temp ~ id, df, mean)
  frame_length < -aggregate(temp ~ id, df, length)
  names(frame_mean)[names(frame_mean) == 'temp'] <- 'mean_temp'
  names(frame_length)[names(frame_length) == 'temp'] <- 'length'
  new_df <- subset(merge(frame_mean, frame_length, by = 'id'), length == 7, select = c('id', 'mean_temp'))
  row.names(new_df) <- NULL
  return(new_df)
  
}

