# This function removes ties from a dataframe/data.table.
# medians for numerics, first value for chars

tie_fighter = function(df, tie.var = "ga.CPC"){
    require(dplyr)
    idx_numerics = which(unlist(lapply(df[1,], is.numeric)))

    # Median of numerics
    df %>%
      select(idx_numerics) %>%
      group_by_(.dots = tie.var) %>%
      summarise_all(funs(median)) %>%
      ungroup() ->
      df_num

    # First for characters
    df %>%
      select( c( (1:ncol(df))[-idx_numerics]), which(names(df) == tie.var)) %>%
      group_by_(.dots = tie.var) %>%
      summarise_all(funs(head), n=1) %>%
      ungroup() ->
      df_char

    # Merge again
    df_out = merge(df_char,df_num)
   
    return(df_out)
}


