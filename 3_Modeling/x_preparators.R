# Filter & Transform GA Data
prepare_transaction = function(dis, min_trans = 30, qu_rev = 0.95, cluster = cluster){
  require(multidplyr)
  
  cluster_copy(cluster, qu_rev)
  cluster_copy(cluster, min_trans)

  
  dis %>% 
      # Partition by ga.viewId and group
      partition(ga.viewId, cluster = cluster) %>%
      as.data.table() %>%
      group_by(ga.viewId) %>% 

      # Feature Engineering on ga.viewID level
      mutate(total_trans_view =  sum(ga.transactions),
             qu = quantile(ga.transactionRevenue,qu_rev) ) %>%
      # Filter out excessive Sales - outliers
      filter(`ga.transactionRevenue` < qu) %>%
      # Filter out ga.viewIds with less than min_trans transactions
      filter(total_trans_view > min_trans) %>% 
      # Normalize the transaction revenue on viewId level
      ungroup() %>%
      collect() %>%

      # Filter and log-transform revenue
      filter(`ga.transactionRevenue` > 0) %>%
      
      # Add Weekday variable
      mutate(weekDay = weekdays(ga.date)) %>% 
      
      na.omit() ->
      dis

  return(dis)
}

prepare_sessions = function(dis, min_sess = 35, cluster = cluster){
  require(multidplyr)

  cluster_copy(cluster, min_sess)
  dis %>% 
      
      partition(ga.viewId, cluster = cluster) %>%
      as.data.table() %>%
      group_by(ga.viewId) %>% 
      
      # Filter out ga.viewIds with less than min_sess in total
      mutate(total_sessions =  sum(`ga.sessions`)) %>%
      filter(total_sessions > min_sess) %>% 
      
      # Add Weekday variable
      mutate(weekDay = weekdays(ymd(ga.date))) %>% 
      
      ungroup() %>%
      collect() %>%
      na.omit() ->
      dis
  return(dis)
}



