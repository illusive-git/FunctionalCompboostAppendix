merge_money = function(dis, money_data, xrates, DATASET, cluster){
    require(multidplyr)
  
    money_data = money_data[!duplicated(money_data$ga.viewId),]  
    
    # Merge Metadata to GA data
    dis %>%
        partition(ga.viewId, cluster = cluster) %>%
        as.data.table() %>%
        merge(x = ., y = money_data , by.x = "ga.viewId", by.y = "ga.viewId", all.x = TRUE) %>%
        collect() ->
        dis

    # Collapse xchange rates from country to currency level
    xrates %>%
      group_by(CURRENCY) %>%
      summarise_if(is.numeric, mean, na.rm = TRUE) %>%
      melt(id.vars = "CURRENCY") ->
      xrates
    names(xrates) = c("Currency","yearMonth","rate")

    if(DATASET == "FULL"){
        # Merge Currency rates and transform financial data
        dis %>%
          mutate(ga.date = ymd(ga.date),
                 yearMonth = paste(year(ga.date),month(ga.date),sep = "-")) %>%
          merge(x = ., y = xrates, by = c("Currency","yearMonth")) %>%
          mutate(ga.adCost = ga.adCost * 1/rate,
                 ga.transactionRevenue = ga.transactionRevenue * 1/rate,
                 meanRev = meanRev * 1/rate,
                 varRev = varRev * (1/rate)^2,
                 ga.CPC = ga.CPC * 1/rate) -> 
          dis
    }
    if(DATASET == "NO_PRODUCT"){
        # Merge Currency rates and transform financial data
        dis %>%
          mutate(ga.date = ymd(ga.date),
                 yearMonth = paste(year(ga.date),month(ga.date),sep = "-")) %>%
          merge(xrates, by = c("Currency","yearMonth")) %>%
          mutate(ga.adCost = ga.adCost * 1/rate,
                 ga.transactionRevenue = ga.transactionRevenue * 1/rate) -> 
          dis
    }
    return(dis)
}
