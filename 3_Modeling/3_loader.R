library(aws.s3)
library(data.table)
library(dplyr)
library(reshape2)

options(stringsAsFactors = FALSE)

# ============================================================================================================ #
display_list = list.files(recursive = TRUE)
display_list = paste0("display-country-2/",display_list)

# get ga ID for merging purposses
display_list_id = display_list
for(i in 1:length(display_list)){
	display_list_id[i] = unlist(strsplit(display_list[i], split = "/"))[4]
}
display_list = data.frame(display = display_list, gaId = display_list_id)

# ============================================================================================================ #
age_gender = list.files(recursive = TRUE)
age_gender = paste0("display-age-gender/",age_gender)


# get ga ID for merging purposses
age_gender_id = age_gender    
for(i in 1:length(age_gender)){
	age_gender_id[i] = unlist(strsplit(age_gender[i], split = "/"))[4]
}
age_gender_list = data.frame(age_gender = age_gender, gaId = age_gender_id)

# ============================================================================================================ #
products = list.files(recursive = TRUE)
products = paste0("display-products-3/",products)


# get ga ID for merging purposses
products_id = products   
for(i in 1:length(products)){
	products_id[i] = unlist(strsplit(products[i], split = "/"))[4]
}
products_list = data.frame(products = products, gaId = products_id)


# =========================================================================================================== #
page_metrics_list = list.files(recursive = TRUE)
page_metrics_list = paste0("display-page-metrics/",page_metrics_list)

# get ga ID for merging purposses
page_metrics_list_id = page_metrics_list
for(i in 1:length(page_metrics_list)){
	page_metrics_list_id[i] = unlist(strsplit(page_metrics_list[i], split = "/"))[4]
}
page_metrics_list = data.frame(page_metric = page_metrics_list, gaId = page_metrics_list_id)


# ========================================================================================================== #
# Now merge page_metrics and display list by ID 
storage = merge(page_metrics_list, display_list, by = "gaId")
# Now merge storage and age_gender list by ID 
storage = merge(storage, age_gender_list, by = "gaId")
# Now merge storage and products by ID
storage = merge(storage, products_list, by = "gaId")


# same viewId means same data in this case
storage = storage[!duplicated(storage$gaId),]

setwd("~/data/merged/")




a = list()
total_rows = 0
for(i in 1:nrow(storage)){
  # load data
  country = tryCatch(fread(paste0("/b2/",storage$display[i])), 
              error = function(x){print("Country File not found") 
              return(NULL) })
  age_gender = tryCatch(fread(paste0("~/data/",storage$age_gender[i])), 
             error = function(x){print("Age Gender File not found") 
             return(NULL) })
  page_metric = tryCatch(fread(paste0("/b1/home/dirty-harry/keywords/data/",
                                      storage$page_metric[i])), 
                  error = function(x){print("Page Metric File not found") 
                  return(NULL) })
  products = tryCatch(fread(paste0("~/data/",storage$products[i])), 
                  error = function(x){print("Product File not found") 
                  return(NULL) })
  
  # merge only if page_metric not empty
  if( !is.null(page_metric) && !is.null(country) && !is.null(age_gender) && !is.null(products)){
	  if( nrow(page_metric) != 0 && nrow(country) !=0 && nrow(age_gender) != 0 && nrow(products)!=0 ){ 
		  if( substr(page_metric[1,1],1,7) != "<Error>" &&
		      substr(country[1,1],1,7) != "<Error>" &&
                      substr(age_gender[1,1],1,7) != "<Error>" && 
                      substr(products[1,1],1,7) != "<Error>" ){

			  # force character
			  country$`ga:date` = as.character(country$`ga:date`)
			  page_metric$`ga:date` = as.character(page_metric$`ga:date`)
			  age_gender$`ga:month` = as.character(age_gender$`ga:month`)
			 
			  country$`ga:adGroup` = as.character(country$`ga:adGroup`)
			  page_metric$`ga:adGroup` = as.character(page_metric$`ga:adGroup`)
			  age_gender$`ga:adGroup` = as.character(age_gender$`ga:adGroup`)
			 
			  country$`ga:campaign` = as.character(country$`ga:campaign`)
			  page_metric$`ga:campaign` = as.character(page_metric$`ga:campaign`)
			  age_gender$`ga:campaign` = as.character(age_gender$`ga:campaign`)
			  
			  country$`ga:keyword` = as.character(country$`ga:campaign`)
			  page_metric$`ga:keyword` = as.character(page_metric$`ga:campaign`)
			  age_gender$`ga:keyword` = as.character(age_gender$`ga:campaign`)

			  # prevent logical merge variables
			  #country$`ga:keyword`[is.na(country$`ga:keyword`)] = ""    
			  #page_metric$`ga:keyword`[is.na(country$`ga:keyword`)] = ""    

		          # there my be non-uniqueness errors in the API request
		          # it is therefore necessary to aggregate country as well
		          # example see: crayoni.digital@gmail.com/101872803.csv
                          # Therefore adCost must summed, the CPC is useless here 
                          # because we dont have the sessions per line
		          # We will calculate the CPC after the merge through adcost/session
		          country %>%
				 group_by(`ga:viewId`,`ga:date`,`ga:adGroup`, `ga:campaign`, 
                                          `ga:deviceCategory`,`ga:keyword`) %>%
				 summarize(`ga:adCost` = sum(`ga:adCost`)) %>%
				 ungroup() ->
			  country

                          # Now we will put age_gender on keyword level
                          age_gender %>%
                            dcast(formula = `ga:viewId` + `ga:month` + `ga:country` + `ga:campaign` + 
                                            `ga:adGroup` + `ga:keyword` ~ `ga:userGender`,
                            value.var = "ga:sessions",
                            fun.aggregate = sum ) %>%
                          # This mutation will add a zero variable if it does not exists already
                            mutate(male   =  tryCatch(male  , error = function(x){ return(0) } ),
                                   female =  tryCatch(female, error = function(x){ return(0) } )
                                  ) %>% 
                          # Get relative female values
                            mutate(female_ratio = ifelse(male==0 & female==0 , NA, female/(female+male)) ) %>%
                            select(-one_of(c("female","male"))) ->
                            gender_casted_sessions
                          
                          age_gender %>%
                            dcast(formula = `ga:viewId` + `ga:month` + `ga:country` + `ga:campaign` + 
                                            `ga:adGroup` + `ga:keyword` ~ `ga:userAgeBracket`,
                            value.var = "ga:sessions",
                            fun.aggregate = sum ) %>%
                          # This mutation will add a zero variable if it does not exists already
                            mutate(`18-24` =  tryCatch(`18-24`, error = function(x){ return(0) } ),
                                   `25-34` =  tryCatch(`25-34`, error = function(x){ return(0) } ), 
                                   `35-44` =  tryCatch(`35-44`, error = function(x){ return(0) } ), 
                                   `45-54` =  tryCatch(`45-54`, error = function(x){ return(0) } ), 
                                   `55-64` =  tryCatch(`55-64`, error = function(x){ return(0) } ),  
                                     `65+` =  tryCatch(`65+`  , error = function(x){ return(0) } )
                                  ) %>%
                          # This will give us relative age values  
                            mutate(age_sum = sum(`18-24`,`25-34`,`35-44`,`45-54`,`55-64`,`65+`,na.rm = TRUE),
                                   `18-24` =  ifelse(age_sum==0, NA, `18-24`/age_sum),
                                   `25-34` =  ifelse(age_sum==0, NA, `25-34`/age_sum),
                                   `35-44` =  ifelse(age_sum==0, NA, `45-54`/age_sum),
                                   `45-54` =  ifelse(age_sum==0, NA, `55-64`/age_sum),
                                   `55-64` =  ifelse(age_sum==0, NA, `55-64`/age_sum),
                                     `65+` =  ifelse(age_sum==0, NA, `65+`/age_sum)
                                  ) %>%
                          # Remove helper sum
                            select(-one_of("age_sum")) ->
                            age_casted_sessions


                          # ======================================== #
                          # products
                          products$`ga:viewId` = as.character(products$`ga:viewId`)

                          # Extract meta variables on ga:viewId level
                          product_extract = data.frame(`ga:viewId` = unique(products$`ga:viewId`)[1])

                          # Prices
                          product_extract$meanRev = mean(products$`ga:itemRevenue` / products$`ga:itemQuantity`, na.rm = TRUE)
                          product_extract$varRev = var(products$`ga:itemRevenue` / products$`ga:itemQuantity`, na.rm = TRUE)

                          # item variety and amount 
                          product_extract$sumQuant = sum(products$`ga:itemQuantity`, na.rm = TRUE)
                          product_extract$uniqueProducts = length(unique(products$`ga:productName`))
  
                          products %>%
                              group_by(`ga:productName`) %>%
                              # Get the product-wise means and price fluctuation (var)
                              summarise( meanProdRev = mean((`ga:itemRevenue` / `ga:itemQuantity`), na.rm = TRUE),
                                         varProdRev =  var((`ga:itemRevenue` / `ga:itemQuantity`), na.rm = TRUE)) %>%
                              ungroup() %>%
                              # Get the mean and var of all product means, get the mean of product vars
                              summarise(meanRev = mean(meanProdRev),
                                        varRev = var(meanProdRev),
                                        meanVarProdRev = mean((varProdRev), na.rm = TRUE)) %>%
                              bind_cols(product_extract) ->
                              product_extract

                          # market basket analysis
                          products %>%
                              group_by(`ga:transactionId`) %>%
                              summarise(basketTotal = sum(`ga:itemQuantity`),
                                        basketVariety = length((unique(`ga:productName`))) ) %>%
                              ungroup() %>%
                              summarise(uniqueBaskets = length(basketTotal),
                                        meanBasketSize = mean(basketTotal, na.rm = TRUE),
                                        varBasketSize = var(basketTotal, na.rm = TRUE),
                                        meanBasketVariety = mean(basketVariety, na.rm = TRUE),
                                        varBasketVariety = var(basketVariety, na.rm = TRUE)
                              ) %>%
                              bind_cols(product_extract) ->
                              product_extract  

                          names(product_extract)[which(names(product_extract) == "ga.viewId")] = "ga:viewId"   
                          

                          # ===================================== #
			  # merge
			  a[[i]] = merge(country, page_metric, by = c("ga:viewId","ga:date","ga:campaign",
							 "ga:adGroup","ga:deviceCategory","ga:keyword")) 
			  
			  # recreate CPC variable with divide zero precautions
			  a[[i]]$`ga:CPC` = ifelse(a[[i]]$`ga:sessions` == 0, 0,a[[i]]$`ga:adCost`) / 
				            ifelse(a[[i]]$`ga:sessions` == 0, 1,a[[i]]$`ga:sessions`)
                          
                          # create month variable for merge with gender and age
			  a[[i]]$`ga:month` = month(lubridate::ymd(a[[i]]$`ga:date`))
                          
                          # now merge
                          a[[i]] = merge(a[[i]],gender_casted_sessions, by = c("ga:viewId","ga:month","ga:country","ga:campaign",
                                                                               "ga:adGroup","ga:keyword"))

                          a[[i]] = merge(a[[i]],age_casted_sessions, by = c("ga:viewId","ga:month","ga:country","ga:campaign",
                                                                               "ga:adGroup","ga:keyword"))

                          a[[i]] = merge(a[[i]],product_extract, by = "ga:viewId")

                          # reporting
		          total_rows = total_rows + nrow(a[[i]])
			  print(paste("Number:",i,"Rows:",nrow(a[[i]]),"Total Rows:",total_rows))
		  } else{
		  	print("Error no such file.")
		  }
	  } else {
              print(paste("page_metric:",nrow(page_metric),
                          "country:",    nrow(country),
                          "age_gender:", nrow(age_gender),
                          "products:",   nrow(products)))
	  }
  } else {
	  print("A File is NULL")
  }
}

# rbind
disp_df = rbindlist(a)

disp_df_2 = disp_df[!duplicated(disp_df),]

# Write
write.table(x = disp_df_2, file = "result_products_all_2019.csv", sep = ";", row.names = FALSE)

