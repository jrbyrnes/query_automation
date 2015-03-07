getParams1 <- function(method){
  ##get categorization data##
  categorization <- readline(cat("Please type name of Categorization column (type NA if no categorization column):\n"))
  
  ##get weight data##
  weight <- readline(cat("Please type name of weighting / projecting column (type NA if raw data is wanted):\n"))
  
  
  
  
  
  
  ##get merch data##
####################  nmerch <- readline(cat("How many merchants of interest? (press enter if 'is not null' constraint wanted):\n"))
####################  if (nmerch == 1){
####################  
##  
##    if (nmerch == ""){
##      merch_col <- readline(cat("Enter column name of Merchant:\n"))
##      if (tolower(method) == 'walletshare'){
##        merch_flag <- readline(cat("Is this condition: primary, secondary, or both?\n"))
##      }
##      else (merch_flag = 'primary')
##      merch_clause <- paste(merch_col, "IS NOT NULL", sep = " ")
##      if (tolower(merch_flag) = 'primary'){
##        pro_clause = merch_clause
##      }
##      else (anti_clause = merch_clause)
##    }
##
##


###### begin sloppy coding
##if (as.numeric(nmerch) == 1){
##  merch <- readline(cat("Merchant of interest:\n"))
##  merch_col <- readline(cat("Column name of Merchant:\n"))
##  if (tolower(method) == 'walletshare'){
##    merch_flag <- readline(cat("Is this condition: primary, secondary, or both?\n"))
##  }
##  else (merch_flag <- 'primary')
##  merch_clause <- paste(merch_col, "=", merch)
##  if(tolower(merch_flag) = 'primary'){
##    pro_clause <- merch_clause
##  }
##  else (anti_clause = merch_clause)
##}

##myfun <- function(){
##if (as.numeric(nmerch) > 1){
##  merch_col <- readline(cat("Column name of Merchant:\n"))
##  if (tolower(method) == 'walletshare'){
##    pmerch <- readline(cat("Primary merchants of interest (seperated by comma):\n"))
##    amerch <- readline(cat("Secondary merchants of interest (seperated by comma):\n"))
##    pro_clause <- paste(merch_col, "IN (", pmerch, ")", sep = " ")
##    anti_clause <- paste(merch_col, "IN (", amerch, ")", sep = " ")
##  }
##  else {
##    merch <- readline(cat("Merchants of interest (seperated by comma):\n"))
##    pro_clause <- paste(merch_col, "IN (", merch, ,")" sep = " ")
##  }
##}
##return(c(pro_clause, anti_clause))
##}
## after merch

##if (nchar(pro_clause) != 0){
##  pro_clause <- paste(pro_clause, "AND", sep = " ")
##}
##if (nchar(anti_clause) != 0){
##  anti_clause <- paste(anti_clause, "AND", sep = " ")
##}

######end sloppy coding


##  if (as.numeric(nmerch) == 0){
##    merch_col = NULL
##    merch_clause = ""
##  }
##  else if (as.numeric(nmerch) == 1){
##    merch_col <- readline(cat("Enter column name of Merchant:\n"))
##    if (tolower(method) == 'walletshare'){
##      merch_flag <- readline(cat("Is this condition: primary, secondary, or both?\n"))
##    }
##    else (merch_flag = 'primary')
##  }
##  else if (as.numeric(nmerch > 1){
##    merch_col <- readline(cat("Enter column name of Merchant:\n"))
##    if (tolower(method) = 'walletshare'){
##      p_merch <- readline(cat("Enter primary merchants of interest seperated by commas:\n"))
##      s_merch <- readline(cat("Enter secondary merchants of interest seperated by commas:\n"))
##    }
##  }
##
#####################  build in merch, anti & pro clauses above (repeat for type & brand -- should be end of dev aside from WS pie)
 
  merch <- readline(cat("Merchant of interest:\n"))}
  if (tolower(merch) != "na"){
    merch_col <- readline(cat("Enter column name of Merchant:\n"))
    if (tolower(method) == 'walletshare'){
    merch_flag <- readline(cat("Is this condition: primary, secondary, or both?\n"))}
    else (merch_flag = 'primary')
  }
  else merch_col = NULL
  
  
  ##get the merchant clause##
  if (tolower(merch) == ""){
    merch_clause <- paste(merch_col, "is not null", sep = " ")
  }
  else if (tolower(merch) != "na" & nchar(merch) != 0){
    merch_clause <- paste(merch_col, "=", "'",merch,"'", sep = "")
  } 
  else (merch_clause = "")





  ##get brand data##
  brand <- readline(cat("Brand of interest (leave empty if none, type NA if no brand data):\n"))
  if (tolower(brand) != "na"){
    brand_col <- readline(cat("Please enter column name of Brand:\n"))
    if (tolower(method) == 'walletshare'){
    brand_flag <- readline(cat("Is this a primary condition, secondary condition, or both?\n"))}
    else (brand_flag = 'primary')
  }
  else {brand_col = NULL}
  
  
  ##get type data##
  type <- readline(cat("Type of interest (leave empty if none, type NA if no type data):\n"))
  if (tolower(type) != "na"){
    type_col <- readline(cat("Please enter column name of Type:\n"))
    if (tolower(method) == 'walletshare'){
    type_flag <- readline(cat("Is this a primary condition, secondary condition, or both?\n"))}
    else (type_flag = 'primary')
  }
  else {type_col = NULL}

  
  
  
  
  ##get the categorization clause##
  if (tolower(categorization) != "na"){
    cat_clause <- paste(categorization, "is null", sep = " ")
  }
  else (cat_clause = "")
  
  ##get the revenue clause##
  weighting = weight
  if (tolower(weight) != "na"){
    rev_clause = "price * quantity * projected_weight/100"
  }
  else {
    rev_clause = "price * quantity / 100"
    weighting = ""
  }
  


  
  ##get the brand clause##
  if (tolower(brand) == ""){
    brand_clause <- paste(brand_col, "is not null", sep = " ")
  }
  else if (tolower(brand) != "na" & nchar(brand) != 0){
    brand_clause <- paste(brand_col, "=", "'", brand, "'", sep = "")
  } 
  else (brand_clause <- "")
  
  ##get the type clause##
  if (tolower(type) == ""){
    type_clause <- paste(type_col, "is not null", sep = " ")
  }
  else if (tolower(type) != "na" & nchar(type) != 0){
    type_clause <- paste(type_col, "=", "'",type,"'", sep = "")
  }     
  else (type_clause <- "")
  
  ##get groupng info##
  ngroup <- readline(cat("how many of merchant, brand, and type columns are you grouping?\n"))
  if (ngroup == 1){
    group <- readline(cat("By which column are you grouping?\n"))
  }
  
  if (ngroup == 2){
    groupm <- matrix(nrow = 2)
    groupm[1,] <- readline(cat("First grouping column?\n"))
    groupm[2,] <- readline(cat("Second grouping column?\n"))
    group = paste(groupm[1,], ",", groupm[2,], sep = " ")
  }
  
  if (ngroup == 3){
    groupm[1,] <- readline(cat("First grouping column?\n"))
    groupm[2,] <- readline(cat("Second grouping column?\n"))
    groupm[3,] <- readline(cat("Third grouping column?\n")) 
    group = paste(groupm[1,], ",", groupm[2,], ",", groupm[3,], sep = " ")
    }
  
  
  ##get pro & anti clause info
  pro_clause = ""
  anti_clause = ""

  if (tolower(merch_flag) == 'primary' | tolower(merch_flag) == 'both'){
    pro_clause = merch_clause
  }
  if (tolower(merch_flag) == 'secondary' | tolower(merch_flag) == 'both'){
    anti_clause = merch_clause
  }
  

  if (tolower(brand_flag) == 'primary' | tolower(brand_flag) == 'both'){
    if (nchar(pro_clause) == 0){
      pro_clause = brand_clause
    }
    else{
      pro_clause = paste(pro_clause, "AND", brand_clause, sep = " ")
    }
  }
  if (tolower(brand_flag) == 'secondary' | tolower(brand_flag) == 'both'){
    if (nchar(anti_clause) == 0){
      anti_clause = brand_clause
    }
    else {
      anti_clause = paste(anti_clause, "AND", brand_clause, sep = " ")
    }
  }


  if (tolower(type_flag) == 'primary' | tolower(type_flag) == 'both'){
    if (nchar(pro_clause) == 0){
      pro_clause = type_clause
    }
    else{
      pro_clause = paste(pro_clause, "AND", type_clause, sep = " ")
    }
  }

  if (tolower(type_flag) == 'secondary' | tolower(type_flag) == 'both'){
    if(nchar(anti_clause) == 0){
      anti_clause = type_clause
    }
    else{
      anti_clause = paste(anti_clause, "AND", type_clause, sep = " ")
    }
  }
  return(c(cat_clause,rev_clause,merch_clause,brand_clause,type_clause,group,anti_clause,pro_clause,weighting))
  
}