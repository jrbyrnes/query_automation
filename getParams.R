getParams <- function(method){
  
  ##get categorization data##
  categorization <- readline(cat("Please type name of Categorization column (type NA if no categorization column):\n"))
  
  ##get revenue data##
  proj <- readline(cat("Are you using weighted/projected Data (Yes/No)?\n"))
    if (tolower(proj) == 'yes'){
      weighting <- readline(cat("What is the weighting/projecting column?\n"))
    }
    else{
      weighting <- ""
    }  
  
  
  ##get merch data##
  merch <- readline(cat("Merchant of interest (leave empty if none, type NA if no merchant data):\n"))
    if (merch != "NA"){
      merch_col <- readline(cat("Please enter column name of Merchant:\n"))
    }
    else {merch_col = NULL}
  
  ##get brand data##
  brand <- readline(cat("Brand of interest (leave empty if none, type NA if no brand data):\n"))
    if (brand != "NA"){
      brand_col <- readline(cat("Please enter column name of Brand:\n"))
    }
    else {brand_col = NULL}
  
  ##get type data##
  type <- readline(cat("Type of interest (leave empty if none, type NA if no type data):\n"))
    if (type != "NA"){
      type_col <- readline(cat("Please enter column name of Type:\n"))
    }
    else {type_col = NULL}
    
  ##get the categorization clause##
  if (tolower(categorization) != "na"){
    cat_clause <- paste(categorization, "is null", sep = " ")
  }
  else (cat_clause = "")
  
  ##get the revenue clause##
  if (tolower(proj) == "yes"){
    rev_clause = "price * quantity * projected_weight/100"
  }
  else (rev_clause = "price * quantity / 100")
  
  ##get the merchant clause##
  if (tolower(merch) == ""){
    merch_clause <- paste(merch_col, "is not null", sep = " ")
  }
  else if (tolower(merch) != "na" & nchar(merch) != 0){
    merch_clause <- paste(merch_col, "=", "'",merch,"'", sep = "")
  } 
  else (merch_clause = "")
  
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
  
  ##group and anti-group clause
  
  ngroup <- readline(cat("how many of merchant, brand, and type columns are you grouping?\n"))
  
  ###N group == 1###
  if (ngroup == 1){
    group <- readline(cat("By which column are you grouping?\n"))
    fact <- readline(cat("Is it a primary grouping column (Yes/No)?\n"))
    
    if (tolower(fact) == 'yes'){
    if (tolower(group) == tolower(merch_col)){
      pro_clause = merch_clause
      if (nchar(brand_clause) == 0){
        anti_clause = type_clause
      }
      else if (nchar(type_clause) == 0){
        anti_clause = brand_clause
      }    
      else if (nchar(type_clause) == 0 & nchar(brand_clause) == 0){
        anti_clause = ""
      }
      else if (nchar(type_clause) != 0 & nchar(brand_clause) != 0){
        anti_clause = paste(brand_clause, "AND", type_clause, sep = " ")
      }
    }
  
    else if (tolower(group) == tolower(brand_col)){
      pro_clause = brand_clause
      if (nchar(merch_clause) == 0){
        anti_clause = type_clause
      }
      else if (nchar(type_clause) == 0){
        anti_clause = merch_clause
      }    
      else if(nchar(type_clause) == 0 & nchar(merch_clause) == 0){
        anti_clause = ""
      }
      else if (nchar(type_clause) != 0 & nchar(merch_clause) != 0){
        anti_clause = paste(merch_clause, "AND", type_clause, sep = " ")
      } 
    }
    else if (tolower(group) == tolower(type_col)){
      pro_clause = type_clause
      if (nchar(brand_clause) == 0){
        anti_clause = merch_clause
      }
      else if (nchar(merch_clause) == 0){
        anti_clause = brand_clause
      }    
      else if(nchar(merch_clause) == 0 & nchar(brand_clause) == 0){
        anti_clause = ""
      }
      else if (nchar(merch_clause) != 0 & nchar(brand_clause) != 0){
        anti_clause = paste(brand_clause, "AND", merch_clause, sep = " ")
      }
    }
  }
  
  if (tolower(fact) == 'no'){
    pro_clause = ""
    if (merch_clause != "" & brand_clause != "" & type_clause != ""){
      anti_clause = paste(merch_clause, "AND", brand_clause, "AND", type_clause, sep = " ")
    }
    else if (merch_clause != "" & brand_clause != "" & type_clause == ""){
      anti_clause = paste(merch_clause, "AND", brand_clause, sep = " ")
    }
    else if (merch_clause == "" & brand_clause != "" & type_clause == ""){
      anti_clause = paste(type_clause, "AND", brand_clause, sep = " ")
    }
    else if (merch_clause != "" & brand_clause == "" & type_clause != ""){
      anti_clause = paste(merch_clause, "AND", type_clause, sep = " ")
    }
    else if (merch_clause != "" & brand_clause == "" & type_clause == ""){
      anti_clause = merch_clause
    }
    else if (merch_clause == "" & brand_clause != "" & type_clause == ""){
      anti_clause = brand_clause
    }
    else if (merch_clause == "" & brand_clause == "" & type_clause != ""){
      anti_clause = type_clause
    }
  }}
  ###ngroup == 2###
  else if (ngroup == 2){
    fact <- readline(cat("How many are primary (0,1,2)?\n"))
    if (fact == '2'){
    groupm <- matrix(nrow = 2)
    groupm[1,] <- readline(cat("First grouping column?\n"))
    groupm[2,] <- readline(cat("Second grouping column?\n"))

    if (!is.null(type_col)){
    if (tolower(groupm[1,]) != type_col & tolower(groupm[2,]) != type_col){
      pro_clause = paste(merch_clause, "AND", brand_clause, sep = " ")
      anti_clause = type_clause
    }}
    
    if(is.null(type_col)){
      pro_clause = paste(merch_clause, "AND", brand_clause, sep = " ")
      if(nchar(type_clause) == 0){
        anti_clause = ""
      }
      else {anti_clause = type_clause}
    }
    
    
    if (!is.null(brand_col)){
    if (tolower(groupm[1,]) != tolower(brand_col) & tolower(groupm[2,]) != tolower(brand_col)){
      pro_clause = paste(merch_clause, "AND", type_clause, sep = " ")
      anti_clause = brand_clause
    }}
    
    if (is.null(brand_col)){
      pro_clause = paste(merch_clause, "AND", type_clause, sep = " ")
      if(nchar(brand_clause) == 0){
        anti_clause = ""
      }
      else {anti_clause = brand_Clause}
    }
  
    
    if (!is.null(merch_col)){
    if (tolower(groupm[1,]) != tolower(merch_col) & tolower(groupm[2,]) != tolower(merch_col)){
      pro_clause = paste(brand_clause, "AND", type_clause, sep = " ")
      anti_clause = merch_clause
    }}
    
    if (is.null(merch_col)){
      pro_clause = paste(brand_clause, "AND", type_clause, sep = " ")
      if(nchar(type_clause) == 0){
        anti_clause = ""
      }
      else {anti_clause = merch_clause}
    }
    
    group = paste(groupm[1,], ",", groupm[2,], sep = " ")
  }
  
  else if (fact == '1'){
    groupm <- matrix(nrow = 2)
    groupm[1,] <- readline(cat("Primary grouping column?\n"))
    groupm[2,] <- readline(cat("Secondary grouping column?\n"))
    group = paste(groupm[1,], ",", groupm[2,], sep = " ")
    
    if (!is.null(merch_col) & !is.null(brand_col)){
    if (tolower(groupm[1,]) == tolower(merch_col) & tolower(groupm[2,]) == tolower(brand_col)){
      pro_clause = paste(merch_clause, "AND", brand_clause, sep = " ")
      if (nchar(type_clause == 0)){
        anti_clause = brand_clause
      }
      else {
        anti_clause = paste(brand_clause, "AND", type_clause)
      }
    }}
    
    if (!is.null(merch_col) & !is.null(type_col)){
    if (tolower(groupm[1,]) == tolower(merch_col) & tolower(groupm[2,]) == tolower(type_col)){
      pro_clause = paste(merch_clause, "AND", type_clause, sep = " ")
      if (nchar(brand_clause == 0)){
        anti_clause = type_clause
      }
      else {
        anti_clause = paste(brand_clause, "AND", type_clause)
      }
    }}
    
    if (!is.null(brand_col) & !is.null(type_col)){
    if (tolower(groupm[1,]) == tolower(brand_col) & tolower(groupm[2,]) == tolower(type_col)){
      pro_clause = paste(brand_clause, "AND", type_clause, sep = " ")
      if (nchar(merch_clause == 0)){
        anti_clause = type_clause
      }
      else {
        anti_clause = paste(type_clause, "AND", merch_clause)
      }
    }}
    
    if (!is.null(brand_col) & !is.null(merch_col)){
    if (tolower(groupm[1,]) == tolower(brand_col) & tolower(groupm[2,]) == tolower(merch_col)){
      pro_clause = paste(merch_clause, "AND", brand_clause, sep = " ")
      if (nchar(type_clause == 0)){
        anti_clause = merch_clause
      }
      else {
        anti_clause = paste(merch_clause, "AND", type_clause)
      }
    }}
    
    if (!is.null(type_col) & is.null(merch_col)){
    if (tolower(groupm[1,]) == tolower(type_col) & tolower(groupm[2,]) == tolower(merch_col)){
      pro_clause = paste(type_clause, "AND", merch_clause, sep = " ")
      if (nchar(brand_clause == 0)){
        anti_clause = merch_clause
      }
      else {
        anti_clause = paste(brand_clause, "AND", merch_clause)
      }
    }}
    
    if (!is.null(type_col) & !is.null(brand_col)){
      if (tolower(groupm[1,]) == tolower(type_col) & tolower(groupm[2,]) == tolower(brand_col)){
        pro_clause = paste(type_clause, "AND", brand_clause, sep = " ")
        if (nchar(merch_clause == 0)){
          anti_clause = brand_clause
        }
        else {
          anti_clause = paste(brand_clause, "AND", merch_clause)
        }
      }
    }
    
}
  else if (fact == '0'){
    groupm <- matrix(nrow = 2)
    groupm[1,] <- readline(cat("First grouping column?\n"))
    groupm[2,] <- readline(cat("Second grouping column?\n"))
    group = paste(groupm[1,], ",", groupm[2,], sep = " ")
    pro_clause = ""
    
    if (!is.null(merch_col) & !is.null(brand_col)){
    if (groupm[1,] == merch_col & groupm[2,] == brand_col){
      if (type_clause == ""){
      anti_clause = paste(merch_clause, "AND", brand_clause, paste = " ")
      }else {anti_clause = paste(merch_clause, "AND", brand_clause, "AND", type_clause, sep = " ")}
      
    
    }}
    
    if (!is.null(type_col) & !is.null(brand_col)){
      if (groupm[1,] == type_col & groupm[2,] == brand_col){
        if (merch_clause == ""){
        anti_clause = paste(type_clause, "AND", brand_clause, sep = " ")
      }
      else {anti_clause = paste(merch_clause, "AND", brand_clause, "AND", type_clause, sep = " ")}
      }
    }
  
    if (!is.null(type_col) & !is.null(merch_col)){
      if (groupm[1,] == type_col & groupm[2,] == merch_col){
        if (brand_clause == ""){anti_clause = paste(type_clause, "AND", merch_clause, sep = " ")
        }else {anti_clause = paste(merch_clause, "AND", brand_clause, "AND", type_clause, sep = " ")}
      }
    }
    }}
    ###ngroup == 3###
  else if (ngroup == 3){
    groupm <- matrix(nrow = 3)
    fact <- readline(cat("How many are primary (0,1,2,3)?\n"))
    
    if (fact == 0){
      groupm[1,] <- readline(cat("First grouping column?\n"))
      groupm[2,] <- readline(cat("Second grouping column?\n"))
      groupm[3,] <- readline(cat("Third grouping column?\n"))                           
      anti_clause = paste(merch_clause, "AND", brand_clause, "AND", type_clause, sep = " ")
      pro_clause = ""      
    }
    
    if (fact == 3){
    groupm[1,] <- readline(cat("First grouping column?\n"))
    groupm[2,] <- readline(cat("Second grouping column?\n"))
    groupm[3,] <- readline(cat("Third grouping column?\n"))                           
    pro_clause = paste(merch_clause, "AND", brand_clause, "AND", type_clause, sep = " ")
    anti_clause = ""
    }
    
    else if (fact == 1){
    groupm[1,] <- readline(cat("Primary grouping column?\n"))
    groupm[2,] <- readline(cat("First secondary grouping column?\n"))
    groupm[3,] <- readline(cat("Second secondary grouping column?\n"))
    
    if (tolower(groupm[1,]) == tolower(merch_col)){
      pro_clause = merch_clause
      if (nchar(type_clause) != 0 & nchar(brand_clause) != 0){
        anti_clause = paste(type_clause, "AND", brand_clause, sep = " ")
      }
      else if (nchar(type_clause) ==0 & nchar(brand_clause) !=0){
        anti_clause = brand_clause
      }
      else if (nchar(type_clause) != 0 & nchar(brand_clause) == 0){
        anti_clause = type_clause
      }
      else {anti_clause = ""}
      }
    
    if (tolower(groupm[1,]) == tolower(brand_col)){
      pro_clause = brand_clause
      if (nchar(type_clause) != 0 & nchar(merch_clause) != 0){
        anti_clause = paste(type_clause, "AND", merch_clause, sep = " ")
      }
      else if (nchar(type_clause) ==0 & nchar(merch_clause) !=0){
        anti_clause = merch_clause
      }
      else if (nchar(type_clause) != 0 & nchar(merch_clause) == 0){
        anti_clause = type_clause
      }
      else {anti_clause = ""}
    }
    
    if (tolower(groupm[1,]) == tolower(type_col)){
      pro_clause = type_clause
      if (nchar(merch_clause) != 0 & nchar(brand_clause) != 0){
        anti_clause = paste(merch_clause, "AND", brand_clause, sep = " ")
      }
      else if (nchar(merch_clause) ==0 & nchar(brand_clause) !=0){
        anti_clause = brand_clause
      }
      else if (nchar(merch_clause) != 0 & nchar(brand_clause) == 0){
        anti_clause = merch_clause
      }
      else {anti_clause = ""}
    }
    }
    
    else if (fact == 2){
      groupm[1,] <- readline(cat("First primary grouping column?\n"))
      groupm[2,] <- readline(cat("Second primary grouping column?\n"))
      groupm[3,] <- readline(cat("Secondary grouping column?\n"))
      
      if (tolower(groupm[1,]) == tolower(merch_col) & tolower(groupm[2,]) == tolower(brand_col)){
        pro_clause = paste(merch_clause, "AND", brand_clause, sep = " ")
        if (nchar(type_clause) != 0){
          anti_clause = type_clause
        }
        else {anti_clause = ""}
      }
      if (tolower(groupm[1,]) == tolower(merch_col) & tolower(groupm[2,]) == tolower(type_col)){
        pro_clause = paste(merch_clause, "AND", type_clause, sep = " ")
        if (nchar(brand_clause) != 0){
          anti_clause = brand_clause
        }
        else {anti_clause = ""}
      }
      if (tolower(groupm[1,]) == tolower(type_col) & tolower(groupm[2,]) == tolower(brand_col)){
        pro_clause = paste(type_clause, "AND", brand_clause, sep = " ")
        if (nchar(merch_clause) != 0){
          anti_clause = merch_clause
        }
        else {anti_clause = ""}
      }
      }
  
    group = paste(groupm[1,], ",", groupm[2,], ",", groupm[3,], sep = " ")
  }
  
  return(c(cat_clause,rev_clause,merch_clause,brand_clause,type_clause,group,anti_clause,pro_clause,weighting))
} 