getOPPQ <- function(clauses,tablename){
  
  item <- unlist(strsplit(clauses[6], ' , '))
  x <- sapply(1:length(item), function(i){out <- paste("t.", item[i], sep = "")})
  if (length(item) == 2){
    out_group <- paste(x[1], ",", x[2], sep = " ")
  }
  else if (length(item) == 3){
    out_group <- paste(x[1], ",", x[2], ",", x[3], sep = " ")
  }
  else (out_group = x)
  
  if (nchar(clauses[9]) == 0){
    weight <- 1
  }
  
  else {
    weight <- paste("AVG(",clauses[9], ")", sep = " ")
  }
  
  i_select_clause = paste('SELECT', clauses[6], ', COUNT( DISTINCT orderID) *', weight, "sq,", weight, "pw", sep = " ")
  i_from_clause = paste("FROM", tablename, sep = " ")
  
  if (nchar(clauses[1]) == 0 & nchar(clauses[8]) != 0){
    i_where_clause = paste("WHERE", clauses[8], sep = " ")
  }
  
  else if (nchar(clauses[1]) != 0 & nchar(clauses[8]) == 0){
    i_where_clause = paste("WHERE", clauses[1], sep = " ")
  }
  
  else if (nchar(clauses[1]) == 0 & nchar(clauses[8]) == 0){
    i_where_clause = ""
  }
  else {
    i_where_clause = paste("WHERE", clauses[1], "AND", clauses[8], sep = " ")
  } 
  
  i_group_clause = paste('GROUP BY', clauses[6], ", userid", sep = " ")
  
  i_clause = paste(i_select_clause, i_from_clause, i_where_clause, i_group_clause, sep = " ")
  
  o_select_clause = paste("SELECT CONVERT(FLOAT, SUM(t.sq)) / CONVERT(FLOAT, SUM(t.pw)),", out_group, sep = " ")
  o_from_clause <- paste("FROM (", i_clause, ") t", sep = " ")
  o_group_clause <- paste("GROUP BY", out_group, sep = " ")
  
  query <- paste(o_select_clause, o_from_clause, o_group_clause, sep = " ")
  return(query)
}
