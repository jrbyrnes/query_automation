getWSQ <- function(clauses,tablename){
i_select_clause = "SELECT DISTINCT userid"
i_from_clause = paste("FROM", tablename, sep = " ")


if (nchar(clauses[1]) == 0){
  i_where_clause = paste("WHERE", clauses[8], sep = " ")
}

else {
  i_where_clause = paste("WHERE", clauses[1], "AND", clauses[8], sep = " ")
} 

i_clause = paste(i_select_clause, i_from_clause, i_where_clause)
o_select_clause = paste("SELECT", clauses[6], ",", "SUM(", clauses[2], ") REV", sep = " ")
o_from_clause = paste("FROM", tablename, sep = " ")


if (nchar(clauses[7]) == 0){
  o_where_clause = paste("WHERE", clauses[1],"AND userid IN (", i_clause, ")", sep = " ")      
}
else if (nchar(clauses[1])==0){
  o_where_clause = paste("WHERE",clauses[7], "AND userid IN (", i_clause, ")", sep = " ")
}
else if (nchar(clauses[1]) == 0 & nchar(clauses[7]) == 0){
  o_where_clause = paste("WHERE userid IN (", i_clause, ")", sep = " ")
  
}
else{
  o_where_clause = paste("WHERE", clauses[1], "AND", clauses[7], "AND userid IN (", i_clause, ")", sep = " ")
}
o_group_clause = paste("GROUP BY", clauses[6], sep = " ")
o_order_clause = paste("ORDER BY REV DESC")
query <- paste(o_select_clause,o_from_clause,o_where_clause,o_group_clause,o_order_clause, sep = " ")

return(query)}