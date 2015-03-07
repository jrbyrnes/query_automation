metric_Gen <- function(channel, tablename, method){
  source("getParams.R")
  source("getWSQ.R")
  source("getBRQ.R")
  source("getAOSQ.R")
  source("getOPPQ.R")
  library(ggplot2)
  library(grid)
  
  ##get meta clauses for method##
  clauses <- getParams1(method)
  
  ##get data corresponding to method query##  
  if (tolower(method) == "walletshare"){
    query <- getWSQ(clauses,tablename)
    dat <- sqlQuery(channel, query)   
    names(dat)[1] <- 'Walletshare'
  }
  
  else if (tolower(method) == 'buy rate'){
    query <- getBRQ(clauses,tablename)
    dat <- sqlQuery(channel, query)  
    names(dat)[1] <- 'Buy Rate'
  }
  
  else if (tolower(method) == 'average order size'){
    query <- getAOSQ(clauses,tablename)
    dat <- sqlQuery(channel, query)
    names(dat)[1] <- 'Average Order Size'
  }
    
  else if (tolower(method) == 'orders per person'){
    query <- getOPPQ(clauses,tablename)
    dat <- sqlQuery(channel, query)
    names(dat)[1] <- 'Orders per Person'
  }
  
  else if (tolower(method) == 'full stats'){
    query1 <- getAOSQ(clauses, tablename)
    query2 <- getOPPQ(clauses, tablename)
    query3 <- getBRQ(clauses, tablename)
    aos <- sqlQuery(channel, query1)
    opp <- sqlQuery(channel, query2)
    br <- sqlQuery(channel, query3)
    
    dat <- data.frame(aos[,1],opp[,1],br)
    names(dat)[1:3] <- c('Average Order Size', 'Orders per Person', 'Buy Rate')
  
    AOS = dat[,1]
    OPP = dat[,2]
    BR = dat[,3]
    
  
    plot.new()
    pushViewport(viewport(layout = grid.layout(1,3)))
    p1 <- qplot(dat[,4],AOS,geom='bar',stat='identity',fill = AOS, main = 'Average Order Size', ylab = 'Average Order Size', xlab = 'Merchant') + coord_flip() +geom_text(label = paste('$',round(AOS),sep = ""))
    p2 <- qplot(dat[,4],OPP,geom='bar',stat='identity',fill = OPP, main = 'Orders per Person', ylab = 'Order per Person', xlab = 'Merchant') + coord_flip() + geom_text(label = round(OPP, digits = 1))
    p3 <- qplot(dat[,4],BR,geom='bar',stat='identity',fill = BR, main = 'Buy Rate', ylab = 'Buy Rate', xlab = 'Merchant') + coord_flip() + geom_text(label = paste('$',round(BR), sep = ""))
    print(p1,vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
    print(p2,vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
    print(p3,vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
    
    
  }
  
  else {return('Invalid query method')}
  
  return(dat)
  
  
}