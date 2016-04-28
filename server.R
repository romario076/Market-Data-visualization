

suppressWarnings( library(RMySQL))
suppressWarnings( library(fasttime))
suppressWarnings( library(data.table))
suppressWarnings( library(knitr))
suppressWarnings( library(dplyr))
suppressWarnings( library(xts))
suppressWarnings( library(scales))

Sys.setenv(TZ='GMT')


MarketData<- function(date, from, to, Symbol) {
  From<- paste("'", from, "'", sep="")
  To<- paste("'", to, "'", sep="")
  Symbol<- paste("'", Symbol, "'", sep="")
  mydb = dbConnect(MySQL(), user='roma', password='2bn4aYjV8bz5OY', dbname='reports', host='192.168.31.15')
  ss<- paste0("select concat(Timestamp, ' ',Time) as Time, Reason,Bid_P, Ask_P, tShares, tSide, (Bid_P+Ask_P)/2 as MidPrice, tPrice,tType, iCBC,iMarket, iShares, iPaired, iExchange, if((tType='OPG' or tType='CLX'),1, tShares) as tShares1, IF (tSide='BID','red', IF (tSide='ASK', 'green', 'blue')) as color from ","`",as.symbol(date),"`", " where Symbol =", Symbol," and Ask_P>0 and Bid_P>0 and Time>",From," and Time<=",To, "")    
  #ss1<- paste0("select Time,tPrice, MidPrice, Bid_P, Ask_P, tShares, color, tSide, tShares, IF(tShares1>2,round(tShares1),2) as tShares1 from (select  concat(Timestamp, ' ',Time) as Time,(Bid_P+Ask_P)/2 as MidPrice, tPrice,  Bid_P, Ask_P, tShares, tSide, IF (tSide='BID','red', IF (tSide='ASK', 'green', 'blue')) as color, (tShares/(select max(tShares) as tShares1 from ","`",as.symbol(date),"`", "  where tType !='OPG' and tType !='CLX' and Symbol=", Symbol,"  and Ask_P>0 and Bid_P>0 and Time>",From,"and Time<=",To, ")) as tShares1 from ","`",as.symbol(date),"`", "  where tType !='OPG' and tType !='CLX' and Symbol=", Symbol,"  and Ask_P>0 and Bid_P>0 and Time>",From,"and Time<=",To, ") as a")
  query <- dbSendQuery(mydb, ss)
  data <- fetch(query, n= -1)
  dbClearResult(dbListResults(mydb)[[1]])
  dbDisconnect(mydb)
  op<-options(digits.secs=6)
  data$Time<- fastPOSIXct(data$Time, required.components = 6L, tz = "GMT")
  return(data)
}


Printu<- function(date, from, to, symbol) {
  From<- paste("'", from, "'", sep="")
  To<- paste("'", to, "'", sep="")
  Symbol<-paste("'", symbol, "'", sep="")
  mydb = dbConnect(MySQL(), user='roma', password='2bn4aYjV8bz5OY', dbname='reports', host='192.168.31.15')
  ss<- paste0("select concat(Timestamp, ' ',min(Time)) as Time, tType, tPrice from ","`",as.symbol(date),"`", "  where Symbol=", Symbol," and (tType='OPG' or tType='CLX') and tPrice>0 and Time>",From," and Time<=",To, " group by tType")
  query <- dbSendQuery(mydb, ss)
  data <- fetch(query, n= -1)
  dbClearResult(dbListResults(mydb)[[1]])
  dbDisconnect(mydb)
  op<-options(digits.secs=6)
  data$Time<- fastPOSIXct(data$Time, required.components = 6L, tz = "GMT")
  return(data)
}


PrevCLX2<- function(date, symbol) {
  Symbol<-paste("'", symbol, "'", sep="")
  date1<- as.Date(date)-1
  data<- data.frame()
  pp<- function(date1, From, To, Symbol){
    mydb = dbConnect(MySQL(), user='roma', password='2bn4aYjV8bz5OY', dbname='reports', host='192.168.31.15')
    ss<- paste0("select concat(Timestamp, ' ',min(Time)) as Time, tType, tPrice, tVenue from ","`",as.symbol(as.character(date1)),"`", "  where Symbol=", Symbol," and tType='CLX' group by tType, tVenue")
    query <- dbSendQuery(mydb, ss)
    data <- fetch(query, n= -1)
    dbClearResult(dbListResults(mydb)[[1]])
    dbDisconnect(mydb)
    return(data)}
  try(data<-pp(date1=date1, From=From, To=To, Symbol=Symbol), silent = TRUE)
  while(nrow(data)==0) {
    date1<- as.Date(date1)-1
    try(data<-pp(date1=date1, From=From, To=To, Symbol=Symbol), silent = TRUE)
    if (nrow(data)>0) {break;}
  }
  op<-options(digits.secs=6)
  data$Time<- fastPOSIXct(data$Time, required.components = 6L, tz = "EET")
  data$tPrice<- as.numeric(data$tPrice)
  data<- data[data$tPrice>0,][1,]
  return(data)
}


PrevCLX<- function(date, symbol) {
  Date<- paste("'", date, "'", sep="")
  Symbol<-paste("'", symbol, "'", sep="")
  
  date1<- as.Date(date)-1
  data<- data.frame()
  mydb = dbConnect(MySQL(), user='roma', password='2bn4aYjV8bz5OY', dbname='stock', host='192.168.31.21')
  ss<- paste0("select distinct Exchange from Stock where Timestamp=",Date," and Symbol=",Symbol,"")
  query <- dbSendQuery(mydb, ss)
  ex <- fetch(query, n= -1)[1,1]
  dbClearResult(dbListResults(mydb)[[1]])
  dbDisconnect(mydb)
  exchange<- NULL
  if (ex=="Q") {exchange<- "NSDQ"}
  if (ex=="P") {exchange<- "AXDP"}
  if (ex=="N") {exchange<- "NYSE"}
  if (ex=="Z") {exchange<- "BAT"}
  #if (ex=="A") {exchange<- "AMEX"}
  
  pp<- function(date1, From, To, Symbol){
    mydb = dbConnect(MySQL(), user='roma', password='2bn4aYjV8bz5OY', dbname='reports', host='192.168.31.15')
    ss<- paste0("select  tType, tPrice, MsgSource from ","`",as.symbol(as.character(date1)),"`", "  where Symbol=", Symbol," and tType='CLX' group by tType, MsgSource")
    query <- dbSendQuery(mydb, ss)
    data <- fetch(query, n= -1)
    dbClearResult(dbListResults(mydb)[[1]])
    dbDisconnect(mydb)
    return(data)
  }
  if (length(exchange)> 0) {
    try(data<-pp(date1=date1, From=From, To=To, Symbol=Symbol), silent = TRUE)
    while(nrow(data)==0) {
      date1<- as.Date(date1)-1
      try(data<-pp(date1=date1, From=From, To=To, Symbol=Symbol), silent = TRUE)
      if (nrow(data)>0) {break;}
    }
  }
  data<- data[data$MsgSource %in% exchange, ]
  if (nrow(data)==0) {
      Timestamp<- paste("'", date1, "'", sep="")
      mydb = dbConnect(MySQL(), user='roma', password='2bn4aYjV8bz5OY', dbname='reports', host='192.168.31.15')
      ss3<- paste0("select  tType, tPrice, MsgSource from ","`",as.symbol(as.character(date1)),"`", "  where Timestamp=",Timestamp," and Symbol=", Symbol," and tPrice>0 order by Time DESC limit 1")
      query3 <- dbSendQuery(mydb, ss3)
      data <- fetch(query3, n= -1)
      dbClearResult(dbListResults(mydb)[[1]])
      dbDisconnect(mydb)
  }
  if (nrow(data)>0 & data$tPrice==0) {
      Timestamp<- paste("'", date1, "'", sep="")
      mydb = dbConnect(MySQL(), user='roma', password='2bn4aYjV8bz5OY', dbname='reports', host='192.168.31.15')
      ss3<- paste0("select  tType, tPrice, MsgSource from ","`",as.symbol(as.character(date1)),"`", "  where Timestamp=",Timestamp," and Symbol=", Symbol," and tPrice>0 order by Time DESC limit 1")
      query3 <- dbSendQuery(mydb, ss3)
      data <- fetch(query3, n= -1)
      dbClearResult(dbListResults(mydb)[[1]])
      dbDisconnect(mydb)
  }
  op<-options(digits.secs=6)
  data$tPrice<- as.numeric(data$tPrice)
  return(data)
}



Orders<- function(date, from, to, symbol) {

  Date<- paste("'",date, "'", sep="")
  Symbol<- paste("'", symbol, "'", sep="") 
  From<- paste("'", from, "'", sep="")
  To<- paste("'", to, "'", sep="")

  mydb = dbConnect(MySQL(), user='roma', password='2bn4aYjV8bz5OY', dbname='reports', host='192.168.31.21')
  select2<- paste0('select concat(`date`," ", `timestamp`) as Time, `strategy`, `messagetype`, `exchange`, `orderid`,`side`, `price`, `timeinforce`, `type`,`sharesexecuted` as Shares,
                   CASE
                     when exchange="NSDQ_OUCH_BK" then "purple"
                     when exchange="ARCA_DIRECT_BK" then "maroon"
                     when exchange="NOMURA_FIX_BK" then "gold"
                     when exchange="NYSE_UTP_BK" then "moccasin"
                     else "pink"
                     END color
                     from History
                     where symbol=',Symbol,' and date=',Date,' and (messagetype="trader_new_order" or messagetype="trader_order_executed" or messagetype="trader_modify_order")  and timestamp>=',From,' and timestamp<= ',To,'')
  query <- dbSendQuery(mydb, select2)
  data <- fetch(query, n= -1)
  dbClearResult(dbListResults(mydb)[[1]])
  dbDisconnect(mydb)
  op<-options(digits.secs=6)
  data$Time<- fastPOSIXct(data$Time, required.components = 6L, tz = "GMT")
  if (nrow(data[data$messagetype=="trader_order_executed",])>0) {
    NwData<- data[(data$orderid %in% unique(data$orderid[data$messagetype=="trader_order_executed"])) & (data$messagetype=="trader_new_order" | data$messagetype=="trader_modify_order"), ]
   
     if (nrow(NwData)>1) {
#       mydb = dbConnect(MySQL(), user='roma', password='2bn4aYjV8bz5OY', dbname='reports', host='192.168.31.21')
#       select<- paste0('select concat(`date`," ", `timestamp`) as Time, `strategy`, `messagetype`, `exchange`, `orderid`,`side`, `price`, `timeinforce`, `type`,`sharesexecuted` as Shares,
#                    CASE
#                      when exchange="NSDQ_OUCH_BK" then "purple"
#                      when exchange="ARCA_DIRECT_BK" then "maroon"
#                      when exchange="NOMURA_FIX_BK" then "gold"
#                      when exchange="NYSE_UTP_BK" then "moccasin"
#                      else "pink"
#                      END color
#                      from History
#                      where symbol=',Symbol,' and date=',Date,' and (messagetype="trader_new_order" or messagetype="trader_order_executed" or messagetype="trader_modify_order")  
#                        and orderid in (select orderid from History where symbol=',Symbol,' and date=',Date,'and timestamp>=',From,' and timestamp<= ',To,')')
#       query <- dbSendQuery(mydb, select)
#       data <- fetch(query, n= -1)
#       dbClearResult(dbListResults(mydb)[[1]])
#       dbDisconnect(mydb)
#       op<-options(digits.secs=6)
#       data$Time<- fastPOSIXct(data$Time, required.components = 6L, tz = "GMT")
#       NwData<- data[(data$orderid %in% unique(data$orderid[data$messagetype=="trader_order_executed"])) & (data$messagetype=="trader_new_order" | data$messagetype=="trader_modify_order"), ]
      colnames(NwData)<- c('NWTime','strategy','NWmessagetype','NWexchange', 'orderid', 'NWside', 'NWprice', 'timeinforce', 'type','Shares', 'color')
      MergeData<- merge(data[data$messagetype=="trader_order_executed", c("Time", "strategy", "messagetype", "exchange", "side", "price", "orderid","Shares", "color")] , NwData[,c('NWTime','strategy','NWprice', 'timeinforce', 'type', 'orderid')], on="orderid")
        shape=c()
        for (i in 1:nrow(MergeData)) {
          if (MergeData$side[i]=="B" & MergeData$type[i]=="HIDDEN_ORDER_TYPE") {shape[i]=105}
          if (MergeData$side[i]=="B" & MergeData$type[i] =="REGULAR_ORDER_TYPE") {shape[i]=5}
          if (MergeData$side[i]=="B" & MergeData$type[i] =="MIDPOINT_PEGGING_ORDER_TYPE") {shape[i]=305}
          if (MergeData$side[i] !="B" & MergeData$type[i] =="HIDDEN_ORDER_TYPE") {shape[i]=106}
          if (MergeData$side[i] !="B" & MergeData$type[i] =="REGULAR_ORDER_TYPE") {shape[i]=6}
          if (MergeData$side[i] !="B" & MergeData$type[i] =="MIDPOINT_PEGGING_ORDER_TYPE") {shape[i]=306}
        }
        MergeData$Shape<- shape
        dd1<-MergeData[,c("Time","strategy", "exchange" ,"price", "orderid" ,"timeinforce", "Shape", "Shares", "color")]
        dd2<-MergeData[,c("NWTime","strategy", "exchange", "price", "orderid" ,"timeinforce", "Shape", "Shares", "color")]
        dd2$Shape<-0
        dd2$Size<-4
        dd1$Size<-16
        colnames(dd2)<- c("Time","strategy","exchange", "price", "orderid" ,"timeinforce", "Shape","Shares","color", "Size")
        dd<- rbind(dd1, dd2)
        dd$exchange<- sapply(dd$exchange, function(x) strsplit(x, "_")[[1]][1], USE.NAMES = FALSE)
     } else {dd <-  dd<- data.frame()}
  } else {
    dd<- data.frame()
  }
  return (dd)
}

News<- function(date, from , to, symbol) {
  
  insert <- function(v,e,pos){
    return(c(v[1:(pos-1)],e,v[(pos):length(v)])) }

  Date<- paste("'", date, "'", sep="")
  Symbol<- paste("'", symbol, "'", sep="") 
  From<- paste("'", from, "'", sep="")
  To<- paste("'", to, "'", sep="")
  
  
  mydb = dbConnect(MySQL(), user='roma', password='2bn4aYjV8bz5OY', dbname='news', host='192.168.31.21')
  select2<- paste0('select concat(srcDate," ",time) as Time, head from News where symbols=',Symbol,' and srcDate=',Date,' and time>=',From,' and time<=',To,'')
  query <- dbSendQuery(mydb, select2)
  news <- fetch(query, n= -1)
  dbClearResult(dbListResults(mydb)[[1]])
  dbDisconnect(mydb)
  news<- news[!duplicated(news$head),]
  news$Time<- fastPOSIXct(news$Time, required.components = 6L, tz = "GMT")
  if (nrow(news)>0) {
      newHead<- c()
      for (i in 1:nrow(news)) {
        k<-strsplit(news$head[i], " ")[[1]]
        ind<- 5*1:round(length(k)/5)
        for (j in 1:length(ind)) {  
          k <- insert(k,  "<br>", ind[j])
          ind[j+1]<- ind[j+1]+1
        }
        newHead[i]<- paste(k, collapse=" ")
      }
      news$head<- newHead
  } else { news<- data.frame()}

return(news)
}



###Shiny server
shinyServer(function(input, output, session) {
  values<- reactiveValues(event=NULL)
  dateText<- renderText(as.character(input$date))
  
  data<- reactive({ MarketData(date =dateText(), from=input$from, to=input$to, Symbol =  input$text )})
  data1<- reactive({data()[,c("Time","Bid_P", "Ask_P", "tShares","tShares1", "tSide", "MidPrice", "tPrice", "color")]})
  output$mess<- renderUI({  if (is(try(data1(), silent=T), "try-error") )   { 
    mess1<- ' Message: Empty data. Choose another day :-) ' }
  })
  
  f<- reactive({ f<-as.xts(data1(), order.by=data1()[, 1], frequency=NULL)
    return (f)})
  Seconds<-  reactive({ep <- endpoints(f(),'seconds')
      data<- as.data.frame(period.apply(f(), INDEX=ep, FUN=function(x) tail(x, 1)))
      row.names(data)<- NULL
      data$Time<- fastPOSIXct(data$Time, required.components = 6L, tz = "GMT")
       data$tPrice<- as.numeric(as.character(data$tPrice))
       data$Ask_P<- as.numeric(as.character(data$Ask_P))
       data$Bid_P<- as.numeric(as.character(data$Bid_P))
  return(data)})
      Seconds10<- reactive({ep <- endpoints(f(),'seconds', k=10)
      data<- as.data.frame(period.apply(f(), INDEX=ep, FUN=function(x) tail(x, 1)))
      row.names(data)<- NULL
      data$Time<- fastPOSIXct(data$Time, required.components = 6L, tz = "GMT")
       data$tPrice<- as.numeric(as.character(data$tPrice))
       data$Ask_P<- as.numeric(as.character(data$Ask_P))
       data$Bid_P<- as.numeric(as.character(data$Bid_P))
  return(data)})
  
  # Seconds30<- reactive({ep <- endpoints(f(),'seconds', k=30)
  #                     data<- as.data.frame(period.apply(f(), INDEX=ep, FUN=function(x) tail(x, 1)))
  #                     row.names(data)<- NULL
  #                     data$Time<- fastPOSIXct(data$Time, required.components = 6L, tz = "GMT")
  #                     data<- rbind( data1()[data1()$tPrice>0,][1,], data, tail(data1(),1) )
  #                return(data)})
  
  Minutes<- reactive({ep <- endpoints(f(),'minutes')
      data<- as.data.frame(period.apply(f()[,], INDEX=ep, FUN=function(x) tail(x, 1)))
      row.names(data)<- NULL
      data$Time<- fastPOSIXct(data$Time, required.components = 6L, tz = "GMT")
       data$tPrice<- as.numeric(as.character(data$tPrice))
       data$Ask_P<- as.numeric(as.character(data$Ask_P))
       data$Bid_P<- as.numeric(as.character(data$Bid_P))
  return(data)})
  
  Imbalance<- reactive({
    imb<- data()[data()$Reason=="Imbalance", c("Time", "Bid_P", "Ask_P", "iCBC","iMarket", "iShares", "iPaired", "iExchange")]
    tDiff<- as.numeric(tail(data1()$Time,1)-data1()$Time[1], units="secs")
    if (tDiff> 3600*2) {
      f= as.xts(imb, order.by=imb[, 1], frequency=NULL)
      ep <- endpoints(f,'seconds', k=30)
      imb1<- as.data.frame(period.apply(f, INDEX=ep, FUN=function(x) tail(x,1)))
      row.names(imb1)<- NULL
      imb1$Time<- fastPOSIXct(imb1$Time, required.components = 6L, tz = "GMT")
      imb1$iCBC<- as.numeric(as.character(imb1$iCBC))
    } else {imb1<- imb}
    imb1$Time<- fastPOSIXct(imb1$Time, required.components = 6L, tz = "GMT")
    # imb1$iExchange<- as.character(imb1$iExchange)
    return(imb1)
    
  })
  
  ImbNSDQ<- reactive({subset(Imbalance(), iExchange=="NSDQ")})
  ImbNYSE<- reactive({subset(Imbalance(), iExchange=="NYSE")})
  ImbARCA<- reactive({subset(Imbalance(), iExchange=="AXDP")})
  
  plotdelay<- reactive({
    tDiff<- as.numeric(tail(data1()$Time,1)-data1()$Time[1], units="secs")
    if (tDiff<= 1800) {data<-data1()} else {
      if (tDiff>1800 & tDiff<5*3600) {data<-Seconds()}
      if (tDiff>5*3600 & tDiff<7*(3600)) {data<-Seconds10()}
      if (tDiff>7*3600) {data<-Minutes()}
    }
    #data<- data[data$tPrice>0,]
    #data$tPrice<- as.numeric(as.character(data$tPrice))
    data$tSide<- as.character(data$tSide)
    data$tShares1<- as.integer(rescale(as.numeric(data$tShares1), c(4,14)))
    return(data)
  })
  
  
  BottomPlot<- reactive({
    tDiff<- as.numeric(tail(data1()$Time,1)-data1()$Time[1], units="secs")
    if (tDiff<2*3600) {data<-Seconds()} else {
      if (tDiff>=2*3600 & tDiff<5*3600) {data<-Seconds10()}
      if (tDiff>=5*3600) {data<-Minutes()} 
    }
    data<- data[ ,c("Time", "MidPrice")]
    return(data)
  })
  
  Size<- reactive({as.integer(6)})
  alpha<- reactive({0.6})
  alpha1<- reactive({0.9})
  Hline<-reactive({15}) 
  Font<- reactive({9})
  
  y0<- reactive({min(plotdelay()$tPrice[plotdelay()$tPrice>0]) })
  y1<- reactive({max(plotdelay()$tPrice) })
  
  pp<- reactive({ Printu(date =dateText(), from=input$from, to=input$to, symbol= input$text ) })
  Order<- reactive({ Orders(date=as.character(dateText()), from=input$from, to=input$to, symbol= input$text) })
  Newsdata<- reactive({ News(date=as.character(dateText()), from=input$from, to=input$to, symbol=input$text) })
  PrevClose<- reactive({
        dat<- PrevCLX(date=as.character(dateText()), symbol=input$text)
        data<- data.frame(Time=c(plotdelay()$Time[1], tail(plotdelay()$Time,1)), tPrice=c(dat$tPrice, dat$tPrice))
      return(data) })
  
#Strategies Orders input  
  dd<- reactive ({
    un = unique(Order()$strategy)
    if (is.null(input$strat)==FALSE) {
       if (input$strat != "None") {
         if (input$strat %in% inputChoices()) {
           dd<- Order()[Order()['strategy']==input$strat, ]
       } else {dd<- data.frame() } 
      } else {dd<- data.frame() }
    } else {dd<- data.frame() }
    return(dd)
  })

  
 trendPlot <- renderPlotly({
    withProgress(message = 'Creating plot', value = 0.1, {

    xax <- list(
      title = "",
      tickfont = list(color = "darkblue")
    )
    yax <- list(
      title = "",
      tickfont = list(color = "darkblue")
    )
    event<- event_data("plotly_selected", source = "subset")

    l<- list( color = toRGB("grey90", alpha = 0.1),
              fillcolor = toRGB("grey90", alpha = 0.1),
              shape = "hv",
              width = .001)
    
    dd<- dd()
  ###For PrevClx
    #if (input$prevclx) { PrevClose<- data.frame(Time=c(plotdelay()$Time[1], tail(plotdelay()$Time,1)), tPrice=c(PrevClose()$tPrice, PrevClose()$tPrice)) }
    
    
    if (nrow(data.frame(event)) <1 & input$spread==FALSE) {
        #list(size=Size(),color=ifelse(tSide=="BID", 'red', ifelse(tSide=="ASK", "green", "blue")))
        py<- plot_ly(plotdelay()[plotdelay()$tPrice>0, ], x = Time, y = tPrice, mode = "markers", text = paste0('Side:',tSide, " Shares:", tShares), marker=list(size=tShares1,color=color, opacity= alpha() ))  %>%
          layout(showlegend = FALSE, hovermode = "closest", legend = list(x = 1, y = 1),paper_bgcolor= 'rgba(249,249,263,.85)')
        py<- layout(xaxis=xax, yaxis=yax)
       
      ###Imbalances 
        if (input$icbcNSDQ &  (is(try(ImbNSDQ(), silent=T), "try-error")==FALSE )) {
          py<- add_trace(ImbNSDQ(), x=Time, y=iCBC, name="iCBC NSDQ", mode="markers", marker=list(symbol = 22, color= "darkorchid", size=Size(), opacity= alpha1())) 
          py<- layout(yaxis=list(range=c( y0(), y1())))
        } 
        if (input$icbcNYSE & (is(try(ImbNYSE(), silent=T), "try-error")==FALSE )) {
          py<- add_trace(ImbNYSE(), x=Time, y=iCBC, mode="markers",name="iCBC NYSE", marker=list(symbol = 22, color= "darkorchid", size=Size(), opacity= alpha1()))  
          py<- layout(yaxis=list(range=c( y0(), y1())))
        } 
        if (input$icbcARCA & (is(try(ImbARCA(), silent=T), "try-error")==FALSE )) {
          py<- add_trace(ImbARCA(), x=Time, y=iCBC, mode="markers",name="iCBC ARCA", marker=list(symbol = 22, color= "darkorchid",size=Size(), opacity= alpha1()))  
          py<- layout(yaxis=list(range=c( y0(), y1())))
        }
      ###Prev Close
        if (input$prevclx) {py<- add_trace(PrevClose(), x=Time, y=tPrice, line=list(width=0.9, color="teal", dash="1"), marker=list(size=2), name="PrevCLX")}
        
      ###Prints
        if (is(try(pp(), silent=T), "try-error")==FALSE)  {
          for (i in 1: nrow(pp())) {
            py <- py %>% add_trace(x = c(pp()$Time[i],pp()$Time[i]), y = c(y0(), y1()), mode = "line",marker=list(size=1), line=list(dash="2", color="blue"), evaluate=TRUE) %>% 
              add_trace(x = c(pp()$Time[i]-Hline(), pp()$Time[i]+Hline()), y = c(pp()$tPrice[i], pp()$tPrice[i]), marker=list(size=1), mode = "line", line=list(dash="1", color="violet"), evaluate=TRUE)
            }
         }
      ###Orders
        if (nrow(dd)>0) {
          id<- unique(dd$orderid)
          for (i in 1:length(id)) {
            tt<-dd[dd$orderid==id[i], ]
            tt<- tt[order(tt$Time), ]
            py<- py %>%  add_trace( x= tt$Time, y=tt$price, mode="markers+lines",name=id[i], text=paste0("Tif:",tt$timeinforce, " Shares:", tt$Shares, "<br>Exchange:", tt$exchange),
                                    marker=list(symbol=tt$Shape, size=tt$Size, color=tt$color),
                                    line=list(width=0.2, color=tt$color[1]), evaluate=TRUE)   
           }
         }
        ###News
        if ( input$news ) {
          if (nrow(Newsdata())>0)  {
            a<- list()
            for (i in 1:nrow(Newsdata())) { 
              tt<- NULL
              tt<- Newsdata()[i,]
              a[[i]] <- list(
                bordercolor="steelblue",
                borderwidth=1,
                bgcolor= "#F0F8FF",
                arrowcolor="navy",
                font= list(color="darkblue", family="Droid Sans", size=Font()),
                align="left",
                opacity=0.8,
                x =tt$Time,
                y = y1(),
                text = gsub("[$]","",tt$head),
                xref = "x",
                yref = "y",
                showarrow = TRUE,
                arrowhead = 3,
                ax = 20,
                ay = -40)
              py<- py %>% add_trace( x = c(tt$Time, tt$Time), y = c(y0(), y1()), hoverinfo="x", marker=list(size=2, color="blue"), line=list(width=0.8, color="blue"), evaluate=TRUE)
            }
            py<- py %>% layout(annotations=a)
          }
        }
      }
    
    if (nrow(data.frame(event)) <1 & input$spread==TRUE) {
        py<- plot_ly(subset(plotdelay(), tPrice>0), x = Time, y = tPrice, name="Price",mode = "markers", text = paste0('Side:',tSide, " Shares:", tShares), marker=list(size=tShares1, color=color, opacity= alpha() ))
        py<- add_trace(plotdelay(), x=Time, y=Bid_P, name = "Bid", line = l)
        py<- add_trace(plotdelay(), x=Time, y=Ask_P, name = "Ask", line = l, fill="tonexty") 
        py<-  layout(showlegend = FALSE,hovermode = "closest", paper_bgcolor= 'rgba(249,249,263,.85)')
        py<- layout(xaxis=xax, yaxis=yax)
        
        #p2<- as.numeric(max(plotdelay()$tPrice))
        #p1<- as.numeric(min(plotdelay()$tPrice[plotdelay()$tPrice>0]))
        p2<- as.numeric(y1())
        p1<- as.numeric(y0())
        
      ###Imbalances  
        if (input$icbcNSDQ &  (is(try(ImbNSDQ(), silent=T), "try-error")==FALSE )) {
          py<- add_trace(ImbNSDQ(), x=Time, y=iCBC, name="iCBC NSDQ", mode="markers", marker=list(symbol = 22, color= "darkorchid", size=Size(), opacity= alpha1())) 
          py<- layout(yaxis=list(range=c(p1-(p2-p1)*0.05, p2+(p2-p1)*0.05)))
        } 
        if (input$icbcNYSE & (is(try(ImbNYSE(), silent=T), "try-error")==FALSE )) {
          py<- add_trace(ImbNYSE(), x=Time, y=iCBC, mode="markers",name="iCBC NYSE", marker=list(symbol = 22, color= "darkorchid", size=Size(), opacity= alpha1())) 
          py<- layout(yaxis=list(range=c(p1-(p2-p1)*0.05, p2+(p2-p1)*0.05)))
        } 
        if (input$icbcARCA & (is(try(ImbARCA(), silent=T), "try-error")==FALSE )) {
          py<- add_trace(ImbARCA(), x=Time, y=iCBC, mode="markers",name="iCBC ARCA", marker=list(symbol = 22, color= "darkorchid",size=Size(), opacity= alpha1())) 
          py<- layout(yaxis=list(range=c(p1-(p2-p1)*0.05, p2+(p2-p1)*0.05)))
        }
      ###Prev Close
        if (input$prevclx) {py<- add_trace(PrevClose(), x=Time, y=tPrice, line=list(width=1, color="teal"), marker=list(size=2), name="PrevCLX")}
        
      ###Prints
        if (is(try(pp(), silent=T), "try-error")==FALSE)  {
          for (i in 1: nrow(pp())) {
            py <- py %>% add_trace(x = c(pp()$Time[i],pp()$Time[i]), y = c(y0(), y1()), mode = "line",marker=list(size=1), line=list(dash="2", color="blue"), evaluate=TRUE) %>% 
              add_trace(x = c(pp()$Time[i]-Hline(),pp()$Time[i]+Hline()), y = c(pp()$tPrice[i], pp()$tPrice[i]), marker=list(size=1), mode = "line", line=list(dash="1", color="violet"), evaluate=TRUE)
          }
         }
      ###Orders
        if (nrow(dd)>0) {
          id<- unique(dd$orderid)
          for (i in 1:length(id)) {
            tt<-dd[dd$orderid==id[i], ]
            tt<- tt[order(tt$Time), ]
            py<- py %>%  add_trace( x= tt$Time, y=tt$price, mode="markers+lines",name=id[i], text=paste0("Tif:",tt$timeinforce, " Shares:", tt$Shares, "<br>Exchange:", tt$exchange),
                                    marker=list(symbol=tt$Shape, size=tt$Size, color=tt$color),
                                    line=list(width=0.2, color=tt$color[1]), evaluate=TRUE)   
          }
        }
      ###News
        if ( input$news  ) {
          if (nrow(Newsdata())>0)  {
            a<- list()
            for (i in 1:nrow(Newsdata())) { 
              tt<- NULL
              tt<- Newsdata()[i,]
              a[[i]] <- list(
                bordercolor="steelblue",
                borderwidth=1,
                bgcolor= "#F0F8FF",
                arrowcolor="navy",
                font= list(color="darkblue", family="Droid Sans", size=Font()),
                align="left",
                opacity= 0.8,
                x =tt$Time,
                y =y1(),
                text = gsub("[$]","",tt$head),
                xref = "x",
                yref = "y",
                showarrow = TRUE,
                arrowhead = 3,
                ax = 20,
                ay = -40)
              py<- py %>% add_trace( x = c(tt$Time, tt$Time), y = c(y0(), y1()), hoverinfo="x", marker=list(size=2, color="blue"), line=list(width=0.8, color="blue"), evaluate=TRUE)
            }
            py<- py %>% layout(annotations=a)
            }
         }
      }
    
    if (nrow(data.frame(event)) >=1 ) {
            t1<- as.POSIXct(as.character(as.POSIXct(min(event$x)/1000, origin="1970-01-01", tz="EET")),  "%Y-%m-%d %H:%M:%S", tz ="GMT")-1
            t2<- as.POSIXct(as.character(as.POSIXct(max(event$x)/1000, origin="1970-01-01", tz="EET")),  "%Y-%m-%d %H:%M:%S", tz ="GMT")+1
            tDiff= as.numeric(t2-t1, units="secs")
            if (tDiff<= 1800) {data<- data1()} else {
              if (tDiff> 1800 & tDiff< 3600*5) {
                data<-Seconds()}
              if (tDiff> 3600*5 & tDiff< 3600*7) {
                data<-Seconds10()}
              if (tDiff> 3600*7) {
                data<-Minutes()}
            }
            data<- data[(data$Time>=t1 & data$Time<=t2) ,]
            data$tSide<- as.character(data$tSide)
            data$tShares1<- rescale(as.numeric(data$tShares1), c(4,14))
            data$color<- as.character(data$color)
            dataTprice<- data[data$tPrice >0 ,]
          
              if (nrow(data.frame(event)) >=1 & input$spread==FALSE)  {
                    py <- plot_ly(dataTprice, x= Time, y=tPrice, mode="markers", name="Price", text = paste0('Side:',tSide, " Shares:", tShares),  marker=list(size=tShares1,color=color, opacity = alpha() )) %>%
                      layout(showlegend = FALSE, hovermode = "closest", legend = list(x = 1, y = 1),paper_bgcolor= 'rgba(249,249,263,.85)')
                    py<- layout(xaxis=xax, yaxis=yax)   
                 }
              if (nrow(data.frame(event)) >=1 & input$spread==TRUE)  {
                  y0<- min(data$tPrice[data$tPrice >0])
                  y1<-max(data$tPrice[data$tPrice >0])
                    py <- plot_ly(dataTprice, x= Time, y=tPrice, mode="markers", name="Price",text = paste0('Side:',tSide, " Shares:", tShares), marker=list(size=tShares1,color=color, opacity= alpha() ) ) 
                    py<- add_trace(data, x=Time, y=Bid_P, name = "Bid", line = l)
                    py<- add_trace(data, x=Time, y=Ask_P, name = "Ask", line = l, fill="tonexty") 
                    py<-  layout(showlegend = FALSE,hovermode = "closest", paper_bgcolor= 'rgba(249,249,263,.85)')
                    py<- layout(xaxis=xax, yaxis=yax) 
                }
          ###Prints       
            if (is(try(pp(), silent=T), "try-error")==FALSE)  {
                temp<- pp()[(pp()$Time>=t1 & pp()$Time<=t2), ]
                for (i in 1: nrow(temp)) {
                  py <- py %>% add_trace(x = c(temp$Time[i], temp$Time[i]), y = c( min(dataTprice$Bid_P), max(dataTprice$Ask_P)  ), mode = "line",marker=list(size=1), line=list(dash="2", color="blue"), evaluate=TRUE) %>% 
                    add_trace(x = c(temp$Time[i]-Hline(), temp$Time[i]+Hline()), y = c(temp$tPrice[i], temp$tPrice[i]), marker=list(size=1), mode = "line", line=list(dash="1", color="violet"), evaluate=TRUE)
                }
              }
          ###Imbalances  
            if (input$icbcNSDQ) {
              tt<- subset(ImbNSDQ(), Time>=t1 & Time<=t2)
              if (nrow(tt)>0) { py<- add_trace(tt, x=Time, y=iCBC, name="iCBC NSDQ", mode="markers", marker=list(symbol = 22, color= "darkorchid", size=Size(), opacity= alpha1()))
              py<- layout(yaxis=list(range=c(min(dataTprice$Bid_P)-(max(dataTprice$Ask_P)-min(dataTprice$Bid_P))*0.005,max(dataTprice$Ask_P)+(max(dataTprice$Ask_P)-min(dataTprice$Bid_P))*0.005 )))
              }
            } 
            if (input$icbcNYSE) {
              tt<- subset(ImbNYSE(), Time>=t1 & Time<=t2)
              if (nrow(tt)>0) {py<- add_trace(tt, x=Time, y=iCBC, mode="markers",name="iCBC NYSE", marker=list(symbol = 22, color= "darkorchid", size=Size(), opacity= alpha1()))
              py<- layout(yaxis=list(range=c(min(dataTprice$Bid_P)-(max(dataTprice$Ask_P)-min(dataTprice$Bid_P))*0.005,max(dataTprice$Ask_P)+(max(dataTprice$Ask_P)-min(dataTprice$Bid_P))*0.005 )))
              }
            } 
            if (input$icbcARCA) {
              tt<- subset(ImbARCA(), Time>=t1 & Time<=t2)
              if (nrow(tt)>0) {py<- add_trace(tt, x=Time, y=iCBC, mode="markers",name="iCBC ARCA", marker=list(symbol = 22, color= "darkorchid",size=Size(), opacity= alpha1()))
              py<- layout(yaxis=list(range=c(min(dataTprice$Bid_P)-(max(dataTprice$Ask_P)-min(dataTprice$Bid_P))*0.005,max(dataTprice$Ask_P)+(max(dataTprice$Ask_P)-min(dataTprice$Bid_P))*0.005 )))
              }  
            }
          ###Prev Close
            if (input$prevclx) {
              #PrevClose<- data.frame(Time=c(data$Time[1], tail(data$Time,1)), tPrice=c(PrevClose()$tPrice, PrevClose()$tPrice))
              #PrevClose()['Time']<- c(data$Time[1], tail(data$Time,1))
              #py<- add_trace(PrevClose, x=Time, y=tPrice, line=list(width=1, color="teal"), marker=list(size=2), name="PrevCLX")
              py<- add_trace(x=c(data$Time[1], tail(data$Time,1)), y=c(PrevClose()$tPrice, PrevClose()$tPrice), line=list(width=1, color="teal"), marker=list(size=2), name="PrevCLX")
             }
          ###Orders
            if (nrow(dd)>0) {
                dd<- subset(dd, Time>=t1 & Time<=t2)
                id<- unique(dd$orderid)
                for (i in 1:length(id)) {
                    tt<-dd[dd$orderid==id[i], ]
                    tt<- tt[order(tt$Time), ]
                    py<- py %>%  add_trace( x= tt$Time, y=tt$price, mode="markers+lines",name=id[i], text=paste0("Tif:",tt$timeinforce, " Shares:", tt$Shares, "<br>Exchange:", tt$exchange),
                                            marker=list(symbol=tt$Shape, size=tt$Size, color=tt$color),
                                            line=list(width=0.2, color=tt$color[1]), evaluate=TRUE)   
                }
            }
          ###News
            if ( input$news ) {
              if (nrow(Newsdata())>0)  {
                Newsdd<- subset(Newsdata(), Time>=t1 & Time<=t2)
                if (nrow(Newsdd)>0) {
                  mx<- max(data$tPrice)
                  mn<- min(data$tPrice[data$tPrice >0])
                  a<- list()
                  for (i in 1:nrow(Newsdd)) { 
                    tt<- NULL
                    tt<- Newsdd[i,]
                    a[[i]] <- list(
                      bordercolor="steelblue",
                      borderwidth=1,
                      bgcolor= "#F0F8FF",
                      arrowcolor="navy",
                      font= list(color="darkblue", family="Droid Sans", size=Font()),
                      align="left",
                      x = tt$Time,
                      y = mx,
                      text = gsub("[$]","",tt$head),
                      xref = "x",
                      yref = "y",
                      showarrow = TRUE,
                      arrowhead = 3,
                      ax = 20,
                      ay = -40)
                    py<- py %>% add_trace( x = c(tt$Time, tt$Time), y = c(mn, mx), hoverinfo="x", marker=list(size=2, color="blue"), line=list(width=0.8, color="blue"), evaluate=TRUE)
                  }
                  py<- py %>% layout(annotations=a)
                }
              }
            }
         }
      incProgress(1)
      setProgress(1)
    })
    py
  })
  
  trendPlot2 <- renderPlotly({
    
    xax <- list(
      title = "",
      tickfont = list(color = "darkblue")
    )
    
    yax <- list(
     # title = "",
      tickfont = list(color = "darkblue")
    )
    
    py<- plot_ly(BottomPlot(), x = Time, y = MidPrice, source="subset", mode = "markers", marker=list( size=3.3 , opacity=0.9), name="") %>%
      layout(showlegend = FALSE, hovermode = "closest", yaxis=list(type = "log"), legend = list(x = 1, y = 1),paper_bgcolor= 'rgba(249,249,263,.85)', dragmode = "select") %>%
      layout(xaxis=xax, yaxis=yax)
    
    if (is(try(pp(), silent=T), "try-error")==FALSE)  {
      for (i in 1: nrow(pp())) {
        py <- py %>% add_trace(x = c(pp()$Time[i], pp()$Time[i]), y = c(y0(), y1()), mode = "line",marker=list(size=1), line=list(dash="2", color="steelblue"), evaluate=TRUE)
      }
    }
    py
  })
  
  
  ImbalPlot <- renderPlotly({ 
    
    event <- event_data("plotly_selected", source = "subset")
    tt<- data.frame()
    
    if (nrow(plotdelay()) !=0) {
      tt1<- data.frame(Time=as.Date(character()), iMarket=numeric(), iPaired=as.numeric(),iShares=as.numeric())
      tt2<- data.frame(Time=as.Date(character()), iMarket=numeric(), iPaired=as.numeric(),iShares=as.numeric())
      tt3<- data.frame(Time=as.Date(character()), iMarket=numeric(), iPaired=as.numeric(),iShares=as.numeric())
      
      if (input$icbcNSDQ) {
        tt1<- ImbNSDQ()[,c("Time", "iMarket" ,"iPaired", "iShares")]
      } 
      if (input$icbcNYSE) {
        tt2<- ImbNYSE()[,c("Time", "iMarket", "iPaired", "iShares")]
      } 
      if (input$icbcARCA) {
        tt3<- ImbARCA()[,c("Time", "iMarket", "iPaired", "iShares")]
      }
      
      if (nrow(data.frame(event)) >0 ) {
        t1<- as.POSIXct(as.character(as.POSIXct(min(event$x)/1000, origin="1970-01-01", tz="EET")),  "%Y-%m-%d %H:%M:%S", tz ="GMT")-1
        t2<- as.POSIXct(as.character(as.POSIXct(max(event$x)/1000, origin="1970-01-01", tz="EET")),  "%Y-%m-%d %H:%M:%S", tz ="GMT")+1
        if (input$icbcNSDQ) {
          tt1<- subset(ImbNSDQ(), Time>=t1 & Time<=t2, select=c("Time", "iMarket","iPaired", "iShares"))
        } 
        if (input$icbcNYSE) {
          tt2<- subset(ImbNYSE(), Time>=t1 & Time<=t2, select=c("Time", "iMarket", "iPaired", "iShares"))
        } 
        if (input$icbcARCA) {
          tt3<- subset(ImbARCA(), Time>=t1 & Time<=t2, select=c("Time", "iMarket", "iPaired", "iShares"))
        }
      } 
      tt<- rbind(tt1,tt2,tt3)
    } 
    
    ay2 <- list(
      zeroline = FALSE,
      tickfont = list(color = "green"),
      overlaying = "y",
      side = "right"
    )
    
    ay1<- list (
      zeroline = FALSE,
      tickfont = list(color = "darkblue"),
      title=""
    )
    xax <- list(
      title="",
      zeroline = FALSE,
      tickfont = list(color = "darkblue")
    )
    
    if (input$icbcARCA==FALSE) {
        py <- plot_ly(tt, x= Time, y=iPaired, mode="markers", marker=list( size=5 , opacity=0.9, color="steelblue"), name="iPaired") %>%
          # add_trace(x=Time, y=iMarket, mode="markers", marker=list( size=5 , opacity=0.9, color="violet"), name="iMarket") %>%
          add_trace(x=Time, y=iShares, mode="markers",yaxis = "y2", marker=list( size=5 , opacity=0.9, color="green"), name="iShares") %>%
          layout(xaxis=xax, showlegend = FALSE,yaxis=ay1, yaxis2 = ay2) %>% layout(  margin = list(autosize=FALSE,r=50), hovermode = "closest", paper_bgcolor= 'rgba(249,249,263,.85)') 
    } else {
      py <- plot_ly(tt, x= Time, y=iPaired, mode="markers", marker=list( size=5 , opacity=0.9, color="steelblue"), name="iPaired") %>%
        add_trace(x=Time, y=iMarket, mode="markers", marker=list( size=5 , opacity=0.9, color="violet"), name="iMarket") %>%
        add_trace(x=Time, y=iShares, mode="markers",yaxis = "y2", marker=list( size=5 , opacity=0.9, color="green"), name="iShares") %>%
        layout(xaxis=xax, showlegend = FALSE,yaxis=ay1, yaxis2 = ay2) %>% layout(  margin = list(autosize=FALSE,r=50), hovermode = "closest", paper_bgcolor= 'rgba(249,249,263,.85)')
    }
  py   
  })
  
  DataOut<-  reactive({
    event <- event_data("plotly_selected", source = "subset")
    if (nrow(data.frame(event))>1) {
      t1<- fastPOSIXct(as.character(as.POSIXct(min(event$x)/1000, origin="1970-01-01", tz="EET")),  required.components = 6L, tz ="GMT")
      t2<- fastPOSIXct(as.character(as.POSIXct(max(event$x)/1000, origin="1970-01-01", tz="EET")),  required.components = 6L, tz ="GMT")
      Out<- data()[(data()$Time>=t1 & data()$Time<=t2), c("Time", "Reason", "Bid_P", "Ask_P", "tShares", "tSide", "tType", "tPrice", "iCBC", "iMarket", "iShares", "iPaired", "iExchange")]
    } else {
      Out= data()[, c("Time", "Reason", "Bid_P", "Ask_P","tShares", "tSide", "tType", "tPrice", "iCBC", "iMarket", "iShares", "iPaired", "iExchange")]
    }
    Out$Reason<- factor(Out$Reason)
    Out$tSide<- factor(Out$tSide)
    Out$tType<- factor(Out$tType)
    Out$Time=format(Out$Time, format="%H:%M:%OS")
    return (Out) })
  suppressWarnings(library(DT))
  output$mytable <- renderDataTable (datatable(DataOut(), extensions = 'ColVis', options = list(pageLength = 15, searchHighlight = TRUE,dom = 'C<"clear">lfrtip', colVis = list(exclude = c(0, 1), activate = 'mouseover')), filter = list(position = 'top', clear = FALSE)))
  #output$mytable <- renderDataTable ({plotdelay2()})
  output$downloadData <- downloadHandler(
    filename = function() {paste0(dateText(),"_Report",as.character(input$Strategy),".xlsx", sep="") },
    content = function(file) {
      write.table(DataOut() , file, quote=F, row.names = F)
    }
  )
  
  output$plotui <- renderUI({
    output$plot<- trendPlot
    plotlyOutput("plot", width="100%", height = "100%") 
  })
  
  output$plotui2 <- renderUI({
    output$plot2<- trendPlot2
    plotlyOutput("plot2", width="100%", height = 180) 
  })
  
  output$plotui3 <- renderUI({
    output$plot3<- ImbalPlot
    plotlyOutput("plot3", width="100%", height = 180) 
  })
  
  inputChoices <- reactive({
    choices<- unique(Order()$strategy)
    #if (length(choices)>1) {choices= c("None", choices, "All")} else  {choices= c("None", choices)}
    choices= c("None", choices)
    return(choices)
  })
  
  output$strat <- renderUI({selectInput('strat', 'Orders:',  choices=inputChoices(), selected = input$strat, width="100") })
  

  output$brush <- renderPrint({
    d <- event_data("plotly_selected", source="subset")
    k1= as.POSIXct(suppressWarnings(max(d$x)/1000), origin="1970-01-01", tz="GMT")- as.POSIXct(suppressWarnings(min(d$x)/1000), origin="1970-01-01", tz="GMT")
    return(k1) 
  })
 }
)


