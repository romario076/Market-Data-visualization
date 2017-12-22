 

suppressWarnings( library(RMySQL))
suppressWarnings( library(fasttime))
suppressWarnings( library(data.table))
suppressWarnings( library(knitr))
suppressWarnings( library(dplyr))
suppressWarnings( library(xts))
suppressWarnings( library(scales))
suppressWarnings( library(DT))
suppressWarnings(library(shinyBS))


Sys.setenv(TZ='GMT')


MarketData<- function(date, from, to, symbol, Level1, host) {
  
  data<- read.csv("./data/Md.csv")
  data$Time= as.character(data$Time)
  data<- data[(data$Time>=paste0(date," ", from) & (data$Time<=paste0(date, " ", to))), ]

  if (nrow(data) !=0) {
    op<-options(digits.secs=6)
    data$Time<- fastPOSIXct(data$Time, required.components = 6L, tz = "GMT")
  } else {data<- data.frame()}
  
  #write.csv(data, file = "Md.csv",row.names=FALSE)
  return(data)
  }



MarketDataFutures<- function(date, from, to, symbol, Level1) {
  
  data<- read.csv("Md.csv")
  if (nrow(data) !=0) {
    op<-options(digits.secs=6)
    data$Time<- fastPOSIXct(data$Time, required.components = 6L, tz = "GMT")
  } else {data<- data.frame()}
  
  return(data)
  }



Printu<- function(date, from, to, symbol, host) {

  data<- read.csv("./data/Printu.csv")
  op<-options(digits.secs=6)
  data$Time<- fastPOSIXct(data$Time, required.components = 6L, tz = "GMT")
  #write.csv(data, file = "Printu.csv",row.names=FALSE)
  return(data)
}




PrevCLX2<- function(date, symbol) {
  
  data<- read.csv("./data/PrevCLX.csv")
  #write.csv(data, file = "PrevCLX.csv",row.names=FALSE)
  return(data)
}


PrevCLXFutures<- function(date, symbol) {
  
  data<- read.csv("./data/PrevCLX.csv")
  #write.csv(data, file = "PrevCLX.csv",row.names=FALSE)
  
  return(yClose)
}



Orders<- function(date, from, to, symbol) {
  
  dd<- read.csv("./data/Orders.csv")
  op<-options(digits.secs=6)
  dd$Time<- fastPOSIXct(dd$Time, required.components = 6L, tz = "GMT")
  dd$strategy<- as.character(dd$strategy)
  return (dd)
}


OrderTable<- function(date, from, to, symbol, orderid) {
  
  return(data.frame())
}


News<- function(date, from , to, symbol) {
  
  news<- read.csv("./data/News.csv")
  news$Time= as.character(news$Time)
  news<- news[(news$Time>=paste0(date," ", from) & (news$Time<=paste0(date, " ", to))), ]
  #write.csv(news, file = "News.csv",row.names=FALSE)
  
  return(news)
}



Nav<- function(date, from, to, symbol, scale, host) {
  
  return(data.frame())
}  

checkErrors<-  function(data) {
  out <- tryCatch(
    {
      #mydb = dbConnect(MySQL(), user='roma', password='2bn4aYjV8bz5OY', dbname='reports', host='192.168.31.21')
      #dbDisconnect(mydb)
      print("Connection")
    },
    error=function(cond) {
      return(FALSE)
    },
    warning=function(cond) {
      return(NULL)
    },
    finally={
    }
  )  
  if (out==FALSE) {
    showModal(modalDialog(
      title = "Warning message",
      "Failed to connect to database: Error: Can't connect to MySQL server ",
      size= "m",
      easyClose = TRUE
    ))
    #stop()
    return(TRUE)
  } else {
    if (is(try(data, silent=T), "try-error"))  { 
      mess1<- ' Message: Empty data. Choose another day :-) '
      showModal(modalDialog(
        title = "Warning message",
        " Message: Empty data. Choose another day :-) ",
        size= "m",
        easyClose = TRUE
      ))
      #stop()
      return(TRUE)
    } else {
      if (nrow(data)<1) {
        showModal(modalDialog(
          title = "Warning message",
          " Message: Empty data. Choose another day :-) ",
          size= "m",
          easyClose = TRUE
        ))
        #stop()
        return(TRUE)
      } 
    }
  } 
  return(FALSE)
}

###Shiny server
shinyServer(function(input, output, session) {
  
  dateText<- renderText({
    input$go
    isolate({as.character(input$date)})
  })
  
  ### Define selected host
  host<- reactive({
    if (input$host==1) {
      host<- "192"
    } else {
      host<- "10" 
    }
    return(host)
  })
  
  observeEvent(input$help, {
    toggle("text_help")
  })
  
  
  data<- eventReactive(input$go, {
    
    rm(list=ls(all=TRUE))
    
    withProgress( message = 'Data downloading', value = 0.2, {
      if (input$futures) {
        dd<-MarketDataFutures(date=dateText(), from=input$from, to=input$to, symbol=input$text, Level1=input$level1) 
      } else {
        dd<- MarketData(date=dateText(), from=input$from, to=input$to, symbol=input$text, Level1=input$level1, host=host()) 
      }
      incProgress(1)
      setProgress(1)
    })
    return(dd)
  })
  

  
  data1<- reactive({ 
    input$go
    isolate({
      if (nrow(data())>1) {
        if (input$futures) {
          if (input$level1) {
            data1<- data()[(data()$Reason=="Trade")|(data()$Reason=="Level1"),
                           c("Time", "Time1", "Time2", "Time3", "Time4", "MsgSource", "Reason", "Bid_P", "Ask_P", "tShares","tShares1", "tSide",
                             "MidPrice", "tPrice", "tType", "color")  ]
          } else {
            data1<- data()[(data()$Reason=="Trade"), 
                           c("Time", "Time1", "Time2", "Time3", "Time4", "MsgSource", "Reason", "Bid_P", "Ask_P", "tShares","tShares1",
                             "tSide", "MidPrice", "tPrice", "tType", "color")  ]
          }
        } else {
          if (input$level1) {
            data1<- data()[(data()$Reason=="Trade")|(data()$Reason=="Level1"),
                           c("Time", "Time1", "Time2", "Time3", "Time4", "MsgSource", "Reason", "Bid_P", "Ask_P", "tShares","tShares1", "tSide","tType" ,"MidPrice",
                             "tPrice", "iCBC", "color") ]
          } else {
            data1<- data()[(data()$Reason=="Trade"), 
                           c("Time", "Time1", "Time2", "Time3", "Time4", "MsgSource", "Reason", "Bid_P", "Ask_P", "tShares","tShares1", "tSide", "tType","MidPrice", 
                             "tPrice", "iCBC", "color") ]
          }
        }
        
      } else {
        data1<- data.frame()
      }
      message(paste0("Data1 shape: ", nrow(data1)))
      isErrors<- checkErrors(data=data1)
      message(paste0("Errors: ", isErrors))
      
      return(data1) 
    })
  })
  
  
  ###Disable or enable inputs depending to DB
  observeEvent(input$futures,
               if (input$futures) {
                 disable("strat")
                 disable("news")
                 disable("icbc")
                 disable("nav")
                 disable("colorEx")
                 disable("host")
               } else {
                 enable("strat")
                 enable("news")
                 enable("icbc")
                 enable("nav")
                 enable("colorEx")
                 enable("host")
               })
  
  
  delta<-  reactive({ as.numeric(max(data1()$Time) - min(data1()$Time), units="secs") })
  
  f<- reactive({ f<-as.xts(data1(), order.by=data1()[, 1], frequency=NULL)
  return (f)})
  
  Seconds<-  reactive({
    if (input$level1) {
      temp<- f()[f()$Reason=="Level1", ]
      ep1 <- endpoints(temp,'seconds')
      data1<- as.data.frame(period.apply(temp, INDEX=ep1, FUN=function(x) tail(x, 1)))
      row.names(data1)<- NULL
      
      temp2<- f()[f()$Reason!="Level1"]
      ep2 <- endpoints(temp2,'seconds')
      data2<- as.data.frame(period.apply(temp2, INDEX=ep2, FUN=function(x) tail(x, 1)))
      row.names(data2)<- NULL
      data<- rbind(data1, data2)
    } else {
      ep <- endpoints(f(),'seconds')
      data<- as.data.frame(period.apply(f(), INDEX=ep, FUN=function(x) tail(x, 1)))
      row.names(data)<- NULL 
    }
    data$Time<- fastPOSIXct(data$Time, required.components = 6L, tz = "GMT")
    data$tPrice<- as.numeric(as.character(data$tPrice))
    data$Ask_P<- as.numeric(as.character(data$Ask_P))
    data$Bid_P<- as.numeric(as.character(data$Bid_P))
    data$tShares1<- as.character(data$tShares1)
    data$tType<- as.character(data$tType)
    data$color<- as.character(data$color)
    data<- data[order(data$Time),]
    return(data)})
  
  Seconds10<- reactive({
    if (delta()> 1800) {
      if (input$level1) {
        temp<- f()[f()$Reason=="Level1", ]
        ep1 <- endpoints(temp,'seconds',  k=10)
        data1<- as.data.frame(period.apply(temp, INDEX=ep1, FUN=function(x) tail(x, 1)))
        row.names(data1)<- NULL
        
        temp2<- f()[f()$Reason!="Level1"]
        ep2 <- endpoints(temp2,'seconds')
        data2<- as.data.frame(period.apply(temp2, INDEX=ep2, FUN=function(x) tail(x, 1)))
        row.names(data2)<- NULL
        data<- rbind(data1, data2)
      } else {
        ep <- endpoints(f(),'seconds',  k=10)
        data<- as.data.frame(period.apply(f(), INDEX=ep, FUN=function(x) tail(x, 1)))
        row.names(data)<- NULL 
      }
      data$Time<- fastPOSIXct(data$Time, required.components = 6L, tz = "GMT")
      data$tPrice<- as.numeric(as.character(data$tPrice))
      data$Ask_P<- as.numeric(as.character(data$Ask_P))
      data$Bid_P<- as.numeric(as.character(data$Bid_P))
      data$tShares1<- as.character(data$tShares1)
      data$tType<- as.character(data$tType)
      data$color<- as.character(data$color)
      data<- data[order(data$Time),]
    } else {data<- c()}
    return(data)})
  
  Minutes<- reactive({
    if (delta()> 1800) {
      if (input$level1) {
        temp<- f()[f()$Reason=="Level1", ]
        ep1 <- endpoints(temp, 'minutes')
        data1<- as.data.frame(period.apply(temp, INDEX=ep1, FUN=function(x) tail(x, 1)))
        row.names(data1)<- NULL
        
        temp2<- f()[f()$Reason!="Level1"]
        ep2 <- endpoints(temp2, 'minutes')
        data2<- as.data.frame(period.apply(temp2, INDEX=ep2, FUN=function(x) tail(x, 1)))
        row.names(data2)<- NULL
        data<- rbind(data1, data2)
      } else {
        ep <- endpoints(f(),'seconds',  k=10)
        data<- as.data.frame(period.apply(f(), INDEX=ep, FUN=function(x) tail(x, 1)))
        row.names(data)<- NULL 
      }
      data$Time<- fastPOSIXct(data$Time, required.components = 6L, tz = "GMT")
      data$tPrice<- as.numeric(as.character(data$tPrice))
      data$Ask_P<- as.numeric(as.character(data$Ask_P))
      data$Bid_P<- as.numeric(as.character(data$Bid_P))
      data$tShares1<- as.character(data$tShares1)
      data$tType<- as.character(data$tType)
      data$color<- as.character(data$color)
      data<- data[order(data$Time),]
    } else {data<- c()}
    return(data)})
  
  
  
  plotdelay<- reactive({
    if (nrow(data1())>0) {
      tDiff<-  delta()
      if (tDiff<= 1800) {data<-data1()} else {
        if (tDiff>1800 & tDiff<3*3600) {data<-Seconds()}
        if (tDiff>3*3600 & tDiff<6*(3600)) {data<-Seconds10()}
        if (tDiff>6*3600) {data<-Minutes()}
      }
      data$tShares1<- as.numeric(data$tShares1)
      data$tShares1[(data$tSide=="ASK") & (data$tType != "OPG") & (data$tType != "CLX")]<- as.integer(rescale(as.numeric(sqrt(data$tShares1[(data$tSide=="ASK") & (data$tType != "OPG") & (data$tType != "CLX")])), c(1,20)))
      data$tShares1[(data$tSide=="BID") & (data$tType != "OPG") & (data$tType != "CLX")]<- as.integer(rescale(as.numeric(sqrt(data$tShares1[(data$tSide=="BID") & (data$tType != "OPG") & (data$tType != "CLX")])), c(1,20)))
      data$tShares1[(data$tSide=="BOTH") & (data$tType != "OPG") & (data$tType != "CLX")]<- as.integer(rescale(as.numeric(sqrt(data$tShares1[(data$tSide=="BOTH") & (data$tType != "OPG") & (data$tType != "CLX")])), c(1,20))) 
      if (!input$futures) {
        if (sum(data$tShares1[(data$tType=="OPG") | (data$tType=="CLX")])>0) {
          data$tShares1[(data$tType=="OPG") | (data$tType=="CLX")]<- as.integer(rescale(as.numeric(data$tShares1[(data$tType=="OPG") | (data$tType=="CLX")]), c(7,24)))
          data$color[(data$tType=="OPG") | (data$tType=="CLX")]<- "#ffb84d" 
        }
      }
    } else {
      data<- data.frame()
    }
    
    return(data)
  })
  
  Imbalance<- reactive({
    if (length(input$icbc)>0) {
      imb<- data()[,c("Time", "iCBC","iMarket", "iShares", "iPaired", "iExchange")]
      imb[imb$Reason !="Imbalance", c("iShares", "iPaired", "iCBC", "iMarket") ]<- NA
      tDiff<- delta()
      if (tDiff> 3600*2) {
        f= as.xts(imb, order.by=imb[, 1], frequency=NULL)
        ep <- endpoints(f,'seconds', k=30)
        imb1<- as.data.frame(period.apply(f, INDEX=ep, FUN=function(x) tail(x,1)))
        imb1$Time<- fastPOSIXct(imb1$Time, required.components = 6L, tz = "GMT")
        imb1$iCBC<- as.numeric(as.character(imb1$iCBC))
      } else {
        imb1<- imb
      }
      imb1$Time<- fastPOSIXct(imb1$Time, required.components = 6L, tz = "GMT")
      m<- data.frame(Time=max(data()$Time), iCBC= NA, iMarket= NA, iShares= NA, iPaired= NA, iExchange= "")
      n<- data.frame(Time=min(data()$Time), iCBC= NA, iMarket= NA, iShares= NA, iPaired= NA, iExchange= "")
      imb1<- rbind(m, imb1, n)
      row.names(imb1)<- NULL
    } else {
      imb1<- c()
    }
    return(imb1)
  })
  
  
  ImbExchange<- reactive({
    imb= Imbalance()
    if ("Q" %in% input$icbc) {
      imb[imb$iExchange !="NSDQ", c("iShares", "iPaired", "iCBC") ]<- NA
    } 
    if ("Y" %in% input$icbc) {
      imb[imb$iExchange !="NYSE", c("iShares", "iPaired", "iCBC") ]<- NA
    } 
    if ("A" %in% input$icbc) {
      imb[imb$iExchange !="AXDP", c("iShares", "iPaired", "iCBC", "iMarket") ]<- NA
    }
    return(imb)
  })
  
  
  BottomPlot<- reactive({
    if (nrow(data1())>0) {
      tDiff<- delta()
      if (tDiff<2*3600) {data<-Seconds()} else {
        if (tDiff>=2*3600 & tDiff<5*3600) {data<-Seconds10()}
        if (tDiff>=5*3600) {data<-Minutes()} 
      }
      data<- data[ ,c("Time", "MidPrice")]
    } else {
      data<- data.frame()
    }
    return(data)
  })
  

  plotDelayGreater<- reactive({
    dd<- plotdelay()
    if (nrow(dd)>0) {
      dd<- dd[dd$tPrice>0, ]
      if (input$OverLap) {dd$tPrice1<- jitter(dd$tPrice)} 
    }
    return(dd)
  })
  
  y0<- reactive({
    if (nrow(plotDelayGreater())>0) {
      min(plotDelayGreater()$tPrice)
    } else {min(plotdelay()$Bid_P)
    } 
  })
  
  y1<- reactive({
    if (nrow(plotDelayGreater())>0) {
      max(plotDelayGreater()$tPrice)
    } else {max(plotdelay()$Ask_P)
    } 
  })
  
  pp<- reactive({
    Printu(date =dateText(), from=input$from, to=input$to, symbol= input$text, host= host() ) 
  })
  
  #Order<- reactive({
  #  input$go
  #  isolate({
  #    dataOrders<- Orders(date=as.character(dateText()), from=input$from, to=input$to, symbol= input$text) 
  #    return(dataOrders)
  #  })
  #})
  
  Order<- eventReactive(input$go, {
    isolate({
        dataOrders<- Orders(date=as.character(dateText()), from=input$from, to=input$to, symbol= input$text) 
        return(dataOrders)
      })
  })
  
  Newsdata<- reactive({
    if (input$news & nrow(data())>0)  {News(date=as.character(dateText()), from=input$from, to=input$to, symbol=input$text)} else {c()} 
  })
  
  NavData1<- reactive({
    input$go
    isolate({
      if (length(input$nav)>0) {
        Nav(date=as.character(dateText()), from=input$from, to=input$to, symbol=input$text, host= host(), scale= 12)} else {c()}
    })
  })
  NavData2<- reactive({
    input$go
    isolate({
      if (length(input$nav)>0) {
        Nav(date=as.character(dateText()), from=input$from, to=input$to, symbol=input$text, host= host(), scale= 9)} else {c()}
    })
  })
  NavData3<- reactive({
    input$go
    isolate({
      if (length(input$nav)>0) {
        Nav(date=as.character(dateText()), from=input$from, to=input$to, symbol=input$text,  host= host(), scale= 8)} else {c()}
    })
  })
  
  Navdata<- reactive({
    input$go
    isolate({
      if (length(input$nav)>0) {
        tDiff<- delta()
        if (tDiff<= 100) {data<- NavData1()} else {
          if (tDiff>100 & tDiff<2*3600) {data<- NavData2()}
          if (tDiff>2*3600) {data<- NavData3()}
        }
      } else {data<- c()}
      
      return(data)
    })
  })
  
  PrevClose<- reactive({
    if (input$prevclx) {
      if (input$futures) {
        dat<- PrevCLXFutures(date=as.character(dateText()), symbol=input$text)
        data<- data.frame(Time=c(plotdelay()$Time[1], tail(plotdelay()$Time,1)), tPrice=c(dat$tPrice, dat$tPrice))
      } else {
        dat<- PrevCLX2(date=as.character(dateText()), symbol=input$text)
        data<- data.frame(Time=c(plotdelay()$Time[1], tail(plotdelay()$Time,1)), tPrice=c(dat$tPrice, dat$tPrice))
      }
    } else {data<-c()}
    return(data)
  })
  
  ###Strategies Orders input  
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
  
  Size<- reactive({as.integer(6)})
  alpha<- reactive({0.7})
  alpha1<- reactive({0.8})
  Hline<-reactive({2}) 
  Font<- reactive({11})
  
  fillcolor = reactive({"#ff6666"})
  hollowcolor = reactive({"#39ac73"})
  plotcolor = reactive({"#3E3E3E"})
  papercolor = reactive({"#1E2022"})
  fontcolor = reactive({"#B3A78C"})
  
  ###Top plot
  trendPlot <- renderPlotly({
    
    event<- event_data("plotly_selected", source = "subset")
    event<- event[event$y>0, ]
    
    input$go
    isolate({
      if (nrow(plotdelay())>0) {
        
        p2<- as.numeric(y1())
        p1<- as.numeric(y0())
        tDiff<- delta()
        fontcolor<- "darkblue"
        
        xax <- list(
          title = "",
          tickfont = list(color = "darkblue")
        )
        yax <- list(
          title = "",
          tickfont = list(color = "darkblue")
        )
        
        navOpacity=0.8
        navSize=5
        
        if (input$radio==1) {
          l<- list( color = toRGB("grey90", alpha = 0.1),
                    fillcolor = toRGB("grey90", alpha = 0.1),
                    shape = "hv",
                    width = .00001)
          
        } else {
          l<- list( color = toRGB("grey40", alpha = 0.1),
                    fillcolor = toRGB("grey40", alpha = 0.1),
                    shape = "hv",
                    width = .00001)
          
        }
        
        if (input$colorEx) {
          colorDesc<-"c('#FF4040','#ff9900','#66b3ff','#ff00ff','#00e6e6','#9933ff','#4dff4d','#ff99dd')[factor(MsgSource)]"
        } else {
          colorDesc<- "color"
        }
        
        dd<- dd()
        if (input$OverLap) {
          y="tPrice1"
          t= "paste0(tPrice, ' Shares:', tShares)"
          hoverinfo= "x+text"
        } else{
          y="tPrice"
          t= "paste0('Shares:', tShares, '<br>Source:',MsgSource)"
          hoverinfo= "x+y+text"
        }
        
        withProgress( message = 'Top Chart', value = 0.4, {
          
          if (nrow(data.frame(event)) <1 & input$spread==FALSE) {
            py<- plot_ly(plotDelayGreater(), x = Time, y = eval(parse(text=y)), mode = "markers", text = eval(parse(text= t)), hoverinfo=eval(hoverinfo), 
                         marker=list(size=tShares1, color=eval(parse(text=colorDesc)), opacity= alpha(), line = list( width = .001) ))  %>%
              layout(showlegend = FALSE, hovermode = "closest", paper_bgcolor= 'rgba(249,249,263,.85)')
            py<- layout(xaxis=xax, yaxis=yax)
            
            ###Imbalances 
            if (!is.null(input$icbc)) {
              py<- add_trace(ImbExchange(), x=Time, y=iCBC, name="iCBC", mode="markers", marker=list(symbol = 22, color= "BF3EFF", size=Size(), opacity= alpha1())) 
              py<- layout(yaxis=list(range=c( y0(), y1())))
            }
            ###Prev Close
            if (input$prevclx) {py<- add_trace(PrevClose(), x=Time, y=tPrice, line=list(width=1, color="00CD66"), marker=list(size=2), name="PrevCLX", hoverinfo = "none")}
            
            ###Prints
            if (is(try(pp(), silent=T), "try-error")==FALSE)  {
              for (i in 1: nrow(pp())) {
                py <- py %>% add_trace(x = c(pp()$Time[i],pp()$Time[i]), y = c(y0(), y1()), mode = "line",marker=list(size=1), line=list(dash="2", color=pp()$color[i]), hoverinfo = "none", evaluate=TRUE) %>% 
                  add_trace(x = c(pp()$Time[i]-Hline(), pp()$Time[i]+Hline()), y = c(pp()$tPrice[i], pp()$tPrice[i]), marker=list(size=1), mode = "line", line=list(dash="1", color="violet"), hoverinfo = "none", evaluate=TRUE)
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
                                        line=list(width=0.3, color=tt$color[1]), evaluate=TRUE)   
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
                    arrowcolor="4F94CD",
                    font= list(color="darkblue", family="Droid Sans", size=Font()),
                    align="left",
                    opacity=0.8,
                    x =tt$Time,
                    y = y1()-0.0015*y1(),
                    text = gsub("[$]","",tt$head),
                    xref = "x",
                    yref = "y",
                    showarrow = TRUE,
                    arrowhead = 3,
                    ax = 20,
                    ay = -40)
                  py<- py %>% add_trace( x = c(tt$Time, tt$Time), y = c(y0(), y1()-0.0015*y1()), hoverinfo="x", marker=list(size=2, color="7093DB"), line=list(width=0.8, color="7093DB"), evaluate=TRUE)
                }
                py<- py %>% layout(annotations=a)
              }
            }
            
            ###Nav
            if (length(input$nav)>0 ){
              if ("B" %in% input$nav) {
                py<-  add_trace(x= Navdata()$Time, y= Navdata()$B_NAV, mode="markers", marker=list(size=navSize, color="navy", opacity=navOpacity, symbol=217), name="B_Nav") 
              }
              if ("A" %in% input$nav) {
                py<-  add_trace(x= Navdata()$Time, y= Navdata()$A_NAV, mode="markers", marker=list(size=navSize, color="#99ccff", opacity=navOpacity, symbol=17), name="A_Nav")
              }
              if ("M" %in% input$nav) {
                py<-  add_trace(x= Navdata()$Time, y= Navdata()$M_NAV, mode="markers", marker=list(size=navSize, color="#3366ff", opacity=navOpacity, symbol=24), name="M_Nav") 
              }
              py<- layout(yaxis=list(range=c(p1-(p2-p1)*0.05, p2+(p2-p1)*0.05)))
            }
            ###Level1
            if (input$level1) {
              py<- add_trace(plotdelay(), x=Time, y=Bid_P, name = "Bid", line = l, hoverinfo = "none")
              py<- add_trace(plotdelay(), x=Time, y=Ask_P, name = "Ask", line = l, fill="tonexty", hoverinfo = "none") 
            }
            
            
            if (input$volumeChart & nrow(plotDelayGreater())>0) {
              #### Volume Chart
              if (tDiff<=30) {x="Time1"}
              if ((tDiff>30) & (tDiff<5*60)) {x="Time2"}
              if ((tDiff>=5*60) & (tDiff<30*60)) {x="Time3"}
              if (tDiff>=30*60) {x="Time4"}
              
              VolumeAggregate<- plotDelayGreater()[ ,c(x, "tShares" )]
              VolumeAggregate$tShares<- as.numeric(as.character(VolumeAggregate$tShares))
              VolumeAggregate<- eval(parse(text=paste0("aggregate(.~",x,", data=VolumeAggregate, FUN=sum)")))
              VolumeAggregate[ ,x]<- fastPOSIXct(VolumeAggregate[ ,x], required.components = 6L, tz = "GMT")
              
              py <- add_trace(data= VolumeAggregate, x =eval(parse(text= x)), y = tShares, type = "bar", marker = list(color = "steelblue"), yaxis="y2", hoverinfo="none") %>% layout(paper_bgcolor= 'rgba(249,249,263,.85)')
              
              py<- layout(
                yaxis = list(
                  tickfont = list(color = fontcolor), 
                  titlefont = list(color = fontcolor),
                  domain = c(0.30, 0.95)),
                
                yaxis2 = list(
                  zerolinecolor='#d9d9d9',
                  tickfont = list(color = fontcolor), 
                  titlefont = list(color = fontcolor),
                  side = "left", 
                  domain = c(0, 0.2))
              )
            }
            
          }
          
          if (nrow(data.frame(event)) <1 & input$spread==TRUE) {
            if (nrow(plotDelayGreater())>0) {
              py<- plot_ly(plotDelayGreater(), x = Time, y = eval(parse(text=y)), mode = "markers", text = eval(parse(text= t)), hoverinfo=eval(hoverinfo),
                           marker=list(size=tShares1, color=eval(parse(text=colorDesc)), opacity= alpha(), line = list( width = .001) )) 
            } else {
              py<- plot_ly(plotdelay(), x = Time, y = Bid_P, mode = "markers", text = eval(parse(text= t)), hoverinfo = "none",
                           marker=list(size=1, color=eval(parse(text=colorDesc)), opacity= alpha(), line = list( width = .001) )) 
            }
            
            py<- add_trace(plotdelay(), x=Time, y=Bid_P, name = "Bid", line = l, hoverinfo = "none")
            py<- add_trace(plotdelay(), x=Time, y=Ask_P, name = "Ask", line = l, fill="tonexty", hoverinfo = "none") 
            py<- layout(showlegend = FALSE, hovermode = "closest", paper_bgcolor= 'rgba(249,249,263,.85)')
            py<- layout(xaxis=xax, yaxis=yax)
            
            
            ###Imbalances 
            if (!is.null(input$icbc)) {
              py<- add_trace(ImbExchange(), x=Time, y=iCBC, name="iCBC", mode="markers", marker=list(symbol = 22, color= "BF3EFF", size=Size(), opacity= alpha1())) 
              py<- layout(yaxis=list(range=c( y0(), y1())))
            }
            ###Prev Close
            if (input$prevclx) {py<- add_trace(PrevClose(), x=Time, y=tPrice, line=list(width=1, color="00CD66"), marker=list(size=2), name="PrevCLX", hoverinfo = "none")}
            
            ###Prints
            if (is(try(pp(), silent=T), "try-error")==FALSE)  {
              for (i in 1: nrow(pp())) {
                py <- py %>% add_trace(x = c(pp()$Time[i],pp()$Time[i]), y = c(y0(), y1()), mode = "line",marker=list(size=1), line=list(dash="2", color=pp()$color[i]), hoverinfo = "none", evaluate=TRUE) %>% 
                  add_trace(x = c(pp()$Time[i]-Hline(),pp()$Time[i]+Hline()), y = c(pp()$tPrice[i], pp()$tPrice[i]), marker=list(size=1), mode = "line", line=list(dash="1", color="violet"), hoverinfo = "none", evaluate=TRUE)
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
                                        line=list(width=0.3, color=tt$color[1]), evaluate=TRUE)   
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
                    arrowcolor="4F94CD",
                    font= list(color="darkblue", family="Droid Sans", size=Font()),
                    align="left",
                    opacity= 0.8,
                    x =tt$Time,
                    y = y1()-0.0015*y1(),
                    text = gsub("[$]","",tt$head),
                    xref = "x",
                    yref = "y",
                    showarrow = TRUE,
                    arrowhead = 3,
                    ax = 20,
                    ay = -40)
                  py<- py %>% add_trace( x = c(tt$Time, tt$Time), y = c(y0(), y1()-0.0015*y1()), hoverinfo="x", marker=list(size=2, color="7093DB"), line=list(width=0.8, color="7093DB"), evaluate=TRUE)
                }
                py<- py %>% layout(annotations=a)
              }
            }
            
            ###Nav
            if (length(input$nav)>0 ){
              if ("B" %in% input$nav) {
                py<-  add_trace(x= Navdata()$Time, y= Navdata()$B_NAV, mode="markers", marker=list(size=navSize, color="navy", opacity=navOpacity, symbol=217), name="B_Nav") 
              }
              if ("A" %in% input$nav) {
                py<-  add_trace(x= Navdata()$Time, y= Navdata()$A_NAV, mode="markers", marker=list(size=navSize, color="#99ccff", opacity=navOpacity, symbol=17), name="A_Nav")
              }
              if ("M" %in% input$nav) {
                py<-  add_trace(x= Navdata()$Time, y= Navdata()$M_NAV, mode="markers", marker=list(size=navSize, color="#3366ff", opacity=navOpacity, symbol=24), name="M_Nav") 
              }
              py<- layout(yaxis=list(range=c(p1-(p2-p1)*0.05, p2+(p2-p1)*0.05)))
            }
            
            if (input$volumeChart & nrow(plotDelayGreater())>0) {
              
              #### Volume Chart
              if (tDiff<=30) {x="Time1"}
              if ((tDiff>30) & (tDiff<5*60)) {x="Time2"}
              if ((tDiff>=5*60) & (tDiff<30*60)) {x="Time3"}
              if (tDiff>=30*60) {x="Time4"}
              
              VolumeAggregate<- plotDelayGreater()[ ,c(x, "tShares" )]
              VolumeAggregate$tShares<- as.numeric(as.character(VolumeAggregate$tShares))
              VolumeAggregate<- eval(parse(text=paste0("aggregate(.~",x,", data=VolumeAggregate, FUN=sum)")))
              VolumeAggregate[ ,x]<- fastPOSIXct(VolumeAggregate[ ,x], required.components = 6L, tz = "GMT")
              
              py <- add_trace(data= VolumeAggregate, x =eval(parse(text= x)), y = tShares, type = "bar", marker = list(color = "steelblue"), yaxis="y2", hoverinfo="none") %>% 
                layout(paper_bgcolor= 'rgba(249,249,263,.85)')
              
              py<- layout(
                yaxis = list(
                  tickfont = list(color = fontcolor), 
                  titlefont = list(color = fontcolor),
                  domain = c(0.30, 0.95)),
                
                yaxis2 = list(
                  zerolinecolor='#d9d9d9',
                  tickfont = list(color = fontcolor), 
                  titlefont = list(color = fontcolor),
                  side = "left", 
                  domain = c(0, 0.2))
              )
            }
            
          }
          
          if (nrow(data.frame(event)) >=1 ) {
            t1<- as.POSIXct(as.character(as.POSIXct(min(event$x)/1000, origin="1970-01-01", tz="EET")),  "%Y-%m-%d %H:%M:%S", tz ="GMT")-1
            t2<- as.POSIXct(as.character(as.POSIXct(max(event$x)/1000, origin="1970-01-01", tz="EET")),  "%Y-%m-%d %H:%M:%S", tz ="GMT")+1
            tDiff= as.numeric(t2-t1, units="secs")
            if (tDiff<= 1800) {data<- data1()} else {
              if (tDiff> 1800 & tDiff< 3600*3) {
                data<-Seconds()}
              if (tDiff> 3600*3 & tDiff< 3600*6) {
                data<-Seconds10()}
              if (tDiff> 3600*6) {
                data<-Minutes()}
            }
            
            data<- data[(data$Time>=t1 & data$Time<=t2) ,]
            
            data$tShares1<- as.numeric(data$tShares1)
            data$tShares1[(data$tSide=="ASK") & (data$tType != "OPG") & (data$tType != "CLX")]<- as.integer(rescale(as.numeric(sqrt(data$tShares1[(data$tSide=="ASK") & (data$tType != "OPG") & (data$tType != "CLX")])), c(1,20)))
            data$tShares1[(data$tSide=="BID") & (data$tType != "OPG") & (data$tType != "CLX")]<- as.integer(rescale(as.numeric(sqrt(data$tShares1[(data$tSide=="BID") & (data$tType != "OPG") & (data$tType != "CLX")])), c(1,20)))
            data$tShares1[(data$tSide=="BOTH") & (data$tType != "OPG") & (data$tType != "CLX")]<- as.integer(rescale(as.numeric(sqrt(data$tShares1[(data$tSide=="BOTH") & (data$tType != "OPG") & (data$tType != "CLX")])), c(1,20))) 
            
            data$tShares1[(data$tType=="OPG") | (data$tType=="CLX")]<- as.integer(rescale(as.numeric(data$tShares1[(data$tType=="OPG") | (data$tType=="CLX")]), c(7,24)))
            data$color[(data$tType=="OPG") | (data$tType=="CLX")]<- "#ffb84d"
            dataTprice<- data[data$tPrice >0 ,]
            
            if (input$OverLap) {dataTprice$tPrice1<- jitter(dataTprice$tPrice)}
            if (nrow(data.frame(event)) >=1 & input$spread==FALSE)  {
              py <- plot_ly(dataTprice, x= Time, y = eval(parse(text=y)), mode = "markers", text = eval(parse(text= t)), hoverinfo=eval(hoverinfo), name="Price",
                            marker=list(size=tShares1, color=eval(parse(text=colorDesc)), opacity = alpha(), line = list( width = .001) )) %>%
                layout(showlegend = FALSE, hovermode = "closest", legend = list(x = 1, y = 1),paper_bgcolor= 'rgba(249,249,263,.85)')
              py<- layout(xaxis=xax, yaxis=yax) 
              ###Level1
              if (input$level1) {
                py<- add_trace(dataTprice, x=Time, y=Bid_P, name = "Bid", line = l, hoverinfo = "none")
                py<- add_trace(dataTprice, x=Time, y=Ask_P, name = "Ask", line = l, fill="tonexty", hoverinfo = "none") 
              }
            }
            if (nrow(data.frame(event)) >=1 & input$spread==TRUE)  {
              y0<- min(data$tPrice[data$tPrice >0])
              y1<-max(data$tPrice[data$tPrice >0])
              py <- plot_ly(dataTprice, x= Time, y = eval(parse(text=y)), mode = "markers", text = eval(parse(text= t)), hoverinfo=eval(hoverinfo), name="Price", marker=list(size=tShares1,color=color, opacity= alpha(), line = list( width = .001) ) ) 
              py<- add_trace(data, x=Time, y=Bid_P, name = "Bid", line = l, hoverinfo = "none")
              py<- add_trace(data, x=Time, y=Ask_P, name = "Ask", line = l, fill="tonexty", hoverinfo = "none") 
              py<-  layout(showlegend = FALSE,hovermode = "closest", paper_bgcolor= 'rgba(249,249,263,.85)')
              py<- layout(xaxis=xax, yaxis=yax) 
            }
            ###Prints       
            if (is(try(pp(), silent=T), "try-error")==FALSE)  {
              temp<- pp()[(pp()$Time>=t1 & pp()$Time<=t2), ]
              for (i in 1: nrow(temp)) {
                py <- py %>% add_trace(x = c(temp$Time[i], temp$Time[i]), y = c( min(dataTprice$Bid_P), max(dataTprice$Ask_P)  ), mode = "line",marker=list(size=1), line=list(dash="2", color=pp()$color[i]), hoverinfo = "none", evaluate=TRUE) %>% 
                  add_trace(x = c(temp$Time[i]-Hline(), temp$Time[i]+Hline()), y = c(temp$tPrice[i], temp$tPrice[i]), marker=list(size=1), mode = "line", line=list(dash="1", color="violet"), hoverinfo = "none", evaluate=TRUE)
              }
            }
            ###Imbalances 
            if (!is.null(input$icbc)) {
              tt<- subset(ImbExchange(), Time>=t1 & Time<=t2)
              if (nrow(tt)>0) { 
                py<- add_trace(tt, x=Time, y=iCBC, name="iCBC NSDQ", mode="markers", marker=list(symbol = 22, color= "BF3EFF", size=Size(), opacity= alpha1()))
                py<- layout(yaxis=list(range=c(min(dataTprice$Bid_P)-(max(dataTprice$Ask_P)-min(dataTprice$Bid_P))*0.005,max(dataTprice$Ask_P)+(max(dataTprice$Ask_P)-min(dataTprice$Bid_P))*0.005 )))
              }
            }
            ###Prev Close
            if (input$prevclx) {
              py<- add_trace(x=c(data$Time[1], tail(data$Time,1)), y=c(PrevClose()$tPrice, PrevClose()$tPrice), line=list(width=1, color="teal"), marker=list(size=2), name="PrevCLX", hoverinfo = "none")
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
                      arrowcolor="4F94CD",
                      font= list(color="darkblue", family="Droid Sans", size=Font()),
                      align="left",
                      x = tt$Time,
                      y = mx - 0.0015*mx,
                      text = gsub("[$]","",tt$head),
                      xref = "x",
                      yref = "y",
                      showarrow = TRUE,
                      arrowhead = 3,
                      ax = 20,
                      ay = -40)
                    py<- py %>% add_trace( x = c(tt$Time, tt$Time), y = c(mn, mx - 0.0015*mx), hoverinfo = "x", marker=list(size=2, color="7093DB"), line=list(width=0.8, color="7093DB"), evaluate=TRUE)
                  }
                  py<- py %>% layout(annotations=a)
                }
              }
            }
            ###Nav
            if (length(input$nav)>0 ){
              if (tDiff<= 100) {dd1<- NavData1()} else {
                if (tDiff>100 & tDiff<2*3600) {dd1<- NavData2()}
                if (tDiff>2*3600) {dd1<- NavData3()}
              }
              if ("B" %in% input$nav) {
                dd<- subset(dd1, Time>=t1 & Time<=t2)
                py<-  add_trace(x= dd$Time, y= dd$B_NAV, mode="markers", marker=list(size=navSize, color="navy", opacity=navOpacity, symbol=217), name="B_Nav") 
              }
              if ("A" %in% input$nav) {
                py<-  add_trace(x= dd$Time, y= dd$A_NAV, mode="markers", marker=list(size=navSize, color="#99ccff", opacity=navOpacity, symbol=17), name="A_Nav")
              }
              if ("M" %in% input$nav) {
                py<-  add_trace(x= dd$Time, y= dd$M_NAV, mode="markers", marker=list(size=navSize, color="#3366ff", opacity=navOpacity, symbol=24), name="M_Nav") 
              }
              py<- layout(yaxis=list(range=c(p1-(p2-p1)*0.05, p2+(p2-p1)*0.05)))
            }
            
            
            if (input$volumeChart & nrow(plotDelayGreater())>0) {
              #### Volume Chart
              
              if (tDiff<=30) {x="Time1"}
              if ((tDiff>30) & (tDiff<5*60)) {x="Time2"}
              if ((tDiff>=5*60) & (tDiff<30*60)) {x="Time3"}
              if (tDiff>=30*60) {x="Time4"}
              
              VolumeAggregate<- dataTprice[ ,c(x, "tShares" )]
              VolumeAggregate<- eval(parse(text=paste0("aggregate(.~",x,", data=VolumeAggregate, FUN=sum)")))
              VolumeAggregate[ ,x]<- fastPOSIXct(VolumeAggregate[ ,x], required.components = 6L, tz = "GMT")
              
              py <- add_trace(data=VolumeAggregate, x =eval(parse(text= x)), y = tShares, type = "bar", marker = list(color = "steelblue"), yaxis="y2", hoverinfo="none") 
              
              py<- layout(
                yaxis = list( 
                  tickfont = list(color = fontcolor), 
                  titlefont = list(color = fontcolor),
                  domain = c(0.30, 0.95)),
                
                yaxis2 = list(
                  zerolinecolor='#d9d9d9',
                  tickfont = list(color = fontcolor), 
                  titlefont = list(color = fontcolor),
                  side = "left", 
                  domain = c(0, 0.2))
              )
            }
          }
          
          
          if (input$radio==2) {
            if (input$volumeChart) {
              py<- layout(py, xaxis=list(title = "",showgrid = F, 
                                         tickfont = list(color = fontcolor())),
                          yaxis = list( gridcolor = "#8c8c8c",
                                        tickfont = list(color = fontcolor()), 
                                        titlefont = list(color = fontcolor())),
                          yaxis2 = list( gridcolor = "#8c8c8c",
                                         zerolinecolor= "#8c8c8c",
                                         tickfont = list(color = fontcolor()), 
                                         titlefont = list(color = fontcolor())),
                          paper_bgcolor = papercolor(),
                          plot_bgcolor = plotcolor())
            } else {
              py<- layout(py, xaxis=list(title = "",showgrid = F, 
                                         tickfont = list(color = fontcolor())),
                          yaxis = list( gridcolor = "#8c8c8c",
                                        tickfont = list(color = fontcolor()), 
                                        titlefont = list(color = fontcolor())),
                          paper_bgcolor = papercolor(),
                          plot_bgcolor = plotcolor())
            }
          }
          incProgress(1)
          setProgress(1)
        })
        py
      }
    })
  })
  
  
  ###Imbalance plot
  ImbalPlot <- renderPlotly({
    
    event <- event_data("plotly_selected", source = "subset")
    input$go
    isolate({
      if (nrow(data())>0) {
        withProgress(message = 'Imbalance Chart', value = 0.3, {
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
          if (length(input$icbc)>0) {
            tt<- data.frame()
            tt<- ImbExchange()[,c("Time", "iMarket" ,"iPaired", "iShares")]
            if (nrow(data.frame(event)) >0 ) {
              t1<- as.POSIXct(as.character(as.POSIXct(min(event$x)/1000, origin="1970-01-01", tz="EET")),  "%Y-%m-%d %H:%M:%S", tz ="GMT")-1
              t2<- as.POSIXct(as.character(as.POSIXct(max(event$x)/1000, origin="1970-01-01", tz="EET")),  "%Y-%m-%d %H:%M:%S", tz ="GMT")+1
              if (!is.null(input$icbc)) {
                tt<- subset(ImbExchange(), Time>=t1 & Time<=t2, select=c("Time", "iMarket","iPaired", "iShares"))
              }
            } 
            if ("A" %in% input$icbc==FALSE) {
              py <- plot_ly(tt, x= Time, y=iPaired, mode="markers", marker=list( size=5 , opacity=0.9, color="steelblue"), name="iPaired") %>%
                # add_trace(x=Time, y=iMarket, mode="markers", marker=list( size=5 , opacity=0.9, color="violet"), name="iMarket") %>%
                add_trace(x=Time, y=iShares, mode="markers",yaxis = "y2", marker=list( size=5 , opacity=0.9, color="green"), name="iShares") %>%
                layout(xaxis=xax, showlegend = FALSE, yaxis=ay1, yaxis2 = ay2) %>% layout(  margin = list(autosize=FALSE,r=30), hovermode = "closest", paper_bgcolor= 'rgba(249,249,263,.85)') 
            } else {
              py <- plot_ly(tt, x= Time, y=iPaired, mode="markers", marker=list( size=5 , opacity=0.9, color="steelblue"), name="iPaired") %>%
                add_trace(x=Time, y=iShares, mode="markers",yaxis = "y2", marker=list( size=5 , opacity=0.9, color="green"), name="iShares")
              py <- add_trace(tt[tt$iMarket !=0, ], x=Time, y=iMarket, mode="markers", marker=list( size=5 , opacity=0.9, color="violet"), name="iMarket") %>%
                layout(xaxis=xax, showlegend = FALSE, yaxis=ay1, yaxis2 = ay2) %>% layout(margin = list(autosize=FALSE, r=30), hovermode = "closest", paper_bgcolor= 'rgba(249,249,263,.85)')
            }
          } else {
            dd<- data.frame(Time=as.Date(character()), iShares=as.numeric())
            py<- plot_ly(dd, x=Time, y=iShares, mode="markers") %>% layout(xaxis=xax, showlegend = FALSE, yaxis=ay1, paper_bgcolor= 'rgba(249,249,263,.85)')
          }
          if (input$radio==2) {
            py<- layout(py, xaxis=list(title = "", showgrid = F,
                                       tickfont = list(color = fontcolor())),
                        yaxis = list( gridcolor = "#8c8c8c",
                                      tickfont = list(color = fontcolor()), 
                                      titlefont = list(color = fontcolor())),
                        yaxis2 = list( gridcolor = "#8c8c8c",
                                       tickfont = list(color = fontcolor()), 
                                       titlefont = list(color = fontcolor())),
                        paper_bgcolor = papercolor(),
                        plot_bgcolor = plotcolor())
          }
          incProgress(1)
          setProgress(1)
        })
        if (nrow(plotdelay())==0) {py<- plot_ly(plotdelay(), x=Time, y=tPrice, mode="markers")}
        py
      }
    })
  })
  
  
  ###Bottom plot 
  trendPlot2 <- renderPlotly({
    input$go
    isolate({
      if (nrow(BottomPlot())>0) {
        withProgress(message = 'Bottom Chart', value = 0.5, {
          
          xax <- list(
            title = "",
            tickfont = list(color = "darkblue")
          )
          
          yax <- list(
            title = "",
            tickfont = list(color = "darkblue")
          )
          
          py<- plot_ly(BottomPlot(), x = Time, y = MidPrice, source="subset", mode = "markers", marker=list( size=3.3 , opacity=0.9), name="") %>%
            layout(showlegend = FALSE, hovermode = "closest", yaxis=list(type = "log") ,paper_bgcolor= 'rgba(249,249,263,.85)', dragmode = "select") %>%
            layout(xaxis=xax, yaxis=yax)
          
          if (is(try(pp(), silent=T), "try-error")==FALSE)  {
            for (i in 1: nrow(pp())) {
              py <- py %>% add_trace(x = c(pp()$Time[i], pp()$Time[i]), y = c(y0(), y1()), mode = "line",marker=list(size=1), line=list(dash="2", color="steelblue"), evaluate=TRUE)
            }
          }
          incProgress(1)
          setProgress(1)
        })
        if (input$radio==2) { 
          py<- layout(py, xaxis=list(title = "", showgrid = F,
                                     tickfont = list(color = fontcolor())),
                      yaxis = list( gridcolor = "#8c8c8c",
                                    tickfont = list(color = fontcolor()), 
                                    titlefont = list(color = fontcolor())),
                      paper_bgcolor = papercolor(),
                      plot_bgcolor = plotcolor())
        }
        py
      }
    })
  })
  
  
  ### Data tab Market data
  DataOut<-  reactive({
    event <- event_data("plotly_selected", source = "subset")
    event<- event[event$y>0, ]
    
    if (input$futures) {
      columns<- c("Time", "MsgSource", "Reason", "Bid_P", "Ask_P", "tShares", "tSide", "tType", "tPrice") 
    } else {
      columns<- c("Time", "MsgSource", "Reason", "Bid_P", "Ask_P", "tShares", "tSide", "tType", "tPrice", "iCBC", "iMarket", "iShares", "iPaired", "iExchange", "B_NAV", "M_NAV", "A_NAV") 
    }
    
    if (nrow(data.frame(event))>1) {
      t1<- fastPOSIXct(as.character(as.POSIXct(min(event$x)/1000, origin="1970-01-01", tz="EET")),  required.components = 6L, tz ="GMT")
      t2<- fastPOSIXct(as.character(as.POSIXct(max(event$x)/1000, origin="1970-01-01", tz="EET")),  required.components = 6L, tz ="GMT")
      tDiff= as.numeric(t2-t1, units="secs")
      if (tDiff<= 1800) {md<-data1()} else {
        if (tDiff>1800 & tDiff<5*3600) {md<-Seconds()}
        if (tDiff>5*3600 & tDiff<7*(3600)) {md<-Seconds10()}
        if (tDiff>7*3600) {md<-Minutes()}
      }
      if (tDiff<= 100) {nav<- NavData1()} else {
        if (tDiff>100 & tDiff<2*3600) {nav<- NavData2()}
        if (tDiff>2*3600) {nav<- NavData3()}
      }
      #md<- md[ ,columns ]
      if (length(input$nav)>0) {
        data<- rbind(md, nav)
        data<- data[order(data$Time),]
        rownames(data)<- NULL
        Out<- data[(data$Time>=t1 & data$Time<=t2), ]
      } else {Out<- md[(md$Time>=t1 & md$Time<=t2), ]}
      
    } else {
      md= data()[, columns]
      if (length(input$nav)>0) {
        nav<- NavData1()
        Out= md
        #Out<- rbind(md, nav)
        #Out<- Out[order(Out$Time),]
        rownames(Out)<- NULL
      } else {Out<- md}
      
    }
    Out$Reason<- factor(Out$Reason)
    Out$tSide<- factor(Out$tSide)
    Out$tType<- factor(Out$tType)
    Out$Time=format(Out$Time, format="%H:%M:%OS")
    return (Out)
  })
  
  output$mytable <- renderDataTable(
    datatable(DataOut(), extensions = 'Buttons',
              options = list(pageLength = 15, searchHighlight = TRUE,dom = 'C<"clear">lfrtip'),
              filter = list(position = 'top', clear = FALSE)))
  
  output$downloadData <- downloadHandler(
    filename = function() {paste0(dateText(),"_MarketData.csv", sep="") },
    content = function(file) {
      write.csv(DataOut() , file, row.names = FALSE, sep=",")
    }
  )
  
  ###Data tab for orders
  OrderOut<- reactive({
    id<- unique(dd()$orderid)
    if ( is.null(id)) {
      data<- data.frame(Time="", strategy="", messagetype="", exchange="", orderid="", destination="", side="", price="", type="", sharesexecuted="", timeinforce="")
    } else {
      data<- OrderTable(date=as.character(dateText()), symbol= input$text, orderid= unique(dd()$orderid))
    }
    return(data)
  })
  
  output$OrdersTable <- renderDataTable(
    datatable(OrderOut(), extensions = 'Buttons',options = list(pageLength = 15, searchHighlight = TRUE,dom = 'C<"clear">lfrtip'),
              filter = list(position = 'top', clear = FALSE))
  )
  output$downloadOrders <- downloadHandler(
    filename = function() {paste0(dateText(),"_Orders.csv", sep="") },
    content = function(file) {
      write.table(OrderOut() , file, row.names = FALSE, sep=",")
    }
  )
  
  output$plotui <- renderUI({
    output$plot<- trendPlot
    plotlyOutput("plot", width="100%", height = 800)
  })
  
  output$plotui2 <- renderUI({
    output$plot2<- trendPlot2
    plotlyOutput("plot2", width="100%", height = 200)
  })
  
  output$plotui3 <- renderUI({
    output$plot3<- ImbalPlot
    plotlyOutput("plot3", width="100%", height = 200)
  })
  
  
  inputChoices <- reactive({
    input$go
    isolate({
      choices<- unique(Order()$strategy)
      choices= c("None", choices)
      return(choices)
    })
  })
  
  output$strat <- renderUI({
      selectInput('strat', 'Orders:',  choices=inputChoices(), selected = input$strat, width="100")
  })
  
  output$name<- renderUI({
    input$go
    isolate({
      if (input$radio=="2") {
        eval(parse(text='includeCSS("slate.css")'))
        }
    })
  })
  
  output$textcol<- renderUI({
    if (input$radio=="2") {
      eval(parse(text= 'tags$style(type="text/css", "#from {background-color: #E3E3E3 }") '))
      }
  })
  
  
  output$brush <- renderPrint({
  })
  
  session$onSessionEnded(stopApp)
}
)
