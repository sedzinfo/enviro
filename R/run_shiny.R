##########################################################################################
# GET DATA
##########################################################################################
#' @title Get Data from Prometheus
#' @param start_time start time in this format: "2025-01-31T11:48:08Z"
#' @param end_time end time in this format: "2025-02-07T11:48:33Z"
#' @param urlq Prometheus url
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET content
#' @keywords prometheus query
#' @export
#' @examples
#' prometheus_url=c("http://pip1.crabdance.com:1507/api/v1/query_range",
#'                  "http://pip1.crabdance.com:1505/api/v1/query_range")
#' start_time<-format(Sys.time()-60*60*24*7,"%Y-%m-%dT%H:%M:%SZ")
#' end_time<-format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ")
#' filtered_df<-get_data(start_time=start_time,end_time=end_time,urlq=prometheus_url[1])
#' head(filtered_df)
#' filtered_df<-get_data(start_time=start_time,end_time=end_time,urlq=prometheus_url[2])
#' head(filtered_df)
get_data<-function(start_time=format(Sys.time()-60*60*24*7,"%Y-%m-%dT%H:%M:%SZ"),
                   end_time=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ"),
                   urlq=prometheus_url[1]) {
  query<<-c("temperature","humidity","pressure","lux","proximity","NH3","reducing","oxidising","PM1","PM25","PM10")
  time_step<-paste0(as.numeric(round(diff(seq(as.POSIXct(start_time),as.POSIXct(end_time),length.out=1000)),0),units="mins"),"m")[1]
  xtsdf<-list()
  for(qm in query) {
    response<-httr::GET(urlq,
                        query=list(query=qm,
                                   start=start_time,
                                   end=end_time,
                                   step=time_step))
    query_result<-httr::content(response,as="text",encoding="UTF-8")
    query_result_json<-jsonlite::fromJSON(query_result,flatten=TRUE,simplifyDataFrame=TRUE)
    time_series_data<-list()
    for(i in length(query_result_json$data$result$values))
      time_series_data<-plyr::rbind.fill(query_result_json$data$result$values[[i]])
    time_series_data<-data.frame(time_series_data)
    time_series_data<-data.frame(date=as.POSIXct(as.numeric(time_series_data[,1])),
                                 timestamp=as.numeric(time_series_data[,1]),
                                 value=as.numeric(time_series_data[,2]))
    names(time_series_data)<-c("date","timestamp",qm)
    xtsdf[[qm]]<-time_series_data
  }
  filtered_df<-dfr<-Reduce(function(x,y) merge(x,y,by=c("date","timestamp"),all=TRUE),xtsdf)
  cn<-data.frame(names=names(dfr),sd=sapply(dfr,sd,na.rm=TRUE))
  filtered_df[,cn[cn$sd==0,]$names]<-NULL
  names(filtered_df)[1:7]<-paste0(toupper(substr(names(filtered_df)[1:7],1,1)),
                             tolower(substring(names(filtered_df)[1:7],2)))
  names(filtered_df)[grep("PM25",names(filtered_df))]<-"PM2.5"
  names(filtered_df)[grep("reducing",names(filtered_df))]<-"Reducing"
  names(filtered_df)[grep("oxidising",names(filtered_df))]<-"Oxidising"
  return(filtered_df)
}
##########################################################################################
# PLOT TIME SERIES
##########################################################################################
#' @title Plot Time Series
#' @param data start time in this format: "2025-01-31T11:48:08Z"
#' @param variable one of "Temperature" "Humidity" "Pressure" "Lux" "Proximity" "NH3" "Reducing" "Oxidising" "PM1" "PM2.5" "PM10"
#' @param scale scale
#' @param k integer width of the rolling window. Must be odd for rollmedian
#' @import plotly
#' @importFrom zoo rollmean
#' @keywords plot timeseries
#' @export
#' @examples
#' prometheus_url=c("http://pip1.crabdance.com:1507/api/v1/query_range",
#'                  "http://pip1.crabdance.com:1505/api/v1/query_range")
#' start_time<-format(Sys.time()-60*60*24*7,"%Y-%m-%dT%H:%M:%SZ")
#' end_time<-format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ")
#' urlq<-prometheus_url[1]
#' filtered_df<-get_data(start_time=start_time,end_time=end_time,urlq=urlq)
#' plot_time_series(data=filtered_df,variable="Temperature")
#' plot_time_series(data=filtered_df,variable="Humidity")
#' plot_time_series(data=filtered_df,variable="Pressure")
#' plot_time_series(data=filtered_df,variable="Lux")
#' plot_time_series(data=filtered_df,variable="Proximity")
#' plot_time_series(data=filtered_df,variable="NH3")
#' plot_time_series(data=filtered_df,variable="Reducing")
#' plot_time_series(data=filtered_df,variable="Oxidising")
#' plot_time_series(data=filtered_df,variable="PM1")
#' plot_time_series(data=filtered_df,variable="PM2.5")
#' plot_time_series(data=filtered_df,variable="PM10")
plot_time_series<-function(data=filtered_df,variable="Temperature",scale="",k=30) {
  smoothed<-zoo::rollmean(as.numeric(data[,variable]),k=k,align="center",fill=NA)
  current_value<-round(as.numeric(data[nrow(data),variable]),2)
  mean_value<-round(mean(as.numeric(data[,variable]),na.rm=TRUE),2)
  min_value<-round(min(as.numeric(data[,variable]),na.rm=TRUE),2)
  max_value<-round(max(as.numeric(data[,variable]),na.rm=TRUE),2)
  sd_value<-round(sd(as.numeric(data[,variable]),na.rm=TRUE),2)
  range_value<-round(max_value-min_value,2)
  plot_ts<-plot_ly(data=data,
                   x=data[,"Date"],
                   y=data[,variable],
                   text=paste0("\nDate=",data[,"Date"],"\n",
                               variable,"=",round(data[,variable],2),"\n"),
                   type="scatter",
                   mode="lines",
                   name=variable) %>%
    add_lines(data=data,
              x=data[,"Date"],
              y=smoothed,
              text=paste0("\nDate=",data[,"Date"],"\n",
                          variable,"=",round(smoothed,2),"\n"),
              type="scatter",
              mode="lines",
              name=paste("Smoothing")) %>%
    add_segments(x=min(data[,"Date"]),
                 xend=max(data[,"Date"]),
                 y=mean_value,
                 yend=mean_value,
                 name="Mean") %>%
    add_annotations(text=paste("\n\n",
                               "last:",current_value,"\n\n",
                               "μ:",mean_value,"\n",
                               "min:",min_value,"\n",
                               "max:",max_value,"\n",
                               "σ:",sd_value,"\n",
                               "range:",range_value,"\n\n",
                               "Start:",min(format(data[,"Date"],"%Y-%m-%d")),"\n",
                               "Stop:",max(format(data[,"Date"],"%Y-%m-%d")),"\n"),
                    xref="paper",
                    yref="paper",
                    x=1.02,
                    y=0.1,
                    xanchor="left",
                    yanchor="bottom",
                    showarrow=FALSE,
                    font=list(size=12),
                    align="left")%>%
    layout(autosize=TRUE,
           # height=400,
           margin=list(l=80,r=80,b=80,t=50,pad=50),
           legend=list(orientation="v",xanchor="left"),
           title=variable,
           xaxis=list(title=""),
           yaxis=list(title=scale),
           template="plotly_dark")
  return(plot_ts)
}
##########################################################################################
# PLOT GAUGE
##########################################################################################
#' @title Plot Gauge
#' @param data dataframe
#' @param variable one of "Temperature" "Humidity" "Pressure" "Lux" "Proximity" "NH3" "Reducing" "Oxidising" "PM1" "PM2.5" "PM10"
#' @import plotly
#' @keywords plot gauge
#' @export
#' @examples
#' prometheus_url=c("http://pip1.crabdance.com:1507/api/v1/query_range",
#'                  "http://pip1.crabdance.com:1505/api/v1/query_range")
#' start_time<-format(Sys.time()-60*60*24*7,"%Y-%m-%dT%H:%M:%SZ")
#' end_time<-format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ")
#' urlq<-prometheus_url[1]
#' filtered_df<-get_data(start_time=start_time,end_time=end_time,urlq=urlq)
#' plot_gauge(data=filtered_df,variable="Temperature")
#' plot_gauge(data=filtered_df,variable="Humidity")
#' plot_gauge(data=filtered_df,variable="Pressure")
#' plot_gauge(data=filtered_df,variable="Lux")
#' plot_gauge(data=filtered_df,variable="Proximity")
#' plot_gauge(data=filtered_df,variable="NH3")
#' plot_gauge(data=filtered_df,variable="Reducing")
#' plot_gauge(data=filtered_df,variable="Oxidising")
#' plot_gauge(data=filtered_df,variable="PM1")
#' plot_gauge(data=filtered_df,variable="PM2.5")
#' plot_gauge(data=filtered_df,variable="PM10")
plot_gauge<-function(data=filtered_df,variable="Temperature") {
  help_pm1<-"Ultra fine particles"
  help_pm25<-"Combustion particles,Organic compounds,Metals"
  help_pm10<-"Dust,Pollen,Mould spores"
  help_NH3<-"Mostly Ammonia,\n Hydrogen,Ethanol,Propane,Iso-butane"
  help_reducing<-"Mostly Carbon Monoxide,\n H2S,Ammonia,Ethanol,Hydrogen,Methane,Propane,Iso-butane"
  help_oxidising<-"Mostly Nitrogen Dioxide,\n NO,Hydrogen"
  names<-c("Temperature","Humidity","Pressure","Lux","Proximity",
           "NH3","Reducing","Oxidising","PM1","PM2.5","PM10")
  scale<-c("°C","%","hPa","Lux",NA,"Ohms","Ohms","Ohms","ug/m3","ug/m3","ug/m3")
  range_ideal_min<-c(20,40,1010,1000,0,NA,NA,NA,0,0,0)
  range_ideal_max<-c(25,60,1015,1500,1,NA,NA,NA,20,20,20)
  point_ideal<-c(24,50,1013,1200,NA,NA,NA,NA,0,0,0)
  range_min<-c(-50,0,900,0,0,NA,NA,NA,0,0,0)
  range_max<-c(50,100,1100,10000,10,NA,NA,NA,400,400,400)
  help<-c(rep("",5),help_NH3,help_reducing,help_oxidising,help_pm1,help_pm25,help_pm10)
  df_reference<-data.frame(query=query,names=names,scale,range_min,range_max,range_ideal_min,range_ideal_max,point_ideal,help=help)
  dflast<-data[data$Timestamp%in%max(data$Timestamp),]
  last_value<-dflast[,variable]
  value_mean<-mean(dflast[,variable],na.rm=TRUE)
  value_sd<-sd(dflast[,variable],na.rm=TRUE)
  value_max<-max(dflast[,variable])
  value_min<-min(dflast[,variable])
  reference<-df_reference[df_reference$names%in%variable,]
  
  range<-c(reference$range_min,reference$range_max)
  range_ideal<-c(reference$range_ideal_min,reference$range_ideal_max)
  point_ideal<-reference$point_ideal
  
  if(unique(is.na(range)))
    range<-c(value_min-value_sd,value_max+value_sd)
  if(unique(is.na(range_ideal)))
    range_ideal<-c(value_mean-value_sd,value_mean+value_sd)
  if(is.na(point_ideal))
    point_ideal<-value_mean
  
  plot_g<-plot_ly(type="indicator",
                  mode="gauge+number+delta",
                  value=last_value,
                  number=list(suffix=reference$scale),
                  title=list(text=variable,font=list(size=20)),
                  delta=list(reference=point_ideal,
                             increasing=list(color="#404444"),
                             decreasing=list(color="#404444")),
                  gauge=list(axis=list(range=range,tickwidth=1,tickcolor="darkblue"),
                             bar=list(color="red"),
                             bgcolor="white",
                             borderwidth=2,
                             bordercolor="gray",
                             steps=list(list(range=range_ideal,color="green")),
                             threshold=list(line=list(color="darkblue",width=1),
                                            thickness=5,
                                            value=point_ideal))) %>%
    layout(margin=list(l=80,r=80,b=80,t=50,pad=50),
           paper_bgcolor="white",
           font=list(color="darkblue")) %>%
    add_annotations(text=reference$help,
                    x=.5,
                    y=-.1,
                    showarrow=FALSE)
  return(plot_g)
}
##########################################################################################
# PIMORONI ENVIRO
##########################################################################################
#' @title enviro
#' @param prometheus_url prometheus url
#' @param timezone the timezone prometheus exports data
#' @import shiny shinydashboard
#' @keywords pimoroni enviro
#' @export
#' @examples
#' pimoroni(prometheus_url=c("http://pip1.crabdance.com:1507/api/v1/query_range",
#'                           "http://pip1.crabdance.com:1505/api/v1/query_range"))
pimoroni<-function(prometheus_url,timezone="Europe/Bucharest") {
  library(shiny)
  library(shinydashboard)
  options(scipen=999)
  Sys.setenv(TZ=timezone)
  ui<-tagList(htmltools::htmlDependencies(icon("")),
              tags$style(".custom-label { display: inline-block; width: 150px; vertical-align: top; }
                        .plot-zoom {
                          position: absolute;
                          border: none;
                          background-color: transparent;
                          bottom: 0;
                          right: 0;}
                        .full-screen {
                          position: fixed;
                          height: 98vh !important;
                          width: 98vw !important;
                          left: 0;
                          top: 0;
                          z-index: 9999;
                          overflow: hidden;}"),
              tags$script(HTML("function plotZoom(el) {
                             el=$(el);
                             var parent = el.parent().parent();
                             if(el.attr('data-full_screen') === 'false') {
                                parent.addClass('full-screen').trigger('resize').fadeOut().fadeIn();
                                el.attr('data-full_screen','true');
                             } else {
                                parent.removeClass('full-screen').trigger('resize').fadeOut().fadeIn();
                                el.attr('data-full_screen','false');
                                    }
                             }
                             $(function() {
                               $('.plotly-full-screen .plotly.html-widget').append(`
                                 <div style='position: relative;'>
                                   <button onclick=plotZoom(this) class='plot-zoom' data-full_screen='false' title='Full Screen'>
                                    <i class='fa fa-expand-arrows-alt'></i>
                                   </button>
                                 </div>`);
                                 })"
              )),
              navbarPage("Prometheus Data Visualization",
                         tags$head(tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                              padding-top:4px;
                              padding-bottom:0;
                              height: 25px;
                              }
                             .navbar {min-height:25px;}'))),
                         tabPanel("Current",
                                  fluidRow(column(width=8,selectInput(inputId="server_gauge",label="Server",choices=c(prometheus_url),selected=prometheus_url[1],width="600px")),
                                           column(width=4,textOutput("last_date"))),
                                  fluidRow(box(width=4,style="background-color: #F8F8F8; padding-top: 20px;",plotlyOutput(outputId="gauge_temperature")),
                                           box(width=4,style="background-color: #F8F8F8; padding-top: 20px;",plotlyOutput(outputId="gauge_humidity")),
                                           box(width=4,style="background-color: #F8F8F8; padding-top: 20px;",plotlyOutput(outputId="gauge_pressure")),
                                           box(width=4,style="background-color: #F8F8F8; padding-top: 20px;",uiOutput(outputId="gauge_nh3")),
                                           box(width=4,style="background-color: #F8F8F8; padding-top: 20px;",uiOutput(outputId="gauge_reducing")),
                                           box(width=4,style="background-color: #F8F8F8; padding-top: 20px;",uiOutput(outputId="gauge_oxidising")),
                                           box(width=4,style="background-color: #F8F8F8; padding-top: 20px;",uiOutput(outputId="gauge_pm1")),
                                           box(width=4,style="background-color: #F8F8F8; padding-top: 20px;",uiOutput(outputId="gauge_pm25")),
                                           box(width=4,style="background-color: #F8F8F8; padding-top: 20px;",uiOutput(outputId="gauge_pm10")),
                                           box(width=4,style="background-color: #F8F8F8; padding-top: 20px;",plotlyOutput(outputId="gauge_lux")),
                                           box(width=4,style="background-color: #F8F8F8; padding-top: 20px;",plotlyOutput(outputId="gauge_proximity")))),
                         tabPanel("Time Series",
                                  fluidRow(column(width=4,selectInput(inputId="server_timeseries",label="Server",choices=c(prometheus_url),selected=prometheus_url[1],width="600px")),
                                           column(width=4,selectInput(inputId="days",label="Days",choices=c(1:7,15,seq(30,365,30),365),selected=7))),
                                  fluidRow(div(class="plotly-full-screen",
                                               column(width=6,style="background-color: #F8F8F8; padding-top: 20px;",plotlyOutput(outputId="plot_temperature")),
                                               column(width=6,style="background-color: #F8F8F8; padding-top: 20px;",plotlyOutput(outputId="plot_humidity")),
                                               column(width=6,style="background-color: #F8F8F8; padding-top: 20px;",plotlyOutput(outputId="plot_pressure")),
                                               column(width=3,style="background-color: #F8F8F8; padding-top: 20px;",plotlyOutput(outputId="plot_lux")),
                                               column(width=3,style="background-color: #F8F8F8; padding-top: 20px;",plotlyOutput(outputId="plot_proximity")),
                                               column(width=4,style="background-color: #F8F8F8; padding-top: 20px;",uiOutput(outputId="plot_nh3")),
                                               column(width=4,style="background-color: #F8F8F8; padding-top: 20px;",uiOutput(outputId="plot_reducing")),
                                               column(width=4,style="background-color: #F8F8F8; padding-top: 20px;",uiOutput(outputId="plot_oxidising")),
                                               column(width=4,style="background-color: #F8F8F8; padding-top: 20px;",uiOutput(outputId="plot_pm1")),
                                               column(width=4,style="background-color: #F8F8F8; padding-top: 20px;",uiOutput(outputId="plot_pm25")),
                                               column(width=4,style="background-color: #F8F8F8; padding-top: 20px;",uiOutput(outputId="plot_pm10"))
                                  )))
              ))
  server<-function(input,output) {
    observeEvent(list(input$days,input$server_timeseries), {
      observe({
        start_time<-format(Sys.time()-as.numeric(input$days)*60*60*24,"%Y-%m-%dT%H:%M:%SZ")
        filtered_df<-get_data(start_time=start_time,end_time=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ"),urlq=input$server_timeseries)
        scale_pm<-"micrograms per cubic metre (ug/m3)"
        scale_g<-"Ohms"
        output$plot_temperature<-renderPlotly({plot_time_series(data=filtered_df,variable="Temperature",scale="°C") %>% as_widget()})
        output$plot_humidity<-renderPlotly({plot_time_series(data=filtered_df,variable="Humidity",scale="Relative humidity (%)") %>% as_widget()})
        output$plot_pressure<-renderPlotly({plot_time_series(data=filtered_df,variable="Pressure",scale="hPa") %>% as_widget()})
        output$plot_lux<-renderPlotly({plot_time_series(data=filtered_df,variable="Lux",scale="LUX") %>% as_widget()})
        output$plot_proximity<-renderPlotly({plot_time_series(data=filtered_df,variable="Proximity") %>% as_widget()})
        if(!is.null(filtered_df$PM1)||!is.null(filtered_df$PM2.5)||!is.null(filtered_df$PM10)) {
          output$plot_pm1<-renderUI({plot_time_series(data=filtered_df,variable="PM1",scale=scale_pm) %>% as_widget()})
          output$plot_pm25<-renderUI({plot_time_series(data=filtered_df,variable="PM2.5",scale=scale_pm) %>% as_widget()})
          output$plot_pm10<-renderUI({plot_time_series(data=filtered_df,variable="PM10",scale=scale_pm) %>% as_widget()})
        }
        if(!is.null(filtered_df$NH3)||!is.null(filtered_df$Reducing)||!is.null(filtered_df$Oxidising)) {
          output$plot_nh3<-renderUI({plot_time_series(data=filtered_df,variable="NH3",scale=scale_g) %>% as_widget()})
          output$plot_reducing<-renderUI({plot_time_series(data=filtered_df,variable="Reducing",scale=scale_g) %>% as_widget()})
          output$plot_oxidising<-renderUI({plot_time_series(data=filtered_df,variable="Oxidising",scale=scale_g) %>% as_widget()})
        }
      })
    })
    observeEvent(list(input$server_gauge), {
      observe({
        start_time<-format(Sys.time()-60*60*24*7,"%Y-%m-%dT%H:%M:%SZ")
        filtered_df<-get_data(start_time=start_time,end_time=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ"),urlq=input$server_gauge)
        # filtered_df<-get_data(start_time=start_time,end_time=end_time,urlq=prometheus_url[1])
        df_last<-filtered_df[filtered_df$Timestamp%in%max(filtered_df$Timestamp),]
        output$last_date<-renderText(format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ"))
        output$gauge_temperature<-renderPlotly({plot_gauge(variable="Temperature",data=df_last) %>% as_widget()})
        output$gauge_humidity<-renderPlotly({plot_gauge(variable="Humidity",data=df_last) %>% as_widget()})
        output$gauge_pressure<-renderPlotly({plot_gauge(variable="Pressure",data=df_last) %>% as_widget()})
        output$gauge_lux<-renderPlotly({plot_gauge(variable="Lux",data=df_last) %>% as_widget()})
        output$gauge_proximity<-renderPlotly({plot_gauge(variable="Proximity",data=df_last) %>% as_widget()})
        if(!is.null(filtered_df$PM1)||!is.null(filtered_df$PM2.5)||!is.null(filtered_df$PM10)) {
          output$gauge_pm1<-renderUI({plot_gauge(variable="PM1",data=df_last) %>% as_widget()})
          output$gauge_pm25<-renderUI({plot_gauge(variable="PM2.5",data=df_last) %>% as_widget()})
          output$gauge_pm10<-renderUI({plot_gauge(variable="PM10",data=df_last) %>% as_widget()})
        }
        if(!is.null(filtered_df$NH3)||!is.null(filtered_df$Reducing)||!is.null(filtered_df$Oxidising)) {
          output$gauge_nh3<-renderUI({plot_gauge(variable="NH3",data=df_last) %>% as_widget()})
          output$gauge_reducing<-renderUI({plot_gauge(variable="Reducing",data=df_last) %>% as_widget()})
          output$gauge_oxidising<-renderUI({plot_gauge(variable="Oxidising",data=df_last) %>% as_widget()})
        }
      })
    })
  }
  shinyApp(ui=ui,server=server)
}


