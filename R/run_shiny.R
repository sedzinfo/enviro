##########################################################################################
# GET DATA
##########################################################################################
# metrics that the Pimoroni Enviro and Enviro+ export to Prometheus
metrics<-c("temperature","humidity","pressure","lux","proximity","NH3","reducing","oxidising","PM1","PM25","PM10")
# column names of the metrics in the data from get_data()
metric_names<-c(date="Date",timestamp="Timestamp",temperature="Temperature",humidity="Humidity",
                pressure="Pressure",lux="Lux",proximity="Proximity",reducing="Reducing",
                oxidising="Oxidising",PM25="PM2.5")
#' @title Get Data from Prometheus
#' @param start_time start time in this format: "2025-01-31T11:48:08Z"
#' @param end_time end time in this format: "2025-02-07T11:48:33Z"
#' @param urlq Prometheus url
#' @return A data frame with the Date and the Timestamp of the readings and one column per sensor,
#'   or NULL, with a message, when the server cannot be reached or sends no data. Sensors whose
#'   readings never change are left out, as the device does not have them.
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET content
#' @keywords prometheus query
#' @export
#' @examples
#' \dontrun{
#' prometheus_url=c("http://pip1.crabdance.com:1507/api/v1/query_range",
#'                  "http://pip1.crabdance.com:1505/api/v1/query_range")
#' start_time<-format(Sys.time()-60*60*24*7,"%Y-%m-%dT%H:%M:%SZ")
#' end_time<-format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ")
#' filtered_df<-get_data(start_time=start_time,end_time=end_time,urlq=prometheus_url[2])
#' head(filtered_df)
#' filtered_df<-get_data(start_time=start_time,end_time=end_time,urlq=prometheus_url[1])
#' head(filtered_df)
#' }
get_data<-function(start_time=format(Sys.time()-60*60*24*7,"%Y-%m-%dT%H:%M:%SZ"),
                   end_time=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ"),
                   urlq) {
  time_step<-paste0(as.numeric(round(diff(seq(as.POSIXct(start_time),as.POSIXct(end_time),length.out=1000)),0),units="mins"),"m")[1]
  xtsdf<-list()
  for(qm in metrics) {
    response<-tryCatch(httr::GET(urlq,
                                 query=list(query=qm,
                                            start=start_time,
                                            end=end_time,
                                            step=time_step),
                                 httr::timeout(15)),
                       error=function(e) e)
    # a server that is offline fails on the first metric: stop there instead of waiting for each one
    if(inherits(response,"error")||httr::http_error(response)) {
      message("Could not get data from ",urlq,": ",
              if(inherits(response,"error")) conditionMessage(response) else httr::http_status(response)$message)
      return(NULL)
    }
    query_result<-httr::content(response,as="text",encoding="UTF-8")
    query_result_json<-tryCatch(jsonlite::fromJSON(query_result,flatten=TRUE,simplifyDataFrame=TRUE),error=function(e) NULL)
    values<-query_result_json$data$result$values
    if(length(values)==0) next
    # a metric can come in several series, for example after a restart: keep them all
    series<-lapply(values,function(v) data.frame(timestamp=as.numeric(v[,1]),value=as.numeric(v[,2])))
    time_series_data<-do.call(rbind,series)
    if(length(series)>1)
      time_series_data<-stats::aggregate(time_series_data["value"],by=time_series_data["timestamp"],FUN=mean)
    time_series_data<-data.frame(date=as.POSIXct(time_series_data$timestamp),
                                 timestamp=time_series_data$timestamp,
                                 value=time_series_data$value)
    names(time_series_data)<-c("date","timestamp",qm)
    xtsdf[[qm]]<-time_series_data
  }
  if(length(xtsdf)==0) {
    message("No data from ",urlq)
    return(NULL)
  }
  filtered_df<-dfr<-Reduce(function(x,y) merge(x,y,by=c("date","timestamp"),all=TRUE),xtsdf)
  cn<-data.frame(names=names(dfr),sd=sapply(dfr,stats::sd,na.rm=TRUE))
  filtered_df[,cn[cn$sd%in%0,]$names]<-NULL
  renamed<-names(filtered_df)%in%names(metric_names)
  names(filtered_df)[renamed]<-metric_names[names(filtered_df)[renamed]]
  return(filtered_df)
}
##########################################################################################
# PLOT TIME SERIES
##########################################################################################
#' @title Plot Time Series
#' @param data data frame from get_data()
#' @param variable one of "Temperature" "Humidity" "Pressure" "Lux" "Proximity" "NH3" "Reducing" "Oxidising" "PM1" "PM2.5" "PM10"
#' @param scale scale
#' @param k integer width of the rolling window. Must be odd for rollmedian
#' @import plotly
#' @importFrom zoo rollmean
#' @keywords plot timeseries
#' @export
#' @examples
#' \dontrun{
#' prometheus_url=c("http://pip1.crabdance.com:1507/api/v1/query_range",
#'                  "http://pip1.crabdance.com:1505/api/v1/query_range")
#' start_time<-format(Sys.time()-60*60*24*7,"%Y-%m-%dT%H:%M:%SZ")
#' end_time<-format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ")
#' filtered_df<-get_data(start_time=start_time,end_time=end_time,urlq=prometheus_url[2])
#' head(filtered_df)
#' filtered_df<-get_data(start_time=start_time,end_time=end_time,urlq=prometheus_url[1])
#' head(filtered_df)
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
#' }
plot_time_series<-function(data,variable="Temperature",scale="",k=30) {
  smoothed<-zoo::rollmean(as.numeric(data[,variable]),k=k,align="center",fill=NA)
  current_value<-round(as.numeric(data[nrow(data),variable]),2)
  mean_value<-round(mean(as.numeric(data[,variable]),na.rm=TRUE),2)
  min_value<-round(min(as.numeric(data[,variable]),na.rm=TRUE),2)
  max_value<-round(max(as.numeric(data[,variable]),na.rm=TRUE),2)
  sd_value<-round(stats::sd(as.numeric(data[,variable]),na.rm=TRUE),2)
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
                               "\u03bc:",mean_value,"\n",
                               "min:",min_value,"\n",
                               "max:",max_value,"\n",
                               "\u03c3:",sd_value,"\n",
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
#' \dontrun{
#' prometheus_url=c("http://pip1.crabdance.com:1507/api/v1/query_range",
#'                  "http://pip1.crabdance.com:1505/api/v1/query_range")
#' start_time<-format(Sys.time()-60*60*24*7,"%Y-%m-%dT%H:%M:%SZ")
#' end_time<-format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ")
#' filtered_df<-get_data(start_time=start_time,end_time=end_time,urlq=prometheus_url[2])
#' head(filtered_df)
#' filtered_df<-get_data(start_time=start_time,end_time=end_time,urlq=prometheus_url[1])
#' head(filtered_df)
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
#' }
plot_gauge<-function(data,variable="Temperature") {
  help_pm1<-"Ultra fine particles"
  help_pm25<-"Combustion particles,Organic compounds,Metals"
  help_pm10<-"Dust,Pollen,Mould spores"
  help_NH3<-"Mostly Ammonia,\n Hydrogen,Ethanol,Propane,Iso-butane"
  help_reducing<-"Mostly Carbon Monoxide,\n H2S,Ammonia,Ethanol,Hydrogen,Methane,Propane,Iso-butane"
  help_oxidising<-"Mostly Nitrogen Dioxide,\n NO,Hydrogen"
  names<-c("Temperature","Humidity","Pressure","Lux","Proximity",
           "NH3","Reducing","Oxidising","PM1","PM2.5","PM10")
  scale<-c("\u00b0C","%","hPa","Lux",NA,"Ohms","Ohms","Ohms","ug/m3","ug/m3","ug/m3")
  range_ideal_min<-c(20,40,1010,1000,0,NA,NA,NA,0,0,0)
  range_ideal_max<-c(25,60,1015,1500,1,NA,NA,NA,20,20,20)
  point_ideal<-c(24,50,1013,1200,NA,NA,NA,NA,0,0,0)
  range_min<-c(-50,0,900,0,0,NA,NA,NA,0,0,0)
  range_max<-c(50,100,1100,10000,10,NA,NA,NA,400,400,400)
  help<-c(rep("",5),help_NH3,help_reducing,help_oxidising,help_pm1,help_pm25,help_pm10)
  df_reference<-data.frame(query=metrics,names=names,scale,range_min,range_max,range_ideal_min,range_ideal_max,point_ideal,help=help)
  dflast<-data[data$Timestamp%in%max(data$Timestamp),]
  last_value<-dflast[,variable]
  value_mean<-mean(dflast[,variable],na.rm=TRUE)
  value_sd<-stats::sd(dflast[,variable],na.rm=TRUE)
  value_max<-max(dflast[,variable])
  value_min<-min(dflast[,variable])
  reference<-df_reference[df_reference$names%in%variable,]

  range<-c(reference$range_min,reference$range_max)
  range_ideal<-c(reference$range_ideal_min,reference$range_ideal_max)
  point_ideal<-reference$point_ideal

  if(anyNA(range))
    range<-c(value_min-value_sd,value_max+value_sd)
  if(anyNA(range_ideal))
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
#' if (interactive()) pimoroni(prometheus_url=c("http://pip1.crabdance.com:1507/api/v1/query_range",
#'                           "http://pip1.crabdance.com:1505/api/v1/query_range"))
pimoroni<-function(prometheus_url,timezone="Europe/Bucharest") {
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
    # each tab downloads its data again when its server or number of days changes
    timeseries_data<-reactive({
      start_time<-format(Sys.time()-as.numeric(input$days)*60*60*24,"%Y-%m-%dT%H:%M:%SZ")
      get_data(start_time=start_time,end_time=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ"),urlq=input$server_timeseries)
    })
    gauge_data<-reactive({
      start_time<-format(Sys.time()-60*60*24*7,"%Y-%m-%dT%H:%M:%SZ")
      get_data(start_time=start_time,end_time=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ"),urlq=input$server_gauge)
    })
    # the rows of the last reading, for the gauges
    last_reading<-reactive({
      filtered_df<-gauge_data()
      validate(need(filtered_df,paste("The Prometheus server",input$server_gauge,"cannot be reached right now")))
      filtered_df[filtered_df$Timestamp%in%max(filtered_df$Timestamp),]
    })
    # an output for each sensor, left empty when the device does not have the sensor
    plot_output<-function(variable,scale="",render=renderPlotly) {
      render({
        filtered_df<-timeseries_data()
        validate(need(filtered_df,paste("The Prometheus server",input$server_timeseries,"cannot be reached right now")))
        req(variable%in%names(filtered_df))
        plot_time_series(data=filtered_df,variable=variable,scale=scale) %>% as_widget()
      })
    }
    gauge_output<-function(variable,render=renderPlotly) {
      render({
        df_last<-last_reading()
        req(variable%in%names(df_last))
        plot_gauge(variable=variable,data=df_last) %>% as_widget()
      })
    }
    scale_pm<-"micrograms per cubic metre (ug/m3)"
    scale_g<-"Ohms"
    output$plot_temperature<-plot_output("Temperature",scale="\u00b0C")
    output$plot_humidity<-plot_output("Humidity",scale="Relative humidity (%)")
    output$plot_pressure<-plot_output("Pressure",scale="hPa")
    output$plot_lux<-plot_output("Lux",scale="LUX")
    output$plot_proximity<-plot_output("Proximity")
    output$plot_pm1<-plot_output("PM1",scale=scale_pm,render=renderUI)
    output$plot_pm25<-plot_output("PM2.5",scale=scale_pm,render=renderUI)
    output$plot_pm10<-plot_output("PM10",scale=scale_pm,render=renderUI)
    output$plot_nh3<-plot_output("NH3",scale=scale_g,render=renderUI)
    output$plot_reducing<-plot_output("Reducing",scale=scale_g,render=renderUI)
    output$plot_oxidising<-plot_output("Oxidising",scale=scale_g,render=renderUI)
    output$last_date<-renderText({
      gauge_data()
      format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ")
    })
    output$gauge_temperature<-gauge_output("Temperature")
    output$gauge_humidity<-gauge_output("Humidity")
    output$gauge_pressure<-gauge_output("Pressure")
    output$gauge_lux<-gauge_output("Lux")
    output$gauge_proximity<-gauge_output("Proximity")
    output$gauge_pm1<-gauge_output("PM1",render=renderUI)
    output$gauge_pm25<-gauge_output("PM2.5",render=renderUI)
    output$gauge_pm10<-gauge_output("PM10",render=renderUI)
    output$gauge_nh3<-gauge_output("NH3",render=renderUI)
    output$gauge_reducing<-gauge_output("Reducing",render=renderUI)
    output$gauge_oxidising<-gauge_output("Oxidising",render=renderUI)
  }
  shinyApp(ui=ui,server=server,
           onStart=function() {
             # for this app run only: times in the time zone of the data, and small values in full
             old_options<-options(scipen=999)
             old_tz<-Sys.getenv("TZ",unset=NA)
             Sys.setenv(TZ=timezone)
             onStop(function() {
               options(old_options)
               if(is.na(old_tz)) Sys.unsetenv("TZ") else Sys.setenv(TZ=old_tz)
             })
           })
}
