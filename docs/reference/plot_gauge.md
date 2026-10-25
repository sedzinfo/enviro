# Plot Gauge

Plots the last reading of one sensor as a gauge, with the ideal range in
green and a short note on what the gas and particle sensors measure.

## Usage

``` r
plot_gauge(data, variable = "Temperature")
```

## Arguments

- data:

  data frame from get_data()

- variable:

  one of "Temperature" "Humidity" "Pressure" "Lux" "Proximity" "NH3"
  "Reducing" "Oxidising" "PM1" "PM2.5" "PM10"

## Value

A plotly object.

## Examples

``` r
prometheus_url <- c(
  "http://pip1.crabdance.com:1507/api/v1/query_range",
  "http://pip1.crabdance.com:1505/api/v1/query_range"
)
filtered_df <- get_data(urlq = prometheus_url[1])
plot_gauge(data = filtered_df, variable = "Temperature")

{"x":{"visdat":{"51d07cf07533":["function () ","plotlyVisDat"]},"cur_data":"51d07cf07533","attrs":{"51d07cf07533":{"mode":"gauge+number+delta","value":28.167283304952093,"number":{"suffix":"°C"},"title":{"text":"Temperature","font":{"size":20}},"delta":{"reference":24,"increasing":{"color":"#404444"},"decreasing":{"color":"#404444"}},"gauge":{"axis":{"range":[-50,50],"tickwidth":1,"tickcolor":"darkblue"},"bar":{"color":"red"},"bgcolor":"white","borderwidth":2,"bordercolor":"gray","steps":[{"range":[20,25],"color":"green"}],"threshold":{"line":{"color":"darkblue","width":1},"thickness":5,"value":24}},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"indicator"}},"layout":{"margin":{"b":80,"l":80,"t":50,"r":80,"pad":50},"paper_bgcolor":"white","font":{"color":"darkblue"},"annotations":[{"text":"","x":0.5,"y":-0.10000000000000001,"showarrow":false}],"hovermode":"closest","showlegend":false},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"mode":"gauge+number+delta","value":28.167283304952093,"number":{"suffix":"°C"},"title":{"text":"Temperature","font":{"size":20}},"delta":{"reference":24,"increasing":{"color":"#404444"},"decreasing":{"color":"#404444"}},"gauge":{"axis":{"range":[-50,50],"tickwidth":1,"tickcolor":"darkblue"},"bar":{"color":"red"},"bgcolor":"white","borderwidth":2,"bordercolor":"gray","steps":[{"range":[20,25],"color":"green"}],"threshold":{"line":{"color":"darkblue","width":1},"thickness":5,"value":24}},"type":"indicator","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}plot_gauge(data = filtered_df, variable = "Humidity")

{"x":{"visdat":{"51d04ee41696":["function () ","plotlyVisDat"]},"cur_data":"51d04ee41696","attrs":{"51d04ee41696":{"mode":"gauge+number+delta","value":44.704316012270503,"number":{"suffix":"%"},"title":{"text":"Humidity","font":{"size":20}},"delta":{"reference":50,"increasing":{"color":"#404444"},"decreasing":{"color":"#404444"}},"gauge":{"axis":{"range":[0,100],"tickwidth":1,"tickcolor":"darkblue"},"bar":{"color":"red"},"bgcolor":"white","borderwidth":2,"bordercolor":"gray","steps":[{"range":[40,60],"color":"green"}],"threshold":{"line":{"color":"darkblue","width":1},"thickness":5,"value":50}},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"indicator"}},"layout":{"margin":{"b":80,"l":80,"t":50,"r":80,"pad":50},"paper_bgcolor":"white","font":{"color":"darkblue"},"annotations":[{"text":"","x":0.5,"y":-0.10000000000000001,"showarrow":false}],"hovermode":"closest","showlegend":false},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"mode":"gauge+number+delta","value":44.704316012270503,"number":{"suffix":"%"},"title":{"text":"Humidity","font":{"size":20}},"delta":{"reference":50,"increasing":{"color":"#404444"},"decreasing":{"color":"#404444"}},"gauge":{"axis":{"range":[0,100],"tickwidth":1,"tickcolor":"darkblue"},"bar":{"color":"red"},"bgcolor":"white","borderwidth":2,"bordercolor":"gray","steps":[{"range":[40,60],"color":"green"}],"threshold":{"line":{"color":"darkblue","width":1},"thickness":5,"value":50}},"type":"indicator","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}plot_gauge(data = filtered_df, variable = "PM2.5")

{"x":{"visdat":{"51d02ca420be":["function () ","plotlyVisDat"]},"cur_data":"51d02ca420be","attrs":{"51d02ca420be":{"mode":"gauge+number+delta","value":16,"number":{"suffix":"ug/m3"},"title":{"text":"PM2.5","font":{"size":20}},"delta":{"reference":0,"increasing":{"color":"#404444"},"decreasing":{"color":"#404444"}},"gauge":{"axis":{"range":[0,400],"tickwidth":1,"tickcolor":"darkblue"},"bar":{"color":"red"},"bgcolor":"white","borderwidth":2,"bordercolor":"gray","steps":[{"range":[0,20],"color":"green"}],"threshold":{"line":{"color":"darkblue","width":1},"thickness":5,"value":0}},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"indicator"}},"layout":{"margin":{"b":80,"l":80,"t":50,"r":80,"pad":50},"paper_bgcolor":"white","font":{"color":"darkblue"},"annotations":[{"text":"Combustion particles,Organic compounds,Metals","x":0.5,"y":-0.10000000000000001,"showarrow":false}],"hovermode":"closest","showlegend":false},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"mode":"gauge+number+delta","value":16,"number":{"suffix":"ug/m3"},"title":{"text":"PM2.5","font":{"size":20}},"delta":{"reference":0,"increasing":{"color":"#404444"},"decreasing":{"color":"#404444"}},"gauge":{"axis":{"range":[0,400],"tickwidth":1,"tickcolor":"darkblue"},"bar":{"color":"red"},"bgcolor":"white","borderwidth":2,"bordercolor":"gray","steps":[{"range":[0,20],"color":"green"}],"threshold":{"line":{"color":"darkblue","width":1},"thickness":5,"value":0}},"type":"indicator","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
