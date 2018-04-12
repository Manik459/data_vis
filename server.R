#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  output$map1 <- renderLeaflet({
    
    # load world spatial polygons
    data(World)
    
    # inspect values in World
    World@data %>% tbl_df()
    
    # gapminder countries not in World. skipping for now
    data_2007 %>% 
      anti_join(World@data, by=c('country'='name')) %>% 
      arrange(desc(pop))
    
    # World countries not in gapminder. skipping for now
    World@data %>% 
      anti_join(data_2007, by=c('name'='country')) %>% 
      arrange(desc(pop_est)) %>%
      select(iso_a3, name, pop_est)
    
    # join gapminder data to World
    World@data = World@data %>%
      left_join(data_2007, by=c('name'='country'))
    m <- tm_shape(World) +
      tm_polygons('lifeExp', palette='RdYlGn', id='name', title='Life expectancy 2007', auto.palette.mapping=F)
    l_m<- tmap_leaflet(m)
    l_m %>% setView(lat = 30.044420 , lng = 31.235712, zoom = 2)
    
    
  })
  
  
  output$map2 <- renderLeaflet({
    
    # load world spatial polygons
    data(World)
    
    # inspect values in World
    World@data %>% tbl_df()
    
    # gapminder countries not in World. skipping for now
    data_2002 %>% 
      anti_join(World@data, by=c('country'='name')) %>% 
      arrange(desc(pop))
    
    # World countries not in gapminder. skipping for now
    World@data %>% 
      anti_join(data_2002, by=c('name'='country')) %>% 
      arrange(desc(pop_est)) %>%
      select(iso_a3, name, pop_est)
    
    # join gapminder data to World
    World@data = World@data %>%
      left_join(data_2002, by=c('name'='country'))
    # make map
    m = tm_shape(World) +
      tm_polygons('lifeExp', palette='RdYlGn', id='name', title='Life expectancy 2002', auto.palette.mapping=F) +
      tm_style_gray() + tm_format_World()
    m
    l_m<- tmap_leaflet(m)
    l_m %>% setView(lat = 30.044420 , lng = 31.235712, zoom = 2)
    
    
  })
  
  output$scatter1 <- renderPlotly({
    p <- plot_ly(data_2007, x = ~gdpPercap, y = ~lifeExp, z = ~pop, color = ~continent, size = ~size, colors = colors1,
                 marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 150),
                 text = ~paste('Country:', country, '<br>Life Expectancy:', lifeExp, '<br>GDP:', gdpPercap,
                               '<br>Pop.:', pop)) %>%
      layout(title = 'Life Expectancy v. Per Capita GDP, 2007',
             scene = list(xaxis = list(title = 'GDP per capita (2000 dollars)',
                                       gridcolor = 'rgb(255, 255, 255)',
                                       range = c(2.003297660701705, 5.191505530708712),
                                       type = 'log',
                                       zerolinewidth = 1,
                                       ticklen = 5,
                                       gridwidth = 2),
                          yaxis = list(title = 'Life Expectancy (years)',
                                       gridcolor = 'rgb(255, 255, 255)',
                                       range = c(36.12621671352166, 91.72921793264332),
                                       zerolinewidth = 1,
                                       ticklen = 5,
                                       gridwith = 2),
                          zaxis = list(title = 'Population',
                                       gridcolor = 'rgb(255, 255, 255)',
                                       type = 'log',
                                       zerolinewidth = 1,
                                       ticklen = 5,
                                       gridwith = 2)),
             paper_bgcolor = 'rgb(243, 243, 243)',
             plot_bgcolor = 'rgb(243, 243, 243)')
    p
  })
  
  
  output$scatter2 <- renderPlotly({
    p <- plot_ly(data_2002, x = ~gdpPercap, y = ~lifeExp, z = ~pop, color = ~continent, size = ~size, colors = colors2,
                 marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 150),
                 text = ~paste('Country:', country, '<br>Life Expectancy:', lifeExp, '<br>GDP:', gdpPercap,
                               '<br>Pop.:', pop)) %>%
      layout(title = 'Life Expectancy v. Per Capita GDP, 2002',
             scene = list(xaxis = list(title = 'GDP per capita (2000 dollars)',
                                       gridcolor = 'rgb(255, 255, 255)',
                                       range = c(2.003297660701705, 5.191505530708712),
                                       type = 'log',
                                       zerolinewidth = 1,
                                       ticklen = 5,
                                       gridwidth = 2),
                          yaxis = list(title = 'Life Expectancy (years)',
                                       gridcolor = 'rgb(255, 255, 255)',
                                       range = c(36.12621671352166, 91.72921793264332),
                                       zerolinewidth = 1,
                                       ticklen = 5,
                                       gridwith = 2),
                          zaxis = list(title = 'Population',
                                       gridcolor = 'rgb(255, 255, 255)',
                                       type = 'log',
                                       zerolinewidth = 1,
                                       ticklen = 5,
                                       gridwith = 2)),
             paper_bgcolor = 'rgb(243, 243, 243)',
             plot_bgcolor = 'rgb(243, 243, 243)')
    p
  })
  
  
  output$treemap1 <- renderHighchart({
    tm <- treemap(data_2007, index = c("continent"),
                  vSize = "pop",title = "Population",
                  type = "value", palette = viridis(6))
    
    hctreemap(tm) %>% 
      hc_title(text = "Gdp VS Population 2007") 
  })
  
  output$treemap2 <- renderHighchart({
    tm <- treemap(data_2002, index = c("continent"),title = "Population",
                  vSize = "pop",
                  type = "value", palette = viridis(6))
    hctreemap(tm)%>% 
      hc_title(text = "Gdp VS Population 2002") 
  })
  
  output$donut1<- renderPlotly({
    af<-filter(data, continent== "Africa")
    
    p <- af %>%
      group_by(country) %>%
      summarize(count = n()) %>%
      plot_ly(labels = ~af$country, values = ~af$gdpPercap) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "Africa Gdp Division",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    p
    
  })

  output$donut2<- renderPlotly({
    as<-filter(data, continent== "Asia")
    
    p <- as %>%
      group_by(country) %>%
      summarize(count = n()) %>%
      plot_ly(labels = ~as$country, values = ~as$gdpPercap) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "Asia Gdp Division",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    p
    
  })
  
  output$donut3<- renderPlotly({
    eu<-filter(data, continent== "Europe")
    
    p <- eu %>%
      group_by(country) %>%
      summarize(count = n()) %>%
      plot_ly(labels = ~eu$country, values = ~eu$gdpPercap) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "Europe Gdp Division",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    p
    
  })
  
  output$donut4<- renderPlotly({
    
    oc<-filter(data, continent== "Oceania")
    p <- oc %>%
      group_by(country) %>%
      summarize(count = n()) %>%
      plot_ly(labels = ~oc$country, values = ~oc$gdpPercap) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "Oceania Gdp Division",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    p
    
  })
  
  output$donut5<- renderPlotly({
    am<-filter(data, continent== "Americas")
    
    p <- am %>%
      group_by(country) %>%
      summarize(count = n()) %>%
      plot_ly(labels = ~am$country, values = ~am$gdpPercap) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "America Gdp Division",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    p
    
  })
  
    
  output$boxplot1 <- renderPlotly({
    
    af<-filter(data, continent== "Africa")
    as<-filter(data, continent== "Asia")
    eu<-filter(data, continent== "Europe")
    oc<-filter(data, continent== "Oceania")
    am<-filter(data, continent== "Americas")
    
    
    p <- plot_ly(data, x = ~year) %>%
      add_boxplot(y = ~lifeExp,data=af,jitter=0.5, name = "Africa") %>%
      add_boxplot(y = ~lifeExp,data=as,jitter=0.5, name = "Asia") %>%
      add_boxplot(y = ~lifeExp,data=eu,jitter=0.5, name = "europe") %>%
      add_boxplot(y = ~lifeExp,data=oc,jitter=0.5, name = "Oceania") %>%
      add_boxplot(y = ~lifeExp,data=am,jitter=0.5, name = "America") %>%
      layout(
        title = "Life Expectancy",
        xaxis = list(
          rangeselector = list(
            buttons = list(
              list(
                count = 3,
                label = "3 mo",
                step = "month",
                stepmode = "backward"),
              list(
                count = 6,
                label = "6 mo",
                step = "month",
                stepmode = "backward"),
              list(
                count = 1,
                label = "1 yr",
                step = "year",
                stepmode = "backward"),
              list(
                count = 1,
                label = "YTD",
                step = "year",
                stepmode = "todate"),
              list(step = "all"))),
          
          rangeslider = list(type = "date")),
        
        yaxis = list(title = "life Expectancy(years)"))
    p
  })
  
})
