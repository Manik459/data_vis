#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tmap)
library(plotly)
library(highcharter)
library(treemap)
library(viridisLite)

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = 'purple',
  dashboardHeader(title ="Gdp and Life expectancy" ,
                  titleWidth = 300),
  dashboardSidebar( sidebarMenu(
    menuItem("Map Plots", tabName = "mapper", icon = icon("dashboard")),
    menuItem("3D Scatter Plots", tabName = "scatterer", icon = icon("th")),
    menuItem("Tree Map", tabName = "treemap", icon = icon("th")),
    menuItem("Donut Chart", tabName = "donut", icon = icon("th")),
    menuItem("Box Plots", tabName = "bplots", icon = icon("th"))
  )),
  dashboardBody(
    
    tabItems(
      # First tab content
      tabItem(tabName = "mapper",
              fluidRow(
                box(leafletOutput("map1", height = 400), width = 12)
              ),
              fluidRow(
                box(leafletOutput("map2", height = 400),  width = 12)
              )
      ),
      
      # Second tab content
      tabItem(tabName = "scatterer",
              fluidRow(
                box(plotlyOutput("scatter1", height = 400), width = 12)
              ),
              fluidRow(
                box(plotlyOutput("scatter2", height = 400),  width = 12)
              )
      ),
      # third tab content
      tabItem(tabName = "treemap",
              fluidRow(
                box(highchartOutput("treemap1", height = 400), width = 12)
              ),
              fluidRow(
                box(highchartOutput("treemap2", height = 400), width = 12)
              )
      ),
      # fourth tab content
      tabItem(tabName = "donut",
              fluidRow(
                box(plotlyOutput("donut1", height = 250), width = 6)
              ,
              
                box(plotlyOutput("donut2", height = 250),  width = 6)
              ),
              fluidRow(
                box(plotlyOutput("donut3", height = 250),  width = 6)
              ,
              
                box(plotlyOutput("donut4", height = 250),  width = 6)
              ),
              fluidRow(
                box(plotlyOutput("donut5", height = 250),  width = 6)
              )
      ),
      # Fifth tab content
      tabItem(tabName = "bplots",
              fluidRow(
                box(plotlyOutput("boxplot1", height = 800), width = 12)
              )
      )
    )
    
    
  )
)

