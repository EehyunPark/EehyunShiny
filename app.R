# Load data
library(tidyverse)
library(readxl)
library(lubridate)
library(ggmap)
library(ggrepel)
library(plotrix)
library(scales)
library(gridExtra)
library(grid)
library(caret)

source("help.R")

vars <- setdiff(names(cluster_2.subset_fixed), "GeoAreaName")


ui <- fluidPage(
  
  headerPanel('K-means clustering about Travel Bubble'),
  
  sidebarLayout(
    
  sidebarPanel(
    
    helpText("원하는 변수를 설정하세요"),
    
    selectInput('xcol', 'X Variable', vars),
    selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
    numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
  ),
  
  mainPanel(
    plotOutput('plot1'),
    
    h2("Shiny를 통한 Clustering 결과 분석"),
    p("X축을 mean_rate, Y축을 propotion으로 놓았을 때, 관광의존도가 높으면서 확진자 수의 비율이 적은 국가는 베트남, 필리핀, 뉴질랜드, 말레이시아, 태국, 홍콩, 인도네시아 등이 있다."),
    p("따라서 해당 국가들과 트래블 버블 협정을 체결하는 것이 바람직하다."),
    br(),
    br(),
    p("X축을 flight_time, Y축을 propotion으로 놓았을 때, 비행 시간이 적으면서 확진자 비율이 낮은 국가인 일본, 홍콩, 필리핀, 말레이시아, 베트남 등이 있다."),
    p("위와 마찬가지로 해당 국가들과 협정을 체결하는 것이 좋다."),
    br(),
    br(),
    p("X축을 mean_rate, Y축을 flight_time으로 놓았을 때, 관광의존도가 높으면서 비행시간이 짧은 국가는 괌, 필리핀 등이 있다."),
    p("위와 마찬가지로 해당 국가들과 협정을 체결하는 것이 좋아보인다."),
    br(),
    br(),
    p("전체적인 비교를 한번에 하고싶다면 X축을 statistics, y축을 flighttime으로 한다면 보고서와 같은 결과를 얻을 수 있다.")
  )
  ) 
)

server <- function(input, output){

  selectedData <- reactive({
    cluster_2.subset_fixed[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(as.matrix(selectedData()), input$clusters)
  })
  
  output$plot1 <- renderPlot({
  
    par(mar = c(5.1, 5.1, 0, 1))
    ggplot(data = cluster_2.subset_fixed,
           mapping = aes(x=selectedData()[,1],
                         y=selectedData()[,2])) +
      theme_bw() +
      geom_point(colour = clusters()$cluster, size = 3) +
      labs(x = input$xcol, y = input$ycol) +
      theme(legend.background = element_rect(colour = "black")) +
      ggtitle("K-means Clustering about Travel Bubble", "eliminate 2 countries") +
      theme(plot.title = element_text(
        size = rel(1.5), lineheight = .9,
        face = "bold", colour = "blue", hjust = 0.5
      )) +
      theme(legend.title = element_text(
        size = rel(1.2), face = "bold", color = "blue", hjust = 0.5
      )) +
      theme(axis.text = element_text(
        size = rel(1), color = "black", face = "bold"
      )) +
      theme(axis.title = element_text(size = rel(1.2), colour = "red")) +
      theme(plot.subtitle = element_text(face = "italic", hjust = 0.5)) +
      geom_text(label = cluster_2.subset_fixed[,1], vjust = -0.7)
  })
  
}


shinyApp(ui = ui, server = server)

