library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(tidyverse)
library(magrittr)
library(readxl)
library(gsheet)
library(googlesheets)


source(file = "functions/myBox.R", local = TRUE)


sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(
            "Giới thiệu", selected = TRUE,
            tabName = "landing_page"
        ),
        actionButton(inputId = "reload", label = "Reload data"),
        menuItem(
            "ILS",
            icon = icon("award"), startExpanded = FALSE,
            menuSubItem("Chung", tabName = "Page1"),
            menuSubItem("Theo sinh viên/lớp", tabName = "Page2")
        ),
        menuItem(
            "Lifelong",
            icon = icon("university"), startExpanded = FALSE,
            menuSubItem("Chung", tabName = "Page3"),
            menuSubItem("Theo sinh viên/lớp", tabName = "Page4")
        )
    )
)


body <- dashboardBody(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
        
        tabItem(
            tabName = "landing_page",
            fluidRow(
                box(width = 12, 
                    p(HTML('<center><img src="logo-dai-hoc-y.jpg" style="width: 4vw; min-width: 150x;"></center>')),
                    p(HTML("<center> <b> <p style='font-size:20px;'> ĐẠI HỌC Y HÀ NỘI </p> </b> </center>")),
                    tags$br(),
                    tags$br(),
                    p(HTML("<center> <b> <p style='font-size:15px;'> CÔNG CỤ ĐÁNH GIÁ </sup> </p> </b> </center>")),
                    p(HTML("<center> <b> <p style='font-size:12px;'> Chỉ số phong cách học tập (ILS) và\nThực hành học tập suốt đời </sup> </p> </b> </center>")),
                    p(HTML("<center> <b> <p style='font-size:12px;'> </b> <i>  Tác giả: Cô Hạnh ^^ </i> </p>  </center>")),
                    #tags$br(),
                    p(HTML('<center><img src="Selfdevelopment.jpg" style="width:600px;"></center>')),
                    tags$br(),
                    p(HTML("<i> <p style='font-size:10px;text-align:left'> (Ghi chú: Trang web chưa làm xong :P :P :P) </p> </i>"))
                )
            ),
        ),
            # box(
            #     width = 12,
            #     dataTableOutput(outputId = "indi_df")
            # )
        
        
        tabItem(
            tabName = "Page1",
            box(
                width = 6,
                p(HTML("<center><b>Điểm số ILS trung bình</b></center>")),
                plotlyOutput(outputId = "overallPlot")
                ),
            box(
                width = 6,
                p(HTML("<center><b>Điểm số ILS trung bình theo giới tính</b></center>")),
                plotlyOutput(outputId = "genderPlot")
            ),
            box(
                width = 12, 
                p(HTML("<center><b>Điểm số ILS trung bình theo giới tính</b></center>")),
                plotlyOutput(outputId = "boxplot1")
            )
        ),
        
        tabItem(
            tabName = "Page2",
            box(
                width = 6,
                uiOutput(outputId = "selectID"),
                plotlyOutput(outputId = "radar_ind")
            ),
            box(
                width = 6,
                uiOutput(outputId = "selectClass"),
                plotlyOutput(outputId = "radar_class")
            ),
            box(
                width = 12, height = 4,
                downloadButton(outputId = "cleandf", label = "Download Data"),
                dataTableOutput(outputId = "indi_df")
            )
        ),
        
        
        tabItem(
            tabName = "Page3",
            box(
                width = 6,
                p(HTML("<center><b>Điểm số học tập suốt đời trung bình</b></center>")),
                plotlyOutput(outputId = "overallPlot_ll")
            ),
            box(
                width = 6,
                p(HTML("<center><b>Điểm số học tập suốt đời trung bình theo giới tính</b></center>")),
                plotlyOutput(outputId = "genderPlot_ll")
            ),
            box(
                width = 6, 
                p(HTML("<center><b>Điểm số học tập suốt đời trung bình theo giới tính</b></center>")),
                plotlyOutput(outputId = "boxplot1_ll")
            )
        ),
        
        tabItem(
            tabName = "Page4",
            box(
                width = 6,
                uiOutput(outputId = "selectID_ll"),
                plotlyOutput(outputId = "radar_ind_ll")
            ),
            box(
                width = 6,
                uiOutput(outputId = "selectClass_ll"),
                plotlyOutput(outputId = "radar_class_ll")
            ),
            box(
                width = 12, height = 4,
                downloadButton(outputId = "cleandfll", label = "Download Data"),
                dataTableOutput(outputId = "indi_dfll")
            )
        )
    )
    
)






dashboardPage(title = "ILS",
              dashboardHeader(
                  title = span(HTML("ILS")),
                  tags$li(
                      a(icon("question-circle"),
                        strong("Help"),
                        href = ""),
                      class = "dropdown"
                  ),
                  tags$li(
                      a(strong("Source code"),
                        href = ""),
                      class = "dropdown"
                  )
              ),
              skin = "black",
              sidebar,
              body
)



