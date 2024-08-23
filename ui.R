library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(tidyverse)
library(readxl)
library(gsheet)

# Load custom functions
source(file = "functions/myBox.R", local = TRUE)

# Sidebar layout
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
            menuSubItem("Theo sinh viên", tabName = "Page2")
        ),
        menuItem(
            "Lifelong",
            icon = icon("university"), startExpanded = FALSE,
            menuSubItem("Chung", tabName = "Page3"),
            menuSubItem("Theo sinh viên", tabName = "Page4")
        )
    )
)

# Body layout
body <- dashboardBody(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
        tabItem(
            tabName = "landing_page",
            fluidRow(
                box(width = 12, 
                    p(HTML('<center><img src="logo-dai-hoc-y.jpg" alt="University Logo" style="width: 4vw; min-width: 150px;"></center>')),
                    p(HTML("<center> <b> <p style='font-size:20px;'> ĐẠI HỌC Y HÀ NỘI </p> </b> </center>")),
                    tags$br(),
                    tags$br(),
                    p(HTML("<center> <b> <p style='font-size:15px;'> CÔNG CỤ ĐÁNH GIÁ </p> </b> </center>")),
                    p(HTML("<center> <b> <p style='font-size:12px;'> Chỉ số phong cách học tập (ILS) và Thực hành học tập suốt đời </p> </b> </center>")),
                    # p(HTML("<center> <b> <p style='font-size:12px;'> </b> <i>  Tác giả: Cô Hạnh ^^ </i> </p>  </center>")),
                    tags$br(),
                    p(HTML('<center><img src="Selfdevelopment.jpg" alt="Self Development" style="width:600px;"></center>')),
                    tags$br(),
                    p(HTML("<i> <p style='font-size:10px;text-align:left'> (Ghi chú: Trang web vẫn đang hoàn thiện) </p> </i>"))
                )
            )
        ),
        tabItem(
            tabName = "Page1",
            fluidRow(
                box(
                    width = 6,
                    p(HTML("<center><b>Điểm số ILS trung bình</b></center>")),
                    plotlyOutput(outputId = "overallPlot")
                ),
                box(
                    width = 6,
                    p(HTML("<center><b>Điểm số ILS trung bình theo giới tính</b></center>")),
                    plotlyOutput(outputId = "genderPlot")
                )
            ),
            fluidRow(
                box(
                    width = 6, 
                    p(HTML("<center><b>Điểm số ILS trung bình theo giới tính</b></center>")),
                    plotlyOutput(outputId = "boxplot1")
                ),
                box(
                    width = 6, 
                    p(HTML("<center><b>Điểm số ILS trung bình theo lớp</b></center>")),
                    plotlyOutput(outputId = "boxplot2")
                )
            )
        ),
        tabItem(
            tabName = "Page2",
            fluidRow(
                box(
                    width = 6,
                    uiOutput(outputId = "selectID"),
                    plotlyOutput(outputId = "radar_ind")
                ),
                box(
                    width = 6,
                    p(HTML("<center><b>Điểm số 4 khía cạnh học tập</b></center>")),
                    tags$br(),
                    tags$br(),
                    plotOutput(outputId = "compare_plot")
                )
            ),
            box(
                width = 12,
                downloadButton(outputId = "cleandf", label = "Download Data"),
                dataTableOutput(outputId = "indi_df")
            )
        ),
        tabItem(
            tabName = "Page3",
            fluidRow(
                box(
                    width = 6,
                    p(HTML("<center><b>Điểm số học tập suốt đời trung bình</b></center>")),
                    plotlyOutput(outputId = "overallPlot_ll")
                ),
                box(
                    width = 6,
                    p(HTML("<center><b>Điểm số học tập suốt đời trung bình theo giới tính</b></center>")),
                    plotlyOutput(outputId = "genderPlot_ll")
                )
            ),
            fluidRow(
                box(
                    width = 6, 
                    p(HTML("<center><b>Điểm số học tập suốt đời trung bình theo giới tính</b></center>")),
                    plotlyOutput(outputId = "boxplot1_ll")
                ),
                box(
                    width = 6, 
                    p(HTML("<center><b>Điểm số học tập suốt đời trung bình theo lớp</b></center>")),
                    plotlyOutput(outputId = "boxplot2_ll")
                )
            )
        ),
        tabItem(
            tabName = "Page4",
            fluidRow(
                box(
                    width = 6,
                    uiOutput(outputId = "selectID_ll"),
                    plotlyOutput(outputId = "radar_ind_ll")
                ),
                box(
                    width = 6,
                    uiOutput(outputId = "selectClass_ll"),
                    plotlyOutput(outputId = "radar_class_ll")
                )
            ),
            box(
                width = 12,
                downloadButton(outputId = "cleandfll", label = "Download Data"),
                dataTableOutput(outputId = "indi_dfll")
            )
        )
    )
)

# Create UI for the dashboard
ui <- dashboardPage(
    title = "Lifelong Learning",
    dashboardHeader(
        title = span(HTML("Lifelong Learning")),
        dropdownMenu(type = "notifications", 
                     notificationItem(
                         text = "Help",
                         href = "#",
                         icon = icon("question-circle")
                     ),
                     notificationItem(
                         text = "Source Code",
                         href = "https://github.com/khuongquynhlong/hmu_ils",
                         icon = icon("github")
                     )
        )
    ),
    skin = "black",
    sidebar,
    body
)
