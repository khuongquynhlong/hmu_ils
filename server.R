library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(tidyverse)
library(magrittr)
library(readxl)
library(gsheet)
library(googlesheets)
library(stringi)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # ILS
    #---------------------------------------------------------------------------
    RV <- reactiveValues()
    RV2 <- reactiveValues()
    
    observeEvent(input$reload, {
        RV$df <- gsheet2tbl("https://docs.google.com/spreadsheets/d/11lQ3UCs95k8l57YO2N11drRsryVTnYBL2giIc3J8txU/edit?usp=sharing")
        
        names(RV$df) <- c("Timestamp", "Name", "DOB", "Gender", "ID", "Class", "Track", 
                       "Email", paste0("item", c(1:44)))
        
        # glimpse(df)
        
        substr1 <- function(x){substr(x, 1, 1)}
        
        # Take the 1st letter
        RV$df %<>% mutate_at(c(9:52), substr1)
        
        RV$df %<>% mutate(
            # Gender = stri_trans_general(Gender,"Latin-ASCII"),
            # Gender = toupper(Gender),
            Class = stri_trans_general(Class,"Latin-ASCII"),
            Class = toupper(Class),
            ChuDong = 0,
            ThuDong = 0,
            GiacQuan = 0,
            TrucGiac = 0,
            HinhAnh = 0,
            NgonTu = 0,
            TuanTu = 0,
            ThongThe = 0)
        
        for (i in 1:nrow(RV$df)) {
            for (j1 in c(9, 13, 17, 21,25, 29, 33, 37, 41, 45, 49)) {
                if (RV$df[i, j1] == "A") {
                    RV$df[i, 53] = RV$df[i, 53] + 1
                } else {
                    RV$df[i, 54] = RV$df[i, 54] + 1
                }
            }
            for (j2 in c(10, 14, 18, 22, 26, 30, 34, 38, 42, 46, 50)) {
                if (RV$df[i, j2] == "A") {
                    RV$df[i, 55] = RV$df[i, 55] + 1
                } else {
                    RV$df[i, 56] = RV$df[i, 56] + 1
                }
            }
            for (j3 in c(11, 15, 19, 23, 27, 31, 35, 39, 43, 47, 51)) {
                if (RV$df[i, j3] == "A") {
                    RV$df[i, 57] = RV$df[i, 57] + 1
                } else {
                    RV$df[i, 58] = RV$df[i, 58] + 1
                }
            }
            for (j4 in c(12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52)) {
                if (RV$df[i, j4] == "A") {
                    RV$df[i, 59] = RV$df[i, 59] + 1
                } else {
                    RV$df[i, 60] = RV$df[i, 60] + 1
                }
            }
        }
        
        RV2$df_ll <-  gsheet2tbl("https://docs.google.com/spreadsheets/d/1qlTxR4I_6ERkcPwVAeQ3mqPz3kEXYjHbypq6oeQCdFI/edit?usp=sharing") %>%
            select(1:22)
        
        names(RV2$df_ll) <- c("Timestamp", "Name", "DOB", "Gender", "ID", "Class", "Track", 
                          "Email", paste0("item", c(1:14)))
        
        RV2$df_ll %<>% mutate(
            Class = stri_trans_general(Class,"Latin-ASCII"),
            Class = toupper(Class),
            DongLuc = round(((item9 + item8 + item11 + item7 + item2 + item3)-6)*100/18),
            KyNang = round(((item6 + item5 + item10 + item14)-4)*100/12),
            ChuTam = round(((item13 + item12 + item4 + item1)-4)*100/12))
        
    }, ignoreNULL=FALSE)


    
    diemtb <- reactive({
        c(mean(RV$df$ChuDong, na.rm = T), mean(RV$df$ThuDong, na.rm = T),
          mean(RV$df$GiacQuan, na.rm = T), mean(RV$df$TrucGiac, na.rm = T),
          mean(RV$df$HinhAnh, na.rm = T), mean(RV$df$NgonTu, na.rm = T),
          mean(RV$df$TuanTu, na.rm = T), mean(RV$df$ThongThe, na.rm = T))
    })
    
    
    diemtb_ll <- reactive({
        c(mean(RV2$df_ll$DongLuc, na.rm = T), mean(RV2$df_ll$KyNang, na.rm = T),
          mean(RV2$df_ll$ChuTam, na.rm = T))
    })
    
    
    diemtb_gender <- reactive({
        RV$df %>% group_by(Gender) %>%
            summarise(ChuDong = mean(ChuDong, na.rm = T),
                      ThuDong = mean(ThuDong, na.rm = T),
                      GiacQuan = mean(GiacQuan, na.rm = T),
                      TrucGiac = mean(TrucGiac, na.rm = T),
                      HinhAnh = mean(HinhAnh, na.rm = T),
                      NgonTu = mean(NgonTu, na.rm = T),
                      TuanTu = mean(TuanTu, na.rm = T),
                      ThongThe = mean(ThongThe, na.rm = T))
    })
    
    
    diemtb_gender_ll <- reactive({
        RV2$df_ll %>% group_by(Gender) %>%
            summarise(DongLuc = mean(DongLuc, na.rm = T),
                      KyNang = mean(KyNang, na.rm = T),
                      ChuTam = mean(ChuTam, na.rm = T))
    })
    
    
    diemtb_class <- reactive({
        RV$df %>% group_by(Class) %>%
            summarise(ChuDong = mean(ChuDong, na.rm = T),
                      ThuDong = mean(ThuDong, na.rm = T),
                      GiacQuan = mean(GiacQuan, na.rm = T),
                      TrucGiac = mean(TrucGiac, na.rm = T),
                      HinhAnh = mean(HinhAnh, na.rm = T),
                      NgonTu = mean(NgonTu, na.rm = T),
                      TuanTu = mean(TuanTu, na.rm = T),
                      ThongThe = mean(ThongThe, na.rm = T))
    })  
    
    diemtb_class_ll <- reactive({
        RV2$df_ll %>% group_by(Class) %>%
            summarise(DongLuc = mean(DongLuc, na.rm = T),
                      KyNang = mean(KyNang, na.rm = T),
                      ChuTam = mean(ChuTam, na.rm = T))
    })  
    
    diemtb_gender_long <- reactive({
        diemtb_gender_long <- RV$df %>% select(Gender, 53:60) %>% 
            gather(-1, key = "Domain", value = "Diem") %>%
            mutate(Domain = factor(Domain, 
                                   levels = c("ChuDong", "ThuDong", "GiacQuan", "TrucGiac",
                                                      "HinhAnh", "NgonTu", "TuanTu", "TongThe")))
    }) 
    
    diemtb_gender_long_ll <- reactive({
        diemtb_gender_long_ll <- RV2$df_ll %>% select(Gender, 23:25) %>% 
            gather(-1, key = "Domain", value = "Diem")
    }) 
    
    
    
    df2 <- reactive({
        RV$df %>% select("Name", "Gender", "ID", "Class", "Track", 53:60)
    })  
    
    
    df_ll2 <- reactive({
        RV2$df_ll %>% select("Name", "Gender", "ID", "Class", "Track", 23:25)
    })  
    
    
    output$cleandf <- downloadHandler(
        filename = "Cleandata_ILS.csv",
        content = function(con1) {
            write.csv(RV$df, row.names = F, con1)
        }
    )
    
    output$cleandfll <- downloadHandler(
        filename = "Cleandata_lifelong.csv",
        content = function(con2) {
            write.csv(RV2$df_ll, row.names = F, con2)
        }
    )
    
    output$selectID <- renderUI(
        selectInput(inputId = "selectID",
                    label = "Chọn mã sinh viên",
                    choices = df2()$ID)
    )
    
    output$selectClass <- renderUI(
        selectInput(inputId = "selectClass",
                    label = "Chọn lớp",
                    choices = diemtb_class()$Class)
    )  
    
    
    output$selectID_ll <- renderUI(
        selectInput(inputId = "selectID_ll",
                    label = "Chọn mã sinh viên",
                    choices = df_ll2()$ID)
    )
    
    output$selectClass_ll <- renderUI(
        selectInput(inputId = "selectClass_ll",
                    label = "Chọn lớp",
                    choices = diemtb_class_ll()$Class)
    )  
    
    
    output$overallPlot <- renderPlotly({
        plot_ly(
            type = 'scatterpolar',
            r = diemtb(),
            theta =  c("Chủ Động", "Thụ Động", "Giác Quan", "Trực Giác",
                       "Hình Ảnh", "Ngôn Từ", "Tuần Tự", "Tổng Thể"),
            fill = 'toself',
            name = "Chung",
        ) %>% 
            layout(
                polar = list(
                    radialaxis = list(
                        visible = T,
                        range = c(0,11)
                    )
                ),
                showlegend = F
            )
    })
    
    output$overallPlot_ll <- renderPlotly({
        plot_ly(
            type = 'scatterpolar',
            r = diemtb_ll(),
            theta =  c("Động Lực", "Kỹ năng", "Chú tâm"),
            fill = 'toself',
            name = "Chung",
        ) %>% 
            layout(
                polar = list(
                    radialaxis = list(
                        visible = T,
                        range = c(0,100)
                    )
                ),
                showlegend = F
            )
    })
    
    output$boxplot1 <- renderPlotly({
        plot_ly(diemtb_gender_long(),
                x = ~Domain, y = ~Diem, color = ~Gender, type = "box", 
                colors = c("#41ab5d", "#fd8d3c")) %>%
            layout(boxmode = "group",
                   xaxis = list(title=''),
                   yaxis = list(title='Điểm số ILS'))
    })
    
    
    output$boxplot1_ll <- renderPlotly({
        plot_ly(diemtb_gender_long_ll(),
                x = ~Domain, y = ~Diem, color = ~Gender, type = "box", 
                colors = c("#41ab5d", "#fd8d3c")) %>%
            layout(boxmode = "group",
                   xaxis = list(title=''),
                   yaxis = list(title='Điểm số học tập suốt đời'))
    })
    
    
    output$genderPlot <- renderPlotly({
        plot_ly(
            type = 'scatterpolar',
            fill = 'toself'
        ) %>%
            add_trace(
                r = diemtb_gender()[1, 2:9] %>% as.numeric(),
                theta = c("Chủ Động", "Thụ Động", "Giác Quan", "Trực Giác",
                          "Hình Ảnh", "Ngôn Từ", "Tuần Tự", "Tổng Thể"),
                name = 'Nam'
            ) %>%
            add_trace(
                r = diemtb_gender()[2, 2:9] %>% as.numeric(),
                theta = c("Chủ Động", "Thụ Động", "Giác Quan", "Trực Giác",
                          "Hình Ảnh", "Ngôn Từ", "Tuần Tự", "Tổng Thể"),
                name = 'Nữ'
            ) %>%
            layout(
                polar = list(
                    radialaxis = list(
                        visible = T,
                        range = c(0,11)
                    )
                ),
                showlegend = T
            )
    })
    
    
    output$genderPlot_ll <- renderPlotly({
        plot_ly(
            type = 'scatterpolar',
            fill = 'toself'
        ) %>%
            add_trace(
                r = diemtb_gender_ll()[1, 2:4] %>% as.numeric(),
                theta = c("Động Lực", "Kỹ năng", "Chú tâm"),
                name = 'Nam'
            ) %>%
            add_trace(
                r = diemtb_gender_ll()[2, 2:4] %>% as.numeric(),
                theta = c("Động Lực", "Kỹ năng", "Chú tâm"),
                name = 'Nữ'
            ) %>%
            layout(
                polar = list(
                    radialaxis = list(
                        visible = T,
                        range = c(0,100)
                    )
                ),
                showlegend = T
            )
    })
    
    output$radar_ind <- renderPlotly({
        plot_ly(
            type = 'scatterpolar',
            r = df2() %>% filter(ID == input$selectID) %>% select(6:13) %>% as.numeric(),
            theta = c("Chủ Động", "Thụ Động", "Giác Quan", "Trực Giác",
                      "Hình Ảnh", "Ngôn Từ", "Tuần Tự", "Tổng Thể"),
            fill = 'toself',
            name = df2() %>% filter(ID == input$selectID) 
            %>% select(Name) 
            %>% as.character()
        ) %>% 
            layout(polar = list(
                    radialaxis = list(
                        visible = T,
                        range = c(0,11)
                    )
                ),
                showlegend = F
            )
        
    })
    
    
    output$radar_ind_ll <- renderPlotly({
        plot_ly(
            type = 'scatterpolar',
            r = df_ll2() %>% filter(ID == input$selectID_ll) %>% select(6:8) %>% as.numeric(),
            theta = c("Động Lực", "Kỹ năng", "Chú tâm"),
            fill = 'toself',
            name = df_ll2() %>% filter(ID == input$selectID_ll) 
            %>% select(Name) 
            %>% as.character()
        ) %>% 
            layout(polar = list(
                radialaxis = list(
                    visible = T,
                    range = c(0,100)
                )
            ),
            showlegend = F
            )
        
    })
    
    
    output$radar_class <- renderPlotly({
        plot_ly(
            type = 'scatterpolar',
            r = diemtb_class() %>% filter(Class == input$selectClass) %>% 
                select(2:9) %>% 
                as.numeric(),
            theta = c("Chủ Động", "Thụ Động", "Giác Quan", "Trực Giác",
                      "Hình Ảnh", "Ngôn Từ", "Tuần Tự", "Tổng Thể"),
            fill = 'toself',
            name = diemtb_class() %>% filter(Class == input$selectClass) 
            %>% select(Class) 
            %>% as.character()
        ) %>% 
            layout(polar = list(
                radialaxis = list(
                    visible = T,
                    range = c(0,11)
                )
            ),
            showlegend = F
            )
    })


    output$radar_class_ll <- renderPlotly({
        plot_ly(
            type = 'scatterpolar',
            r = diemtb_class_ll() %>% filter(Class == input$selectClass_ll) %>% 
                select(2:4) %>% 
                as.numeric(),
            theta = c("Động Lực", "Kỹ năng", "Chú tâm"),
            fill = 'toself',
            name = diemtb_class_ll() %>% filter(Class == input$selectClass_ll) 
            %>% select(Class) 
            %>% as.character()
        ) %>% 
            layout(polar = list(
                radialaxis = list(
                    visible = T,
                    range = c(0,100)
                )
            ),
            showlegend = F
            )
    })
    
    output$indi_df <- renderDataTable({
        df2()
    })

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    output$indi_dfll <- renderDataTable({
        df_ll2()
    })
    
})


