library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(tidyverse)
library(readxl)
library(gsheet)
library(stringi)

shinyServer(function(input, output) {
    
    # Reactive Values
    RV <- reactiveValues()
    RV2 <- reactiveValues()
    
    # Load Data
    observeEvent(input$reload, {
        # Load and process ILS data
        RV$df <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1KMHArXnJWLTQ9BabHAqU7KlqBGhEIzndF2t5EBTBuMI/edit?usp=sharing") |>
            select(-c(3,4))
        
        # Renaming columns
        names(RV$df) <- c("Timestamp", "Name", "DOB", "Gender", "ID", "Class", "Year", 
                          "Email", paste0("item", 1:44))
        
        # Select relevant columns
        RV$df <- RV$df |> 
            mutate(across(9:52, ~ substr(.x, 1, 1))) |>
            mutate(ChuDong = 0, ThuDong = 0, CamGiac = 0, TrucGiac = 0,
                   HinhAnh = 0, NgonTu = 0, TrinhTu = 0, TongQuat = 0)
        
        calculate_scores <- function(prefix_indexes) {
            RV$df |> transmute(
                across(all_of(prefix_indexes), ~ ifelse(. == 'A', 1, 0))
            ) |> rowSums(na.rm = TRUE)
        }
        
        # Calculate scores based on given logic
        RV$df <- RV$df |> mutate(
            ChuDong = calculate_scores(c(9, 13, 17, 21, 25, 29, 33, 37, 41, 45, 49)),
            ThuDong = 11 - ChuDong,  # Based on assumption of 11 questions per group
            CamGiac = calculate_scores(c(10, 14, 18, 22, 26, 30, 34, 38, 42, 46, 50)),
            TrucGiac = 11 - CamGiac,
            HinhAnh = calculate_scores(c(11, 15, 19, 23, 27, 31, 35, 39, 43, 47, 51)),
            NgonTu = 11 - HinhAnh,
            TrinhTu = calculate_scores(c(12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52)),
            TongQuat = 11 - TrinhTu,
            XuLyThongTin = ChuDong - ThuDong,
            TiepThu = HinhAnh - NgonTu,
            NhanThuc = CamGiac - TrucGiac,
            GhiNho = TrinhTu - TongQuat
        )
        
        # Load and process lifelong learning data
        RV2$df_ll <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1xk85vSN6izPGKJmYK8wxLwIl6TLtDpdx1QyP6OR7C1o/edit?usp=sharing") |>
            select(-c(3, 4))
        
        names(RV2$df_ll) <- c("Timestamp", "Name", "DOB", "Gender", "ID", "Class", "Year", 
                              "Email", paste0("item", 1:14))
        
        # Calculate lifelong learning scores
        RV2$df_ll <- RV2$df_ll |> mutate(
            DongLuc = round(((item9 + item8 + item11 + item7 + item2 + item3) - 6) * 100 / 18),
            KyNang = round(((item6 + item5 + item10 + item14) - 4) * 100 / 12),
            ChuTam = round(((item13 + item12 + item4 + item1) - 4) * 100 / 12)
        )
    }, ignoreNULL=FALSE)
    
    # Helper Reactive for Calculating Means
    mean_scores <- function(data, cols) {
        colMeans(data[cols], na.rm = TRUE)
    }
    
    # ILS Scores
    diemtb <- reactive({ mean_scores(RV$df, c("ChuDong", "ThuDong", "CamGiac", "TrucGiac", "HinhAnh", "NgonTu", "TrinhTu", "TongQuat")) })
    
    # Lifelong Scores
    diemtb_ll <- reactive({ mean_scores(RV2$df_ll, c("DongLuc", "KyNang", "ChuTam")) })
    
    # Groupby Gender
    diemtb_gender <- reactive({
        RV$df |> group_by(Gender) |>
            summarise(across(c(ChuDong:TongQuat), mean, na.rm = TRUE))
    })
    
    diemtb_gender_ll <- reactive({
        RV2$df_ll |> group_by(Gender) |>
            summarise(across(c(DongLuc, KyNang, ChuTam), mean, na.rm = TRUE))
    })
    
    # Groupby Class
    diemtb_class <- reactive({
        RV$df |> group_by(Class) |>
            summarise(across(c(ChuDong:TongQuat), mean, na.rm = TRUE))
    })
    
    diemtb_class_ll <- reactive({
        RV2$df_ll |> group_by(Class) |>
            summarise(across(c(DongLuc, KyNang, ChuTam), mean, na.rm = TRUE))
    })
    
    df_long <- function(df, group_var, score_range) {
        df |> select(all_of(c(group_var, score_range))) |> 
            pivot_longer(-all_of(group_var), names_to = "Domain", values_to = "Diem") |>
            mutate(Domain = factor(Domain, levels = score_range))
    }
    
    diemtb_gender_long <- reactive({ df_long(RV$df, "Gender", c("ChuDong", "ThuDong", "CamGiac", "TrucGiac", "HinhAnh", "NgonTu", "TrinhTu", "TongQuat")) })
    
    diemtb_class_long <- reactive({ df_long(RV$df, "Class", c("ChuDong", "ThuDong", "CamGiac", "TrucGiac", "HinhAnh", "NgonTu", "TrinhTu", "TongQuat")) })
    
    diemtb_gender_long_ll <- reactive({ df_long(RV2$df_ll, "Gender", c("DongLuc", "KyNang", "ChuTam")) })
    
    diemtb_class_long_ll <- reactive({ df_long(RV2$df_ll, "Class", c("DongLuc", "KyNang", "ChuTam")) })
    
    # Filtered DataFrames for UI Outputs
    df2 <- reactive({ RV$df |> select("Name", "Gender", "ID", "Class", "Year", 53:64) })
    df_ll2 <- reactive({ RV2$df_ll |> select("Name", "Gender", "ID", "Class", "Year", 23:25) })
    
    # Download Handlers
    output$cleandf <- downloadHandler(
        filename = "Cleandata_ILS.csv",
        content = function(con1) {
            write.csv(RV$df, row.names = FALSE, con1)
        }
    )
    
    output$cleandfll <- downloadHandler(
        filename = "Cleandata_lifelong.csv",
        content = function(con2) {
            write.csv(RV2$df_ll, row.names = FALSE, con2)
        }
    )
    
    # Dynamic UI Components
    output$selectID <- renderUI({
        selectInput(inputId = "selectID",
                    label = "Chọn mã sinh viên",
                    choices = df2()$ID)
    })
    
    output$selectID_ll <- renderUI({
        selectInput(inputId = "selectID_ll",
                    label = "Chọn mã sinh viên",
                    choices = df_ll2()$ID)
    })
    
    output$selectClass_ll <- renderUI({
        selectInput(inputId = "selectClass_ll",
                    label = "Chọn lớp",
                    choices = diemtb_class_ll()$Class)
    })
    
    # Plotly Outputs
    output$overallPlot <- renderPlotly({
        plot_ly(
            type = 'scatterpolar',
            r = diemtb(),
            theta =  c("Chủ Động", "Thụ Động", "Cảm Giác", "Trực Giác", "Hình Ảnh", "Ngôn Từ", "Trình Tự", "Tổng Quát"),
            fill = 'toself',
            name = "Chung"
        ) |> 
            layout(
                polar = list(
                    radialaxis = list(
                        visible = TRUE,
                        range = c(0, 11)
                    )
                ),
                showlegend = FALSE
            )
    })
    
    output$overallPlot_ll <- renderPlotly({
        plot_ly(
            type = 'scatterpolar',
            r = diemtb_ll(),
            theta = c("Động Lực", "Kỹ năng", "Chú tâm"),
            fill = 'toself',
            name = "Chung"
        ) |> 
            layout(
                polar = list(
                    radialaxis = list(
                        visible = TRUE,
                        range = c(0, 100)
                    )
                ),
                showlegend = FALSE
            )
    })
    
    output$boxplot1 <- renderPlotly({
        plot_ly(diemtb_gender_long(),
                x = ~Domain, y = ~Diem, color = ~Gender, type = "box", 
                colors = c("#41ab5d", "#fd8d3c")) |>
            layout(
                boxmode = "group",
                xaxis = list(title = ''),
                yaxis = list(title = 'Điểm số ILS')
            )
    })
    
    output$boxplot2 <- renderPlotly({
        plot_ly(diemtb_class_long(),
                x = ~Domain, y = ~Diem, color = ~Class, type = "box") |>
            layout(
                boxmode = "group",
                xaxis = list(title = ''),
                yaxis = list(title = 'Điểm số ILS')
            )
    })
    
    output$boxplot1_ll <- renderPlotly({
        plot_ly(diemtb_gender_long_ll(),
                x = ~Domain, y = ~Diem, color = ~Gender, type = "box", 
                colors = c("#41ab5d", "#fd8d3c")) |>
            layout(
                boxmode = "group",
                xaxis = list(title = ''),
                yaxis = list(title = 'Điểm số học tập suốt đời')
            )
    })
    
    output$boxplot2_ll <- renderPlotly({
        plot_ly(diemtb_class_long_ll(),
                x = ~Domain, y = ~Diem, color = ~Class, type = "box") |>
            layout(
                boxmode = "group",
                xaxis = list(title = ''),
                yaxis = list(title = 'Điểm số học tập suốt đời')
            )
    })
    
    output$genderPlot <- renderPlotly({
        plot_ly(type = 'scatterpolar', fill = 'toself') |>
            add_trace(r = diemtb_gender()[1, 2:9] |> as.numeric(), theta = c("Chủ Động", "Thụ Động", "Cảm Giác", "Trực Giác", "Hình Ảnh", "Ngôn Từ", "Trình Tự", "Tổng Quát"), name = 'Nam') |>
            add_trace(r = diemtb_gender()[2, 2:9] |> as.numeric(), theta = c("Chủ Động", "Thụ Động", "Cảm Giác", "Trực Giác", "Hình Ảnh", "Ngôn Từ", "Trình Tự", "Tổng Quát"), name = 'Nữ') |>
            layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, 11))), showlegend = TRUE)
    })
    
    output$genderPlot_ll <- renderPlotly({
        plot_ly(type = 'scatterpolar', fill = 'toself') |>
            add_trace(r = diemtb_gender_ll()[1, 2:4] |> as.numeric(), theta = c("Động Lực", "Kỹ năng", "Chú tâm"), name = 'Nam') |>
            add_trace(r = diemtb_gender_ll()[2, 2:4] |> as.numeric(), theta = c("Động Lực", "Kỹ năng", "Chú tâm"), name = 'Nữ') |>
            layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))), showlegend = TRUE)
    })
    
    output$radar_ind <- renderPlotly({
        plot_ly(type = 'scatterpolar',
                r = df2() |> filter(ID == input$selectID) |> select(6:13) |> as.numeric(),
                theta = c("Chủ Động", "Thụ Động", "Cảm Giác", "Trực Giác", "Hình Ảnh", "Ngôn Từ", "Trình Tự", "Tổng Quát"),
                fill = 'toself',
                name = df2() |> filter(ID == input$selectID) |> select(Name) |> as.character()
        ) |> 
            layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, 11))), showlegend = FALSE)
    })
    
    output$radar_ind_ll <- renderPlotly({
        plot_ly(type = 'scatterpolar',
                r = df_ll2() |> filter(ID %in% input$selectID_ll) |> select(6:8) |> as.numeric(),
                theta = c("Động Lực", "Kỹ năng", "Chú tâm"),
                fill = 'toself',
                name = df_ll2() |> filter(ID %in% input$selectID_ll) |> select(Name) |> as.character()
        ) |> 
            layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))), showlegend = FALSE)
    })
    
    output$compare_plot <- renderPlot({
        df2() |> filter(ID == input$selectID) |> select(14:17) |>
            pivot_longer(everything(), names_to = "domain", values_to = "score") |>
            mutate(domain = recode(domain, 
                                   XuLyThongTin = "Xử lý thông tin",
                                   TiepThu = "Tiếp thu",
                                   NhanThuc = "Nhận thức",
                                   GhiNho = "Hiểu và ghi nhớ")) |>
            ggplot(aes(x = domain, y = score)) +
            geom_segment(aes(x = domain, xend = domain, y = -11, yend = 11), color = "grey80", alpha = 0.5, size = 20) +
            geom_col(width = 0.5, fill = "#e7298a") +
            annotate(geom = "text", x = "Xử lý thông tin", y = 10, label = "Chủ Động") +
            annotate(geom = "text", x = "Xử lý thông tin", y = -10, label = "Thụ Động") +
            annotate(geom = "text", x = "Tiếp thu", y = 10, label = "Hình ảnh") +
            annotate(geom = "text", x = "Tiếp thu", y = -10, label = "Ngôn từ") +
            annotate(geom = "text", x = "Nhận thức", y = 10, label = "Cảm giác") +
            annotate(geom = "text", x = "Nhận thức", y = -10, label = "Trực giác") +
            annotate(geom = "text", x = "Hiểu và ghi nhớ", y = 10, label = "Trình tự") +
            annotate(geom = "text", x = "Hiểu và ghi nhớ", y = -10, label = "Tổng quát") +
            geom_hline(yintercept = c(0, 3, 9, -3, -9), linetype = c(1, 2, 2, 2, 2), alpha = 0.5) +
            scale_y_continuous(breaks = seq(-11, 11, 2), limits = c(-11, 11)) +
            coord_flip() +
            labs(x = NULL, y = NULL, title = df2() |> filter(ID == input$selectID) |> select(Name) |> as.character()) +
            theme_minimal() +
            theme(axis.text = element_text(size = 12))
    })
    
    output$radar_class_ll <- renderPlotly({
        plot_ly(type = 'scatterpolar',
                r = diemtb_class_ll() |> filter(Class == input$selectClass_ll) |> select(2:4) |> as.numeric(),
                theta = c("Động Lực", "Kỹ năng", "Chú tâm"),
                fill = 'toself',
                name = diemtb_class_ll() |> filter(Class == input$selectClass_ll) |> select(Class) |> as.character()
        ) |>
            layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))), showlegend = FALSE)
    })
    
    output$indi_df <- renderDataTable({
        df2()
    })
    
    output$indi_dfll <- renderDataTable({
        df_ll2()
    })
    
})
