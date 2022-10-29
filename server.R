library(shiny)
library(highcharter)

ShinyServer <- function(input, output, session) { 
  output$hcontainer <- renderHighchart ({
    hc <- hchart(df_clean, "packedbubble", hcaes(name = Position,value = EngagementSurvey,group = RecruitmentSource))
    q95 <- as.numeric(quantile(df_clean$EngagementSurvey, .95))
    hc %>% 
      hc_tooltip(
        useHTML = TRUE,
        pointFormat = "<b>{point.name}<br> Engagement SUrvey :</b> {point.value}"
      ) %>% hc_exporting(enabled = TRUE) %>% 
      hc_tooltip(crosshairs = TRUE,
                 shared = TRUE, borderWidth = 2) %>%
      hc_title(text="Recruitment Source",align="center") %>%
      # hc_add_theme(hc_theme_elementary()) %>% 
      hc_plotOptions(
        packedbubble = list(
          maxSize = "50%",
          zMin = 0,
          layoutAlgorithm = list(
            gravitationalConstant =  0.05,
            splitSeries =  TRUE, 
            seriesInteraction = TRUE,
            dragBetweenSeries = TRUE,
            parentNodeLimit = TRUE
          ),
          dataLabels = list(
            enabled = TRUE,
            format = "{point.name}",
            filter = list(
              property = "y",
              operator = ">",
              value = q95
            ),
            style = list(
              color = "black",
              textOutline = "none",
              fontWeight = "normal"
            )
          )
        )
      )
  }) # end hcontainer
  
  output$hcontainer2 <- renderHighchart ({
  # Plot 2
  hc2 <- df_clean %>% filter(df_clean$Salary <= input$range)
  
         hchart(hc2,
         type = "column", 
         hcaes(x = Department, 
               y = Salary,
               group = EmpSatisfaction)) %>% 
    hc_tooltip(
      useHTML = TRUE,
      pointFormat = "Mean Salary :<b>{point.y}"
    ) %>% hc_exporting(enabled = TRUE) %>% 
           hc_title(text="Employee Satisfaction on Scale (1-5) Based on Salary",align="center") %>% 
           hc_subtitle(text = "with mean value from each department group by Emp Satisfaction", align = "center")
  })
  output$hcontainer3 <- renderHighchart ({
    hc2 <- df_clean %>% filter(df_clean$Salary <= input$range)
    
          hchart(hc2, 
           type = "column", 
           hcaes(x =Department , 
               y = Salary, 
               group = PerformanceScore))%>% 
            hc_tooltip(
              useHTML = TRUE,
              pointFormat = "Mean Salary :<b>{point.y}"
            ) %>% hc_exporting(enabled = TRUE) %>% 
            hc_title(text="Employeee Performance Based on Salary",align="center") %>% 
            hc_subtitle(text = "with mean value from each department group by Performance Score", align = "center")
  })
  
  output$hcontainer4 <- DT::renderDataTable (
     tb,options = list(pageLength = 20, scrollx =T)
  )
  
  output$hcontainer5 <- renderHighchart ({
  df_baru <- 
    select(df_clean, Department, Position, Salary) %>% 
    arrange(desc(Salary)) 
  
  df_baru_2 <- df_baru %>% 
    group_by(Department) %>% 
    summarise(dailyAvg = mean(Salary))
  
  drilldown <- df_baru %>%
    # group_by(Department, Position, .add = T) %>% 
    group_nest(Department) %>%
    mutate(
      id = Department,
      type = "column",
      data = map(data, ~ .x %>%
                   mutate(
                     name = Position,
                     y    = Salary
                   ) %>%
                   list_parse())
    )
  
  mygraph <- hchart(
    df_baru_2,
    "column",
    hcaes(x = Department, y = dailyAvg, name = Department, drilldown = Department),
    name = "Salary",
    colorByPoint = TRUE
  )
  
  mygraph <- mygraph %>% 
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = list_parse(drilldown)
    ) 
  
  mygraph <- mygraph %>%
    hc_tooltip(
      useHTML = TRUE 
      
    ) %>% 
    hc_yAxis( title = "") %>% 
    hc_xAxis( title = "" ) %>%
    hc_title(text = "Salary" ) %>% 
  hc_caption(
    text = "This is a <i>Drilldown</i> chart to visualize the mean Salary from each department and position. <br>
        Click one of the chart to use the <i>Drilldown</i> feature", 
    align = "center",
    useHTML = TRUE
  ) %>% hc_exporting(enabled = TRUE) %>% 
    hc_tooltip(crosshairs = TRUE,
               shared = TRUE, borderWidth = 2)
  
})

  # output$vbox <- renderValueBox({
  # 
  #   hc2 <- df_clean %>% filter(df_clean$Salary <= input$range)
  # 
  #   valueBox(value = paste0(efb$Total.Ecological.Footprint,"/",
  #                           efb$Total.Biocapacity),
  #            subtitle = "Footprint/Biocapacity",
  #            color = "teal",
  #            icon = icon("pagelines"))
  # 
  # })
  
  output$hcontainer6 <- renderHighchart ({
    hc2 <- df_clean %>% filter(df_clean$RecruitmentSource == input$rect)
    hc2 <- df_clean %>% filter(df_clean$SpecialProjectsCount <= input$rectr & df_clean$EngagementSurvey <= input$rectrr)
    # hc2 <- df_clean %>% filter(df_clean$EngagementSurvey <= input$rectrr)
    
    hchart(hc2, "scatter", hcaes(x= EngagementSurvey, y = SpecialProjectsCount, name = Position, group = Department)) %>% 
      # hc_chart(type = "scatter") %>%
      hc_tooltip(
        headerFormat = "",
        pointFormat = "<b>{point.Position}</b> is in <b>{point.Department}</b> <br> with <b> {point.y} </b> Projects <br>  and <b>{point.x}</b> Engagement Survey"
      ) %>% hc_exporting(enabled = TRUE) %>% 
      hc_tooltip(crosshairs = TRUE,
                 shared = TRUE, borderWidth = 2) 
  })
  
  output$hcontainer7 <- renderHighchart({
    hc2 <- df_clean %>% filter(df_clean$ManagerName == input$name)
    hchart(hc2, "organization", hcaes(from = ManagerName, to = Employee_Name)) %>% 
      hc_chart(inverted = TRUE) %>% 
      hc_tooltip(
        headerFormat = "",
        pointFormat = "<b>{point.from}</b> is in <b>{point.to}</b>"
      ) %>% 
      hc_title(text="Manager Tree Map",align="center")
      # hc_subtitle(text = "", align = "center")
  })
  
  output$selected <- renderText({
    paste(input$name)
  })
  
  output$hcontainer8 <- renderHighchart({
    var <- textOutput("selected")
    hc2 <- df_clean %>% filter(df_clean$ManagerName == input$name)
    hchart(hc2, "lollipop", hcaes(name = Employee_Name, low = Absences ),name = "Absences") %>% 
      hc_xAxis(type = "category") %>% 
      hc_yAxis(labels = list(format = "{value}")) %>% 
      hc_title(text=paste0("Employee Absences under ",input$name),align="center")  %>% hc_exporting(enabled = TRUE) %>% 
      hc_tooltip(crosshairs = TRUE,
                 shared = TRUE, borderWidth = 2)
  })
  
  output$hcontainer9 <- renderHighchart({
    var <- textOutput("selected")
    hc2 <- df_clean_2 %>% filter(df_clean$ManagerName == input$name)
    hchart(hc2, "lollipop", hcaes(name = Employee_Name, low = EmpSatisfaction),name = "EmpSatisfaction") %>% 
      hc_xAxis(type = "category") %>% 
      hc_yAxis(labels = list(format = "{value}")) %>% 
      hc_title(text=paste0("Employee Satisfaction under ",input$name),align="center") %>% hc_exporting(enabled = TRUE) %>% 
      hc_tooltip(crosshairs = TRUE,
                 shared = TRUE, borderWidth = 2)
    })
  
  output$Box1 <- renderValueBox({
    
    hc2 <- df_clean %>% filter(df_clean$ManagerName == input$name)
    
    valueBox(value = paste0(sum(hc2$Absences, na.rm = F)), 
             subtitle = "Total Absence", 
             color = "teal",
             icon = icon("calendar"))
    
  })
  
  output$Box2 <- renderValueBox({
    
    hc2 <- df_clean_2 %>% filter(df_clean$ManagerName == input$name)
    value <- mean(hc2$EmpSatisfaction)
    
    valueBox(value = paste0(round(mean(hc2$EmpSatisfaction))), 
             subtitle = "Average of Employee Satisfaction", 
             color ="fuchsia",
             icon = icon("users"))
    
  })
  
  output$Box3 <- renderValueBox({
    
    hc2 <- df_clean_2 %>% filter(df_clean$ManagerName == input$name)
    value <- mean(hc2$EmpSatisfaction)
    
    valueBox(value = paste0(text = "", input$name), 
             subtitle = "Manager Name", 
             color ="fuchsia",
             icon = icon("briefcase"))
    
  })
}
