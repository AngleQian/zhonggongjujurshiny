library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(extrafont)
# random change
# source("../PaymentScheduleCalculator.R")

# -------------------- Start of PaymentScheduleCalculator.R --------------------


payments_schedule = function(annual_interest_rate, principal, years) {
  #first calculate monthly interest rate
  #second calculate the constant monthly payment amount
  monthly_interest_rate = annual_interest_rate / 12
  payment_months = years * 12
  a = (1 + monthly_interest_rate) ^ payment_months - 1
  total_payment = principal * monthly_interest_rate * (a + 1) / a
  
  interest_payment = principal_payment = unpaid_balance = total_payments = vector("numeric", payment_months)
  upb = principal
  for (i in 1:payment_months) {
    intrst = upb * monthly_interest_rate
    pricil = total_payment - intrst
    upb = upb - pricil
    
    
    interest_payment[i] = intrst
    principal_payment[i] = pricil
    unpaid_balance[i] = upb
    total_payments[i] = total_payment
  }
  
  df = data.frame(
    1:payment_months,
    interest_payment,
    principal_payment,
    total_payments,
    unpaid_balance
  )
  df
}

data_frame_plotter = function(data_frame) {
  knitr::kable(data_frame)
}

# data_frame_plotter(payments_schedule(.......))

vec_to_const_ratio = function(vec, denom) {
  len = length(vec)
  ret = vector("numeric", len)
  for (i in 1:len) {
    ret[i] = vec[i] / denom
  }
  ret
}

quarterly_upb_to_loanamt = function(annual_interest_rate,
                                    principal,
                                    years,
                                    years_plot = years,
                                    mltv_init = 0.72) {
  df = payments_schedule(annual_interest_rate, principal, years)
  # ratio_vec = vec_to_const_ratio(unlist(df[5], use.names=FALSE), principal)
  const = principal / mltv_init
  ratio_vec = vec_to_const_ratio(df[[5]], const)
  ratio_vec_ = vector("numeric", length(ratio_vec) / 3)
  
  j = 1
  for (i in seq(1, length(ratio_vec), by = 3)) {
    ratio_vec_[j] = (ratio_vec[i] + ratio_vec[(i + 1)] + ratio_vec[(i + 2)]) / 3
    j = j + 1
  }
  
  plot_length = years_plot * 4
  data_frame_ = data.frame("Quarter" = 1:plot_length, "MLTV" = ratio_vec_[1:plot_length])
  data_frame_
  # ggplot(data_frame_, mapping = aes(x=Quarters, y=MLTV)) + geom_point()
}

quarterly_upb = function(annual_interest_rate,
                         principal,
                         years,
                         years_plot = years) {
  df = payments_schedule(annual_interest_rate, principal, years)
  ret = df[[5]]
  ret_ = vector("numeric", length(ret) / 3)
  
  j = 1
  for (i in seq(1, length(ret), by = 3)) {
    ret_[j] = (ret[i] + ret[i + 1] + ret[i + 2]) / 3
    j = j + 1
  }
  
  plot_length = years_plot * 4
  data_frame_ = data.frame("Quarter" = 1:plot_length, "UPB" = ret_[1:plot_length])
  data_frame_
}
# -------------------- End of PaymentScheduleCalculator.R --------------------

ui <- fluidPage(
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
  # ),
  shinyjs::useShinyjs(),
  theme = shinythemes::shinytheme("united"),
  titlePanel("Quarterly UPB/MLTV Grapher"),
  hr(),
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      tags$form(
        # shinythemes::themeSelector(),
        radioButtons(
          "option",
          "Graph type",
          c("Unpaid Balance" = "upb",
            "MLTV" = "mltv")
        ),
        textInput("principal", "Initial Principal Amount of the Loan", value = "200000"),
        tags$div(
          class = "form-group shiny-input-container",
          tags$label(class = "control-label", "Annual interest rate (%)"),
          tags$div(
            class = "input-group",
            tags$input(
              type = "text",
              class = "form-control",
              id = "interest",
              placeholder = "enter 1 for 1%",
              value = "3.25"
            ),
            tags$div(class = "input-group-addon",
                     tags$span(class = "input-group-text", "%"))
          )
        ),
        textInput(
          "years",
          "Mortgage Term (Years)",
          value = "30",
          placeholder = "years"
        ),
        textInput("years_graph", "Number of years to graph", value = "30"),
        tags$div(
          class = "form-group shiny-input-container",
          id = "mltv_init_panel",
          # textInput(
          #   "down_payment",
          #   "Down payment",
          #   value = "0",
          #   placeholder = ""
          # )
          fluidRow(
            column(
              6,
              tags$label(class = "control-label shiny-bound-input", "Starting MLTV (%)"),
              tags$div(
                class = "input-group",
                tags$input(
                  type = "text",
                  class = "form-control",
                  id = "mltv_init",
                  placeholder = "enter 1 for 1%",
                  value = "72"
                ),
                tags$div(class = "input-group-addon",
                         tags$span(class = "input-group-text", "%"))
              )
            ),
            column(
              6,
              tags$label(class = "control-label shiny-bound-input", "Down payment"),
              tags$input(
                type = "text",
                class = "form-control",
                id = "down_payment",
                value = "abcd"
              )
            )
          )
        ),
      )
    ),
    
    mainPanel(
      plotlyOutput(outputId = "mltv_plot"),
      verbatimTextOutput("info")
      
    )
  ),
  hr()
)

server <- function(input, output, session) {
  old_mltv_init <- reactiveVal(value = "72")
  old_down_payment <- reactiveVal(value = "abcd")
  
  output$mltv_plot <- renderPlotly({
    interest = as.numeric(input$interest) / 100
    principal = as.numeric(input$principal)
    years = as.numeric(input$years)
    
    years_graph = as.numeric(input$years_graph)
    years_graph_ = if (years_graph > years) years else years_graph
    
    if (input$option == "upb") {
      data_frame = quarterly_upb(interest, principal, years, years_graph_)
      
      g <- ggplot(data_frame, mapping = aes(x = Quarter, y = UPB)) +
        geom_point(size = 1, color = "#003f5c") +
        theme(text = element_text(family = "Ubuntu", size = 12))
      g <- g + scale_x_continuous(sec.axis = sec_axis(~ . / 4, name = "Hi")) 
      
    } else if (input$option == "mltv") {
      mltv_init = as.numeric(input$mltv_init) / 100
      mltv_init_ = if (mltv_init < 0) 0 else mltv_init
      
      if (old_down_payment() == "abcd") {
          down_payment = toString(round(principal / mltv_init_ - principal, digits=-1))
          old_mltv_init(down_payment)
          updateTextInput(session, "down_payment", value=down_payment)
      }
      
      data_frame = quarterly_upb_to_loanamt(interest, principal, years, years_graph_, mltv_init_)
      
      g <-
        ggplot(data_frame, mapping = aes(x = Quarter, y = MLTV)) +
        geom_point(size = 1, color = "#003f5c") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        theme(text = element_text(family = "Ubuntu", size = 12)) +
        expand_limits(y = c(0, 1))
    }
    ggplotly(g)
  })
  
  observeEvent(input$option, {
    if (input$option == "mltv") {
      shinyjs::show("mltv_init_panel")
    } else {
      shinyjs::hide("mltv_init_panel")
    }
  })
  
  observeEvent(input$down_payment, {
    print("down_payment event called")
    if (old_down_payment() == input$down_payment) {
      return()
    }
    old_down_payment(input$down_payment)
    print("down_payment different")
    
    principal = as.numeric(input$principal)
    if (is.na(principal)) { return() }
    down_payment = as.numeric(input$down_payment)
    if (is.na(down_payment)) { return() }
    down_payment_ = if (down_payment < 0) 0 else down_payment

    mltv = toString(round((principal / (principal + down_payment_)) * 100, 1)) 
    old_mltv_init(mltv)
    updateTextInput(session, "mltv_init", value=mltv)
  })
  
  observeEvent(input$mltv_init, {
    print("mltv_init event called")
    if (old_mltv_init() == input$mltv_init) {
      return()
    }
    old_mltv_init(input$mltv_init)
    print("mltv_init different")
    
    
    principal = as.numeric(input$principal)
    if (is.na(principal)) { return() }
    mltv_init = as.numeric(input$mltv_init) / 100
    if (is.na(mltv_init)) { return() }
    mltv_init_ = if (mltv_init < 0) 0 else mltv_init

    down_payment = toString(round(principal / mltv_init_ - principal, digits=1))
    old_mltv_init(down_payment)
    updateTextInput(session, "down_payment", value=down_payment)
  })
  
  
  
  # output$info <- renderText({
  #   xy_str <- function(e) {
  #     if(is.null(e)) return("NULL\n")
  #     paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
  #   }
  #   xy_range_str <- function(e) {
  #     if(is.null(e)) return("NULL\n")
  #     paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1),
  #            " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
  #   }
  #
  #   paste0(
  #     "click: ", xy_str(input$plot_click),
  #     "dblclick: ", xy_str(input$plot_dblclick),
  #     "hover: ", xy_str(input$plot_hover),
  #     "brush: ", xy_range_str(input$plot_brush)
  #   )
  # })
  #
}

options(shiny.port = 8082)
shinyApp(ui, server)
# shinyApp(ui = htmlTemplate("www/mainui.html"), server)