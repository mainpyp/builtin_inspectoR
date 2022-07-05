library(shiny)
library(shinyBS)
library(DT)
library(ggplot2)
library(shinyalert)
library(data.table)
source("interactive_table_functions.R")

all_datasets <- as.list(as.data.table(data()$results)$Item)
all_datasets <- sapply(as.list(as.data.table(data()$results)$Item), str_split, pattern=" ")
all_datasets <- sapply(all_datasets,"[[", 1)

ui <- fluidPage(
    selectInput("data", label = "Select Datatable",
              choices = all_datasets, selected = "mtcars"),
    uiOutput("modals"),
    DTOutput("table")
)

server <- function(input, output, session){
      observeEvent(input$data, {

        data_id <- input$data

        dat <- tryCatch(
          {
              get( data_id  )
          },
          warning= function(cond) {
            message(paste0(data_id, " not found."))
            message(cond)
            return
          })
        if(typeof(dat) != "list") {
          shinyalert(sprintf("%s not a list", data_id))
          return()
        }

        if(length(dat) == 0) {
          shinyalert(sprintf("%s is empty", data_id))
          return()
        }
        if(length( colnames(dat) ) > 1) {
          colnames(dat) <- gsub("-", "_", colnames(dat))
        }

        buttons <- create_buttons(dat, file = data_id)

        output[["table"]] <- DT::renderDataTable({
          create_datatable(dat, buttons)
        })

        # modals ####
        output[["modals"]] <- renderUI({
          create_modals(dat, file = data_id)
        })


        # plots in modals ####
        for(i in 1:ncol(dat)){
          local({
            ii <- i
            print(ii)
            output[[paste0("plot",ii)]] <- renderPlot({
              create_modal_plots(input, dat, file=data_id, index=ii)
            })
          })
        }

      })


}

shinyApp(ui, server)
