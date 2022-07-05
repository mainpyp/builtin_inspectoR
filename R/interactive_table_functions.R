library(shiny)
library(shinyBS)
library(DT)
library(ggplot2)
library(data.table)
library(plotly)

process_table <- function(data, id) {
  buttons <- create_buttons(data, id)
  datatable <- create_datatable(data, buttons)
  modals <- create_modals(data, id)
  get_datatable()
}

create_buttons <- function(dat, file) {
    if(length(dat) != 0 ){
        lapply(1:ncol(dat), function(i) {
            actionButton(
                paste0("this_id_is_not_used", i),
                "Column information",
                class = "btn-primary btn-sm",
                style = "border-radius:5px;",
                onclick = sprintf(
                    "Shiny.setInputValue('%sbutton', %d, {priority:'event'});
                     $('#%smodal%d').modal('show');",  file, i, file,  i)
            )
        })
    }
}

# Arguments
# editable = T,
# filter = "top",
# were removed from datatable() because of perform-ace loss
# to change the underlying datatable see this tutorial:
#
# https://yihui.shinyapps.io/DT-edit/
# https://rstudio.github.io/DT/shiny.html
create_datatable <- function(dat, buttons, y_size=800) {
  sketch <- tags$table(
    class = "row-border stripe hover compact",
    tableHeader(c("", names(dat))),
    tableFooter(c("", buttons))  # add buttons to the footer row
  )
  datatable(
    dat, container = sketch,
    extensions = "Scroller",
    options =
      list(
        columnDefs = list(
          list(
            className = "dt-center",
            targets = "_all"
          )
        ),
        deferRender = TRUE,
        scrollX = 200,
        scroller = TRUE,
        scrollY = y_size
      )
  )
}

create_modals <- function(dat, file) {
  lapply(1:ncol(dat), function(i) {
    bsModal(
      id = paste0(file, "modal", i),
      title = names(dat)[i],
      trigger = paste0("this_is_not_used", i),
      if (is_continous(dat[[i]])) {
        fluidRow(
          column(5, radioButtons(paste0(file, "radio", i), "",
                                 c("density", "histogram", "point"),
                                 inline = TRUE
          )),
          column(
            7,
            conditionalPanel(
              condition = sprintf("input.%sradio%d=='histogram'", file, i),
              sliderInput(paste0(file, "slider", i), "Number of bins",
                          min = 5, max = 100, value = 30
              )
            )
          )
        )
      },
      plotOutput(paste0("plot", i))
    )
  })
}

create_modal_plots <- function(input, dat, file, index) {
  if( is_continous( dat[[index]]) ) {

    button_name <- paste0(file, "radio", index)

    if(input[[button_name]] == "density"){
      ggplot(dat, aes_string(gsub("-", "_", names(dat)[index]))) +
        geom_density(fill = "seashell", color = "seashell") +
        stat_density(geom = "line", size = 1) +
        theme_minimal() + theme(axis.title = element_text(size = 16))
    }else if (input[[button_name]] == "histogram") {
      ggplot(dat, aes_string(names(dat)[index])) +
        geom_histogram(bins = input[[paste0(file, "slider", index)]]) +
        theme_minimal() + theme(axis.title = element_text(size = 16))
    } else {
      ggplot(dat, aes_string(y = names(dat)[index])) +
        geom_point(aes(x = rownames(dat))) +
        theme_minimal() +
        theme(axis.title = element_text(size = 16, angle = 90))
    }
  } else {
    dat[[".x"]] <-
      factor(dat[[index]], levels = names(sort(table(dat[[index]]),
                                            decreasing=TRUE)))
    gg <- ggplot(dat, aes(.x)) + geom_bar() +
      geom_text(stat="count", aes(label=..count..), vjust=-0.5) +
      xlab(names(dat)[index]) + theme_minimal()
    if(max(nchar(levels(dat$.x)))*nlevels(dat$.x)>40){
      gg <- gg + theme(axis.text.x =
                         element_text(size = 12, angle = 45,
                                      vjust = 0.5, hjust = 0.5))
    }else{
      gg <- gg + theme(axis.text.x = element_text(size = 12))
    }
    gg + theme(axis.title = element_text(size = 16))
  }
}

is_continous <- function(column, n_unique = 19) {
  if( is.numeric( column ) && (length(unique( column )) > n_unique)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}


