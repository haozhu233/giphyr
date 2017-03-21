#' giphyr RStudio Addin
gif_Addin <- function() {
  addResourcePath("rsc",
                  system.file("gadgets/gif_Addin", package = "giphyr"))

  ui <- miniPage(
    tags$head(
      includeCSS(system.file("gadgets/gif_Addin/app.css", package = "giphyr"))
    ),
    gadgetTitleBar("Add GIFs"),
    miniContentPanel(
      fillRow(flex = c(1, 1, 1, 6, 3, 3), height = "45px",
              uiOutput("back_button"), uiOutput("forward_button"),
              uiOutput("random_button"),
              uiOutput("gif_search_box"),
              uiOutput("download_gif_button"),
              uiOutput("insert_gif_button")),
      fillRow(flex = c(3, 2), height = "55px",
              uiOutput("rating_ui"),
              uiOutput("download_size_ui")),
      uiOutput("preview"),
      htmlOutput("space_holder"),
      tags$div(htmlOutput("pagination_note"),
               class = "pagination_note pull-right"),
      tags$div(class = "giphylogo",
               img(src = file.path("rsc/giphy.png"), width = 100))
    ),
    includeScript(system.file("gadgets/gif_Addin/app.js", package = "giphyr"))
  )

  server <- function(input, output, session) {
    output$gif_search_box <- renderUI({
      textInput("search_text", label = NULL,
                placeholder = "Search all the GIFs",
                width = "98%")
    })

    output$download_gif_button <- renderUI({
      actionButton("download_gif", label = "Download",
                   icon = icon("download"),
                   width = "95%", class = "btn-primary")
    })

    output$insert_gif_button <- renderUI({
      actionButton("insert_gif", label = "Insert to Rmd",
                   icon = icon("file-text-o"),
                   width = "95%", class = "btn-success")
    })

    output$back_button <- renderUI({
      actionButton("pagination_back", label = NULL, width = "95%",
                   icon = icon("arrow-left"), title = "Previous Page")
    })

    output$forward_button <- renderUI({
      actionButton("pagination_forward", label = NULL, width = "95%",
                   icon = icon("arrow-right"), title = "Next Page")
    })
    output$random_button <- renderUI({
      actionButton("pagination_random", label = NULL, width = "95%",
                   icon = icon("gift"), title = "Go to a random page")
    })

    output$download_size_ui <- renderUI({
      radioButtons(
        "download_size",
        HTML('Download file size: <sup><i class="fa fa-question-circle-o" title',
             '="medium: max 2mb; large: max 5mb"></i></sup>'),
        c("medium" = "downsized", "large" = "downsized_medium", "original"),
        inline = T)
    })

    output$rating_ui <- renderUI({
      radioButtons(
        "rating", "Rating",
        c("G", "Y", "PG", "PG-13", "R"), inline = T
      )
    })

    values <- reactiveValues(offset = 0, pre_offsets = c(0), off_position = 1,
                             total_count = 0)

    observeEvent(input$pagination_back, {
      if (values$off_position >= 2) {
        values$off_position <- values$off_position - 1
        values$offset <- values$pre_offsets[values$off_position]
      }
    })

    observeEvent(input$pagination_forward, {
      if (values$off_position == length(values$pre_offsets)) {
        values$offset <- values$offset + 10
        values$pre_offsets <- c(values$pre_offsets, values$offset)
      } else {
        values$offset <- values$pre_offsets[values$off_position + 1]
      }
      values$off_position <- values$off_position + 1
    })

    preview_gifs <- reactive({
      req(input$search_text)
      out <- suppressWarnings(
        gif_search(input$search_text,
                   img_format = c("fixed_height_small", "downsized",
                                  "downsized_medium", "original"),
                   offset = values$offset,
                   rating = tolower(input$rating)))
      if (is.null(out)) return(NULL)
      return(out)
    })

    observeEvent(input$search_text, {
      if (!is.null(preview_gifs())) {
        values$total_count <- attr(preview_gifs(), "total_count")
        if (values$total_count > 4999) {
          values$total_count <- 4999
        }
      }
    })

    observeEvent(input$rating, {
      if (!is.null(preview_gifs())) {
        values$total_count <- attr(preview_gifs(), "total_count")
        values$offset <- 0
        values$pre_offsets <- c(0)
        values$off_position <- 1
        if (values$total_count > 4999) {
          values$total_count <- 4999
        }
      }
    })

    observeEvent(input$pagination_random, {
      if (values$off_position != length(values$pre_offsets)) {
        values$pre_offsets <- values$pre_offsets[1:values$off_position]
      }
      values$offset <- sample(10:(values$total_count - 10), 1)
      values$pre_offsets <- c(values$pre_offsets, values$offset)
      values$off_position <- values$off_position + 1
    })
    # output$values <- renderText({
    #   paste(
    #     values$offset, "--",
    #     paste(values$pre_offsets, collapse = ","), "--",
    #     values$off_position, "--", values$total_count
    #   )
    # })

    output$pagination_note <- function() {
      req(preview_gifs())
      page_note <- paste(
        attr(preview_gifs(), "offset") + 1, "~",
        attr(preview_gifs(), "offset") + attr(preview_gifs(), "count"),
        "of", attr(preview_gifs(), "total_count")
      )
      if (values$total_count != attr(preview_gifs(), "total_count")) {
        page_note <- paste(
          page_note,
          '<sup><i class="fa fa-warning small" title="Although we have a lot,',
          'GIPHY API only supports the first 5000 gifs. :P"></i></sup>'
          )
      }
      return(HTML(page_note))
    }

    observeEvent(input$search_text, {
      values$offset = 0
    })



    output$preview <- renderUI({
      req(preview_gifs())
      apply(preview_gifs(), 1, function(x) {
        actionLink(x[1], title = x[2], label = NULL, class = "gifpreview",
                   icon = NULL,
                   tags$img(src = x[6]))
      })
    })

    output$space_holder <- function() {
      if (is.null(input$search_text)) return(NULL)
      if (input$search_text != "") return(NULL)
      app_message <- paste0(
        '<p>Step 1. Search any keywords</p>',
        '<p>Step 2. Select a GIF you like</p><p>Step 3. Download it or directly ',
        'insert that to your Rmarkdown.</p><br>'
      )
      app_tips <- c(
        '<i class="fa fa-gift"></i> will drop you at a random page. Use it to discover more fun gifs!',
        'Unfortunately, GIPHY API can only export the first 5000 gifs. :(',
        'Downloaded gifs are saved in the "img" folder in your project directory.',
        "For download file sizes, <code>medium</code>: file < 2 MB, <code>large</code>: file < 5 MB & <code>original</code>: no restriction."
      )
      HTML(
        '<div class="spaceHolder">', app_message,
        'Tip: <br>', app_tips[sample(1:length(app_tips), 1)],
        '</div>'
      )
    }

    observeEvent(input$done, {
      invisible(stopApp())
    })

    observeEvent(input$download_gif, {
      if (!is.null(input$clickedgif) & !is.null(input$search_text)) {
        dir.create("img", showWarnings = F)
        dl_file_name <- paste0(
          "img/", search_query_parser(input$search_text),
          "_", input$clickedgif, ".gif"
        )
        status <- try(download.file(
          preview_gifs()[[input$download_size]][preview_gifs()$id == input$clickedgif],
          dl_file_name
        ), silent = T)
      }
    })

    observeEvent(input$insert_gif, {
      if (!is.null(input$clickedgif) & !is.null(input$search_text)) {
        dir.create("img", showWarnings = F)
        dl_file_name <- paste0(
          "img/", search_query_parser(input$search_text),
          "_", input$clickedgif, ".gif"
        )
        status <- try(download.file(
          preview_gifs()[[input$download_size]][preview_gifs()$id == input$clickedgif],
          dl_file_name
        ), silent = T)
        rstudioapi::insertText(
          paste0('![', search_query_parser(input$search_text), "](",
                 dl_file_name, ")\n"))
      }
    })
  }

  viewer <- dialogViewer("Add GIFs", width = 800, height = 600)
  runGadget(ui, server, viewer = viewer)
}
