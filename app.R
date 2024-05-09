ui <- page_fluid(
  theme = bs_theme(5,
                   secondary = "#B23F00",
                   base_font = font_google("Inter")
                   ),
  layout_column_wrap(
    card(
      card_header("Deduplikacja"),
      card_body(
        fileInput("file_to_dedupe", "Plik .xlsx do deduplikacji", accept = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", buttonLabel = "Szukaj...", placeholder = "Nie wybrano pliku"),
        layout_column_wrap(width = 1/3, fill = FALSE,
                           selectInput("cols_to_dedupe", label = "Kolumny do deduplikacji", choices = NULL, multiple = TRUE),
                           selectInput("cols_to_extend", label = "Kolumny do rozszerzenia", choices = NULL, multiple = TRUE),
                           numericInput("max_new_cols", label = "Max. liczba kolumn", value = 0, min = 0),
                           input_task_button("dedupe_btn", label = "Deduplikuj"),
                           downloadButton("download_btn", label = "Pobierz")
                           ),
        textOutput("result_message")
      )
    ),
    card(
      card_header("Instrukcja"),
      card_body(
        tags$ol(
          tags$li("Należy wybrać plik o rozszerzeniu .xlsx, w którym dane do deduplikacji będą znajdować się w pierwszym arkuszu."),
          tags$li("Następnie należy wybrać kolumny, na podstawie których odbędzie się deduplikacja, np. kolumny z numerami telefonów."),
          tags$li("Następny krok to wybór kolumn, które zostaną poszerzone o niepowtarzające się informacje z rekordów, które zostały uznane za zdublowane (inne kolumny niż te wykorzystane do deduplikacji)."),
          tags$li("W ostatnim kroku wybieramy, ile maksymalnie nowoutworzonych kolumn chcemy. Jeśli zero - oznacza to, że tyle kolumn zostanie zachowanych, ile potrzeba, żeby zachować wszystkie informacje z kolumn użytych do deduplikacji."),
          tags$li("Jeśli w pliku były kolumny z datami, to teraz będą miały reprezentację numeryczną - należy je ponownie przeformatować na datę.")
        ),
        tags$p("Jako przykład rozważmy plik składający się z pięciu kolumn: trzy pierwsze z numerów telefonów, dalej: kolumny z nazwą i kolumny mówiącej o źródle pochodzenia rekordu. Przypuśćmy, że do deduplikacji wybrano wszystkie kolumny z telefonami, a jako kolumny, które zostaną rozszerzone, wybrano kolumnę z nazwą. Następnie załóżmy, że rekord pierwszy i drugi okazał się zdublowany. W wyniku deduplikacji rekord pierwszy i drugi będą miały nastepującą postać w wynikowej bazie: zostaną zamienione na jeden rekord taki, że wszystkie niepowtarzające się numery telefonów ze zdublowanych rekordów zostaną zachowane, a także zostaną zachowane obie nazwy tych rekordów, o ile nie były powtórzone - wszystkie numery telefonów oraz wszystkie nazwy zostaną podzielone na osobne kolumny. Natomiast w przypadku kolumny mówiącej o źródle pochodzenia rekordu, zachowana zostanie tylko jedna wartość.")
      )
    )
  )
)

server <- function(input, output, session) {
  
  uploaded_file <- reactive({
    req(input$file_to_dedupe$datapath)
    req(tools::file_ext(input$file_to_dedupe$datapath) == "xlsx")
    readxl::read_xlsx(input$file_to_dedupe$datapath, guess_max = 1000000)
  })
  
  observe({
    req(uploaded_file())
    updateSelectInput(inputId = "cols_to_dedupe", choices = names(uploaded_file()))
  })
  
  observe({
    req(uploaded_file())
    req(input$cols_to_dedupe)
    updateSelectInput(inputId = "cols_to_extend", choices = names(uploaded_file())[!names(uploaded_file()) %in% input$cols_to_dedupe])
  })
  
  file_deduped <- reactive({
    req(uploaded_file())
    req(length(input$cols_to_dedupe) >= 2)
    req(input$max_new_cols)
    max_cols <- input$max_new_cols
    if (max_cols < 1) {
      max_cols <- NULL
    }
    tryCatch(dedupewider::dedupe_wide(uploaded_file(), input$cols_to_dedupe,
                             cols_expand = input$cols_to_extend,
                             max_new_cols = max_cols),
             error = function(e) paste0("Wystąpił błąd podczas deduplikacji: ", e))
  }) |> 
    bindEvent(input$dedupe_btn)
  
  output$result_message <- renderText({
    req(file_deduped())
    if (inherits(file_deduped(), "character")) {
      file_deduped()
    } else {
      "Gotowe"
    }
  })
  
  output$download_btn <- downloadHandler(
    filename = function() {
      req(input$file_to_dedupe$datapath)
      paste0("after_deduplication_", basename(input$file_to_dedupe$datapath))
    },
    content = function(file) {
      req(inherits(file_deduped(), "data.frame"))
      writexl::write_xlsx(file_deduped(), file, format_headers = FALSE)
    }
  )
  
}

shinyApp(ui, server)
