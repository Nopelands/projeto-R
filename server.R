getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Define server logic required to draw a histogram
server <- function(input, output) {
    ################### INPUT ####################
    select_stock <- eventReactive(input$go, {
        
        stock_name <- input$stock
        twin <- input$true_date
        
        df_stock <- master_df %>% 
            filter(Index == stock_name) 
        ## FALTA -> FILTRAR O DF POR DATA!!
        
        return(df_stock)
    })
    
    select_stock_comp <- eventReactive(input$go_comp, {
      
      stock_name_comp <- input$stock_comp
      twin_comp <- input$true_date_comp
      
      df_stock_comp <- master_df %>% 
        filter(Index == stock_name_comp[1] | Index == stock_name_comp[2]) 
      ## FALTA -> FILTRAR O DF POR DATA!!
      
      return(df_stock_comp)
    })
    
    output$timedate <- renderUI({
        
        stock_name <- input$stock
        
        df <- master_df %>% 
            filter(Index == stock_name)
        
        min_time <- min(df$Date)
        max_time <- max(df$Date)
        dateRangeInput("true_date", "Período de análise",
                       end = max_time,
                       start = min_time,
                       min  = min_time,
                       max  = max_time,
                       format = "dd/mm/yy",
                       separator = " - ",
                       language='pt-BR')
    })
    
    output$timedate_comp <- renderUI({
        
        stock_name <- input$stock_comp
        
        df <- master_df %>% 
            filter(Index %in% stock_name)
        
        maxmin_time <- df %>% 
            group_by(Index) %>% 
            summarise(MD = min(Date)) %>% 
            .$MD %>% 
            max()
        
        minmax_time <- df %>% 
            group_by(Index) %>% 
            summarise(MD = max(Date)) %>% 
            .$MD %>% 
            min()
        
        min_time <- maxmin_time
        max_time <- minmax_time
        
        dateRangeInput("true_date_comp", "Período de análise",
                       end = max_time,
                       start = min_time,
                       min    = min_time,
                       max    = max_time,
                       format = "dd/mm/yy",
                       separator = " - ",
                       language='pt-BR')
    })
    
    ################ OUTPUT #####################
    Info_DataTable <- eventReactive(input$go,{
        df <- select_stock()
        
        max <- df %>% select(Close) %>% apply(2, max)
        Maximo <- max[[1]]
        
        min <- df %>% select(Close) %>% apply(2, min)
        Minimo <- min[[1]]
        
        standard_dev <- df %>% select(Close) %>% apply(2, sd)
        Desvio_padrao <- standard_dev[[1]]
        
        mode <- df %>% select(Close) %>% apply(2, getmode)
        Moda <- mode[[1]]
        
        median <- df %>% select(Close) %>% apply(2,median)
        Mediana <- median[[1]]
        
        mean <- df %>% select(Close) %>% colMeans()
        Media <- mean[[1]]
        
        Stock <- input$stock
        
        df_tb <-  data.frame(Stock, Media, Mediana, Moda, Desvio_padrao, Maximo, Minimo)
        
        df_tb <- as.data.frame(t(df_tb))
        
        # tb  <- as_tibble(cbind(nms = names(df_tb), t(df_tb)))
        # tb <- tb %>% 
        #     rename('Informações' = nms,
        #            'Valores' = V2)
        # 
        return(df_tb)
    })
    
    Info_DataTable_comp <- eventReactive(input$go_comp,{
      df <- select_stock_comp()
      stock_names_comp = input$stock_comp
      df_1 <- df %>% filter(Index == stock_names_comp[1])
      df_2 <- df %>% filter(Index == stock_names_comp[2])
      Correlação <- cor(df_1$Close, df_2$Close)
      
      df_tb <-  data.frame(Correlação)
      
      df_tb <- as.data.frame(t(df_tb))
      
      # tb  <- as_tibble(cbind(nms = names(df_tb), t(df_tb)))
      # tb <- tb %>% 
      #     rename('Informações' = nms,
      #            'Valores' = V2)
      # 
      return(df_tb)
    })
    
    output$info <- renderDT({
        Info_DataTable() %>%
            as.data.frame() %>% 
            DT::datatable(options=list(
                language=list(
                    url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
                )
            ))
    })
    
    output$info2 <- renderDT({
      Info_DataTable_comp() %>%
        as.data.frame() %>% 
        DT::datatable(options=list(
          language=list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
          )
        ))
    })
    
    output$sh <- renderPlot({
        # All the inputs
        df <- select_stock()
        
        aux <- df$Close %>% na.omit() %>% as.numeric()
        aux1 <- min(aux)
        aux2 <- max(aux)
        
        df$Date <- ymd(df$Date)
        a <- df %>% 
            ggplot(aes(Date, Close, group=1)) +
            geom_path() +
            ylab('Preço da Ação em $') +
            coord_cartesian(ylim = c(aux1, aux2)) +
            theme_bw() +
            scale_x_date(date_labels = "%Y-%m-%d")
        
        a
    })
    output$hh <- renderPlot({
      # All the inputs
      df <- select_stock()
      
      aux <- df$Close %>% na.omit() %>% as.numeric()
      aux1 <- min(aux)
      aux2 <- max(aux)
      
      df$Date <- ymd(df$Date)
      a <- df %>% 
        ggplot(aes(Close)) +
        geom_histogram() +
        theme_bw() +
        xlab('Preço da Ação em $')
      
      a
    })
    output$bh <- renderPlot({
      # All the inputs
      df <- select_stock()
      
      aux <- df$Close %>% na.omit() %>% as.numeric()
      aux1 <- min(aux)
      aux2 <- max(aux)
      
      df$Date <- ymd(df$Date)
      a <- df %>% 
        ggplot(aes(1, Close, group=1)) +
        geom_boxplot() +
        ylab('Preço da Ação em $') +
        theme_bw() +
        xlab('')
      
      a
    })
    output$sh_comp <- renderPlot({
      # All the inputs
      df <- select_stock_comp()
      stock_names_comp = input$stock_comp
      df_1 <- df %>% filter(Index == stock_names_comp[1])
      df_2 <- df %>% filter(Index == stock_names_comp[2])
      
      aux_1 <- df_1$Close %>% na.omit() %>% as.numeric()
      aux_2 <- df_2$Close %>% na.omit() %>% as.numeric()
      aux_min <- min(min(aux_1, aux_2))
      aux_max <- max(max(aux_1, aux_2))
      
      df_1$Date <- ymd(df_1$Date)
      df_2$Date <- ymd(df_2$Date)
      a <- ggplot() +
        geom_path(data = df_1, aes(Date, Close, colour = "red")) +
        geom_path(data = df_2, aes(Date, Close, colour = "blue")) +
        ylab('Preço da Ação em $') +
        coord_cartesian(ylim = c(aux_min, aux_max)) +
        theme_bw() +
        scale_x_date(date_labels = "%Y-%m-%d") +
        scale_color_brewer(labels=c(stock_names_comp[1], stock_names_comp[2]))
      
      a
    })
    output$mh_comp <- renderPlot({
      # All the inputs
      df <- select_stock_comp()
      stock_names_comp = input$stock_comp
      df_1 <- df %>% filter(Index == stock_names_comp[1])
      df_2 <- df %>% filter(Index == stock_names_comp[2])
      averages_df = data.frame(
        stock = c(stock_names_comp[1], stock_names_comp[2]),
        averages = c(df_1$Close %>% mean(),df_2$Close %>% mean())
        )
      
      
      aux_1 <- df_1$Close %>% na.omit() %>% as.numeric()
      aux_2 <- df_2$Close %>% na.omit() %>% as.numeric()
      aux_min <- min(min(aux_1, aux_2))
      aux_max <- max(max(aux_1, aux_2))
      
      a <- ggplot(averages_df, aes(x = stock, y = averages)) +
        geom_bar(stat = "identity") +
        ylab('Média de preços em $') +
        xlab('Ação')
      
      a
    })
    output$sph_comp <- renderPlot({
      # All the inputs
      df <- select_stock_comp()
      stock_names_comp = input$stock_comp
      df_1 <- df %>% filter(Index == stock_names_comp[1])
      df_2 <- df %>% filter(Index == stock_names_comp[2])
      
      scatter = data.frame(
        stock_1 = df_1$Close, stock_2 = df_2$Close
        )
      
      a <- ggplot(scatter, aes(x = stock_1, y = stock_2)) +
        geom_point() +
        ylab(stock_names_comp[2]) +
        xlab(stock_names_comp[1])
      a
    })
}
