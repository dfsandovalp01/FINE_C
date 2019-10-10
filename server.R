#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

{
    library(openxlsx)
    library(tidyr)
    library(dplyr)
    library(splitstackshape)
    library(readr)
    #library(tm)
    library(lubridate)
    library(rowr)
    library(shiny)
    library(shinydashboard)
    library(ggplot2)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

###             SCRIPT DATA ####
    ##          FUNCIONES ####
    add_cliente <- function(name, lastname, cc, comerc, tel, correo){
        old_reg <- read.csv('/home/daniel/FINANCARE/APP_WEB/FINE_DASH/www/clientes.csv')
        registro = matrix(c(name, lastname, cc, comerc, tel, correo), nrow = 1) %>%
            as.data.frame() %>%
            mutate_all(as.character, is.factor)
        names(registro) = c("NOMBRE", "APELLIDO", "DOCUMENTO", "ASESOR")
        registro = registro %>%
            mutate(TELEFONO = "telefono",
                   CORREO = "correo")
        nombre = c(name)
        apellido = c(lastname)
        cedula = c(cc)
        asesor = c(comerc)
        
        nuevo_cliente = data.frame(nombre, apellido, cedula, comerc)
        names(nuevo_cliente) <- c('nombre', 'apellido', 'cedula', 'asesor')
        
        write.csv(registro,'/home/daniel/FINANCARE/APP_WEB/FINE_DASH/www/clientes.csv')
        
    }
##              TABLA Y FCN AMORTIZACION ####
    
    options(digits = 7)
    options(scipen = 999)
    
    SALDO.INICIAL <- numeric()
    SALDO.INICIAL1 <- numeric()
    CUOTA <- numeric()
    CUOTA1 <- numeric()
    ABONO <- numeric()
    ABONO1 <- numeric()
    ABONO.ACUMULADO <- numeric()
    ABONO.ACUMULADO1 <- numeric()
    MATRICULA <- numeric()
    MATRICULA1 <- numeric()
    MANEJO <- numeric()
    MANEJO1 <- numeric()
    MANEJO.ACUMULADO <- numeric()
    MANEJO.ACUMULADO1 <- numeric()
    EXITO <- numeric()
    EXITO.ACUMULADO <- numeric()
    MES <- numeric()
    PAGOS <- numeric()
    PAGOS1 <- numeric()
   
    
    t.pago <- function(d, n_d, matricula, mensual, p, tasa.exito){ # d=deuda n_d=nuevadeuda hrs=honorarios_fine p=periodos
      
        SALDO.INICIAL[1] <<- round(n_d, digits = -1)
        CUOTA[1] <<- round((n_d/p)+((d*tasa.exito)/p)+(mensual), digits = 0) #((matricula)/p)+
        
        PAGOS[1] <<- round((n_d/p)+((matricula)/p)+((d*tasa.exito)/p)+(mensual), digits = -1)
        # MATRICULA1[1] <<- ifelse(matricula > CUOTA[1], 
        #                          CUOTA[1], 
        #                          matricula)
        MATRICULA1[1] <<- ifelse(input$diferir ==F,
                                 matricula,
                                 ifelse(matricula > CUOTA[1],
                                        CUOTA[1],
                                        matricula)
                                 )
        CUOTA1[1] <<- ifelse(input$diferir == F,
                             matricula,
                             # ifelse(matricula < CUOTA[1],
                             #        CUOTA[1],
                             #        matricula),
                             CUOTA[1])
        SALDO.INICIAL1[1] <<- 0 #round(n_d, digits = -1)
        #SALDO.INICIAL1[2] <<- round(n_d, digits = -1)
        ABONO1[1] <<- 0
          # ifelse(matricula < CUOTA1[1],
          #                    (CUOTA1[1] - MATRICULA1[1]),
          #                    0)
        MANEJO1[1] <<- 0
        EXITO[1] <<-0
        #MANEJO.ACUMULADO1[1] <<- 0
        
        for(j in 1:(p+1)){
            MES[j] <<- j
            CUOTA[j] <<- CUOTA[1] #round((n_d/p)+((matricula)/p)+((d*tasa.exito)/p)+(mensual), digits = -1)
            ABONO[j] <<-  round(SALDO.INICIAL[1]/p, digits = 0)
            ABONO.ACUMULADO[j] <<- round(sum(ABONO[1:j]), digits = 0)
            PAGOS[j] <<- round(CUOTA[1]*j, digits = -1) 
            SALDO.INICIAL[j+1] <<- round(SALDO.INICIAL[j]-ABONO[j], digits = -1)
            MATRICULA <<- round(matricula/p, digits = -1)
            MANEJO[j] <<- round(mensual, digits = -1)
            MANEJO.ACUMULADO[j] <<- round(sum(MANEJO[1:j]), digits = -1)
            EXITO[j+1] <<- round(d*tasa.exito/p)
            EXITO.ACUMULADO[j] <<- round(sum(EXITO[1:j]), digits = -1)
            
            
            CUOTA1[j+1] <<- CUOTA[1] 
            MATRICULA1[j+1] <<-  ifelse(sum(MATRICULA1[1:j]) < matricula,
                                        matricula - sum(MATRICULA1[1:j]),
                                        0)
            SALDO.INICIAL1[j+1] <<- ifelse(SALDO.INICIAL1[j] == 0,
                                           round(n_d, digits = -1),
                                           round(SALDO.INICIAL1[j]-ABONO1[j], digits = -1)
                                           )
            ABONO1[j+1] <<- ifelse(ABONO1[j] == 0,
                                   round(n_d/p, digits = 0),
                                   round(n_d/p, digits = 0)
                                   )
            ABONO.ACUMULADO1[j] <<- round(sum(ABONO1[1:j]), digits = 0)
            PAGOS1[j] <<- round(sum(CUOTA1[1:j]), digits = -1)
            MANEJO1[j+1] <<- round(mensual, digits = -1)
            MANEJO.ACUMULADO1[j] <<- round(sum(MANEJO1[1:j]), digits = 0) 
            
            
            
        }
        
    }
    

###             VALORES REACTIVOS       ####
    
    tiempo_mora <- reactive({
     as.numeric(ymd(c(Sys.Date()))-ymd(c(input$inicio.mora))) 
     #as.numeric(ymd(c(Sys.Date()))-ymd(c("2016-10-01")))  
    }) #valor numerico igual a dias entre fecha actual y fecha mora
    
    promesa_descuento <- reactive({ #((A8^2)-1)/A8
      ((((input$time^2)-1)/input$time) + 
        ((1/(input$maxima.promesa-(((input$time^2)-1)/input$time)))*200)) 
    }) #indice periodica
    
    promesa.por.mora <- reactive({
      ((((1-(((((48^2)-1)/48) + ((1/(input$maxima.promesa-(((48^2)-1)/48)))*200))/input$maxima.promesa))/input$maxima.promesa))*input$maxima.promesa)*input$maxima.promesa
      # ((1-
      # (((((48^2)-1)/48) + ((1/(input$maxima.promesa-(((48^2)-1)/48)))*200))/input$maxima.promesa)) * 100) * (1-(1/tiempo_mora()))
    })
    
    promesa_descuento1 <- reactive({
      ifelse(tiempo_mora() < 90, 0, promesa_descuento())#+promesa.por.mora())
    })
    
    
    nivel_reduc <-reactive({
        #input$`%_reduccion`/100
        #round(promesa_descuento1()/100, digits = -1)
      promesa_descuento1()/100
      #input$desc.ficti
    })
    ahorro <- reactive({
        round(input$deuda*nivel_reduc(), digits = -1)
    })
    prima_exito <- reactive({
        round((input$deuda*nivel_reduc())*input$tasa_exito, digits = -1)
    })
    
    matricula <- reactive({
        round(input$deuda*input$tasa_matricula, digits = -1)
    })
    
    hrs_mensuales <- reactive({
        round(input$deuda*input$tasa_manejo, digits = -1)
    })
    
    total_hrs <- reactive({
        # (input$deuda*0.021+(input$deuda*0.006*(input$tiempo-1)))
      round((input$deuda*input$tasa_matricula+(input$deuda*input$tasa_manejo*(input$time-1))), digits = -1)
      #prima_exito()+(hrs_mensuales*input$time)#+matricula()
    })
    
    beneficios <- reactive({
        round(prima_exito()+total_hrs(), digits = -1)#+matricula()
    })
    
    nuevo_saldo_sf <- reactive({
        input$deuda*(1-nivel_reduc())
    })
    
    total_pago_fine <- reactive({
        nuevo_saldo_sf()+beneficios()
    })
    
    cuota <- reactive({
      #function(d, n_d, matricula, mensual, p, tasa.exito)
      # ifelse(input$diferir == T, 
      #        round((nuevo_saldo_sf()/input$time)+((matricula())/input$time)+((input$deuda*input$tasa_exito)/input$time)+(hrs_mensuales()), digits = 1),
      #        round((nuevo_saldo_sf()/input$time)+((input$deuda*input$tasa_exito)/input$time)+(hrs_mensuales()), digits = 1))
      round((nuevo_saldo_sf()/input$time)+((input$deuda*input$tasa_exito)/input$time)+(hrs_mensuales()), digits = -1)
      # round((n_d/p)+((d*tasa.exito)/p)+(mensual), digits = -1) #((matricula)/p)+
      
    })
    
    time.amort <- reactive({
      input$time
      # ifelse(input$diferir == F,
      #        input$time + 1,
      #        input$time) 
    })
    
    ahorro.final <- reactive({
      round(input$deuda-(nuevo_saldo_sf()+beneficios()))
    })
    
    ahorro.final.M <- reactive({
      round(input$deuda-(nuevo_saldo_sf()+beneficios()+matricula()))
    })
    
###             OUTPUTS CALCULADORA     ####
    output$time.difference<-renderPrint({
      paste(tiempo_mora(), 'días de mora')
    })
    
    output$bono.mora <- renderPrint({
      promesa.por.mora()
      #input$diferir
      #cuota()
      #tiempo_mora()
      #promesa.por.mora()
      #promesa_descuento()
    })
    
    output$promesa_descuento <- renderPrint({
      input$time
    })
    
    output$prima_exito <- renderPrint({
        prima_exito()
        
    })
    output$matricula <- renderPrint({
        paste('$',matricula(), sep = '')
        
    })
    output$hrs_mensuales <- renderPrint({
        hrs_mensuales()
        
    })
    output$total_hrs <- renderPrint({
        total_hrs()
        
    })
    
    ###         OUTPUTS CAJAS ####
    
    output$beneficios <- renderValueBox({
        valueBox(
            paste("$",round(beneficios(), digits = -1)),
                  "FINE Profit", icon=icon("wallet"), color = "green", width = 4
        )
    })
    
    output$nuevo_saldo <- renderValueBox({
        valueBox(
            paste('$', round(nuevo_saldo_sf(), digits = -1), sep = ''),
                "NUEVO SALDO SF", icon = icon('bank'), color = 'yellow'
        )
    })
    
    output$ahorro_con_sf <- renderValueBox({
        valueBox(
            paste("$", round(ahorro(), digits = -1), sep = ""),
                "AHORRO CON SF", icon = icon("wallet"), color = "blue"
        )
    })
    
    output$prom.desc <- renderValueBox({
      valueBox(
        paste(round(promesa_descuento1(), digits = 1), '%', sep = ''),
        'REDUCCION BANCOS', icon = icon('piggy-bank'), color = 'yellow'
      )
    })
    
    output$cuota <- renderValueBox({
      valueBox(
        paste('$', round(cuota(), digits = -1)),
        'CUOTA', icon = icon('business-time'), color = 'green'
      )
    })
    
    output$ppt.cuota <- renderValueBox({##pagina pro
      valueBox(
        paste('$', round(cuota(), digits = -1)),
        'CUOTA', icon = icon('donate'), color = 'green'
      )
    })
    
    output$matricula.val <- renderValueBox({
      valueBox(
        paste('$', round(matricula(), digits = -1)),
        'MATRICULA', icon = icon('address-card'), color = 'blue'
      )
    })
    
    output$ppt.matricula.val <- renderValueBox({##pagina ppt
      valueBox(
        paste('$', round(matricula(), digits = -1)),
        'MATRICULA', icon = icon('address-card'), color = 'blue'
      )
    })
    
    output$ahorro.proceso <- renderValueBox({
      valueBox(
        paste(round((ahorro.final()/input$deuda)*100, digits = 0), '%'),
        'AHORRO FINAL', icon = icon('address-card'), color = 'blue'
      )
    })
    
    output$ppt.ahorro.proceso <- renderValueBox({
      valueBox(
        paste(round((ahorro.final()/input$deuda)*100, digits = 0), '%'),
        'AHORRO FINAL', icon = icon('piggy-bank'), color = 'blue'
      )
    })
    
    output$ppt.tiempo <- renderValueBox({
      valueBox(
        paste(input$time+1, ' meses', sep = ''),
        'TIEMPO', icon = icon('calendar-alt')
      )
    })
    
    output$ppt.deuda <- renderValueBox({
      valueBox(
        paste('$', input$deuda, sep = ''),
        'DEUDA INICIAL', icon = icon('file-invoice-dollar')
      )
    })
    
    output$ppt.nombre <- renderPrint({
      paste('Nombre: ', input$name.client, sep = '')
    })
    
    output$ppt.fecha <- renderPrint({
      paste('Nombre: ', input$name.client, sep = '')
    })

    ##          amortizacion (cuota minima para pago 170000) ####
    
    #boton generar tabla
    observeEvent(input$amort.bott, {
      output$amortizacion <-  renderDataTable({
      
      t.pago(input$deuda, nuevo_saldo_sf(), matricula(), hrs_mensuales(), input$time, input$tasa_exito)
      
      
      ifelse(input$diferir == T, 
             tabla.amort.fine <- cbind(MES,SALDO.INICIAL,CUOTA,ABONO,ABONO.ACUMULADO, PAGOS, MATRICULA, MANEJO, MANEJO.ACUMULADO, EXITO, EXITO.ACUMULADO, MATRICULA1, CUOTA1),
             tabla.amort.fine <- cbind(MES,CUOTA1,PAGOS1,SALDO.INICIAL1,ABONO1,ABONO.ACUMULADO1, MANEJO1, MANEJO.ACUMULADO1, EXITO, EXITO.ACUMULADO, MATRICULA1)
      )
      
      tabla.amort.fine <- tabla.amort.fine[1:(nrow(tabla.amort.fine)-1),] %>%
        as.data.frame()
      
      
    })}) 
    # output$amortizacion <-  renderDataTable({
    #   
    #   t.pago(input$deuda, nuevo_saldo_sf(), matricula(), hrs_mensuales(), input$time, input$tasa_exito)
    #   
    #   
    #   ifelse(input$diferir == T, 
    #          tabla.amort.fine <- cbind(MES,SALDO.INICIAL,CUOTA,ABONO,ABONO.ACUMULADO, PAGOS, MATRICULA, MANEJO, MANEJO.ACUMULADO, EXITO, EXITO.ACUMULADO, MATRICULA1, CUOTA1),
    #          tabla.amort.fine <- cbind(MES,SALDO.INICIAL1,CUOTA1,ABONO1,ABONO.ACUMULADO1, PAGOS1, MANEJO1, MANEJO.ACUMULADO1, EXITO, EXITO.ACUMULADO, MATRICULA1)
    #          )
    #   
    #   tabla.amort.fine <- tabla.amort.fine[1:(nrow(tabla.amort.fine)-1),] %>%
    #     as.data.frame()
    #   
    #          
    # }) 
    
   ## OFERTA
    output$texto.oferta <- renderText({
      paste('Cordial saludo ', input$name.client, '. \n', sep = '')
    })
    
    output$texto.oferta1 <- renderText({
      paste('En Financare estamos para ayudarte a recuperar tu tranquilidad. A continuaciòn te enviamos
            una propuesta para que puedas liquidar tus deudas. \n Muy pronto uno de nuestros analistas se pondra en contacto para continuar el proceso.', sep = '')
    })
    
    ##          Tabla Clientes ####
    
    data_client <- eventReactive(input$new_client,{
        add_cliente(input$client_name, input$client_apellido, input$client_cedula, input$client_asesor, input$client_numero, input$client_correo)
    })
    
    ##          grafico de reduccion ####
    output$resultados <- renderPlot({
        # load library
        library(ggplot2)
      
      # Create test data.
      data <- data.frame(
        category=c("Pago a Bancos", "Comisión FINE", "Ahorro"),
        count=c(round(nuevo_saldo_sf(), digits = -2), round(beneficios(), digits = -2), round(ahorro(), digits = -2))
      )
      
      # Compute percentages
      data$fraction <- data$count / sum(data$count)
      
      # Compute the cumulative percentages (top of each rectangle)
      data$ymax <- cumsum(data$fraction)
      
      # Compute the bottom of each rectangle
      data$ymin <- c(0, head(data$ymax, n=-1))
      
      # Compute label position
      data$labelPosition <- (data$ymax + data$ymin) / 2
      
      # Compute a good label
      data$label <- paste0(data$category, "\n value: ", data$count)
      
      # Make the plot
      ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
        geom_rect() +
        geom_text( x=2, aes(y=labelPosition, label=label, color=category), size=6) + # x here controls label position (inner / outer)
        scale_fill_brewer(palette=3) +
        scale_color_brewer(palette=3) +
        coord_polar(theta="y") +
        xlim(c(-1, 4)) +
        theme_void() +
        theme(legend.position = "none")
    })
    
    output$resultados1 <- renderPlot({
      # Create test data.
      data <- data.frame(
          category=c("Pago a Bancos", "Comisión FINE", "Ahorro"),
          count=c((nuevo_saldo_sf()), beneficios(), ahorro())
      )

      # Compute percentages
      data$fraction = data$count / sum(data$count)

      # Compute the cumulative percentages (top of each rectangle)
      data$ymax = cumsum(data$fraction)

      # Compute the bottom of each rectangle
      data$ymin = c(0, head(data$ymax, n=-1))

      # Make the plot
      ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
          geom_rect() +
          coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
          xlim(c(2, 4)) # Try to remove that to see how to make a pie chart
    })
    
    output$resultados2 <- renderPlot({
      # Create test data.
      data <- data.frame(
        category=c("Pago a Bancos", "Comisión FINE", "Ahorro"),
        count=c(round(nuevo_saldo_sf(), digits = -2), round(beneficios(), digits = -2), round(ahorro.final(), digits = -2))
      )
      
      # Compute percentages
      data$fraction <- data$count / sum(data$count)
      
      # Compute the cumulative percentages (top of each rectangle)
      data$ymax <- cumsum(data$fraction)
      
      # Compute the bottom of each rectangle
      data$ymin <- c(0, head(data$ymax, n=-1))
      
      # Compute label position
      data$labelPosition <- (data$ymax + data$ymin) / 2
      
      # Compute a good label
      data$label <- paste0(data$category, "\n value: ", data$count)
      
      # Make the plot
      ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
        geom_rect() +
        geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
        scale_fill_brewer(palette=4) +
        coord_polar(theta="y") +
        xlim(c(2, 4)) +
        theme_void() +
        theme(legend.position = "none")
    })
  
    output$profit <- renderPlot({
      # Create test data.
      data <- data.frame(
        category=c("PRIMA", "MANEJO"),
        count=c(round(prima_exito(), digits = 0), round(total_hrs(), digits = 0))
      )
      
      # Compute percentages
      data$fraction <- data$count / sum(data$count)
      
      # Compute the cumulative percentages (top of each rectangle)
      data$ymax <- cumsum(data$fraction)
      
      # Compute the bottom of each rectangle
      data$ymin <- c(0, head(data$ymax, n=-1))
      
      # Compute label position
      data$labelPosition <- (data$ymax + data$ymin) / 2
      
      # Compute a good label
      data$label <- paste0(data$category, "\n value: ", data$count)
      
      # Make the plot
      ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
        geom_rect() +
        geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
        scale_fill_brewer(palette=4) +
        coord_polar(theta="y") +
        xlim(c(2, 4)) +
        theme_void() +
        theme(legend.position = "none")
    })
    
    output$con.mat <- renderPlot({
      # Create test data.
      data <- data.frame(
        category=c("Pago a Bancos", "Comisión FINE", 'M', "Ahorro"),
        count=c(round(nuevo_saldo_sf(), digits = -2), round(beneficios(), digits = -2), matricula(), round(ahorro.final.M(), digits = -2))
      )
      
      # Compute percentages
      data$fraction <- data$count / sum(data$count)
      
      # Compute the cumulative percentages (top of each rectangle)
      data$ymax <- cumsum(data$fraction)
      
      # Compute the bottom of each rectangle
      data$ymin <- c(0, head(data$ymax, n=-1))
      
      # Compute label position
      data$labelPosition <- (data$ymax + data$ymin) / 2
      
      # Compute a good label
      data$label <- paste0(data$category, "\n value: ", data$count)
      
      # Make the plot
      ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
        geom_rect() +
        geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
        scale_fill_brewer(palette=4) +
        coord_polar(theta="y") +
        xlim(c(2, 4)) +
        theme_void() +
        theme(legend.position = "none")
    })
})
