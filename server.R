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
    CUOTA <- numeric()
    ABONO <- numeric()
    ABONO.ACUMULADO <- numeric()
    MATRICULA <- numeric()
    MANEJO <- numeric()
    EXITO <- numeric()
    MES <- numeric()
    PAGOS <- numeric()
   
    
    t.pago <- function(d, n_d, matricula, mensual, p, tasa.exito){ # d=deuda n_d=nuevadeuda hrs=honorarios_fine p=periodos
      
        SALDO.INICIAL[1] <<- round(n_d)
        PAGOS[1] <<- round((n_d/p)+((matricula)/p)+((d*tasa.exito)/p)+(mensual), digits = 1)
        
        for(j in 1:p){
            MES[j] <<- j
            CUOTA[j] <<- round((n_d/p)+((matricula)/p)+((d*tasa.exito)/p)+(mensual), digits = 1)
            ABONO[j] <<-  round(n_d/p, digits = 1)
            ABONO.ACUMULADO[j] <<- round(sum(ABONO[1:j]), digits = 1)
            PAGOS[j] <<- round(CUOTA[1]*j, digits = 1) 
            SALDO.INICIAL[j+1] <<- round(SALDO.INICIAL[j]-ABONO[j], digits = 1)
            MATRICULA <<- round(matricula/p, digits = 2)
            MANEJO <<- round(mensual, digits = 1)
            EXITO <<- round(d*tasa.exito/p)
            
            
        }
        
    }
    

###             VALORES REACTIVOS       ####
    
    tiempo_mora <- reactive({
     as.numeric(ymd(c(Sys.Date()))-ymd(c(input$inicio.mora))) 
       
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
      ifelse(tiempo_mora() < 90, 0, promesa_descuento()+promesa.por.mora())
    })
    
    
    nivel_reduc <-reactive({
        #input$`%_reduccion`/100
        promesa_descuento1()/100
    })
    ahorro <- reactive({
        input$deuda*nivel_reduc()
    })
    prima_exito <- reactive({
        (input$deuda*nivel_reduc())*input$tasa_exito
    })
    
    matricula <- reactive({
        input$deuda*input$tasa_matricula
    })
    
    hrs_mensuales <- reactive({
        input$deuda*input$tasa_manejo
    })
    
    total_hrs <- reactive({
        # (input$deuda*0.021+(input$deuda*0.006*(input$tiempo-1)))
      (input$deuda*0.021+(input$deuda*0.006*(input$time-1)))
    })
    
    beneficios <- reactive({
        prima_exito()+total_hrs()+matricula()
    })
    
    nuevo_saldo_sf <- reactive({
        input$deuda*(1-nivel_reduc())
    })
    
    total_pago_fine <- reactive({
        nuevo_saldo_sf()+beneficios()
    })
    
    cuota <- reactive({
      #function(d, n_d, matricula, mensual, p, tasa.exito)
      round((nuevo_saldo_sf()/input$time)+((matricula())/input$time)+((input$deuda*input$tasa_exito)/input$time)+(hrs_mensuales()), digits = 1)
    })
    
    
###             OUTPUTS CALCULADORA     ####
    output$time.difference<-renderPrint({
      paste(tiempo_mora(), 'días de mora')
    })
    
    output$bono.mora <- renderPrint({
      cuota()
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
        'PROMESA DESCUENTO', icon = icon('piggy-bank'), color = 'yellow'
      )
    })
    
    output$cuota <- renderValueBox({
      valueBox(
        paste('$', round(cuota(), digits = -2)),
        'CUOTA', icon = icon('business-time'), color = 'green'
      )
    })
    

    ##          amortizacion (cuota minima para pago 170000) ####
    output$amortizacion <- renderDataTable({
        
        t.pago(input$deuda, nuevo_saldo_sf(), matricula(), hrs_mensuales(), input$time, input$tasa_exito)
        
        tabla.amort.fine <- cbind(MES,SALDO.INICIAL,CUOTA,ABONO,ABONO.ACUMULADO, PAGOS, MATRICULA, MANEJO, EXITO)
        
        tabla.amort.fine <- tabla.amort.fine[1:(nrow(tabla.amort.fine)-1),] %>%
            as.data.frame() 
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

})
