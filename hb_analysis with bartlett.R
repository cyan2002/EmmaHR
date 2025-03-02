#copied over for Emma's work
Sys.setenv(TZ = "UTC")


#install.packages("tidyverse")

#install.packages("lubridate")

#install.packages("shiny")
library(signal)
library(tidyverse)
library(lubridate)
library(shiny)


# it may be necessary to adjust the height of the 2 plots for them to fit in your screen
height <- 700
h1 <- str_c(2 * (height / 5), "px")
h2 <- str_c(3.5 * (height / 5), "px")

# the batch of files to be processed must be inside a single folder, with NO subfolders
# navigate to the desired folder and choose one of the files
#path <- file.choose() %>% dirname %>% str_c("/")
path <- "/Users/chanceyan/Documents/R/EmmaHR/EmmaHR/CardiacF(x)_converted/F2_Trial2_22C_converted"

# list target files
targetFiles <- dir(path, pattern = ".txt", ignore.case = TRUE, full.names = TRUE)
targetFiles <- targetFiles[!grepl("output", targetFiles)]
targetFiles <- targetFiles[file.info(targetFiles)$size != 0]

# ATTENTION: setting restart to TRUE will DELETE the "_output.csv" file!
# restart <- TRUE
restart <- TRUE
outputFiles <- list(RData = str_c(path, "_output.RData"), csv = str_c(path, "_output.csv"))

# if not present, generate an output file
if (!file.exists(outputFiles$RData) | !file.exists(outputFiles$csv) | restart) {
  tmp <- list()
  for (i in 1:length(targetFiles)) {
    raw <- suppressMessages(read_tsv(targetFiles[i], col_names = c("hb")))
    rawdata=raw
    rawdata$id=basename(targetFiles[i])
    rawdata=rawdata %>% separate(id, c("id","empty"), ".txt")
    # rawdata=rawdata %>% separate(id, c("input","group","date","hms"), "_")
    rawdata=rawdata %>% separate(id, c("input","group","exp","trial","treat","date","hms"), "_")# will need changing depending on file format
    rawdata$time <- paste(rawdata$date, rawdata$hms, sep="_")
    rawdata$time=as.POSIXlt(rawdata$time,    format = "%Y%m%d_%H%M%S")
    dev_id <- rawdata$group[1]
    file_id <- rawdata$hms[1]
    sensor <- rawdata$input[1]
    t0     <- rawdata$time[1]
    
    raw$time=row(raw)*(1000/40) # need to be changed depending on sample rate (in HZ)
    raw <- mutate_all(raw, as.numeric)
    raw$time <- t0 + (raw$time / 1000) - as.numeric(t0)
    
    
    tmp[[i]] <- list(dev_id = dev_id, file_id = file_id, sensor = sensor, t0 = t0, val = raw)
  }
  
  dat <- tibble(fname = basename(targetFiles))
  dat$date    <- map(tmp, "t0") %>% do.call(c, .)
  dat$dev_id  <- map_chr(tmp, "dev_id")
  dat$file_id  <- map_chr(tmp, "file_id")
  dat$sensor  <- map_chr(tmp, "sensor")
  dat$freq    <- NA %>% as.numeric
  dat$conf    <- NA %>% as.character
  dat$start_time <- NA
  dat$end_time <- NA
  write_csv(dat, path = outputFiles$csv)
  
  dat$dat <- map(tmp, "val")
  save(dat, file = outputFiles$RData)
}

# load the output file
load(outputFiles$RData)

# initiate plot
ui <- fluidPage(
  fluidRow(
    column(2,
           verbatimTextOutput("info1")),
    
    column(6,
           plotOutput('plot2', height = h1)),
    
    column(2, 
           plotOutput('diagnosticplot', height = h1)),
    
    column(2,
           verbatimTextOutput("info2"))),                                                
  
  
  
  plotOutput('plot1', height = h2, brush = brushOpts(id = "plot1_brush", direction = "x", resetOnNew = FALSE)),
  
  hr(),
  
  fluidRow(
    column(2,
           sliderInput("bandwidth", label = "bandwidth", min = 0.1, max = 10, value = 1, step = 0.05)),
    
    column(1,
           numericInput("manual_hr", label = "Manual Heart Rate", value = 1)),
    column(1,
           radioButtons("apply_manual", "Manual override?", choices = list("Yes" = "Yes", "No" = "No"), selected = "No")),
    
    
    column(1,
           radioButtons("multiplier", label = "multiplier", choices = list("0.5x" = 0.5, "1x" = 1, "2x" = 2), selected = 1)),
    
    column(1,
           numericInput("f", label = "file", value = dat$freq %>% is.na %>% which %>% first, min = 1, max = nrow(dat))),
    
    column(1,
           radioButtons("conf", label = "confidence", choices = list("OK" = "OK", "Low" = "Low", "Discard" = "Discard", "Flatline" = "Flatline", "Zero" = "Zero"), selected = "OK")),
    
    column(2,
           sliderInput("edgeBufferL",  label = "remove x secs (LEFT, RIGHT)", min = 0, max = 450, value = 10, step = 2),
           sliderInput("edgeBufferR",  label = NULL, min = 0, max = 450, value = 2, step = 2)),
    
    column(1,
           actionButton("SAVE",  label = "SAVE")),
    
    column(1,
           radioButtons("apply_bartlett", "Apply Bartlett?", choices = list("Yes" = "Yes", "No" = "No"), selected = "No")),
    column(2,
           sliderInput("bartlett_windowsize", label = "Bartlett window size", min = 10, max = 100, value = 20, step = 10))))






server <- function(input, output) {
  
  ranges <- reactiveValues(x   = NULL)
  
  # when a double-click happens, check if there's a brush on the plot
  # if so, zoom to the brush bounds, if not, reset the zoom
  observe({
    brush <- input$plot1_brush
    if(!is.null(brush)) {
      ranges$x <- as.POSIXct(c(brush$xmin, brush$xmax), origin = "1970-01-01")
    }else{
      ranges$x <- NULL
    }
  })
  
  
  
  
  observeEvent(input$SAVE, {
    save(dat, file = outputFiles$RData)
    write_csv(select(dat, -dat), path = outputFiles$csv)
  })
  
  prepareData <- reactive({
    
    
    
    #input <- list(bandwidth = 1.5, multiplier = 1, showSmooth = TRUE, showPeaks = TRUE, f = 1, edgeBufferL = 10, edgeBufferR = 2, conf = "OK"); ranges <- list(x = NULL)
    x  <- dat[input$f,] %>% as.list
    
    hb <- x$dat[[1]]
    
    if(input$apply_bartlett=="Yes") {
      windowsize = input$bartlett_windowsize #change this to specify window size for the Bartlett window
      window <- bartlett(windowsize)
      convolved_data <- convolve(hb$hb, window, type = "open")
      hb$hb <- convolved_data[((windowsize/2)+1):(length(convolved_data) - ((windowsize/2)-+1))]
    } 
    
    
    hb$time <- hb$time - as.numeric(first(hb$time))
    hb <- hb[hb$time >= first(hb$time) + input$edgeBufferL & hb$time <= last(hb$time) - input$edgeBufferR, ]
    hb2 <- hb
    if(!is.null(ranges$x)) hb2 <- hb2[hb2$time >= ranges$x[1] & hb2$time <= ranges$x[2], ]
    smooth <- ksmooth(hb2$time, hb2$hb, "normal", input$bandwidth) %>% as_tibble
    peaks  <- smooth$y %>% diff %>% sign %>% diff %>% "=="(-2) %>% which %>% "+"(1)
    gaps <- diff(peaks)/40
    peakshort <- peaks[-length(peaks)]
    std_err <- sd(gaps) / sqrt(length(gaps))
    freq <- (length(peaks) - 1) / as.numeric(smooth$x[last(peaks)] - as.numeric(smooth$x[first(peaks)]))
    freq <- round(freq * as.numeric(input$multiplier), 2)
    if(input$apply_manual=="Yes") {
      freq <- (input$manual_hr/as.integer((seconds(last(hb2$time))-seconds(first((hb2$time))))))
      std_err<- 0
    }
    
    dat_sensor     <- filter(dat, sensor == x$sensor)
    n_files_sensor <- dat_sensor %>% nrow
    n_this_file    <- dat_sensor %>% pull(fname) %>% str_detect(x$fname) %>% which
    
    prev_freq2 <- prev_freq1 <- ""
    if(n_this_file > 2) prev_freq2 <- if(dat_sensor[n_this_file - 2,]$conf == "Discard") "NA" else dat_sensor[n_this_file - 2,]$freq
    if(n_this_file > 1) prev_freq1 <- if(dat_sensor[n_this_file - 1,]$conf == "Discard") "NA" else dat_sensor[n_this_file - 1,]$freq
    
    dat[input$f,]$freq <- freq
    #dat[input$f,]$std_err <- std_err
    dat[input$f,]$start_time <- as.numeric(min(hb2$time))
    dat[input$f,]$end_time <- as.numeric(max(hb2$time))
    dat[input$f,]$conf <- input$conf
    if(input$f < nrow(dat)) {
      dat[(input$f + 1):nrow(dat),]$freq <- NA
      dat[(input$f + 1):nrow(dat),]$conf <- NA
    }
    dat <<- dat
    
    x <- list(
      info = head(x, -3), n_files_sensor = n_files_sensor, n_this_file = n_this_file,
      hb = hb, smooth = smooth, peaks = peaks, freq = freq, gaps=gaps, peakshort=peakshort, std_err=std_err, bpm = round(freq * 60, 1),
      prev_freq2 = prev_freq2, prev_freq1 = prev_freq1)
  })
  
  prepareText <- reactive({
    x  <- prepareData()
    
    t1 <- str_c("  file: ", x$info$fname, "\n") %>%
      str_c(    "        ", x$n_this_file, " of ", x$n_files_sensor, " for this sensor\n\n") %>%
      str_c(    "  date: ", as.Date(x$info$date), "\n") %>%
      str_c(    "  time: ", str_sub(x$info$date, 12, 16), "\n\n") %>%
      str_c(    "dev_id: ", x$info$dev_id, "\n") %>%
      str_c(    "file_id: ", x$info$file_id, "\n") %>%
      str_c(    "sensor: ", x$info$sensor, "\n")
    
    t2 <- str_c("previous files ---\nfreq -2: ", x$prev_freq2, " Hz\n") %>%
      str_c("freq -1: ", x$prev_freq1, " Hz\n\n") %>%
      str_c("this file --------\n  peaks: ", length(x$peaks), "\n") %>%
      str_c("seconds: ", difftime(last(x$smooth$x), first(x$smooth$x), units = "secs") %>% round %>% as.numeric, "\n") %>%
      str_c("   freq: ", x$freq, " Hz\n") %>%
      str_c("   freq: ", x$bpm, " Bpm\n") %>%
      str_c("   std_err: ", x$std_err, "\n")
    
    list(t1, t2)
  })
  
  #observeEvent(input$apply_manual, {
  #  if (!is.na(input$manual_hr)) {
  #    dat[input$f,]$freq <- input$manual_hr
  #    save(dat, file = outputFiles$RData)
  #    write_csv(select(dat, -dat), path = outputFiles$csv)
  #  }
  #})
  
  
  
  output$plot1 <- renderPlot({
    x <- prepareData()
    p <- ggplot() +
      #coord_cartesian(ylim = c(-1, 5)) +
      geom_line(data = x$hb, aes(time, hb), col = "grey90", size = 1.5) +
      theme_classic() +
      geom_line( data = x$smooth, aes(x, y), col = "red", size = 1) +
      geom_point(data = x$smooth[x$peaks,], aes(x, y), col = "blue", size = 2)
    p
  })
  
  output$plot2 <- renderPlot({
    x <- prepareData()
    p <- ggplot() +
      coord_cartesian(xlim = ranges$x, expand = 0) +
      geom_line(data = x$hb, aes(time, hb), col = "white", size = 1) +
      theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
    
    p <- p + geom_line( data = x$smooth, aes(x, y), col = "red", size = 1)
    p <- p + geom_point(data = x$smooth[x$peaks,], aes(x, y), col = "blue", size = 2)
    p
  })
  
  
  output$diagnosticplot <- renderPlot({
    # Calculate the differences between successive heartbeats
    x <- prepareData()
    #qqplot(x$gaps, (qunif(ppoints(length(x$gaps)))))
    qqnorm(x$gaps, pch = 1, frame = FALSE)
    qqline(x$gaps, col = "steelblue", lwd = 2)
  })
  
  
  
  output$info1 <- renderText({
    t <- prepareText()
    t[[1]]
  })
  
  output$info2 <- renderText({
    t <- prepareText()
    t[[2]]
  })
}


shinyApp(ui = ui, server = server)
