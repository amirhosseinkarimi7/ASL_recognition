library(shiny)
library(tensorflow)
library(keras)
library(reticulate)
library(tidyverse)


server <- function(input, output) {
  
  #model <- keras::load_model_hdf5("brain_tumor_classification.keras")
  #setwd("~/Documents/kaggle_datasets/Cancer Image Classification")
  
  
  # Load the pre-trained model
  model <- keras::load_model_hdf5("ASL.keras")
  
  # Function to process image and make prediction
  predict_image <- function(image_path) {
    prediction <- image_load(image_path, target_size = c(100, 100)) %>%
      image_to_array() %>%
      array_reshape(dim = c(1, 100, 100, 3)) %>% # Add batch dimension (assuming 3 channels)
      model$predict() %>%
      { exp(.) / sum(exp(.)) }
    
    # Get the class name corresponding to the highest prediction
    class_index <- which.max(prediction)
    class_name <- switch(class_index,
                         '1'='A',
                         '2'='B', 
                         '3'='C',
                         '4'='D',
                         '5'='E',
                         '6'='F',
                         '7'='G',
                         '8'='H',
                         '9'='I',
                         '10'='J',
                         '11'='K',
                         '12'='L',
                         '13'='M',
                         '14'='N',
                         '15'='O',
                         '16'='P',
                         '17'='Q',
                         '18'='R',
                         '19'='S',
                         '20'='T',
                         '21'='U',
                         '22'='V',
                         '23'='W',
                         '24'='X',
                         '25'='Y',
                         '26'='Z',
                         '27'='del',
                         '28'='nothing',
                         '29'='space')
    
    score <- prediction[which.max(prediction)]
    
    return(list(class_name = class_name, score = score))
  }
  
  # When the predict button is clicked
  observeEvent(input$predict, {
    req(input$file)
    
    # Get the path of the uploaded file
    img <- input$file$datapath
    
    # Make prediction
    result <- predict_image(img)
    
    # Update outputs
    output$class_name <- renderText({
      paste("Predicted Class: ", result$class_name)
    })
    output$score <- renderText({
      paste("Confidence Score: ", result$score)
    })
    
    # Display the uploaded image
    output$uploaded_image <- renderImage({
      list(src = img, contentType = 'image/png', width = 300)
    })
  })
}


