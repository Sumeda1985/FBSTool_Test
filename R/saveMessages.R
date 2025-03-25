saveMessages=function(input,output,session, buttons){
  
  
  lapply(buttons, function(x){
    
    
    
    observeEvent(input[[paste(x)]], {
      if(x == "residualSave"){
      sendSweetAlert(
        session = session,
        title = "Saved !!",
        text = "All Modules have been updated successfully. You can proceed with the compilation of FBS.",
        type = "success"
      )
      }else if(grepl(x,"_impute")){
        sendSweetAlert(
          session = session,
          title = "Imputed !!",
          text = "Missing values have been imputed successfully. Please refer to the manual for the methodology applied.",
          type = "success"
        )
        
      }else{
        sendSweetAlert(
          session = session,
          title = "Saved !!",
          text = "Your entries have been added to the Data.",
          type = "success"
        )
      }
    }
    )
    
  })
    
 
  
  
  # observeEvent(input$foodSave, {
  #    if (input$endyear %in% c(2010:2013)){
  # 
  #      sendSweetAlert(
  #        session = session,
  #        title = paste("Tourist Food is not an element in ",input$endyear, "  !!", sep = ""),
  #        text = "Moving to Loss Module",
  #        type = "info"
  #      )
  # 
  #    }
  # 
  # })
  
  
   
  observeEvent(input$seedSave, {
    if (input$endyear %in% c(2010:2013)){
      
      sendSweetAlert(
        session = session,
        title = paste("Industrial use is not an element in ",input$endyear, "  !!", sep = ""),
        text = "Moving to Residual and Other Uses Module",
        type = "info"
      )
      
    }
    
  })}