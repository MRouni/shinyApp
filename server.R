library(shiny)
library("data.table")
library(dplyr)
options(shiny.maxRequestSize=30*1024^2)


shinyServer(function(input, output) {
    

    data_input <- reactive({
        req(input$csv_input)
        fread(input$csv_input$datapath)
    })
    

    
    observeEvent(data_input(),{
        choices <- c("Select",names(data_input()[,]))
        updateSelectInput(inputId = "var_1", choices = choices[c(1,20:23)])
        updateSelectInput(inputId = "var_2", choices = choices[c(1,20:23)])
        updateSelectInput(inputId = "var_0", choices = choices)
    })

    
    var_1 <- eventReactive(input$run_button,input$var_1)
    var_2 <- eventReactive(input$run_button,input$var_2)
    var_0 <- eventReactive(input$run_button2,input$var_0)
    
    

    plot_1 <- eventReactive(input$run_button ,{
        
        if (var_1() == "Select" || var_2() == "Select"){return (NULL)}
        
        df <- data.frame(data_input())
        
        
        df$BMIclass <- 0
        df[which(df$BMI>18.5 & df$BMI<24.99), 'BMIclass'] <- 1
        df[which(df$BMI>=24.99 & df$BMI<25), 'BMIclass'] <- 2
        df[which(df$BMI>=30), 'BMIclass'] <- 3
        df$BMIclass <- df$BMIc
        
        
        df <- df[,-5]
        
        y <- data.frame(df[,22])
        colnames(y) <- "BMI"
        
        dat1 <- df[,1:4]
        dat2 <- df[5:21]
        
        df <- cbind(dat1,y,dat2)
        
        
        
        df$MentHlth[df$MentHlth >= 0 & df$MentHlth < 6] = 0
        df$MentHlth[df$MentHlth >= 6 & df$MentHlth < 11] = 1
        df$MentHlth[df$MentHlth >= 11 & df$MentHlth < 16] = 2
        df$MentHlth[df$MentHlth >= 16 & df$MentHlth < 21] = 3
        df$MentHlth[df$MentHlth >= 21 & df$MentHlth < 26] = 4
        df$MentHlth[df$MentHlth >= 26 & df$MentHlth <= 30] = 5
        
        
        df$PhysHlth[df$PhysHlth >= 0 & df$PhysHlth < 6] = 0
        df$PhysHlth[df$PhysHlth >= 6 & df$PhysHlth < 11] = 1
        df$PhysHlth[df$PhysHlth >= 11 & df$PhysHlth < 16] = 2
        df$PhysHlth[df$PhysHlth >= 16 & df$PhysHlth < 21] = 3
        df$PhysHlth[df$PhysHlth >= 21 & df$PhysHlth < 26] = 4
        df$PhysHlth[df$PhysHlth >= 26 & df$PhysHlth <= 30] = 5
        
        
        
        df$HeartDiseaseorAttack <- as.factor(df$HeartDiseaseorAttack)
        levels(df$HeartDiseaseorAttack) <- c("No","Yes")
        
        df[,2]<-factor(df$HighBP,labels = c("No", "Yes"))
        df[,3]<-factor(df$HighChol,labels = c("No", "Yes"))
        df[,4]<-factor(df$CholCheck,labels = c("No", "Yes"))
        df[,5]<-factor(df$BMI,labels =  c("Underweight", "Normal","Obese"))
        df[,6]<-factor(df$Smoker,labels =  c("No", "Yes"))
        df[,7]<-factor(df$Stroke,labels =  c("No", "Yes"))
        df[,8]<-factor(df$Diabetes,labels =  c("No/Pregnancy", "Pre-/Borderline", "Yes"))
        df[,9]<-factor(df$PhysActivity,labels =  c("No", "Yes"))
        df[,10]<-factor(df$Fruits,labels =  c("No", "Yes"))
        df[,11]<-factor(df$Veggies,labels =  c("No", "Yes"))
        df[,12]<-factor(df$HvyAlcoholConsump,labels =  c("No", "Yes"))
        df[,13]<-factor(df$AnyHealthcare,labels =  c("No", "Yes"))
        df[,14]<-factor(df$NoDocbcCost,labels =  c("No", "Yes"))
        df[,15]<-factor(df$GenHlth,labels =  c("Excellent", "Good", "OK", "Not Good", "Poor"))
        df[,16]<-factor(df$MentHlth,labels =  c("0-5","6-10","11-15","16-20","21-25","26-30"))
        df[,17]<-factor(df$PhysHlth,labels =  c("0-5","6-10","11-15","16-20","21-25","26-30"))
        df[,18]<-factor(df$DiffWalk,labels =  c("No", "Yes"))
        df[,19]<-factor(df$Sex,labels =  c("Female", "Male"))
        df[,20]<-factor(df$Age,labels =  c("18-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+"))
        df[,21]<-factor(df$Education,labels =  c("Never attended school","Elementary","Some high school","High school graduate","Some college or technical school","College graduate"))
        df[,22]<-factor(df$Income,labels =  c("<$10k","$15k","$25k","$35k","$45k","$55k","$65k",">$75k"))
        df <- data.frame(df)
        
        group <- df %>% group_by(df[[var_2()]])
        
        
        data.frame(group_names <- group %>% group_keys())
        indices <- group %>% group_rows()
        
        
        if (length(indices)<=5){
            rows <- 1
            columns <- length(indices)
        }
        else if(length(indices)>5 & length(indices)<11){
            rows <- 2
            columns <- 5
        }
        else{
            rows <- 4
            columns <- 4
        }
        # browser()
            
        par(mfrow = c(rows, columns),xaxt='s',cex.lab = 1)
        group_names <- table(group_names)
        # browser()
        for (i in 1:length(indices)){
            # plot_ly(y = df[[var_1()]][unlist(indices[i])],xlab = names(group_names[i]),ylab = "", yaxt ='n',type = "box")
            
            boxplot(df[[var_1()]][unlist(indices[i])],xlab = names(group_names[i]),ylab = "", yaxt ='n',cex.lab=1.2, axes = FALSE)
            axis(side=2,at=c(1:length(levels(df[[var_1()]]))),labels = levels(df[[var_1()]]),las=2,cex.lab=1.2)
        }
        mtext(paste(var_1(),"grouped by",var_2(),sep = ' '), side = 3, line = -2, outer = TRUE)

    })
    
    
    plot_2 <- eventReactive(input$run_button2,{
        
        if (var_0() == "Select"){return (NULL)}
        
        
        df <- data.frame(data_input())
        
        
        df$BMIclass <- 0
        df[which(df$BMI>18.5 & df$BMI<24.99), 'BMIclass'] <- 1
        df[which(df$BMI>=24.99 & df$BMI<25), 'BMIclass'] <- 2
        df[which(df$BMI>=30), 'BMIclass'] <- 3
        df$BMIclass <- df$BMIc
        
        
        df <- df[,-5]
        
        y <- data.frame(df[,22])
        colnames(y) <- "BMI"
        
        dat1 <- df[,1:4]
        dat2 <- df[5:21]
        
        df <- cbind(dat1,y,dat2)
        
        
        
        df$MentHlth[df$MentHlth >= 0 & df$MentHlth < 6] = 0
        df$MentHlth[df$MentHlth >= 6 & df$MentHlth < 11] = 1
        df$MentHlth[df$MentHlth >= 11 & df$MentHlth < 16] = 2
        df$MentHlth[df$MentHlth >= 16 & df$MentHlth < 21] = 3
        df$MentHlth[df$MentHlth >= 21 & df$MentHlth < 26] = 4
        df$MentHlth[df$MentHlth >= 26 & df$MentHlth <= 30] = 5
        
        
        df$PhysHlth[df$PhysHlth >= 0 & df$PhysHlth < 6] = 0
        df$PhysHlth[df$PhysHlth >= 6 & df$PhysHlth < 11] = 1
        df$PhysHlth[df$PhysHlth >= 11 & df$PhysHlth < 16] = 2
        df$PhysHlth[df$PhysHlth >= 16 & df$PhysHlth < 21] = 3
        df$PhysHlth[df$PhysHlth >= 21 & df$PhysHlth < 26] = 4
        df$PhysHlth[df$PhysHlth >= 26 & df$PhysHlth <= 30] = 5
        

        df <- setDT(df)

        # browser()

        if (var_0()=="BMI"){
            labels = c("Underweight", "Normal","Obese")
        }
        else if (var_0()=="GenHlth"){
            labels = c("Excellent", "Good", "OK", "Not Good", "Poor")
        }
        else if (var_0()=="MenHlth" || var_0() =="PhysHlth"){
            labels = c("0-5","6-10","11-15","16-20","21-25","26-30")
        }
        else if(var_0()=="Diabetes"){
            labels = c("No/Pregnancy", "Pre-/Borderline", "Yes")
        }
        else if(var_0()=="Sex"){
            labels = c("Female", "Male")
        }
        else if(var_0()=="Age"){
            labels =  c("18-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")
        }
        else if (var_0()=="Education"){
            labels = c("Never attended school","Elementary","Some high school","High school graduate","Some college or technical school","College graduate")
        }
        else if (var_0()=="Income"){
            labels = c("<$10k","$15k","$25k","$35k","$45k","$55k","$65k",">$75k")
        }
        else{labels = c("No","Yes")}
        
        
        # browser()
        
        tab0 <- table(df[[var_0()]])
        
        barplot(tab0,
                main = var_0(),
                names.arg = labels,
                cex.axis=1.2, cex.names=1.2,
                font.main=20,
                line=0,
                # col = c("darkred","darkblue"),
                ylab = "Frequency",
                horiz = FALSE)
        
    })

    
    
    output$plot_1 <- renderPlot(plot_1(),height = 900, width = 1200)
    output$plot_2 <- renderPlot(plot_2(),height = 400, width = 600)
    
    
})
