
#Load Libraries #####
    library(caret)

#Read in Data #####
    data <- read.csv("./data/data.csv")
    dir.create(file.path("./Plots"), showWarnings = FALSE)

#Explore Data #####

    pdf("./Plots/Pairs.pdf", width = 20, height = 20)
    pairs(data[1:50,])
    dev.off()
    
    summary(data[1:50,])
    sapply(c(2:11), function(x) sd(data[1:50,x]))
    
    for (i in 1:10){
      
      pdf(paste0("./Plots/","S",i,".pdf"), width = 6, height = 6)
      plot(data[,i+1], type = "l", main= paste0("S",i," vs Time"), xlab= "Days", ylab = "Value", ylim = c(-10,10))
      grid()
      dev.off()
      
    }
    
    pdf("./Plots/AllLines.pdf", width = 10, height = 10)
    
    colors = c("red", "blue", "green", "orange", "yellow", "violet", "cyan", "coral", "brown")
    plot(data$S1, type = "l", main= "S1 - S10 vs Time", xlab= "Days", ylab = "Value", ylim = c(-10,10))
    for (i in 3:11){
      lines(data[,i], col = colors[i-2])
    }
    
    grid()
    dev.off()
    
    pdf("./Plots/TimeChange.pdf", width = 10, height = 10)
    TotChange <- data
    
    for (i in 2:11){
      TotChange[1,i+10] = 1
      for (j in 2:100){
        TotChange[j,i+10] <- TotChange[j-1,i+10]*(1+TotChange[j-1,i]/100)
      }
    }
    
    plot(TotChange$S1, type ="n", main = "Cumulative Change vs Time", xlab= "Days", ylab = "Value", ylim = c(0.8,1.5))
    grid()
    for (i in 13:21){
      lines(TotChange[,i], col = "grey")
    }
    lines(TotChange[,12], col = "red")
    legend("topleft",legend = c("S1", "S2-S10"),bg="white",fill = c("red", "grey"))
    dev.off()
    rm(TotChange,i,j)

#PreProcessing
    #Scale all Data
    ScaledData <- data
    range1 <- function(x){-1+2*(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))}
    for (i in 2:11){
      ScaledData[,i] <- range1(data[,i])
    }
  
    #Replot
    pdf("./Plots/AllLinesScaled.pdf", width = 10, height = 10)
    
    colors = c("red", "blue", "green", "orange", "yellow", "violet", "cyan", "coral", "brown")
    plot(ScaledData$S1, type = "l", main= "S1 - S10 vs Time", xlab= "Days", ylab = "Value", ylim = c(-1,1))
    for (i in 3:11){
      lines(ScaledData[,i], col = colors[i-2])
    }
    
    grid()
    dev.off()
    rm(i,range1,colors)

#Create Dataset(s) #####
    NoDates <- data[1:50,-1]
    NoDatesHistory <- cbind(data[3:50,-1], data[2:49,-1], data[1:48,-1])
    names(NoDatesHistory) <- paste0("S",c(1:30))
    NoDatesScaled <- ScaledData[1:50,-1]
    NoDatesScaledTest <- ScaledData[51:100,-1]
    Scores <- as.data.frame(matrix(nrow=9, ncol=3))
    names(Scores) <- c("Model","R2","RMSE")

    rm(ScaledData)

#Create a few models to see which one is the best with only looking at current row #####
  
    #Linear Regression
        set.seed(1)
        #Setup Repeated k-fold Cross Validation
        ctrl <- trainControl(method="repeatedcv", number=10, repeats=3)
        
        fitLM <- train(S1~.,
                   data = NoDates,
                   method="lm",
                   trControl=ctrl)
        #fitLM
        BestFit <- fitLM$results[fitLM$results$Rsquared == max(fitLM$results$Rsquared),]
        paste0("LM: ", round(BestFit$Rsquared,3))
        Scores$Model[1] <- "LM - NoDates"
        Scores$R2[1] <- round(BestFit$Rsquared,3)
        Scores$RMSE[1] <- round(BestFit$RMSE,3)
        
        pdf("./Plots/LMVarImp.pdf", width = 3, height = 5)
        LMImp <- varImp(fitLM, scale = FALSE)
        plot(LMImp, top = 5)
        dev.off()

        
        rm(fitLM, ctrl, LMImp)
      
      
    #Gradient Boosting
        set.seed(1)
        #Setup Repeated k-fold Cross Validation
        ctrl <- trainControl(method="repeatedcv", number=10, repeats=3)
        gridGBM<-expand.grid(interaction.depth = 3,
                           n.trees = c(5,10,15,20,25,30,35,40),
                           shrinkage = c(0.01,0.03,0.1,0.3,1,3,10,30),
                           n.minobsinnode = c(1,2,3,4))
        
        fitGBM<-train(S1~.,
                  data = NoDates,
                  method="gbm",
                  trControl=ctrl,
                  tuneGrid = gridGBM,
                  verbose = FALSE)
        #print(fit1)
        BestFit <- fitGBM$results[fitGBM$results$Rsquared == max(fitGBM$results$Rsquared),]
        paste0("GBM: ", round(BestFit$Rsquared,3))
        Scores$Model[2] <- "GBM - NoDates"
        Scores$R2[2] <- round(BestFit$Rsquared,3)
        Scores$RMSE[2] <- round(BestFit$RMSE,3)
        rm(fitGBM, ctrl, gridGBM)
  
    #Random Forest
        set.seed(1)
        #Setup Repeated k-fold Cross Validation
        ctrl <- trainControl(method="repeatedcv", number=10, repeats=3)
        gridRF<-expand.grid(mtry=c(1,2,3,4,5))
        
        fitRF<-train(S1~.,
                    data = NoDates,
                    method="rf",
                    trControl=ctrl,
                    tuneGrid = gridRF,
                    verbose = FALSE)
        #print(fit1)
        BestFit <- fitRF$results[fitRF$results$Rsquared == max(fitRF$results$Rsquared),]
        paste0("RF: ", round(BestFit$Rsquared,3))
        BestFit
        Scores$Model[3] <- "RF - NoDates"
        Scores$R2[3] <- round(BestFit$Rsquared,3)
        Scores$RMSE[3] <- round(BestFit$RMSE,3)
        rm(fitRF, ctrl, gridRF)

    rm(NoDates)

#Create a few models to see which one is the best with only looking at current row and last two #####
    
    #Linear Regression
        set.seed(1)
        #Setup Repeated k-fold Cross Validation
        ctrl <- trainControl(method="repeatedcv", number=10, repeats=3)
        
        fitLM <- train(S1~.,
                       data = NoDatesHistory,
                       method="lm",
                       trControl=ctrl)
        #fitLM
        BestFit <- fitLM$results[fitLM$results$Rsquared == max(fitLM$results$Rsquared),]
        paste0("LM: ", round(BestFit$Rsquared,3))
        Scores$Model[4] <- "LM - NoDatesHistory"
        Scores$R2[4] <- round(BestFit$Rsquared,3)
        Scores$RMSE[4] <- round(BestFit$RMSE,3)
        rm(fitLM, ctrl)
    
    
    #Gradient Boosting
        set.seed(1)
        #Setup Repeated k-fold Cross Validation
        ctrl <- trainControl(method="repeatedcv", number=10, repeats=3)
        gridGBM<-expand.grid(interaction.depth = 3,
                             n.trees = c(5,10,15,20,25,30,35,40),
                             shrinkage = c(0.01,0.03,0.1,0.3,1,3,10,30),
                             n.minobsinnode = c(1,2,3,4))
        
        fitGBM<-train(S1~.,
                      data = NoDatesHistory,
                      method="gbm",
                      trControl=ctrl,
                      tuneGrid = gridGBM,
                      verbose = FALSE)
        #print(fit1)
        BestFit <- fitGBM$results[fitGBM$results$Rsquared == max(fitGBM$results$Rsquared),]
        paste0("GBM: ", round(BestFit$Rsquared,3))
        BestFit
        Scores$Model[5] <- "GBM - NoDatesHistory"
        Scores$R2[5] <- round(BestFit$Rsquared,3)
        Scores$RMSE[5] <- round(BestFit$RMSE,3)
        rm(fitGBM, ctrl, gridGBM)
    
    #Random Forest
        set.seed(1)
        #Setup Repeated k-fold Cross Validation
        ctrl <- trainControl(method="repeatedcv", number=10, repeats=3)
        gridRF<-expand.grid(mtry=c(1,2,3,4,5))
        
        fitRF<-train(S1~.,
                     data = NoDatesHistory,
                     method="rf",
                     trControl=ctrl,
                     tuneGrid = gridRF,
                     verbose = FALSE)
        #print(fit1)
        BestFit <- fitRF$results[fitRF$results$Rsquared == max(fitRF$results$Rsquared),]
        paste0("RF: ", round(BestFit$Rsquared,3))
        BestFit
        Scores$Model[6] <- "RF - NoDatesHistory"
        Scores$R2[6] <- round(BestFit$Rsquared,3)
        Scores$RMSE[6] <- round(BestFit$RMSE,3)
        rm(fitRF, ctrl, gridRF)
        
        rm(NoDatesHistory)

        
#Create a few models to see which one is the best with only looking at current row and last two #####
        
      #Linear Regression
        set.seed(1)
        #Setup Repeated k-fold Cross Validation
        ctrl <- trainControl(method="repeatedcv", number=10, repeats=3)
        
        fitLM <- train(S1~.,
                       data = NoDatesScaled,
                       method="lm",
                       trControl=ctrl)
        #fitLM
        BestFit <- fitLM$results[fitLM$results$Rsquared == max(fitLM$results$Rsquared),]
        paste0("LM: ", round(BestFit$Rsquared,3))
        Scores$Model[7] <- "LM - NoDatesScaled"
        Scores$R2[7] <- round(BestFit$Rsquared,3)
        Scores$RMSE[7] <- round(BestFit$RMSE,3)
        rm(fitLM, ctrl)
        
        
      #Gradient Boosting
        set.seed(1)
        #Setup Repeated k-fold Cross Validation
        ctrl <- trainControl(method="repeatedcv", number=10, repeats=3)
        gridGBM<-expand.grid(interaction.depth = 3,
                             n.trees = c(5,10,15,20,25,30,35,40),
                             shrinkage = c(0.01,0.03,0.1,0.3,1,3,10,30),
                             n.minobsinnode = c(1,2,3,4))
        
        fitGBM<-train(S1~.,
                      data = NoDatesScaled,
                      method="gbm",
                      tuneGrid = gridGBM,
                      verbose = FALSE)
        #print(fit1)
        BestFit <- fitGBM$results[fitGBM$results$Rsquared == max(fitGBM$results$Rsquared),]
        paste0("GBM: ", round(BestFit$Rsquared,3))
        BestFit
        Scores$Model[8] <- "GBM - NoDatesScaled"
        Scores$R2[8] <- round(BestFit$Rsquared,3)
        Scores$RMSE[8] <- round(BestFit$RMSE,3)
        rm(fitGBM, ctrl, gridGBM)
        
      #Random Forest
        set.seed(1)
        #Setup Repeated k-fold Cross Validation
        ctrl <- trainControl(method="repeatedcv", number=10, repeats=3)
        gridRF<-expand.grid(mtry=c(1,2,3,4,5))
        
        fitRF<-train(S1~.,
                     data = NoDatesScaled,
                     method="rf",
                     trControl=ctrl,
                     tuneGrid = gridRF,
                     verbose = FALSE)
        #print(fit1)
        BestFit <- fitRF$results[fitRF$results$Rsquared == max(fitRF$results$Rsquared),]
        paste0("RF: ", round(BestFit$Rsquared,3), round(BestFit$RMSE,3))
        BestFit
        Scores$Model[9] <- "RF - NoDatesScaled"
        Scores$R2[9] <- round(BestFit$Rsquared,3)
        Scores$RMSE[9] <- round(BestFit$RMSE,3)
        rm(fitRF, ctrl, gridRF)
        rm(BestFit)
        
# Train Best Model - RF #####
        #Gradient Boosting
        set.seed(1)
        #Setup Repeated k-fold Cross Validation
        ctrl <- trainControl(method="repeatedcv", number=10, repeats=3)
        gridRF<-expand.grid(mtry=2)
        
        fitRF<-train(S1~.,
                     data = NoDatesScaled,
                     method="rf",
                     trControl=ctrl,
                     tuneGrid = gridRF,
                     verbose = FALSE,
                     importance = TRUE)
          
          pdf("./Plots/FinalRFVarImp.pdf", width = 3, height = 5)
          impRF <- varImp(fitRF, scale = FALSE)
          plot(impRF, top = 5)
          dev.off()
          
          rm(ctrl, gridRF, impRF)
          
                
  
#Make Final Predictions - Send it #####
    S1<-predict(fitRF, NoDatesScaledTest)
    mean(S1)
    sd(S1)
    CI <- predict(fitRF$finalModel, NoDatesScaled, interval = "confidence")
    mean(CI)
    sd(CI)
    min(abs(CI))
    max(abs(CI))
    date <- as.data.frame(data$date[51:100])
    Output <- cbind(date, S1)
    names(Output) <- c("date","S1")
    write.csv(Output,"predictions.csv", row.names = FALSE)
    write.csv(Scores, "Scores.csv", row.names = FALSE)
    rm(S1, Output)
    rm(date, data, Scores, fitRF, NoDatesScaledTest, NoDatesScaled, CI)
  
