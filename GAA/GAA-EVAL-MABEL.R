######### This function needs to be more flexible, e.g., col 2 can be non-numberic 
Validate.Submission.File <- function(infile.path, template.file,
                                       header = TRUE, sep = "\t", 
                                     na.strings='', ...){
  # submission template
  sub.template <- read.table(template.file, sep = sep, header = header,
                             na.strings = na.strings, ...)

  number.of.rows <- nrow(sub.template)
  number.of.columns <- ncol(sub.template) 
  correct.header <- colnames(sub.template)
  correct.column.1 <-  sub.template[, 1]

  #prediction file
  cat('checking file', infile.path, '\n')
  #check if file exists
  if (!file.exists(infile.path)) {
    stop("File ", infile," does not exist!")
  }
  #check permission to read file
  if (file.access(infile.path, mode = 4) < 0) {
    stop("No permission to read file ", infile.path, " !")
  }
  pred.table <- read.table(infile.path, sep = sep, header = header, 
                           na.strings = na.strings, ...)
  
  #check file has not empty cells
  if (any(grepl('NA', pred.table) == TRUE)){
    stop('submission file ', infile.path, ' has a blank cell in column ',
         match('TRUE',grepl('NA', pred.table)))
  }
  #check file has correct number of lines
  if (number.of.rows != nrow(pred.table)) { 
    stop('submission file ', infile.path,
         ' has not a correct number of lines')
  }
  #check number of fields of each line is correct
  if (number.of.columns != ncol(pred.table)) {
    stop ('submission file ', infile.path, 
          ' has not a correct number of columns')
  }
  #check if header is valid
  if (any(correct.header != colnames(pred.table))) {
    stop ('submission file', infile.path, 
          ' has not a correct header') # check all element of header are correct
  } 
  #check each column contains valid character (for example: in some columns only numbers are accepted, etc....)
  #check if column 1 contains valid character
  if ( any(as.character(correct.column.1) !=  as.character(pred.table[, 1]))) {       # escape sequences \d, \s and \w represent any decimal digit, space character and ‘word??? character, * means zero or one match,  + means preceding item will match one or more times
    stop("First column of ", infile.path," must contain valid alleles! Check row ",
         which(as.character(correct.column.1) !=  as.character(pred.table[, 1])))
  }
  #check if column 2 is numerical value or star
  if (any(regexpr('[*]|\\d+', pred.table[, 2]) != 1)) {
    stop("Second column of ", infile.path," must be numeric or \"*\"!")
  } else if (any(grepl('[*]', pred.table[, 2]) == grepl('\\d+', pred.table[, 2])) == TRUE) { # check both * and digits are not present in column 2
    stop("Second column of ", infile.path, " must be numeric or \"*\", not both!")
  }
  #check if column 3 is numerical value or star
  if (any(regexpr('[*]|\\d+', pred.table[, 3]) != 1)) {
    stop("Third column of ", infile.path, " must be numeric or \"*\"!")
  } else if (any(grepl('[*]', pred.table[, 3]) == grepl('\\d+', pred.table[, 3])) == TRUE) { # check both * and digits are not present in column 3
    stop("Third column of ", infile.path, " must be numeric or \"*\", not both!")
  }
  cat('submission file', infile.path, 'format check passed!', '\n')
}

Validate.Submission.Folder <- function(folder.name, template.file,
                               header = TRUE, sep = "\t", na.strings ='', ...){

  # prediction file list
  #read top folder  
  group.path <- paste(folder.name,list.files(folder.name), sep="/")
  #obtain files in each sub-folder of the top folder
  submission.files <- unname(unlist(sapply(group.path,function(x) paste(x, list.files(x), sep="/"))))
  tmp=sapply(submission.files,
         function(x) Validate.Submission.File(x, template.file, 
                                              header = header, sep = sep,
                                              na.strings = na.strings, ...))
}

read.Submission.Folder <- function(folder.name, col.id = 1, col.value = 2, 
                                   col.sd = 3, with.sd = TRUE,
                                   file.sep = "\t", file.header = TRUE,
                                   chop.prefix = TRUE, chop.suffix = TRUE,
                                   na.character = "*", real.data = NULL,...){
  #define file-reading sub-function 
  read.Prediction.File <- function(file.name, col.id, col.value, col.sd, 
                                   with.sd = TRUE, 
                                   sep = "\t", header = TRUE,
                                   na.character = na.character, ...){
    
    data <- read.table(file.name, sep = sep, header = header, na.strings = na.character, ...)
    
    id <- data[ , col.id[1]]
    if(length(col.id) > 1){
      id <- sapply(2:length(col.id),function(x) paste(id,data[,col.id[x]],sep='.'))
    }
    value <- as.numeric(as.character(data[ , col.value]))
    names(value) <- id
    
    if(with.sd){
      sd <- as.numeric(as.character(data[ , col.sd]))
      names(sd) = id
      if(is.na(sd[1])){
        sd <- rep(NA,length(sd))
      }
    }
    else{
      sd <- NA
    }
    
    return.data <- list(value,sd)
    names(return.data) <- c("value", "sd")
    return(return.data)
  }
  
  #adjust data structure function (convert file-centered data structure 
  #into value matrix and sd matrix)
  adjustDataStructure <- function(data, with.sd){
    order=names(data[[1]]$value)
    value <- sapply(1:length(data), function(x) return(data[[x]]$value[order]))
    colnames(value) <- names(data)
    sd <- NA
    if(with.sd){
      sd <- sapply(1:length(data), function(x) return(data[[x]]$sd))
      colnames(sd) <- names(data)
    }
    data <- list(value, sd)
    return(data)
  }
  read.RealData <- function(file, col.id = 1, col.value = 2, 
                            col.sd = 3, with.sd = TRUE,
                            sep = "\t", header = TRUE, na.character = '*', ...){
    
    data <- read.table(file, sep = sep, header = header, na.strings = na.character, ...)
    id <- data[, col.id[1]]
    if(length(col.id)>1){
      id <- sapply(2:length(col.id),function(x) paste(id,data[,col.id[x]],sep='.'))
    }
    value <- as.numeric(as.character(data[ , col.value]))
    names(value) <- id
    sd <- NA
    if(with.sd){
      sd <- data[ , col.sd]
      sd <- as.numeric(as.character(sd))
      names(sd) <- id
    }
    
    return.data <- list(value, sd)
    names(return.data) <- c("value", "sd")
    return(return.data)
  }
  
  validate.Submission.data <- function(real.data,pred.data){
    pred.data$value=pred.data$value[names(real.data$value),]
    pred.data$sd=pred.data$sd[names(real.data$sd),]
    return(pred.data)
  }
  #read top folder  
  group.path <- paste(folder.name,list.files(folder.name), sep="/")
  #obtain files in each sub-folder of the top folder
  file.path <- unname(unlist(sapply(group.path,function(x) paste(x, list.files(x), sep="/"))))
  #read files to matrices 
  data <- lapply(file.path,function(x) read.Prediction.File(x, col.id = col.id,
                                                            col.value = col.value,
                                                            col.sd = col.sd, 
                                                            sep = file.sep,
                                                            header = file.header,
                                                            na.character = na.character,
                                                            with.sd = with.sd))
  #modify id by removing folder names
  if(chop.prefix){
    
    chopHeader <- function(x){
      tmp <- unlist(strsplit(x,split="/"))
      return(tmp[length(tmp)])
    }
    
    file.path <- sapply(file.path, chopHeader)
  }
  #modify id by removing suffix like ".txt"
  if(chop.suffix){
    
    chopTail <- function(x){
      tmp <- unlist(strsplit(x,split=".",fixed=TRUE))
      if(length(tmp) > 1){
        tmp <- substr(x,1,nchar(x)-nchar(tmp[length(tmp)])-1)
      }
      return(tmp)
    }
    
    file.path <- sapply(file.path, chopTail)
  }
  
  names(data) <- file.path
  
  #adjst data structure
  data <- adjustDataStructure(data = data, with.sd = with.sd)
  names(data)=c("value","sd")
  if(!is.null(real.data)){
    data <- validate.Submission.data(real.data,data)
  }
  return(data)
}

read.RealData <- function(file, col.id = 1, col.value = 2, 
                           col.sd = 3, with.sd = TRUE,
                           sep = "\t", header = TRUE, na.character = '*', ...){
  
  data <- read.table(file, sep = sep, header = header, na.strings = na.character, ...)
  id <- data[, col.id[1]]
  if(length(col.id)>1){
    id <- sapply(2:length(col.id),function(x) paste(id,data[,col.id[x]],sep='.'))
  }
  value <- as.numeric(as.character(data[ , col.value]))
  names(value) <- id
  sd <- NA
  if(with.sd){
    sd <- data[ , col.sd]
    sd <- as.numeric(as.character(sd))
    names(sd) <- id
  }

  return.data <- list(value, sd)
  names(return.data) <- c("value", "sd")
  return(return.data)
}

validate.Submission.data <- function(real.data,pred.data){
  pred.data$value=pred.data$value[names(real.data$value),]
  pred.data$sd=pred.data$sd[names(real.data$sd),]
  return(pred.data)
}

eval.Correlation <- function(real.data, pred.data, method = "pearson", sd.use = NA,  ...){
  sd.enable <- is.numeric(sd.use) & length(real.data$sd) >1
  if(sd.enable){
    keep <- (real.data$sd < sd.use) & (!is.na(real.data$sd))
    real.data$value <- real.data$value[keep]
    pred.data$value <- pred.data$value[keep, ]
  }
  n <- length(real.data$value)
  suppressWarnings(
    cor.coefficient <- sapply(1:ncol(pred.data$value), 
                            function(x) cor.test(real.data$value, pred.data$value[,x], 
                                                 method = method, ...)$estimate))
  
  suppressWarnings(
    cor.p.value <- sapply(1:ncol(pred.data$value), 
                              function(x) cor.test(real.data$value, pred.data$value[,x], 
                                                   method = method, ...)$p.value))
  cor.result <- data.frame(cor.coefficient, cor.p.value)
  #print(real.data$value)
  # cor.to.plot <- cor(pred.data$value,real.data$value, method=method, use = "complete.obs")#tbd
  # corrplot(cor.to.plot, method = "number") #tbd
  colnames(cor.result) <- c(paste(method,".coefficient",".n=",n, sep=''), "p.value")
  rownames(cor.result)=colnames(pred.data$value)
  if(sd.enable){
    colnames(cor.result)[1] <- paste(colnames(cor.result)[1], ".sd<", sd.use, sep="")
  }
  return(cor.result)
}


eval.RMSD <- function(real.data, pred.data, sd.use = NA, 
                      density.distance = FALSE, density.distance.adjust = FALSE){

  sd.enable <- is.numeric(sd.use) & length(real.data$sd) >1
  if(sd.enable){
    keep <- (real.data$sd < sd.use) & (!is.na(real.data$sd))
    real.data$value <- real.data$value[keep]
    real.data$sd <- real.data$sd[keep]
    pred.data$value <- pred.data$value[keep, ]
  }
  if(density.distance){
    if (length(real.data$sd) == 1){
      stop("This evaluation requires standard deviations for real data!")
    }
  }
  n <- length(real.data$value)
  
  dis.matrix <- sapply(1:ncol(pred.data$value),
                       function(x) pred.data$value[,x] - real.data$value)
  if(density.distance){
    if(density.distance.adjust){
      dis.matrix <- Density.Distance(real.data = real.data, 
                                          pred.data = pred.data, adjusted = TRUE)
    }
    else{
      dis.matrix <- Density.Distance(real.data = real.data, 
                                          pred.data = pred.data, adjusted = FALSE)
    }
  }
  
  rmsd <- sapply(1:ncol(pred.data$value), 
                  function(x) sqrt(sum((na.exclude(dis.matrix[,x]))^2)/n))
  
  rmsd <-data.frame(rmsd)
  colnames(rmsd) <- "RMSD"
  rownames(rmsd) <- colnames(pred.data$value)
  if(sd.enable){
    colnames(rmsd)[1] <- paste(colnames(rmsd),".n=",n,".sd<", sd.use, sep="")
  }
  return(rmsd)
}

eval.AUC <- function(real.data, pred.data, threshold = 0.5, sd.use = NA, lower.positive=F){
  require(ROCR)

  sd.enable <- is.numeric(sd.use) & length(real.data$sd) >1
  if(sd.enable){
    keep <- (real.data$sd < sd.use) & (!is.na(real.data$sd))
    real.data$value <- real.data$value[keep]
    pred.data$value <- pred.data$value[keep, ]
  }
  n <- length(real.data$value)
  
  realClass <- real.data$value
  if(lower.positive){
    realClass[ which(real.data$value <= threshold)] <- 1
    realClass[ which(real.data$value > threshold)] <- 0
  }
  else{
    realClass[ which(real.data$value > threshold)] <- 1
    realClass[ which(real.data$value <= threshold)] <- 0
  }
  AUC.Helper <- function(x,lower.positive){
    index <- x
    x <- pred.data$value[,x]
    #get predicted classes
    predClass <- x
    if(lower.positive){
      predClass[ which(x <= threshold)] <- 1
      predClass[ which(x > threshold)] <- 0
    }
    else{
      predClass[ which(x > threshold)] <- 1
      predClass[ which(x <= threshold)] <- 0
    }
    #get positive
    positive <- which(realClass == 1)
    TP <- length( which(predClass[positive] == realClass[positive]))
    FN <- length( which(predClass[positive] != realClass[positive]))
    #get negative
    neutral <- which(realClass == 0)
    TN <- length( which(predClass[neutral] == realClass[neutral] ) )
    FP <- length( which(predClass[neutral] != realClass[neutral] ) )
    #get specificity and sensitivity
    sens <- TP / (TP + FN)
    spec <- TN / (TN + FP)
    bAccu <- (sens + spec) /2
    pred <<- prediction( predictions= x, labels= realClass)
    perf <<- performance(pred, "tpr", "fpr")
    auc <- performance(pred, "auc")
    auc <- round(unlist(slot(auc, "y.values")), digits = 6)
    result <- c(auc, sens, spec)
    names(result) <- c("AUC", "sensitivity", "specificity")
    if (sd.enable) {
      pdf(file = paste("./plots/", colnames(pred.data$value)[index], "thresh", threshold, "sd<", sd.use, "ROCcurve.pdf"))
      plot(perf, main = paste(colnames(pred.data$value)[index], "ROC Curve",  "sd<", sd.use, sep=" "))
    } else {
      pdf(file = paste("./plots/", colnames(pred.data$value)[index], "ROCcurve.pdf"))
      plot(perf, main = paste(colnames(pred.data$value)[index], "ROC Curve", sep=" ")) 
    }
    textbox(c(0.73,0.9), 0.4, c(paste("AUC ", threshold, ": ", auc, sep=""), paste("Sensitivity: ", round(sens, digits=6)), paste("Specificity: ", round(spec, digits=6))), cex=1,
            col="black", border="black")
    dev.off()
    return(result)
  }
  auc <- sapply(1:ncol(pred.data$value), 
                function(x) AUC.Helper(x,lower.positive))
  auc <- data.frame(t(auc))
  rownames(auc) <- colnames(pred.data$value)
  colnames(auc)[1] <- paste("AUC", threshold, sep = '.')
  if(sd.enable){
    colnames(auc)[1] <- paste(colnames(auc)[1], ".n=",n,".sd<", sd.use, sep="")
  }
  return(auc)
}

Density.Distance <-  function(real.data, pred.data, sd.use = NA, 
                                   adjusted=FALSE){
  
  if (length(real.data$sd) == 1){
    stop("This evaluation requires standard deviations for real data!")
  }
  keep <- !is.na(real.data$sd)
  if(is.numeric(sd.use) & length(real.data$sd) >1){
    keep <- (real.data$sd < sd.use) & keep
  }

  real.data$value <- real.data$value[keep]
  real.data$sd <- real.data$sd[keep]
  pred.data$value <- pred.data$value[keep,]
  
  averDis <-function(val,mu,sigma){
    return(val*(2*pnorm(val,mean=mu,sd=sigma)-1)+
             exp(-(val-mu)^2/(2*sigma^2))*(sigma*sqrt(2/pi))-
             mu*(2*pnorm(val-mu,mean=0,sd=sigma)-1))
  }
  
  dis <- sapply(1:ncol(pred.data$value),function(x) 
    sapply(1:length(real.data$value),function(y) 
      averDis(pred.data$value[y,x],real.data$value[y],real.data$sd[y]+0.001)))

  if(adjusted){
    bacDis <- sapply(1:length(real.data$value),function(y) 
      averDis(real.data$value[y],real.data$value[y],real.data$sd[y]+0.001))
    dis <- sapply(1:ncol(pred.data$value),function(x) dis[,x]-bacDis)
  }
  colnames(dis) <- colnames(pred.data$value)
  row.names(dis) <- rownames(real.data$value)
  return(as.data.frame(dis))
}

#lowest value should be -1. keep the colors for 1, 0, -1, provide parameter to change color, color_low, color_high, consistent for all the heatmap function calls 
eval.Correlation.Between <- function(real.data, pred.data, method = "pearson", 
                                     sd.use = NA,  ...){
  sd.enable <- is.numeric(sd.use) & length(real.data$sd) >1
  if(sd.enable){
    keep <- (real.data$sd < sd.use) & (!is.na(real.data$sd))
    real.data$value <- real.data$value[keep]
    pred.data$value <- pred.data$value[keep, ]
  }
  mat <- cbind(real.data$value,pred.data$value)
  colnames(mat)[1] <- "real.value"
  suppressWarnings(
    cor.coefficient <- sapply(1:ncol(mat), function(x) 
      sapply(1:ncol(mat), function(y) 
        cor.test(mat[,x], mat[,y], method = method, ...)$estimate)))
  
  suppressWarnings(
    cor.p.value <- sapply(1:ncol(mat), function(x) 
      sapply(1:ncol(mat), function(y) 
        cor.test(mat[,x], mat[,y], method = method, ...)$estimate)))
  rownames(cor.coefficient) <- colnames(mat)
  colnames(cor.coefficient) <- colnames(mat)
  rownames(cor.p.value) <- colnames(mat)
  colnames(cor.p.value) <- colnames(mat)
  cor.result <- list(cor.coefficient, cor.p.value)
  names(cor.result) <- c("coefficient", "p.value")
  return(cor.result)
} 

eval.Partial.Correlation <- function(real.data, pred.data, method = "pearson", 
                                     sd.use = NA,  ...){
  require(ppcor)
  sd.enable <- is.numeric(sd.use) & length(real.data$sd) >1
  if(sd.enable){
    keep <- (real.data$sd < sd.use) & (!is.na(real.data$sd))
    real.data$value <- real.data$value[keep]
    pred.data$value <- pred.data$value[keep, ]
  }
  n <- length(real.data$value)
  mat <- cbind(real.data$value,pred.data$value)
  colnames(mat)[1] <- "real.value"
  mat <-na.exclude(mat)
  cor <- pcor(mat, method = "spearman", ...)
  cor.coefficient=cor$estimate[2:nrow(cor$estimate),1]
  cor.p.value=cor$p.value[2:nrow(cor$p.value),1]
  cor.result <- data.frame(cor.coefficient, cor.p.value)
  names(cor.result) <- c("coefficient", "p.value")
  if(sd.enable){
    colnames(cor.result)[1] <- paste(colnames(cor.result)[1], ".sd<", sd.use, sep="")
  }
  return(cor.result)
} #you need to plot this too! tbd (needs a barplot, correlation of exp and p1 controlled for all other p's. add p values too)

#---------This function will plot the correlation given by the function "eval.Correlation" above! ---------------------------
plot.Correlation <- function(result.cor, method, pval=TRUE){
  mydata <-data.frame(method=labels(result.cor)[[1]], Correlation=result.cor[[1]])
  plot <- ggplot(data=mydata, aes(x=method, y=Correlation)) + 
    geom_bar(stat="identity") + theme_classic() + 
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) 
  #If pval argument is given, we use geom_text (http://ggplot2.tidyverse.org/reference/geom_text.html) to plot appropriate number of stars
  #if p smaller than 0.05 => *, p < 0.01 => **, p <0.001 => ***
  stars1<-c()
  if(pval) {
    for (i in 1:nrow(result.cor)){ 
      if(result.cor[[2]][i] < 0.001) {
        stars1<-c(stars1, "***")
      } else if(result.cor[[2]][i] < 0.01) {
        stars1<-c(stars1, "**")
      } else if(result.cor[[2]][i] < 0.05) {
        stars1<-c(stars1, "*")
      } else {
        stars1<-c(stars1, " ")
      }
    }
  }
  plot2 <- plot + geom_text(aes(label = stars1), vjust = -0.5, position = position_dodge(0.9)) +
    ggtitle(paste("Plot of", method, "Correlation vs. Predictor", sep=" ")) + theme(plot.title = element_text(hjust = 0.5))
  ggsave(filename=paste("plots/", method, "correlation",".pdf"), plot=plot2, width = 7, height = 7, units = "in",dpi = 300)
}

#---------This function will plot the correlation between given by the function "eval.Correlation.Between" above!, and save it to a pdf file in the working directory -----------
#Reference: http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
#(pairwise correlation with 1's down the diagonal, output is a matrix for  heatmap)
plot.Correlation.Between <- function(result.cor, method, color_low="blue", color_high="red", color_mid="white") {
  result.cor.melted <- melt(result.cor)
  p <- ggplot(data = result.cor.melted, aes(X2, X1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = color_low, high = color_high, mid = color_mid, 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name=paste(method, "\nCorrelation")) +
    theme_classic()+ 
    theme(text = element_text(size = 10), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),  axis.title.x = element_blank(),
          axis.title.y = element_blank()) + coord_fixed()
  ggsave(filename=paste("plots/", method, "correlationBetween",".pdf"), plot=p, width = 7, height = 7, units = "in",dpi = 300)
}

#---------This function will plot the partial correlation given by the funciton "eval.Partial.Correlation" above!, and save it to a pdf file in the working directory ------------
plot.Partial.Correlation <- function(result.cor, method, pval=TRUE){
  mydata <-data.frame(method=labels(result.cor)[[1]], Correlation=result.cor[[1]])
  head(mydata)
  plot <- ggplot(data=mydata, aes(x=method, y=Correlation)) + 
    geom_bar(stat="identity") + theme_classic() + 
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) 
  #If pval argument is given, we use geom_text (http://ggplot2.tidyverse.org/reference/geom_text.html) to plot appropriate number of stars
  #if p smaller than 0.05 => *, p < 0.01 => **, p <0.001 => ***
  stars1<-c()
  if(pval) {
    for (i in 1:nrow(result.cor)){ 
      if(result.cor[[2]][i] < 0.001) {
        stars1<-c(stars1, "***")
      } else if(result.cor[[2]][i] < 0.01) {
        stars1<-c(stars1, "**")
      } else if(result.cor[[2]][i] < 0.05) {
        stars1<-c(stars1, "*")
      } else {
        stars1<-c(stars1, " ")
      }
    }
    plot2 <- plot + geom_text(aes(label = stars1), vjust = -0.5, position = position_dodge(0.9)) +
      ggtitle(paste("Plot of", method, "Partial Correlation vs. Predictor", sep=" ")) + theme(plot.title = element_text(hjust = 0.5))
    ggsave(filename=paste("plots/", method, "PartialCorrelation",".pdf"), plot=plot2, width = 7, height = 7, units = "in",dpi = 300)
  }
}

#---------These are scatterplots inherited from CAGI_5_assessment Scripts ---------------------------------------------------
# see how to call this from the test.R file
ExperimentalVsPredicted <- function(path.prediction, real.data){
  #get the real data
  realValue <- real.data[1:nrow(real.data), 2]
  # make plots
  ##################
  #### plot 1 : axis dimension is defined by min aand max value of all predictions
  ############
  png("./plots/ExperimentalVsPredicted.png", width = 18, height = 26, units = 'in', res = 300)
  par(mfrow = c(ceiling(length(path.prediction)/4), 4)) # decide number of plots in each row of the picture and total number of lines
  # check min and max value of predicted to decide plot axis length
  min.vector=rep(1.0, length(path.prediction)) # create a vector for min of each prediction file
  max.vector=rep(1.0, length(path.prediction)) # create a vector for max of each prediction file
  for(i in 1:length(path.prediction)) { 
    #select prediction file i
    pat <- paste('-',i,'.txt', sep='')  #pat<-paste('-', 1, '.txt', sep='')
    CurrentSubmission <- submission.files[grepl(pat, submission.files)]
    prediction.table <- read.table(file=CurrentSubmission, sep="\t", header=TRUE)
    predicted.value <- prediction.table[1:nrow(prediction.table), 2]
    min.vector[i] <- min(as.numeric(predicted.value))
    max.vector[i] <- max(as.numeric(predicted.value))
  }
  min.max.predictions <- c(min(min.vector)*0.90, max(max.vector)*1.10) # vector for plot size
  
  #load the prediction
  for(i in 1:length(path.prediction)) { 
    #select prediction file i
    pat <- paste('-',i,'.txt', sep='') 
    CurrentSubmission <- submission.files[grepl(pat, submission.files)]
    prediction.table <- read.table(file=CurrentSubmission, sep="\t", header=TRUE)
    predicted.value <- prediction.table[1:nrow(prediction.table), 2]
    par(srt = 45) # point lables orientation
    plot(min.max.predictions, min.max.predictions, main=  paste("Submission", i), 
         type= 'n', col = "#0000FFFF", xlab="Experimental values", ylab="Predicted values", 
         cex.axis=1.3, cex.lab=1.5, cex.main=1.5, cex = 2)
    points(realValue, predicted.value, pch=20, cex = 2, col = "#0000FFFF")
    text(realValue, predicted.value, labels = real.data[1:nrow(real.data), 1], col = "#0000FFFF", pos = 4, cex=1.3) # comment this line to remove point lables
    segments(min.max.predictions[1], min.max.predictions[1], min.max.predictions[2], min.max.predictions[2], col= "#0000FFFF", lty= 2, lwd = 2)
  }
  dev.off()
  
  # ##################
  # #### plot 2: axis dimension based on min and max experimental value 
  # ####        predicted value outside range of experimental values are rescaled as min_exp_value*0.92 and max_exp_value*1.05
  # ################################## create plot with predictions outside range of experimental values in red ##########################
  png("./plots/ExperimentalVsPredicted_out_of_scale.png", width = 18, height = 26, units = 'in', res = 300)
  par(mfrow = c(ceiling(length(path.prediction)/4), 4)) # decide number of plots in each row of the picture and total number of lines
  min.max.predictions <- c(min(as.numeric(realValue))*0.9, max(as.numeric(realValue))*1.10) # vector for plot size

  #load the prediction
  for(i in 1:length(path.prediction)) {
    #select prediction file i
    pat <- paste('-',i,'.txt', sep='')
    CurrentSubmission <- submission.files[grepl(pat, submission.files)]
    prediction.table <- read.table(file=CurrentSubmission, sep="\t", header=TRUE, stringsAsFactors = FALSE)
    predicted.value <- prediction.table[1:nrow(prediction.table), 2]
    color.list=rep('#0000FFFF', length(predicted.value)) # vector of point colors
    color.list[which(as.numeric(predicted.value) < min(realValue))] = 'red' # plot points out of range of esperimental values in red
    color.list[which(as.numeric(predicted.value) > max(realValue))] = 'red'
    predicted.value[which(as.numeric(predicted.value) < min(realValue))] = min(realValue)*0.92 # change value of predicted points outside experimental range in min or max of experimental range
    predicted.value[which(as.numeric(predicted.value) > max(realValue))] = max(realValue)*1.05
    par(srt = 45) # point lables orientation
    plot(min.max.predictions, min.max.predictions, main=  paste("Submission", i), type= 'n', col = "#0000FFFF", xlab="Experimental values", ylab="Predicted values", cex.axis=1.3, cex.lab=1.5, cex.main=1.5, cex = 2)
    points(realValue, predicted.value, pch=20, cex = 2, col = color.list)
    text(realValue, predicted.value, labels = real.data[1:nrow(real.data), 1], col = color.list, pos = 4, cex=1.3) # comment this line to remove point lables
    segments(min.max.predictions[1], min.max.predictions[1], min.max.predictions[2], min.max.predictions[2], col= "#0000FFFF", lty= 2, lwd = 2)
  }
  dev.off() 
}

#PCA Plot (raw data)---------------------------------------------------------------------------------------------------------
  #PCA with experimental data + all predictions. That many points will show up on the graph. 
  #Scatterplot just PC1 and PC2. Have parameters to add label near point. Have legend with colors. 
  #no normalize
  # add + theme_classic()
  #Labels and legend act as enable parameters
  #Need: Label only values above a certain threshold?
Plot.PCA <- function(experimental.data, labels=TRUE, legend=TRUE) {
  pcaresult = prcomp(t(experimental.data), scale=TRUE)
  output <- as.data.frame(pcaresult$x)
  output$Legend <- rownames(t(experimental.data))
  p <- ggplot(output,aes(x=PC1, y=PC2, color=Legend)) + theme_classic() + theme(plot.title = element_text(hjust = 0.5))
  p <- p+geom_point() + ggtitle("PCA plot of Experimental Data + All Predictions") 
  if (labels) {
    p <- p + geom_label_repel(aes(label = Legend), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50') 
    p <- p + guides(fill = guide_legend(override.aes = list(shape = 15)))
  }
  if (!legend) {
    p <- p + theme(legend.position="none")
  }
  p
  ggsave(filename=paste("plots/", "PCA Plot",".pdf"), plot=p, width = 10, height = 7, units = "in",dpi = 300)
}



#RMSD Scatter Plots (since discontinuous we use a regular scatterplot instead of packages)----------------------------------
#Parameters: result.rmsd = data, method = any string that dictates the title of the plot indicating what type of RMSD was used
plot.RMSD <- function(result.rmsd, method="") {
  mydata <-data.frame(method=labels(result.rmsd)[[1]], rmsd=result.rmsd[[1]])
  head(mydata)
  p <- ggplot(data=mydata, aes(x=method, y=rmsd)) + 
    geom_bar(stat="identity") + theme_classic() + 
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
    ggtitle(paste("Plot of", method, "RMSD vs. Predictor", sep=" ")) + theme(plot.title = element_text(hjust = 0.5))
  ggsave(filename=paste("plots/", "RMSDPlot", method, ".pdf"), plot=p, width = 10, height = 7, units = "in",dpi = 300)
}


