library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(lattice)
library(pROC)
library(ROCR)
require(reshape2)
library(ggrepel)
library(rlist)
library(gmodels)
library(parallel)
library(stringr)
######### This function needs to be more flexible, e.g., col 2 can be non-numberic
##################################################################################
#Validation
Validate.Submission.File <- function(infile.path,
                                     template.file,
                                     header = TRUE,
                                     sep = "\t",
                                     na.strings = '',
                                     ...) {
  # #Inputs
  # header = TRUE
  # sep = "\t"
  # na.strings=''
  # template.file = "5-GAA_submission_template.txt"
  # infile.path = "prediction/Group_1/Group_1-prediction_file_nocomments-1.txt"
  
  # submission template
  sub.template <-
    read.table(
      template.file,
      sep = sep,
      header = header,
      na.strings = na.strings
    )
  
  number.of.rows <- nrow(sub.template)
  number.of.columns <- ncol(sub.template)
  correct.header <- colnames(sub.template)
  correct.column.1 <-  sub.template[, 1]
  
  #prediction file
  cat('checking file', infile.path, '\n')
  #check if file exists
  if (!file.exists(infile.path)) {
    stop("File ", infile, " does not exist!")
  }
  #check permission to read file
  if (file.access(infile.path, mode = 4) < 0) {
    stop("No permission to read file ", infile.path, " !")
  }
  pred.table <- read.table(infile.path,
                           sep = sep,
                           header = header,
                           na.strings = na.strings)
  
  #check file has correct number of lines
  correct.num.row = TRUE
  if (number.of.rows != nrow(pred.table)) {
    cat('submission file ',
        infile.path,
        ' has not a correct number of lines',
        '\n')
    correct.num.row = FALSE
  }
  
  #check number of fields of each line is correct
  correct.num.column = TRUE
  if (number.of.columns != ncol(pred.table)) {
    cat('submission file ',
        infile.path,
        ' has not a correct number of columns',
        '\n')
    correct.num.column = FALSE
  }
  #check if header is valid
  correct.header = TRUE
  if (any(correct.header != colnames(pred.table))) {
    cat(
      'submission file',
      infile.path,
      ' has not a correct header, or order may be different from template',
      '\n'
    ) # check all element of header are correct
    correct.header = FALSE
  }
  empty.cell = FALSE
  #check file has not empty cells
  if (any(grepl('NA', pred.table) == TRUE)) {
    cat(
      'submission file ',
      infile.path,
      ' has a blank cell in column ',
      match('TRUE', grepl('NA', pred.table)),
      '\n'
    )
    empty.cell = TRUE
  }
  
  #check each column contains valid character (for example: in some columns only numbers are accepted, etc....)
  #check if column 1 contains valid character
  #YuC: change this function since col1 not necessarily in order. First sort two list and then compare
  #     then get back the original row number of the prediction file with unmatched name
  valid.column.1 = TRUE
  if (any(sort(as.character(correct.column.1)) !=  sort(as.character(pred.table[, 1])))) {
    # escape sequences \d, \s and \w represent any decimal digit, space character and ‘word??? character, * means zero or one match,  + means preceding item will match one or more times
    cat(
      "First column of ",
      infile.path,
      " must contain valid alleles! Check row ",
      which(as.character(pred.table[, 1]) == sort(pred.table[, 1])[sort(as.character(correct.column.1)) !=  sort(as.character(pred.table[, 1]))]),
      '\n'
    )
    valid.column.1 = FALSE
  }
  
  #check if column 2 is numerical value or star
  valid.column.2 = TRUE
  if (any(regexpr('[*]|\\d+', pred.table[, 2]) != 1)) {
    cat("Second column of ",
        infile.path,
        " must be numeric or \"*\"!",
        '\n')
    valid.column.2 = FALSE
  } else if (any(grepl('[*]', pred.table[, 2]) == grepl('\\d+', pred.table[, 2])) == TRUE) {
    # check both * and digits are not present in column 2
    cat("Second column of ",
        infile.path,
        " must be numeric or \"*\", not both!",
        '\n')
    valid.column.2 = FALSE
  }
  #check if column 3 is numerical value or star
  valid.column.3 = TRUE
  if (any(regexpr('[*]|\\d+', pred.table[, 3]) != 1)) {
    cat("Third column of ",
        infile.path,
        " must be numeric or \"*\"!",
        '\n')
    valid.column.3 = FALSE
  } else if (any(grepl('[*]', pred.table[, 3]) == grepl('\\d+', pred.table[, 3])) == TRUE) {
    # check both * and digits are not present in column 3
    cat("Third column of ",
        infile.path,
        " must be numeric or \"*\", not both!",
        '\n')
    valid.column.3 = FALSE
  }
  
  # YuC: After this step, should implement new functions to determine the valid file's following
  # check if column 1 is in the same order with template
  ordered.rows = TRUE
  
  if (any(as.character(correct.column.1) !=  as.character(pred.table[, 1]))) {
    ordered.rows = FALSE
    
    cat("lines are not ordered as in template", '\n')
  }
  # sd existence (if whole column 3 is *)
  with_sd = TRUE
  
  if (all(regexpr('[*]', pred.table[, 3]) == 1)) {
    cat("there's no standard deviation input", '\n')
    with_sd = FALSE
    
  }
  cat('submission file', infile.path, 'format check passed!', '\n')
  validation.info = c(
    correct.num.row = correct.num.row,
    correct.num.column = correct.num.column,
    correct.header = correct.header,
    empty.cell = empty.cell,
    valid.column.1 = valid.column.1,
    valid.column.2 = valid.column.2,
    valid.column.3 = valid.column.3,
    ordered.rows = ordered.rows,
    with_sd = with_sd
  )
  return(validation.info)
}

Validate.Submission.Folder <- function(folder.name,
                                       template.file,
                                       header = TRUE,
                                       sep = "\t",
                                       na.strings = '',
                                       ...) {
  #####################################
  #inputs for debug
  # folder.name = "prediction"
  # template.file = "5-GAA_submission_template.txt"
  # header = TRUE
  # sep = "\t"
  # na.strings =''
  #######################
  
  #read top folder
  group.path <-
    paste(folder.name, list.files(folder.name), sep = "/")
  #obtain files in each sub-folder of the top folder
  submission.files <-
    unname(unlist(sapply(group.path, function(x)
      paste(x, list.files(x), sep = "/"))))
  validation.info = sapply(submission.files,
                           function(x)
                             Validate.Submission.File(
                               x,
                               template.file,
                               header = header,
                               sep = sep,
                               na.strings = na.strings
                             ))
  # YuC: Should unlist the tmp and output a table with
  #      |submission.files|order|col2_na|col3_na|with_sd|
  validation.info = t(validation.info)
}

#######################################################################################
# Read
read.RealData <- function(file,
                          col.id = 1,
                          col.value = 2,
                          col.sd = 3,
                          with.sd = TRUE,
                          sep = "\t",
                          header = TRUE,
                          na.character = '*',
                          ...) {
  data <-
    read.table(file,
               sep = sep,
               header = header,
               na.strings = na.character,
               ...)
  id <- data[, col.id[1]]
  if (length(col.id) > 1) {
    id <-
      sapply(2:length(col.id), function(x)
        paste(id, data[, col.id[x]], sep = '.'))
  }
  value <- as.numeric(data[, col.value])
  names(value) <- id
  sd <- NA
  if (with.sd) {
    sd <- data[, col.sd]
    sd <- as.numeric(sd)
    names(sd) <- id
  }
  return.data <- list(value, sd)
  names(return.data) <- c("value", "sd")
  return(return.data)
}

#define file-reading sub-function
read.Prediction.File <-
  function(file.name,
           col.id,
           col.value,
           col.sd,
           with.sd = TRUE,
           sep = "\t",
           header = TRUE,
           na.character = na.character,
           ...) {
    # file.name = "prediction/group_9/Group_9-SPARKS-4.txt"
    # sep = "\t"
    # header = TRUE
    # na.character = "*"
    # col.value = 3
    # with.sd = 1
    # col.sd = 4
    # print(file.name)
    data <-
      read.table(file.name,
                 sep = sep,
                 header = header,
                 na.strings = na.character)
    id <- data[, col.id[1]]
    if (length(col.id) > 1) {
      id <-
        sapply(2:length(col.id), function(x)
          paste(id, data[, col.id[x]], sep = '.'))
    }
    value <- as.numeric(as.character(data[, col.value]))
    names(value) <- id
    if (with.sd) {
      sd <- as.numeric(as.character(data[, col.sd]))
      names(sd) = id
      # if(is.na(sd[1])){
      #   sd <- rep(NA,length(sd))
      #   # YuC: don't get why to execute so, comment out as Zhiqiang suggests
      # }
    } else{
      sd <- NA
    }
    return.data <- list(value, sd)
    names(return.data) <- c("value", "sd")
    return(return.data)
  }

# reorder the prediction submision value and sd column
validate.Submission.data <- function(real.data, pred.data) {
  pred.data$value = pred.data$value[names(real.data$value),]
  pred.data$sd = pred.data$sd[names(real.data$sd),]
  return(pred.data)
}

#adjust data structure function (convert file-centered data structure
#into value matrix and sd matrix)
adjustDataStructure <- function(data, with.sd) {
  order = names(data[[1]]$value)
  value <-
    sapply(1:length(data), function(x)
      return(data[[x]]$value[order]))
  colnames(value) <- names(data)
  sd <- NA
  if (with.sd) {
    sd <- sapply(1:length(data), function(x)
      return(data[[x]]$sd))
    colnames(sd) <- names(data)
  }
  data <- list(value, sd)
  names(data) = c("value", "sd")
  return(data)
}

read.Submission.Folder <-
  function(folder.name,
           col.id = 1,
           col.value = 2,
           col.sd = 3,
           with.sd = TRUE,
           file.sep = "\t",
           file.header = TRUE,
           chop.prefix = TRUE,
           chop.suffix = TRUE,
           na.character = "*",
           real.data = NULL,
           ...) {
    chopHeader <- function(x) {
      tmp <- unlist(strsplit(x, split = "/"))
      return(tmp[length(tmp)])
    }
    chopTail <- function(x) {
      tmp <- unlist(strsplit(x, split = ".", fixed = TRUE))
      if (length(tmp) > 1) {
        tmp <- substr(x, 1, nchar(x) - nchar(tmp[length(tmp)]) - 1)
      }
      return(tmp)
    }
    
    #for running ####################
    # exp.data <- read.RealData(file = "exp_data.txt", sep = ",",
    #                           col.id = 1, col.value = 3, col.sd = 4)
    # folder.name = "prediction"
    # col.id = 1
    # col.value = 3
    # col.sd = 4
    # with.sd = TRUE
    # file.sep = "\t"
    # file.header = TRUE
    # chop.prefix = TRUE
    # chop.suffix = TRUE
    # na.character = "*"
    # real.data = exp.data
    #################################
    
    #read top folder
    group.path <-
      paste(folder.name, list.files(folder.name), sep = "/")
    #obtain files in each sub-folder of the top folder
    file.path <-
      unname(unlist(sapply(group.path, function(x)
        paste(x, list.files(x), sep = "/"))))
    #read files to matrices
    data <-
      lapply(file.path, function(x)
        read.Prediction.File(
          x,
          col.id = col.id,
          col.value = col.value,
          col.sd = col.sd,
          sep = file.sep,
          header = file.header,
          na.character = na.character,
          with.sd = with.sd
        ))
    
    
    #modify id by removing folder names
    if (chop.prefix) {
      file.path <- sapply(file.path, chopHeader)
    }
    #modify id by removing suffix like ".txt"
    if (chop.suffix) {
      file.path <- sapply(file.path, chopTail)
    }
    
    names(data) <- file.path
    
    #adjst data structure
    #fix this
    data <- adjustDataStructure(data = data, with.sd = with.sd)
    if (!is.null(real.data)) {
      # Reordering of the rows
      data <- validate.Submission.data(real.data, data)
    }
    
    tmp.group = sapply(colnames(data$value), function(x)
      unlist(strsplit(x, split = "_"))[2])
    tmp.group = sapply(1:length(tmp.group), function(x)
      unlist(strsplit(tmp.group[x], split = "-"))[1])
    tmp.group = as.numeric(tmp.group)
    group = data.frame(colnames(data$value), tmp.group)
    names(group) = c("file", "group")
    data$group = group
    return(data)
  }



# YuC: Add the Grouping functions
#' @decription Adding grouping information after parsing prediction files from folder
#' @param data the list with $value and $sd after parsing the prediction file folder
#' @param group_info a 1D integer vector specifying grouping of each column of value/sd matrix
#' @return original data with an addtional $group field as a list
#'         with value as group and rownames as file names
#' @example to get all files of a group : group %>% filter(group == 2) %>% select(file)
addGroup = function(data, group_info) {
  file.labels = data.frame(colnames(data$value), stringsAsFactors = FALSE)
  group = cbind(file.labels, group_info)
  names(group) = c("file", "group")
  data$group = group
  return(data)
}


lmFitHelpher = function(real.data, pred.data, sub.name) {
  exp = real.data$value
  sub = pred.data$value[, sub.name]
  dat = as.data.frame(cbind(exp, sub))
  lm.fit = lm(exp ~ sub, data = dat)
  x = summary(lm.fit)
  return(x$adj.r.squared)
}

# YuC: Add the Grouping functions
#' @decription fit each submission and the experimental value with linear value, then for each group,
#' keep the best fit submission based on adjusted R^2 statistics/pearson correlation
#' @name groupFilter
#' @param real.data the list with $value and $sd after parsing the prediction file folder
#' @param pred.data a 1D integer vector specifying grouping of each column of value/sd matrix
#' @return The original data set only keeping the best submission from each group
groupFilter = function(real.data, pred.data, top = NA) {
  ######### debug input  #####################
  # real.data <- read.RealData(file = "exp_data.csv", sep = ",",
  #                            col.id = 2, col.value = 5, col.sd = 6)
  # pred.data <- read.Submission.Folder(folder.name = "prediction/",col.id = 1,
  #                                     col.value = 2, col.sd = 3, real.data = real.data)
  # To change the name o unique identifiers
  # real.data.2 <- read.RealData(file = "exp_data.csv", sep = ",",
  #                            col.id = c(2,4), col.value = 5, col.sd = 6)
  # names(pred.data$value) = names(real.data.2$value)
  # names(pred.data$sd) = names(real.data.2$value)
  # names(real.data$value) = names(real.data.2$value)
  # names(real.data$sd) = names(real.data.2$value)
  ###################################################################
  r_squares <- unlist(lapply(colnames(pred.data$value),
                             function(x)
                               lmFitHelpher(real.data, pred.data, x)))
  r_squares_group = cbind(pred.data$group, r_squares)
  max.r = r_squares_group %>% group_by(group) %>% summarise(max.r = max(r_squares), file = file[1])
  pred.data$value = pred.data$value[, max.r$file]
  pred.data$sd = pred.data$sd[, max.r$file]
  pred.data$group = max.r$group
  return (pred.data)
}






#####################################################################################################
# YuC: add a z-score transformation funcition to Normalize all the prediction samples to N(0,1) for cross-group
# comparison/distance calculation
# Since the standard deviation will also be changed, the sd.use input should be filtered before z-transformation
#' @decription normalize each of the column of the value to N(0,1)
#' @param real.data the experimental data from CAGI assessors
#' @param pred.data prediction data parsed in from submissions
#' @return z.real.data and z.pred.data
z.normalization = function(real.data, pred.data) {
  z.Helper = function(column) {
    std = sd(column, na.rm = TRUE)
    avg = mean(column, na.rm = TRUE)
    dat = (column - avg) / std
    return(dat)
  }
  suppressWarnings(z.pred.data <- sapply(1:ncol(pred.data$value),
                                         function(x)
                                           z.Helper(pred.data$value[, x])))
  z.real.data = z.Helper(real.data$value)
  colnames(z.pred.data) = colnames(pred.data$value)
  z = c()
  z$real.data = z.real.data
  z$pred.data = z.pred.data
  return(z)
}


#' @decription evaluate the correlation coefficient between different samples and the real value
#' @name eval.Correlation
eval.Correlation <-
  function(real.data,
           pred.data,
           method = "pearson",
           sd.use = NA,
           z.transform = FALSE,
           boot = FALSE,
           boot.var = F,
           ...) {
    if (z.transform) {
      z_dat = z.normalization(real.data, pred.data)
      real.data$value = z_dat$real.data
      pred.data$value = z_dat$pred.data
    }
    
    
    sd.enable <- is.numeric(sd.use) & length(real.data$sd) > 1
    if (sd.enable) {
      keep <- (real.data$sd < sd.use) & (!is.na(real.data$sd))
      real.data$value <- real.data$value[keep]
      real.data$sd <- real.data$sd[keep]
      pred.data$value <- pred.data$value[keep, ]
      pred.data$sd <- pred.data$sd[keep, ]
    }
    n <- length(real.data$value)
    
    suppressWarnings(cor.coefficient <-
                       sapply(1:ncol(pred.data$value),
                              function(x)
                                cor.test(real.data$value, pred.data$value[, x],
                                         method = method)$estimate))
    
    suppressWarnings(cor.p.value <- sapply(1:ncol(pred.data$value),
                                           function(x)
                                             cor.test(real.data$value, pred.data$value[, x],
                                                      method = method)$p.value))
    cor.result <- data.frame(cor.coefficient, cor.p.value)
    #print(real.data$value)
    # cor.to.plot <- cor(pred.data$value,real.data$value, method=method, use = "complete.obs")#tbd
    # corrplot(cor.to.plot, method = "number") #tbd
    colnames(cor.result) <-
      c(paste(method, ".coefficient", ".n=", n, sep = ''), "p.value")
    rownames(cor.result) = colnames(pred.data$value)
    if (sd.enable) {
      colnames(cor.result)[1] <-
        paste(colnames(cor.result)[1], ".sd<", sd.use, sep = "")
    }
    
    # add bootstrap
    if (boot) {
      # #####input################
      # real.data <- read.RealData(file = "exp_data.csv", sep = ",",
      #                            col.id = 2, col.value = 5, col.sd = 6)
      # pred.data <- read.Submission.Folder(folder.name = "prediction/",col.id = 1,
      #                                     col.value = 2, col.sd = 3, real.data = exp.data)
      # method = "pearson"
      # sd.use = 0.3
      # z.transform = TRUE
      # boot.var = T
      # ################################
      
      rep.time = 500
      rep.obj = bootstrap.Helper(real.data, pred.data, rep.time, boot.var)
      real.rep = rep.obj$real.rep
      pred.rep = rep.obj$pred.rep
      
      cor.rep = lapply(1:rep.time, function(x)
        eval.Correlation(
          real.data = real.rep[[x]],
          pred.data = pred.rep[[x]],
          method,
          sd.use,
          z.transform,
          boot = F
        ))
      coefficient.rep = list.cbind(lapply(1:rep.time, function(x)
        cor.rep[[x]][, 1]))
      pval.rep = list.cbind(lapply(1:rep.time, function(x)
        cor.rep[[x]][, 2]))
      row.names(coefficient.rep) = row.names(cor.rep[[1]])
      ci_low = list.rbind(lapply(1:nrow(coefficient.rep), function(x)
        quantile(coefficient.rep[x,], probs = 0.05)))
      ci_high = list.rbind(lapply(1:nrow(coefficient.rep), function(x)
        quantile(coefficient.rep[x,], probs = 0.95)))
      sd = list.rbind(lapply(1:nrow(coefficient.rep), function(x)
        sd(coefficient.rep[x,])))
      avg = list.rbind(lapply(1:nrow(coefficient.rep), function(x)
        mean(coefficient.rep[x,])))
      p.median = list.rbind(lapply(1:nrow(coefficient.rep), function(x)
        median(pval.rep[x,])))
      
      cor.result = cbind(avg, ci_low, ci_high, sd, p.median)
      row.names(cor.result) = row.names(cor.rep[[1]])
      colnames(cor.result) = c("avg", "low_ci", "high_ci", "sd", "p.value")
    }
    return(as.data.frame(cor.result))
  }




#' @decription The function takes in the distance matrix from the eval.RMSD function and output a normalized
#' version based on variance
#' For RMSD, the normalization for X1~N(V1,sd1^2), X2~N(V2,sd2^2) —>D = X1-X2~N(V1-V2,sd1^2 + sd2^2),
#' normalize all the variance for RMSD to 1
#' @name var.normalize
#' @param rmsd the rmsd result from the eval.RMSD function
#' @param real.data
#' @param pred.data
#' @return a normalized dis.matrix
var.normalize <- function(rmsd, real.data, pred.data) {
  real.sd = sd(real.data$value, na.rm = TRUE)
  pred.sd = sapply(1:ncol(pred.data$value),
                   function(x)
                     sd(pred.data$value[, x], na.rm = TRUE))
  new.sd = real.sd ^ 2 + pred.sd ^ 2
  names(new.sd) = colnames(pred.data$value)
  normalized.rmsd = sapply(rownames(rmsd),
                           function(x)
                             rmsd[x, "RMSD"] / new.sd[x])
  normalized.rmsd = as.data.frame(normalized.rmsd)
  rownames(normalized.rmsd) = rownames(rmsd)
  colnames(normalized.rmsd) = "RMSD"
  return(normalized.rmsd)
}


#' @name eval.RMSD
#' @description  evaluate the root mean square distance between
eval.RMSD <- function(real.data,
                      pred.data,
                      sd.use = NA,
                      density.distance = FALSE,
                      density.distance.adjust = FALSE,
                      variance.normalization = TRUE,
                      boot = FALSE,
                      boot.var = F,
                      ...) {
  ######input###################################################
  # real.data <- read.RealData(file = "exp_data.csv", sep = ",",
  #                            col.id = 2, col.value = 5, col.sd = 6)
  # pred.data <- read.Submission.Folder(folder.name = "prediction/",col.id = 1,
  #                                     col.value = 2, col.sd = 3, real.data = exp.data)
  # sd.use = NA
  # density.distance = FALSE
  # density.distance.adjust = FALSE
  # variance.normalization = TRUE
  ##############################################################
  
  
  
  sd.enable <- is.numeric(sd.use) & length(real.data$sd) > 1
  if (sd.enable) {
    keep <- (real.data$sd < sd.use) & (!is.na(real.data$sd))
    real.data$value <- real.data$value[keep]
    real.data$sd <- real.data$sd[keep]
    pred.data$value <- pred.data$value[keep, ]
  }
  if (density.distance) {
    if (length(real.data$sd) == 1) {
      stop("This evaluation requires standard deviations for real data!")
    }
  }
  
  n <- length(real.data$value)
  
  dis.matrix <- sapply(1:ncol(pred.data$value),
                       function(x)
                         pred.data$value[, x] - real.data$value)
  if (density.distance) {
    if (density.distance.adjust) {
      dis.matrix <- Density.Distance(real.data = real.data,
                                     pred.data = pred.data,
                                     adjusted = TRUE)
    }
    else{
      dis.matrix <- Density.Distance(real.data = real.data,
                                     pred.data = pred.data,
                                     adjusted = FALSE)
    }
  }
  
  rmsd <- sapply(1:ncol(pred.data$value),
                 function(x)
                   sqrt(sum((
                     na.exclude(dis.matrix[, x])
                   ) ^ 2) / n))
  rmsd <- data.frame(rmsd)
  colnames(rmsd) <- "RMSD"
  rownames(rmsd) <- colnames(pred.data$value)
  if (sd.enable) {
    colnames(rmsd)[1] <-
      paste(colnames(rmsd), ".n=", n, ".sd<", sd.use, sep = "")
  }
  if (variance.normalization) {
    rmsd = var.normalize(rmsd, real.data, pred.data)
  }
  
  if (boot) {
    rep.time = 500
    rep.obj = bootstrap.Helper(real.data, pred.data, rep.time, boot.var)
    real.rep = rep.obj$real.rep
    pred.rep = rep.obj$pred.rep
    
    # rmsd.rep <- mclapply(1:500, function(x) {eval.RMSD(real.data = real.rep[[x]], pred.data = pred.rep[[x]], sd.use,
    #                                                    density.distance, density.distance.adjust, variance.normalization,
    #                                                    boot = FALSE)}, mc.cores = 4)
    
    rmsd.rep = lapply(1:rep.time, function(x)
      eval.RMSD(
        real.data = real.rep[[x]],
        pred.data = pred.rep[[x]],
        sd.use,
        density.distance,
        density.distance.adjust,
        variance.normalization,
        boot = FALSE
      ))
    
    rmsd.rep = list.cbind(lapply(1:rep.time, function(x)
      rmsd.rep[[x]][, 1]))
    row.names(rmsd.rep) = row.names(rmsd.rep[[1]])
    
    ci_low = list.rbind(lapply(1:nrow(rmsd.rep), function(x)
      quantile(rmsd.rep[x,], probs = 0.05)))
    ci_high = list.rbind(lapply(1:nrow(rmsd.rep), function(x)
      quantile(rmsd.rep[x,], probs = 0.95)))
    sd = list.rbind(lapply(1:nrow(rmsd.rep), function(x)
      sd(rmsd.rep[x,])))
    avg = list.rbind(lapply(1:nrow(rmsd.rep), function(x)
      mean(rmsd.rep[x,])))
    
    rmsd.result = cbind(avg, ci_low, ci_high, sd)
    row.names(rmsd.result) = colnames(pred.data$value)
    colnames(rmsd.result) = c("RMSD", "low_ci", "high_ci", "sd")
    rmsd = rmsd.result
  }
  return(as.data.frame(rmsd))
}



AUC.build.class = function(dat, lower.positive, threshold = 0.5) {
  if (lower.positive) {
    dat[dat <= threshold] = 1
    dat[dat > threshold] <- 0
  }
  else{
    dat[dat > threshold] <- 1
    dat[dat <= threshold] <- 0
  }
  return(dat)
}


# YuC: remove textbox and perform logistic regression
eval.AUC <-
  function(real.data,
           pred.data,
           threshold = 0.5,
           sd.use = NA,
           lower.positive = F,
           z.transform = F) {
    # remove rows with NA values
    filter <- !is.na(real.data$value)
    real.data$value <- real.data$value[filter]
    pred.data$value <- pred.data$value[filter, ]
    # sd
    sd.enable <- is.numeric(sd.use) & length(real.data$sd) > 1
    if (sd.enable) {
      keep <- (real.data$sd < sd.use) & (!is.na(real.data$sd))
      real.data$value <- real.data$value[keep]
      pred.data$value <- pred.data$value[keep, ]
    }
    n <- length(real.data$value)
    if (z.transform) {
      z_dat = z.normalization(real.data, pred.data)
      real.data$value = z_dat$real.data
      pred.data$value = z_dat$pred.data
    }
    realClass <-
      AUC.build.class(real.data$value, lower.positive, threshold)
    
    AUC.Helper <- function(file.name, lower.positive) {
      #get predicted classes
      predClass <-
        AUC.build.class(pred.data$value[, file.name], lower.positive, threshold)
      #get positive
      positive <- which(realClass == 1)
      TP <-
        length(which(predClass[positive] == realClass[positive]))
      FN <-
        length(which(predClass[positive] != realClass[positive]))
      #get negative
      neutral <- which(realClass == 0)
      TN <-
        length(which(predClass[neutral] == realClass[neutral]))
      FP <-
        length(which(predClass[neutral] != realClass[neutral]))
      #get stats
      sens <- TP / (TP + FN)
      spec <- TN / (TN + FP)
      bAccu <- (sens + spec) / 2
      accuracy = (TP + TN) / (TP + FP + FN + TN)
      precision = TP / (TP + FP)
      f1_score = 2 * (sens * precision) / (sens + precision)
      # print(file.name)
      pred <<-
        prediction(predictions = pred.data$value[, file.name], labels = realClass)
      perf <<- performance(pred, "tpr", "fpr")
      auc <- performance(pred, "auc")
      auc <- round(unlist(slot(auc, "y.values")), digits = 6)
      result <-
        c(
          AUC = auc,
          sensitivity = sens,
          specificity = spec,
          accuracy = accuracy,
          precision = precision,
          f1_score = f1_score,
          bAccu = bAccu
        )
      return(result)
    }
    auc.summary <- sapply(colnames(pred.data$value),
                          function(x)
                            AUC.Helper(x, lower.positive))
    auc.summary <- data.frame(t(auc.summary))
    rownames(auc.summary) <- colnames(pred.data$value)
    colnames(auc.summary)[1] <- paste("AUC", threshold, sep = '.')
    if (sd.enable) {
      colnames(auc.summary)[1] <-
        paste(colnames(auc.summary)[1], ".n=", n, ".sd<", sd.use, sep = "")
    }
    ret = list(
      sd.use = sd.use,
      threshold = threshold,
      realClass = realClass,
      results = round(auc.summary, 4),
      pred.data = pred.data,
      real.data = real.data
    )
    return(ret)
  }





# YuC: A new plot function for plotting a single AUC curve with ggplot
#' @decription The function takes the output from eval.AUC function and plot one of the ROC plots for each submission file with
#' its respective AUC,sensitivity,specificity,accuracy,precision,f1_score,bAccu
#' @name plot.AUC.helper
#' @param real.data The real data object being parse in from experimental value
#' @param pred.data The real data object being parse in from submission folder
#' @param sd.use a sd threshold for filtering undescriptive samples
#' @param lower.positive whether lower than threshold is used as positive binary value
#' @return a ggplot object with all the ROC plots arranged
plot.AUC.helper = function(file.name, results) {
  # render binary class data
  #realClass <- AUC.build.class(real.data$value,lower.positive,threshold)
  # sd.enable dependent plot title
  sd.enable <-
    is.numeric(results$sd.use) & length(results$real.data$sd) > 1
  if (sd.enable) {
    title = paste0(file.name,
                   "thresh=",
                   results$threshold,
                   "sd<",
                   results$sd.use,
                   "ROCcurve")
  } else {
    title = paste0(file.name,
                   "thresh=",
                   results$threshold,
                   results$sd.use,
                   "ROCcurve")
  }
  # get the evaluated statistics
  rocobj <-
    roc(results$realClass, results$pred.data$value[, file.name])
  result.label = paste(colnames(results$results) , results$results[file.name,], sep = ":")
  g <- ggroc(rocobj, legacy.axes = TRUE)
  g = g + theme_minimal(base_size = 5) + ggtitle(title) +
    geom_segment(aes(
      x = 0,
      xend = 1,
      y = 0,
      yend = 1
    ),
    color = "grey",
    linetype = "dashed") +
    annotate(
      "text",
      x = 0.75,
      y = seq(0.125, 0.5, 3 / 64)[1:7],
      label = result.label,
      size = 2
    )
  return(g)
}



# YuC: A new plot function for plotting AUC curve with ggplot
#' @decription The function takes the output from eval.AUC function and plot all the ROC plots for each submission file with
#' their respective AUC,sensitivity,specificity,accuracy,precision,f1_score,bAccu
#' @name plot.AUC
#' @param real.data The real data object being parse in from experimental value
#' @param pred.data The real data object being parse in from submission folder
#' @param sd.use a sd threshold for filtering undescriptive samples
#' @param lower.positive whether lower than threshold is used as positive binary value
#' @return a ggplot object with all the ROC plots arranged
plot.AUC = function(ret) {
  ##########################################
  # # for debugging input
  # real.data <- read.RealData(file = "exp_data.csv", sep = ",",
  #                            col.id = 2, col.value = 5, col.sd = 6)
  # pred.data <- read.Submission.Folder(folder.name = "prediction/",col.id = 1,
  #                                     col.value = 2, col.sd = 3, real.data = exp.data)
  # threshold = 0.5
  # sd.use = NA
  # lower.positive=F
  ###########################################
  # get AUC related statistic values
  # sd.enable <- is.numeric(sd.use) & length(real.data$sd) >1
  # if(sd.enable){
  #   keep <- (real.data$sd < sd.use) & (!is.na(real.data$sd))
  #   real.data$value <- real.data$value[keep]
  #   pred.data$value <- pred.data$value[keep, ]
  # }
  # results = eval.AUC(real.data, pred.data, threshold, sd.use, lower.positive,z.transform)
  plots <- lapply(colnames(ret$pred.data$value),
                  function(x)
                    plot.AUC.helper(x, ret))
  ml <- marrangeGrob(plots, nrow = 2, ncol = 2)
  return(ml)
}



Density.Distance <-  function(real.data,
                              pred.data,
                              sd.use = NA,
                              adjusted = FALSE) {
  if (length(real.data$sd) == 1) {
    stop("This evaluation requires standard deviations for real data!")
  }
  keep <- !is.na(real.data$sd)
  if (is.numeric(sd.use) & length(real.data$sd) > 1) {
    keep <- (real.data$sd < sd.use) & keep
  }
  
  real.data$value <- real.data$value[keep]
  real.data$sd <- real.data$sd[keep]
  pred.data$value <- pred.data$value[keep,]
  
  averDis <- function(val, mu, sigma) {
    return(val * (2 * pnorm(val, mean = mu, sd = sigma) - 1) +
             exp(-(val - mu) ^ 2 / (2 * sigma ^ 2)) * (sigma * sqrt(2 / pi)) -
             mu * (2 * pnorm(
               val - mu, mean = 0, sd = sigma
             ) - 1))
  }
  
  dis <- sapply(1:ncol(pred.data$value), function(x)
    sapply(1:length(real.data$value), function(y)
      averDis(pred.data$value[y, x], real.data$value[y], real.data$sd[y] +
                0.001)))
  
  if (adjusted) {
    bacDis <- sapply(1:length(real.data$value), function(y)
      averDis(real.data$value[y], real.data$value[y], real.data$sd[y] +
                0.001))
    dis <-
      sapply(1:ncol(pred.data$value), function(x)
        dis[, x] - bacDis)
  }
  colnames(dis) <- colnames(pred.data$value)
  row.names(dis) <- rownames(real.data$value)
  return(as.data.frame(dis))
}


#lowest value should be -1. keep the colors for 1, 0, -1, provide parameter to change color, color_low, color_high, consistent for all the heatmap function calls
eval.Correlation.Between <-
  function(real.data,
           pred.data,
           method = "pearson",
           sd.use = NA,
           z.transform = TRUE,
           grouped = FALSE,
           ...) {
    ###############################################
    # for debugging input
    # real.data <- read.RealData(file = "exp_data.csv", sep = ",",
    #                            col.id = 2, col.value = 5, col.sd = 6)
    # pred.data <- read.Submission.Folder(folder.name = "prediction/",col.id = 1,
    #                                     col.value = 2, col.sd = 3, real.data = exp.data)
    # sd.use = 0.01
    # method = "pearson"
    # z.transform = TRUE
    ###############################################
    
    if (z.transform) {
      z_dat = z.normalization(real.data, pred.data)
      real.data$value = z_dat$real.data
      pred.data$value = z_dat$pred.data
    }
    
    sd.enable <- is.numeric(sd.use) & length(real.data$sd) > 1
    if (sd.enable) {
      keep <- (real.data$sd < sd.use) & (!is.na(real.data$sd))
      real.data$value <- real.data$value[keep]
      pred.data$value <- pred.data$value[keep, ]
    }
    if (grouped) {
      pred.data = groupFilter(real.data, pred.data)
    }
    mat <- cbind(real.data$value, pred.data$value)
    colnames(mat)[1] <- "real.value"
    suppressWarnings(cor.coefficient <-
                       sapply(1:ncol(mat), function(x)
                         sapply(1:ncol(mat), function(y)
                           cor.test(mat[, x], mat[, y], method = method)$estimate)))
    
    suppressWarnings(cor.p.value <- sapply(1:ncol(mat), function(x)
      sapply(1:ncol(mat), function(y)
        cor.test(mat[, x], mat[, y], method = method)$estimate)))
    rownames(cor.coefficient) <- colnames(mat)
    colnames(cor.coefficient) <- colnames(mat)
    rownames(cor.p.value) <- colnames(mat)
    colnames(cor.p.value) <- colnames(mat)
    cor.result <- list(cor.coefficient, cor.p.value)
    names(cor.result) <- c("coefficient", "p.value")
    return(cor.result)
  }


eval.Partial.Correlation <-
  function(real.data,
           pred.data,
           method = "pearson",
           sd.use = NA,
           ...) {
    require(ppcor)
    sd.enable <- is.numeric(sd.use) & length(real.data$sd) > 1
    if (sd.enable) {
      keep <- (real.data$sd < sd.use) & (!is.na(real.data$sd))
      real.data$value <- real.data$value[keep]
      pred.data$value <- pred.data$value[keep, ]
    }
    n <- length(real.data$value)
    mat <- cbind(real.data$value, pred.data$value)
    colnames(mat)[1] <- "real.value"
    mat <- na.exclude(mat)
    cor <- pcor(mat, method = "spearman", ...)
    cor.coefficient = cor$estimate[2:nrow(cor$estimate), 1]
    cor.p.value = cor$p.value[2:nrow(cor$p.value), 1]
    cor.result <- data.frame(cor.coefficient, cor.p.value)
    names(cor.result) <- c("coefficient", "p.value")
    if (sd.enable) {
      colnames(cor.result)[1] <-
        paste(colnames(cor.result)[1], ".sd<", sd.use, sep = "")
    }
    return(cor.result)
  }


#' @decription The function call groupFilter to pick one submission from each group, then build
#' linear model excluding one group one at a time. R^2_tot - R^2_excl = unique contribution
#' @name eval.uniqueness
#' @param real.data The real data object being parse in from experimental value
#' @param pred.data The real data object being parse in from submission folder
#' @return a table with the uniqueness of each group
eval.uniqueness = function(real.data,
                           pred.data,
                           boot = FALSE,
                           top = NA,
                           boot.var = F) {
  ######### debug input  #####################
  # real.data <- read.RealData(file = "exp_data.csv", sep = ",",
  #                            col.id = 2, col.value = 5, col.sd = 6)
  # pred.data <- read.Submission.Folder(folder.name = "prediction/",col.id = 1,
  #                                     col.value = 2, col.sd = 3, real.data = real.data)
  # # To change the name o unique identifiers
  # real.data.2 <- read.RealData(file = "exp_data.csv", sep = ",",
  #                              col.id = c(2,4), col.value = 5, col.sd = 6)
  # names(pred.data$value) = names(real.data.2$value)
  # names(pred.data$sd) = names(real.data.2$value)
  # names(real.data$value) = names(real.data.2$value)
  # names(real.data$sd) = names(real.data.2$value)
  ###################################################################
  
  grouped.pred.data = groupFilter(real.data, pred.data, top = top)
  input.group = lapply(1:length(colnames(grouped.pred.data$value)), function(x)
    colnames(grouped.pred.data$value)[-x])
  
  
  r.square.excl = unlist(lapply(1:length(input.group), function(x)
    lmFitHelpher(real.data, grouped.pred.data, input.group[[x]])))
  r.square.tot = lmFitHelpher(real.data,
                              grouped.pred.data,
                              colnames(grouped.pred.data$value))
  uniqueness = r.square.tot - r.square.excl
  result = data.frame(uniqueness)
  row.names(result) = colnames(grouped.pred.data$value)
  
  if (boot) {
    rep.time = 500
    rep.obj = bootstrap.Helper(real.data, pred.data, rep.time, boot.var)
    real.rep = rep.obj$real.rep
    pred.rep = rep.obj$pred.rep
    unique.rep = lapply(1:rep.time, function(x)
      eval.uniqueness(real.data = real.rep[[x]], pred.data = pred.rep[[x]]))
    rname = row.names(unique.rep[[1]])
    unique.rep = list.cbind(lapply(1:rep.time, function(x)
      unique.rep[[x]][, 1]))
    ci_low = list.rbind(lapply(1:nrow(unique.rep), function(x)
      quantile(unique.rep[x,], probs = 0.05)))
    ci_high = list.rbind(lapply(1:nrow(unique.rep), function(x)
      quantile(unique.rep[x,], probs = 0.95)))
    sd = list.rbind(lapply(1:nrow(unique.rep), function(x)
      sd(unique.rep[x,])))
    avg = list.rbind(lapply(1:nrow(unique.rep), function(x)
      mean(unique.rep[x,])))
    result = cbind(avg, ci_low, ci_high, sd)
    row.names(result) = rname
    colnames(result) = c("uniqueness", "low_ci", "high_ci", "sd")
  }
  return(as.data.frame(result))
}


#' @decription This function perform the bootstrap resampling with replacement on the value table and perform
#' the specified function to access the accuracy of model more thoroughly
#' @name bootstrap.Helper
#' @param real.data The real data object being parse in from experimental value
#' @param pred.data The real data object being parse in from submission folder
#' @param var.method randomly drawn from a Gaussian distribution defined by the reported growth score and the standard error
bootstrap.Helper = function(real.data,
                            pred.data,
                            rep.time,
                            var.method = F) {
  # real.data <- read.RealData(file = "exp_data.csv", sep = ",",
  #                            col.id = 2, col.value = 5, col.sd = 6)
  # pred.data <- read.Submission.Folder(folder.name = "prediction/",col.id = 1,
  #                                   col.value = 2, col.sd = 3, real.data = real.data)
  # rep.time = 5
  if (var.method) {
    # 1. make sure only with row that have a non-NA value and a non-NA sd
    # 2. get rep.time number of the same value based on Gaussian distribution
    # 3. make rep.time number of real value
    # 4. append the pred value to result
    naming = function(name, vec) {
      names(vec) = name
      return (vec)
    }
    real.norm = lapply(1:length(real.data$value), function(x)
      rnorm(rep.time, real.data$value[x], real.data$sd[x]))
    real.norm = as.data.frame(do.call(rbind, real.norm))
    real.rep = lapply(1:rep.time, function(x)
      list(value = naming(names(
        real.data$value
      ), real.norm[[x]]), sd = real.data$sd))
    pred.rep = lapply(1:rep.time, function(x)
      list(
        value = pred.data$value,
        sd = pred.data$sd,
        group = pred.data$group
      ))
  } else {
    ind = lapply(1:rep.time, function(x)
      sample(
        1:length(real.data$value),
        length(real.data$value),
        replace = TRUE
      ))
    real.value.rep = lapply(1:rep.time, function(x)
      real.data$value[ind[[x]]])
    real.sd.rep = lapply(1:rep.time, function(x)
      real.data$sd[ind[[x]]])
    
    pred.value.rep = lapply(1:rep.time, function(x)
      pred.data$value[ind[[x]],])
    pred.sd.rep = lapply(1:rep.time, function(x)
      pred.data$sd[ind[[x]],])
    
    real.rep = lapply(1:rep.time, function(x)
      list(value = real.value.rep[[x]], sd = real.sd.rep[[x]]))
    pred.rep = lapply(1:rep.time, function(x)
      list(
        value = pred.value.rep[[x]],
        sd = pred.sd.rep[[x]],
        group = pred.data$group
      ))
    
  }
  result = list(real.rep = real.rep, pred.rep = pred.rep)
  return(result)
}



#---------This function will plot the correlation given by the function "eval.Correlation" above! ---------------------------
plot.Correlation <-
  function(result.cor,
           method,
           pval = TRUE,
           boot = FALSE,
           use.ci = T) {
    mydata <-
      data.frame(submissions = row.names(result.cor),
                 Correlation = result.cor[, 1])
    plot <-
      ggplot(data = mydata, aes(x = submissions, y = Correlation)) +
      geom_bar(stat = "identity") + theme_classic() +
      theme(axis.text.x = element_text(
        angle = 90,
        hjust = 1,
        vjust = 0.5
      ))
    #If pval argument is given, we use geom_text (http://ggplot2.tidyverse.org/reference/geom_text.html) to plot appropriate number of stars
    #if p smaller than 0.05 => *, p < 0.01 => **, p <0.001 => ***
    stars1 <- c()
    if (pval) {
      for (i in 1:nrow(result.cor)) {
        if (result.cor$p.value[i] < 0.001) {
          stars1 <- c(stars1, "***")
        } else if (result.cor$p.value[i] < 0.01) {
          stars1 <- c(stars1, "**")
        } else if (result.cor$p.value[i] < 0.05) {
          stars1 <- c(stars1, "*")
        } else {
          stars1 <- c(stars1, " ")
        }
      }
    }
    plot2 <-
      plot + geom_text(aes(label = stars1),
                       vjust = -0.5,
                       position = position_dodge(0.9)) +
      ggtitle(paste("Plot of", method, "Correlation vs. Predictor", sep =
                      " ")) + theme(plot.title = element_text(hjust = 0.5))
    # directly return the plot object instead of saving
    #ggsave(filename=paste("plots/", method, "correlation",".pdf"), plot=plot2, width = 7, height = 7, units = "in",dpi = 300)
    if (boot) {
      if (use.ci) {
        plot2 <-
          plot2 + geom_errorbar(
            aes(ymin = result.cor$low_ci, ymax = result.cor$high_ci),
            width = .2,
            position = position_dodge(.9)
          )
      } else {
        plot2 <-
          plot2 + geom_errorbar(
            aes(
              ymin = result.cor$avg - result.cor$avg$sd,
              ymax = result.cor$avg + result.cor$avg$sd
            ),
            width = .2,
            position = position_dodge(.9)
          )
      }
    }
    return(plot2)
  }

#---------This function will plot the correlation between given by the function "eval.Correlation.Between" above!, and return the plot object
#Reference: http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
#(pairwise correlation with 1's down the diagonal, output is a matrix for  heatmap)
plot.Correlation.Between <-
  function(result.cor,
           method,
           color_low = "blue",
           color_high = "red",
           color_mid = "white") {
    ######### for running ############
    # result.cor = cor.result
    # color_low="blue"
    # color_high="red"
    # color_mid="white"
    # method = "pearson"
    #################################
    result.cor.melted <- melt(result.cor)
    p <-
      ggplot(data = result.cor.melted,
             aes(result.cor.melted$Var1, result.cor.melted$Var2, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(
        low = color_low,
        high = color_high,
        mid = color_mid,
        midpoint = 0,
        limit = c(-2, 2),
        space = "Lab",
        name = paste(method, "\nCorrelation")
      ) +
      theme_classic() +
      theme(
        text = element_text(size = 10),
        axis.text.x = element_text(
          angle = 90,
          vjust = 1,
          hjust = 1
        ),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) + coord_fixed()
    #ggsave(filename=paste("plots/", method, "correlationBetween",".pdf"), plot=p, width = 7, height = 7, units = "in",dpi = 300)
    return(p)
  }

#---------This function will plot the partial correlation given by the funciton "eval.Partial.Correlation" above!, and return plot object ------------
plot.Partial.Correlation <-
  function(result.cor, method, pval = TRUE) {
    mydata <-
      data.frame(method = labels(result.cor)[[1]], Correlation = result.cor[[1]])
    head(mydata)
    plot <- ggplot(data = mydata, aes(x = method, y = Correlation)) +
      geom_bar(stat = "identity") + theme_classic() +
      theme(axis.text.x = element_text(
        angle = 90,
        hjust = 1,
        vjust = 0.5
      ))
    #If pval argument is given, we use geom_text (http://ggplot2.tidyverse.org/reference/geom_text.html) to plot appropriate number of stars
    #if p smaller than 0.05 => *, p < 0.01 => **, p <0.001 => ***
    stars1 <- c()
    if (pval) {
      for (i in 1:nrow(result.cor)) {
        if (result.cor[[2]][i] < 0.001) {
          stars1 <- c(stars1, "***")
        } else if (result.cor[[2]][i] < 0.01) {
          stars1 <- c(stars1, "**")
        } else if (result.cor[[2]][i] < 0.05) {
          stars1 <- c(stars1, "*")
        } else {
          stars1 <- c(stars1, " ")
        }
      }
      plot2 <-
        plot + geom_text(aes(label = stars1),
                         vjust = -0.5,
                         position = position_dodge(0.9)) +
        ggtitle(paste("Plot of", method, "Partial Correlation vs. Predictor", sep =
                        " ")) + theme(plot.title = element_text(hjust = 0.5))
      #ggsave(filename=paste("plots/", method, "PartialCorrelation",".pdf"), plot=plot2, width = 7, height = 7, units = "in",dpi = 300)
      return(plot2)
    }
  }


# YuC: A new plot function for the Prediction VS Experimental scatterplot, axies can be unified by the largest x and y value
#' @decription The function is a helper function for ploting all the scatter plots for Prediction VS Experimental values, but
#' can also be used as an independent function to plot a single scatterplot
#' @name plot_single_scatter
#' @param real.data The real data object being parse in from experimental value
#' @param pred.data The real data object being parse in from submission folder
#' @param file.name The submitted prediction name to be plotted
#' @return a ggplot object with the single scatterplot
plot_single_scatter = function(real.data, pred.data, file.name) {
  ################################
  # #Debugging input
  # real.data <- read.RealData(file = "exp_data.csv", sep = ",",
  #                            col.id = 2, col.value = 5, col.sd = 6)
  # pred.data <- read.Submission.Folder(folder.name = "prediction/",col.id = 1,
  #                                     col.value = 2, col.sd = 3, real.data = exp.data)
  # file.name = "Group_2-prediction_file_nocomments-2"
  ################################
  real.value = real.data$value
  pred.value = pred.data$value[, colnames(pred.data$value) == file.name]
  plot.title = paste("Experimental", "VS", file.name, sep = " ")
  dat = data.frame(cbind(real.value, pred.value), row.names = NULL)
  p = ggplot(dat, aes(x = real.value, y = pred.value)) +
    geom_point(color = 'red') +
    #geom_text(label = names(real.value),size = 1.5) +
    labs(
      title = plot.title,
      x = "Experimental Value",
      y = file.name,
      cex.lab = 1.5
    ) +
    theme_classic()
  # Plot
  p = p +
    geom_segment(aes(
      x = min(real.value),
      y = min(real.value),
      xend = max(real.value),
      yend = max(real.value)
    ))
  p
  return(p)
}

# YuC: A new plot function for all Prediction VS Experimental scatterplot
#' @decription plot all Prediction VS Experimental scatterplot
#' @name plot_all_scatter
#' @param real.data The real data object being parse in from experimental value
#' @param pred.data The real data object being parse in from submission folder
#' @return a ggplot object with all scatterplots
plot_all_scatter = function(real.data, pred.data, z.transform = TRUE) {
  ################################
  # #Debugging input
  # real.data <- read.RealData(file = "exp_data.csv", sep = ",",
  #                            col.id = 2, col.value = 5, col.sd = 6)
  # pred.data <- read.Submission.Folder(folder.name = "prediction/",col.id = 1,
  #                                     col.value = 2, col.sd = 3, real.data = real.data)
  # z.transform = TRUE
  ###################################
  if (z.transform) {
    z_dat = z.normalization(real.data, pred.data)
    real.data$value = z_dat$real.data
    pred.data$value = z_dat$pred.data
  }
  
  plots <-
    lapply(colnames(pred.data$value), function(x)
      plot_single_scatter(real.data, pred.data, x))
  ml <- marrangeGrob(plots, nrow = 2, ncol = 2)
  return(ml)
}



#PCA Plot (raw data)---------------------------------------------------------------------------------------------------------
#PCA with experimental data + all predictions. That many points will show up on the graph.
#Scatterplot just PC1 and PC2. Have parameters to add label near point. Have legend with colors.
#no normalize
# add + theme_classic()
#Labels and legend act as enable parameters
#Need: Label only values above a certain threshold?
Plot.PCA <- function(experimental.data,
                     labels = TRUE,
                     legend = TRUE) {
  pcaresult = prcomp(t(experimental.data), scale = TRUE)
  output <- as.data.frame(pcaresult$x)
  output$Legend <- rownames(t(experimental.data))
  p <-
    ggplot(output, aes(x = PC1, y = PC2, color = Legend)) + theme_classic() + theme(plot.title = element_text(hjust = 0.5))
  p <-
    p + geom_point() + ggtitle("PCA plot of Experimental Data + All Predictions")
  if (labels) {
    p <-
      p + geom_label_repel(
        aes(label = Legend),
        box.padding = 0.35,
        point.padding = 0.5,
        segment.color = 'grey50'
      )
    p <-
      p + guides(fill = guide_legend(override.aes = list(shape = 15)))
  }
  if (!legend) {
    p <- p + theme(legend.position = "none")
  }
  return(p)
  #ggsave(filename=paste("plots/", "PCA Plot",".pdf"), plot=p, width = 10, height = 7, units = "in",dpi = 300)
}



#RMSD Scatter Plots (since discontinuous we use a regular scatterplot instead of packages)----------------------------------
#Parameters: result.rmsd = data, method = any string that dictates the title of the plot indicating what type of RMSD was used
plot.RMSD <- function(result.rmsd,
                      method = "",
                      boot = FALSE,
                      use.ci = T,
                      ...) {
  mydata <-
    data.frame(submissions = row.names(result.rmsd), rmsd = result.rmsd[, 1])
  head(mydata)
  p <- ggplot(data = mydata, aes(x = submissions, y = rmsd)) +
    geom_bar(stat = "identity") + theme_classic() +
    theme(axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5
    )) +
    ggtitle(paste("Plot of", method, "RMSD vs. Predictor", sep = " ")) + theme(plot.title = element_text(hjust = 0.5))
  if (boot) {
    if (use.ci) {
      p <-
        p + geom_errorbar(
          aes(ymin = result.rmsd$low_ci, ymax = result.rmsd$high_ci),
          width = .2,
          position = position_dodge(.9)
        )
    } else {
      p <-
        p + geom_errorbar(
          aes(
            ymin = result.rmsd$RMSD - result.rmsd$RMSD$sd,
            ymax = result.rmsd$RMSD + result.rmsd$RMSD$sd
          ),
          width = .2,
          position = position_dodge(.9)
        )
    }
  }
  return(p)
  #ggsave(filename=paste("plots/", "RMSDPlot", method, ".pdf"), plot=p, width = 10, height = 7, units = "in",dpi = 300)
}

plot.uniqueness <-
  function(result.uniq,
           method = "",
           boot = FALSE,
           use.ci = T) {
    mydata <-
      data.frame(submissions.group = row.names(result.uniq),
                 uniqueness = result.uniq[, 1])
    p <-
      ggplot(data = mydata, aes(x = submissions.group, y = uniqueness)) +
      geom_bar(stat = "identity") + theme_classic() +
      theme(axis.text.x = element_text(
        angle = 90,
        hjust = 1,
        vjust = 0.5
      )) +
      ggtitle(paste("Plot of", "Unique Linear Model contribution of each group", sep =
                      " ")) + theme(plot.title = element_text(hjust = 0.5))
    if (boot) {
      if (use.ci) {
        # print(length(result.uniq$low_ci))
        p <-
          p + geom_errorbar(
            aes(ymin = result.uniq$low_ci, ymax = result.uniq$high_ci),
            width = .2,
            position = position_dodge(.9)
          )
      } else {
        p = p + geom_errorbar(
          aes(
            ymin = result.uniq$uniqueness - result.uniq$sd,
            ymax = result.uniq$uniqueness + result.uniq$sd
          ),
          width = .2,
          position = position_dodge(.9)
        )
      }
    }
    return(p)
  }



# YuC: 190530 A function that extract the protein residue number and proportion of user-defined "correctness"
#' @decription use similar method as ROC function and get the proportion of correct prediction
#' @name Eval.Correctness
#' @param similar to ROC ones
#' @param prot_name: The corresponding pdb entry for pymol
#' @return A .csv file with proper name of the pdb file of the protein
eval.correctness <-
  function(real.data,
           pred.data,
           threshold = 0.5,
           sd.use = NA,
           lower.positive = F,
           z.transform = F,
           protname) {
    ##########################################
    # for debugging input
    # real.data <- read.RealData(file = "exp_data.csv", sep = ",",
    #                            col.id = 2, col.value = 5, col.sd = 6)
    # pred.data <- read.Submission.Folder(folder.name = "prediction/",col.id = 1,
    #                                     col.value = 2, col.sd = 3, real.data = real.data)
    # threshold = 0.5
    # sd.use = NA
    # lower.positive=F
    # protname = "5nn3"
    # z.transform = T
    ###########################################
    # remove rows with NA values
    filter <-
      (!is.na(real.data$value)) &
      (!sapply(1:nrow(pred.data$value), function(x)
        any(is.na(pred.data$value[x,]))))
    real.data$value <- real.data$value[filter]
    pred.data$value <- pred.data$value[filter, ]
    # sd
    sd.enable <- is.numeric(sd.use) & length(real.data$sd) > 1
    if (sd.enable) {
      keep <- (real.data$sd < sd.use) & (!is.na(real.data$sd))
      real.data$value <- real.data$value[keep]
      pred.data$value <- pred.data$value[keep, ]
    }
    n <- length(real.data$value)
    if (z.transform) {
      z_dat = z.normalization(real.data, pred.data)
      real.data$value = z_dat$real.data
      pred.data$value = z_dat$pred.data
    }
    realClass <-
      AUC.build.class(real.data$value, lower.positive, threshold)
    predClass <- sapply(colnames(pred.data$value),
                        function(x)
                          AUC.build.class(pred.data$value[, x], lower.positive, threshold))
    correctness <- sapply(names(realClass),
                          function(x)
                            sum(predClass[x,] == realClass[x]) / ncol(predClass))
    ret = data.frame(res = as.numeric(str_extract(names(correctness), "[0-9]+")), correctness = correctness)
    ret = ret %>% group_by(res) %>% summarise(avg.correctness = mean(correctness))
    tabl.name = paste0(protname, ".csv")
    ret$res = as.character(ret$res)
    write.csv(ret, file = tabl.name)
    return(ret)
  }







#########################Discarded function##############################
#---------These are scatterplots inherited from CAGI_5_assessment Scripts ---------------------------------------------------
# see how to call this from the test.R file
ExperimentalVsPredicted <- function(path.prediction, real.data) {
  #get the real data
  realValue <- real.data[1:nrow(real.data), 2]
  # make plots
  ##################
  #### plot 1 : axis dimension is defined by min aand max value of all predictions
  ############
  png(
    "./plots/ExperimentalVsPredicted.png",
    width = 18,
    height = 26,
    units = 'in',
    res = 300
  )
  par(mfrow = c(ceiling(length(path.prediction) / 4), 4)) # decide number of plots in each row of the picture and total number of lines
  # check min and max value of predicted to decide plot axis length
  min.vector = rep(1.0, length(path.prediction)) # create a vector for min of each prediction file
  max.vector = rep(1.0, length(path.prediction)) # create a vector for max of each prediction file
  for (i in 1:length(path.prediction)) {
    #select prediction file i
    pat <-
      paste('-', i, '.txt', sep = '')  #pat<-paste('-', 1, '.txt', sep='')
    CurrentSubmission <-
      submission.files[grepl(pat, submission.files)]
    prediction.table <-
      read.table(file = CurrentSubmission,
                 sep = "\t",
                 header = TRUE)
    predicted.value <-
      prediction.table[1:nrow(prediction.table), 2]
    min.vector[i] <- min(as.numeric(predicted.value))
    max.vector[i] <- max(as.numeric(predicted.value))
  }
  min.max.predictions <-
    c(min(min.vector) * 0.90, max(max.vector) * 1.10) # vector for plot size
  
  #load the prediction
  for (i in 1:length(path.prediction)) {
    #select prediction file i
    pat <- paste('-', i, '.txt', sep = '')
    CurrentSubmission <-
      submission.files[grepl(pat, submission.files)]
    prediction.table <-
      read.table(file = CurrentSubmission,
                 sep = "\t",
                 header = TRUE)
    predicted.value <-
      prediction.table[1:nrow(prediction.table), 2]
    par(srt = 45) # point lables orientation
    plot(
      min.max.predictions,
      min.max.predictions,
      main =  paste("Submission", i),
      type = 'n',
      col = "#0000FFFF",
      xlab = "Experimental values",
      ylab = "Predicted values",
      cex.axis = 1.3,
      cex.lab = 1.5,
      cex.main = 1.5,
      cex = 2
    )
    points(
      realValue,
      predicted.value,
      pch = 20,
      cex = 2,
      col = "#0000FFFF"
    )
    text(
      realValue,
      predicted.value,
      labels = real.data[1:nrow(real.data), 1],
      col = "#0000FFFF",
      pos = 4,
      cex = 1.3
    ) # comment this line to remove point lables
    segments(
      min.max.predictions[1],
      min.max.predictions[1],
      min.max.predictions[2],
      min.max.predictions[2],
      col = "#0000FFFF",
      lty = 2,
      lwd = 2
    )
  }
  dev.off()
  
  # ##################
  # #### plot 2: axis dimension based on min and max experimental value
  # ####        predicted value outside range of experimental values are rescaled as min_exp_value*0.92 and max_exp_value*1.05
  # ################################## create plot with predictions outside range of experimental values in red ##########################
  png(
    "./plots/ExperimentalVsPredicted_out_of_scale.png",
    width = 18,
    height = 26,
    units = 'in',
    res = 300
  )
  par(mfrow = c(ceiling(length(path.prediction) / 4), 4)) # decide number of plots in each row of the picture and total number of lines
  min.max.predictions <-
    c(min(as.numeric(realValue)) * 0.9, max(as.numeric(realValue)) * 1.10) # vector for plot size
  
  #load the prediction
  for (i in 1:length(path.prediction)) {
    #select prediction file i
    pat <- paste('-', i, '.txt', sep = '')
    CurrentSubmission <-
      submission.files[grepl(pat, submission.files)]
    prediction.table <-
      read.table(
        file = CurrentSubmission,
        sep = "\t",
        header = TRUE,
        stringsAsFactors = FALSE
      )
    predicted.value <-
      prediction.table[1:nrow(prediction.table), 2]
    color.list = rep('#0000FFFF', length(predicted.value)) # vector of point colors
    color.list[which(as.numeric(predicted.value) < min(realValue))] = 'red' # plot points out of range of esperimental values in red
    color.list[which(as.numeric(predicted.value) > max(realValue))] = 'red'
    predicted.value[which(as.numeric(predicted.value) < min(realValue))] = min(realValue) *
      0.92 # change value of predicted points outside experimental range in min or max of experimental range
    predicted.value[which(as.numeric(predicted.value) > max(realValue))] = max(realValue) *
      1.05
    par(srt = 45) # point lables orientation
    plot(
      min.max.predictions,
      min.max.predictions,
      main =  paste("Submission", i),
      type = 'n',
      col = "#0000FFFF",
      xlab = "Experimental values",
      ylab = "Predicted values",
      cex.axis = 1.3,
      cex.lab = 1.5,
      cex.main = 1.5,
      cex = 2
    )
    points(
      realValue,
      predicted.value,
      pch = 20,
      cex = 2,
      col = color.list
    )
    text(
      realValue,
      predicted.value,
      labels = real.data[1:nrow(real.data), 1],
      col = color.list,
      pos = 4,
      cex = 1.3
    ) # comment this line to remove point lables
    segments(
      min.max.predictions[1],
      min.max.predictions[1],
      min.max.predictions[2],
      min.max.predictions[2],
      col = "#0000FFFF",
      lty = 2,
      lwd = 2
    )
  }
  dev.off()
}
############################################################
