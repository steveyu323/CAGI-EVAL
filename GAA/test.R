#Most updated version: June 24
setwd("~/cagi_eval/")
#rm(list=ls())
source("GAA-EVAL.R")
library(ggplot2)
library(reshape2)
library(ppcor)
library(reshape)
library(plotrix)
library(plyr)
library(grid)
library(plotrix)
library(gridExtra)
library(ggrepel)
library(Publish)
#Dependencies: reshape2, ggplot2, ppcor, reshape, plyr, grid, gridextra
# Validate.Submission.Folder(folder.name = "prediction/",
#                           template.file = "5_PTEN_TPMT_submission_template_v01.txt")
#This is NOT working yet

##### Get Data ###############################
exp.data <- read.RealData(file = "exp_data.csv", sep = ",",
                          col.id = 2, col.value = 5, col.sd = 6) 
sub.data <- read.Submission.Folder(folder.name = "prediction/",col.id = 1, 
                                   col.value = 2, col.sd = 3, real.data = exp.data)

##############################################

##### correlation-based evaluation #####

result.cor.pearson <- eval.Correlation(real.data = exp.data, pred.data = sub.data,
                                       method="pearson")


head(result.cor.pearson) 
plot.Correlation(result.cor.pearson, "Pearson")


result.cor.spearman <- eval.Correlation(real.data = exp.data, pred.data = sub.data,
                                       method="spearman")
head(result.cor.spearman)
#export table result as tsv
plot.Correlation(result.cor.spearman, "Spearman")


result.cor.kendall <- eval.Correlation(real.data = exp.data, pred.data = sub.data,
                                       method="kendall")
head(result.cor.kendall)
#export table result as tsv
plot.Correlation(result.cor.kendall, "Kendall")

# evaluation based on experimental data with sd < 0.5
result.cor.pearson.sd0.5 <- eval.Correlation(real.data = exp.data, pred.data = sub.data,
                                             sd.use = 0.1)
head(result.cor.pearson.sd0.5)
plot.Correlation(result.cor.pearson.sd0.5, "Pearson sd < 0.5")
##############################################


####### cut-off-based evaluation ####### 
# value above the given threshold will be considered as positive 
result.auc.0.9 <- eval.AUC(real.data =exp.data, pred.data = sub.data, 
                           threshold = 0.9)
head(result.auc.0.9)
tab.result.auc.0.9 <- result.auc.0.9
export.tab.result.auc.0.9 <- as.matrix(tab.result.auc.0.9)
write.table(export.tab.result.auc.0.9,file="/Users/mabelfurutsuki/Box/Mabel/GAA/plots/table.result.auc.0.9.tsv")
#export summary as tsv
tab.result.auc.0.9 <- summary(result.auc.0.9)
export.tab.result.auc.0.9 <- as.matrix(tab.result.auc.0.9)
write.table(export.tab.result.auc.0.9,file="/Users/mabelfurutsuki/Box/Mabel/GAA/plots/summarytable.result.auc.0.9.tsv")

# evaluation based on experimental data with sd < 0.5
result.auc.0.9.sd0.5 <- eval.AUC(real.data =exp.data, pred.data = sub.data, 
                             threshold = 0.9, sd.use = 0.5)
head(result.auc.0.9.sd0.5)
tab.result.auc.0.9.sd0.5 <- result.auc.0.9.sd0.5
export.tab.result.auc.0.9.sd0.5 <- as.matrix(tab.result.auc.0.9.sd0.5)
write.table(export.tab.result.auc.0.9.sd0.5,file="/Users/mabelfurutsuki/Box/Mabel/GAA/plots/table.result.auc.0.9.sd0.5.tsv")
#export summary as tsv
tab.result.auc.0.9.sd0.5 <- summary(result.auc.0.9.sd0.5)
export.tab.result.auc.0.9.sd0.5 <- as.matrix(tab.result.auc.0.9.sd0.5)
write.table(export.tab.result.auc.0.9.sd0.5,file="/Users/mabelfurutsuki/Box/Mabel/GAA/plots/summarytable.result.auc.0.9.sd0.5.tsv")
########################################

#######################################
result.auc.0.1.sd0.5 <- eval.AUC(real.data =exp.data, pred.data = sub.data, 
                                 threshold = 0.1, sd.use = 0.5)
head(result.auc.0.1.sd0.5)
tab.result.auc.0.1.sd0.5 <- result.auc.0.1.sd0.5
export.tab.result.auc.0.1.sd0.5 <- as.matrix(tab.result.auc.0.1.sd0.5)
write.table(export.tab.result.auc.0.1.sd0.5,file="/Users/mabelfurutsuki/Box/Mabel/GAA/plots/table.result.auc.0.1.sd0.5.tsv")
#export summary as tsv
tab.result.auc.0.1.sd0.5 <- summary(result.auc.0.1.sd0.5)
export.tab.result.auc.0.1.sd0.5 <- as.matrix(tab.result.auc.0.1.sd0.5)
write.table(export.tab.result.auc.0.1.sd0.5,file="/Users/mabelfurutsuki/Box/Mabel/GAA/plots/summarytable.result.auc.0.1.sd0.5.tsv")
#######################################


################ RMSD ##################
result.rmsd <- eval.RMSD(real.data = exp.data, pred.data = sub.data)
head(result.rmsd)
tab.result.rmsd <- result.rmsd
export.tab.result.rmsd <- as.matrix(tab.result.rmsd)
write.table(export.tab.result.rmsd,file="/Users/mabelfurutsuki/Box/Mabel/GAA/plots/table.result.rmsd.tsv")
tab.result.rmsd <- summary(result.rmsd)
export.tab.result.rmsd <- as.matrix(tab.result.rmsd)
write.table(export.tab.result.rmsd,file="/Users/mabelfurutsuki/Box/Mabel/GAA/plots/summarytable.result.rmsd.tsv")
plot.RMSD(result.rmsd, method="")

# evaluation based on experimental data with sd < 0.5 (this should be the input for plotting)
result.rmsd.sd0.5 <- eval.RMSD(real.data = exp.data, pred.data = sub.data, sd.use = 0.5)
head(result.rmsd.sd0.5)
#export result as tsv
tab.result.rmsd.sd0.5 <- result.rmsd.sd0.5
export.tab.result.rmsd.sd0.5 <- as.matrix(tab.result.rmsd.sd0.5)
write.table(export.tab.result.rmsd.sd0.5,file="/Users/mabelfurutsuki/Box/Mabel/GAA/plots/table.result.rmsd.sd0.5.tsv")
#export summary as tsv
tab.result.rmsd.sd0.5 <- summary(result.rmsd.sd0.5)
export.tab.result.rmsd.sd0.5 <- as.matrix(tab.result.rmsd.sd0.5)
write.table(export.tab.result.rmsd.sd0.5,file="/Users/mabelfurutsuki/Box/Mabel/GAA/plots/summarytable.result.rmsd.sd0.5.tsv")
plot.RMSD(result.rmsd, method="SD < 0.5")


#density-distance-based (average distance of the predicted value 
#to a experimental normal distribution, experimental sd required) RMSD
#normal distribution weighted distance is calculated by Density.Distance(pred.value,exp.value)
result.rmsd.density <- eval.RMSD(real.data = exp.data, pred.data = sub.data,
                                 density.distance = T)
head(result.rmsd.density)
#export result as tsv
tab.result.rmsd.density <- result.rmsd.density
export.tab.result.rmsd.density <- as.matrix(tab.result.rmsd.density)
write.table(export.tab.result.rmsd.density,file="/Users/mabelfurutsuki/Box/Mabel/GAA/plots/table.result.rmsd.density.tsv")
#export summary as tsv
tab.result.rmsd.density <- summary(result.rmsd.density)
export.tab.result.rmsd.density <- as.matrix(tab.result.rmsd.density)
write.table(export.tab.result.rmsd.density,file="/Users/mabelfurutsuki/Box/Mabel/GAA/plots/summarytable.result.rmsd.density.tsv")
plot.RMSD(result.rmsd, method="Density")

#density-distance-based RMSD with adjustment
#Density.Distance(pred.value,exp.value,adjusted =T)
result.rmsd.density.adjust <- eval.RMSD(real.data = exp.data, pred.data = sub.data,
                                        density.distance = T, density.distance.adjust = T)
head(result.rmsd.density.adjust)
#export result as tsv
tab.result.rmsd.density.adjust <- result.rmsd.density.adjust
export.tab.result.rmsd.density.adjust <- as.matrix(tab.result.rmsd.density.adjust)
write.table(export.tab.result.rmsd.density.adjust,file="/Users/mabelfurutsuki/Box/Mabel/GAA/plots/table.result.rmsd.density.adjust.tsv")
#export summary as tsv
tab.result.rmsd.density.adjust <- summary(result.rmsd.density.adjust)
export.tab.result.rmsd.density.adjust <- as.matrix(tab.result.rmsd.density.adjust)
write.table(export.tab.result.rmsd.density.adjust,file="/Users/mabelfurutsuki/Box/Mabel/GAA/plots/summarytable.result.rmsd.density.adjust.tsv")
plot.RMSD(result.rmsd, method="Density Distance Based with Adjustment")

#density-distance-based RMSD + extreme sd filtering (not required for most cases)
result.rmsd.density.sd1.5 <- eval.RMSD(real.data = exp.data, pred.data = sub.data,
                                        density.distance = T, sd.use = 1.5)
head(result.rmsd.density.sd1.5)
#export result as tsv
tab.result.rmsd.density.sd1.5 <- result.rmsd.density.sd1.5
export.tab.result.rmsd.density.sd1.5 <- as.matrix(tab.result.rmsd.density.sd1.5)
write.table(export.tab.result.rmsd.density.sd1.5,file="/Users/mabelfurutsuki/Box/Mabel/GAA/plots/table.result.rmsd.density.sd1.5.tsv")
#export summary as tsv
tab.result.rmsd.density.sd1.5 <- summary(result.rmsd.density.sd1.5)
export.tab.result.rmsd.density.sd1.5 <- as.matrix(tab.result.rmsd.density.sd1.5)
write.table(export.tab.result.rmsd.density.sd1.5,file="/Users/mabelfurutsuki/Box/Mabel/GAA/plots/summarytable.result.rmsd.density.sd1.5.tsv")
plot.RMSD(result.rmsd, method="Density Distance Based + Extreme SD Filtering")
########################################


##### between-method evaluation ############
#correlation between methods
result.bM.spearman <-eval.Correlation.Between(real.data = exp.data, pred.data = sub.data,
                                              method = "spearman")
head(result.bM.spearman$coefficient)
head(result.bM.spearman$p.value)
tab.result.bM.spearman <- result.bM.spearman
export.tab.result.bM.spearman <- as.matrix(tab.result.bM.spearman)
write.table(export.tab.result.bM.spearman,file="/Users/mabelfurutsuki/Box/Mabel/GAA/plots/table.result.bM.spearman.tsv")
tab.result.bM.spearman <- summary(result.bM.spearman)
export.tab.result.bM.spearman <- as.matrix(tab.result.bM.spearman)
write.table(export.tab.result.bM.spearman,file="/Users/mabelfurutsuki/Box/Mabel/GAA/plots/summarytable.result.bM.spearman.tsv")
plot.Correlation.Between(result.bM.spearman$coefficient, method="Spearman")


result.bM.spearman.sd0.5 <-eval.Correlation.Between(real.data = exp.data, pred.data = sub.data, method = "spearman", sd.use = 0.5)
head(result.bM.spearman.sd0.5$coefficient)
head(result.bM.spearman.sd0.5$p.value)
tab.result.bM.spearman.sd0.5 <- result.bM.spearman.sd0.5
export.tab.result.bM.spearman.sd0.5 <- as.matrix(tab.result.bM.spearman.sd0.5)
write.table(export.tab.result.bM.spearman.sd0.5,file="/Users/mabelfurutsuki/Box/Mabel/GAA/plots/table.result.bM.spearman.sd0.5.tsv")
tab.result.bM.spearman.sd0.5 <- summary(result.bM.spearman.sd0.5)
export.tab.result.bM.spearman.sd0.5 <- as.matrix(tab.result.bM.spearman.sd0.5)
write.table(export.tab.result.bM.spearman.sd0.5,file="/Users/mabelfurutsuki/Box/Mabel/GAA/plots/summarytable.result.bM.spearman.sd0.5.tsv")
plot.Correlation.Between(result.bM.spearman$coefficient, method="Spearman SD 0.5")
##################################################


#partial-correlation evaluation (unique contribution of each method)################################
result.pCor <- eval.Partial.Correlation(real.data = exp.data, pred.data = sub.data, method = "spearman")
head(result.pCor)
tab.result.pCor <- result.pCor
export.tab.result.pCor <- as.matrix(tab.result.pCor)
write.table(export.tab.result.pCor,file="/Users/mabelfurutsuki/Box/Mabel/GAA/plots/table.result.pCor.tsv")
tab.result.pCor <- summary(result.pCor)
export.tab.result.pCor <- as.matrix(tab.result.pCor)
write.table(export.tab.result.pCor,file="/Users/mabelfurutsuki/Box/Mabel/GAA/plots/summarytable.result.pCor.tsv")
plot.Partial.Correlation(result.pCor, "Spearman", TRUE)


result.pCor.sd0.5 <- eval.Partial.Correlation(real.data = exp.data, pred.data = sub.data, method = "spearman", sd.use = 0.5)
result.pCor.sd0.5
tab.result.pCor.sd0.5 <- result.pCor.sd0.5
export.tab.result.pCor.sd0.5 <- as.matrix(tab.result.pCor.sd0.5)
write.table(export.tab.result.pCor.sd0.5,file="/Users/mabelfurutsuki/Box/Mabel/GAA/plots/table.result.pCor.sd0.5.tsv")
tab.result.pCor.sd0.5 <- summary(result.pCor.sd0.5)
export.tab.result.pCor.sd0.5 <- as.matrix(tab.result.pCor.sd0.5)
write.table(export.tab.result.pCor.sd0.5,file="/Users/mabelfurutsuki/Box/Mabel/GAA/plots/summarytable.result.pCor.sd0.5.tsv")
plot.Partial.Correlation(result.pCor.sd0.5, "Spearman SD < 0.5", TRUE)
####################################################################################################


#----------Scatterplots---------------------------
# Create experimental-observed scatterplots
ExperimentalVsPredicted(path.prediction = submission.files, real.data = exp.val.cleaned)


#---------PCA Plot---------------------------------
Plot.PCA(na.omit(total), labels=FALSE, legend=TRUE) 

#---------Results Table----------------------------
#https://datascienceplus.com/exporting-data-from-r/
total.results.table <- result.cor.pearson
total.results.table <- total.results.table[-c(2:3)]
total.results.table[2] <- result.cor.spearman[1]
total.results.table[3] <- result.cor.kendall[1]
total.results.table[4] <- result.cor.pearson.sd0.5[1]
total.results.table[5] <- result.auc.0.9[1]
total.results.table[6] <- result.auc.0.9.sd0.5[1]
total.results.table[7] <- result.rmsd[1]
total.results.table[8] <- result.rmsd.sd0.5[1]
total.results.table[9] <- result.rmsd.density[1]
total.results.table[10] <- result.rmsd.density.adjust[1]
total.results.table[11] <- result.rmsd.density.sd1.5[1]
# total.results.table <- as.matrix(total.results.table)
write.table(total.results.table, file="/Users/mabelfurutsuki/Box/Mabel/GAA/plots/total.results.table.csv", sep=",")
write.table(total.results.table, file="/Users/mabelfurutsuki/Box/Mabel/GAA/plots/total.results.table.tsv", sep="\t")

