################################################
##Summaries of the data
################################################
load("./data/ptsTrainList_clean.RData")

#How many of each tooth type
unlist(lapply(ptsTrainList_clean, length))

#How many in each tribe
ref_file <- read.csv("./data/reference_db.csv")

##########################################
## Table 1
##########################################
for (tooth in c("LM1","LM2","LM3","UM1","UM2","UM3")){
temp <- subset(ref_file, Image.Name %in% names(ptsTrainList_clean[[tooth]])) 
print(table(temp$Tribe))
}



##########################################
## Table 2
##########################################
res <- data.frame()

for (tooth in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  for (side in 1:2){
    for (k in c(5,10,20)){
      for (M in c(5,10,20)){
        try(load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=",k,"_M=",M,"_tooth=",tooth,"_summaries.RData")))
        
        res <- rbind(res,data.frame(tooth = tooth,side = side,k = k,M = M,accuracy_imputed = acc_imputed[1], accuracy_part = acc_part[1]))
        
      }}}}

test <- rbind(
  c("LM1",1,round(res$accuracy_imputed[res$tooth=="LM1" & res$side == 1],3)),
  c("LM1",2,round(res$accuracy_imputed[res$tooth=="LM1" & res$side == 2],3)),
  c("LM2",1,round(res$accuracy_imputed[res$tooth=="LM2" & res$side == 1],3)),
  c("LM2",2,round(res$accuracy_imputed[res$tooth=="LM2" & res$side == 2],3)),
  c("LM3",1,round(res$accuracy_imputed[res$tooth=="LM3" & res$side == 1],3)),
  c("LM3",2,round(res$accuracy_imputed[res$tooth=="LM3" & res$side == 2],3)),
  c("UM1",1,round(res$accuracy_imputed[res$tooth=="UM1" & res$side == 1],3)),
  c("UM1",2,round(res$accuracy_imputed[res$tooth=="UM1" & res$side == 2],3)),
  c("UM2",1,round(res$accuracy_imputed[res$tooth=="UM2" & res$side == 1],3)),
  c("UM2",2,round(res$accuracy_imputed[res$tooth=="UM2" & res$side == 2],3)),
  c("UM3",1,round(res$accuracy_imputed[res$tooth=="UM3" & res$side == 1],3)),
  c("UM3",2,round(res$accuracy_imputed[res$tooth=="UM3" & res$side == 2],3))
)

test <- cbind(test,round(res$accuracy_part[seq(1,nrow(res),9)],3)[1:12])

library(xtable)
addtorow <- list()
addtorow$pos <- list(0, 0)
addtorow$command <- c("& & \\multicolumn{3}{c}{k=5} & \\multicolumn{3}{c}{k=10} & \\multicolumn{3}{c}{k=20} & \\\\\n",
                      "Tooth & & Side & M=5 & M=10 & M=20 & M=5 & M=10 & M=20 & M=5 & M=10 & M=20 & Partial \\\\\n")
print(xtable(test), add.to.row = addtorow, include.colnames = FALSE, include.rownames = FALSE)


##########################################
## Table 3
##########################################
res <- data.frame()

for (tooth in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  for (side in 1:2){
    for (k in c(5,10,20)){
      for (M in c(5,10,20)){
try(load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=",k,"_M=",M,"_tooth=",tooth,"scaled_summaries.RData")))

res <- rbind(res,data.frame(tooth = tooth,side = side,k = k,M = M,accuracy_imputed = acc_imputed[1], accuracy_part = acc_part[1]))

      }}}}

  test <- rbind(
        c("LM1",1,round(res$accuracy_imputed[res$tooth=="LM1" & res$side == 1],3)),
        c("LM1",2,round(res$accuracy_imputed[res$tooth=="LM1" & res$side == 2],3)),
        c("LM2",1,round(res$accuracy_imputed[res$tooth=="LM2" & res$side == 1],3)),
         c("LM2",2,round(res$accuracy_imputed[res$tooth=="LM2" & res$side == 2],3)),
        c("LM3",1,round(res$accuracy_imputed[res$tooth=="LM3" & res$side == 1],3)),
        c("LM3",2,round(res$accuracy_imputed[res$tooth=="LM3" & res$side == 2],3)),
        c("UM1",1,round(res$accuracy_imputed[res$tooth=="UM1" & res$side == 1],3)),
        c("UM1",2,round(res$accuracy_imputed[res$tooth=="UM1" & res$side == 2],3)),
        c("UM2",1,round(res$accuracy_imputed[res$tooth=="UM2" & res$side == 1],3)),
        c("UM2",2,round(res$accuracy_imputed[res$tooth=="UM2" & res$side == 2],3)),
        c("UM3",1,round(res$accuracy_imputed[res$tooth=="UM3" & res$side == 1],3)),
        c("UM3",2,round(res$accuracy_imputed[res$tooth=="UM3" & res$side == 2],3))
        )

  test <- cbind(test,round(res$accuracy_part[seq(1,nrow(res),9)],3))

  library(xtable)
addtorow <- list()
addtorow$pos <- list(0, 0)
addtorow$command <- c("& & \\multicolumn{3}{c}{k=5} & \\multicolumn{3}{c}{k=10} & \\multicolumn{3}{c}{k=20} &  \\\\\n",
"Tooth & Side & M=5 & M=10 & M=20 & M=5 & M=10 & M=20 & M=5 & M=10 & M=20 & Partial  \\\\\n")
print(xtable(test), add.to.row = addtorow, include.colnames = FALSE, include.rownames = FALSE)



###################################################
## Figure 5 - Tribe Unscaled Accuracy Results 
###################################################
library(cowplot)
library(ggplot2)
acc_res_list <- list()
for (tooth in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  for (side in 1:2){print(c(tooth, side))
load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=5_tooth=",tooth,"_summaries.RData"))
acc_part_k5_M5 <- acc_part
acc_imputed_k5_M5 <- acc_imputed[1:20]
 rm(acc_part)
rm(acc_imputed)

load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=10_tooth=",tooth,"_summaries.RData"))
acc_part_k5_M10 <- acc_part
acc_imputed_k5_M10 <- acc_imputed[1:20]
rm(acc_part)
rm(acc_imputed)

load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=20_tooth=",tooth,"_summaries.RData"))
acc_part_k5_M20 <- acc_part
acc_imputed_k5_M20 <- acc_imputed[1:20]
rm(acc_part)
rm(acc_imputed)

load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=5_tooth=",tooth,"_summaries.RData"))
acc_part_k10_M5 <- acc_part
acc_imputed_k10_M5 <- acc_imputed[1:20]
rm(acc_part)
rm(acc_imputed)

load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=10_tooth=",tooth,"_summaries.RData"))
acc_part_k10_M10 <- acc_part
acc_imputed_k10_M10 <- acc_imputed[1:20]
rm(acc_part)
rm(acc_imputed)

load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=20_tooth=",tooth,"_summaries.RData"))
acc_part_k10_M20 <- acc_part
acc_imputed_k10_M20 <- acc_imputed[1:20]
rm(acc_part)
rm(acc_imputed)

load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=5_tooth=",tooth,"_summaries.RData"))
acc_part_k20_M5 <- acc_part
acc_imputed_k20_M5 <- acc_imputed[1:20]
rm(acc_part)
rm(acc_imputed)

load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=10_tooth=",tooth,"_summaries.RData"))
acc_part_k20_M10 <- acc_part
acc_imputed_k20_M10 <- acc_imputed[1:20]
rm(acc_part)
rm(acc_imputed)

load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=20_tooth=",tooth,"_summaries.RData"))
acc_part <- acc_part[1:20]
acc_imputed_k20_M20 <- acc_imputed[1:20]

dat <- data.frame(knn = rep(1:20,10), acc = c(acc_part,acc_imputed_k5_M5,acc_imputed_k5_M10,acc_imputed_k5_M20,acc_imputed_k10_M5,acc_imputed_k10_M10,acc_imputed_k10_M20,acc_imputed_k20_M5,acc_imputed_k20_M10,acc_imputed_k20_M20), k = c(rep("No Imp", 20), rep(c(5,10,20),each = 60)), M = c(rep("No Imp",20),rep(rep(c(5,10,20), each = 20),3)))


dat$k <- factor(dat$k, levels = c("20", "10", "5", "No Imp"))
dat$M <- factor(dat$M, levels = c("20", "10", "5", "No Imp"))


if(side == 1){
g1 <- ggplot(aes(x = knn, y = acc, colour = k, shape = M), data = dat) + geom_point() + geom_line() + ggtitle(paste0("tooth=",tooth,", side=",side)) + ylim(.6,.95) + theme(legend.position = "none")
}

if (side == 2){
g2 <- ggplot(aes(x = knn, y = acc, colour = k, shape = M), data = dat) + geom_point() + geom_line() + ggtitle(paste0("tooth=",tooth,", side=",side))  + ylim(.6,.95)
}

#acc_res_list[[paste0(tooth,"_",side)]] <- data.frame(accuracy  = c(mean(acc_imputed_20[1:20]),mean(acc_imputed_10[1:20]),mean(acc_imputed_5[1:20]),mean(acc_part_5[1:20])), tooth_type = rep(paste0(tooth,"_",side),4),  M_k = c("M20 l20", "M10 l10", "M5 l5", "No Imp"))

  }

  #png(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/fig/acc_by_tooth_by_knn_tribe_unscaled_",tooth,".png"), res = 300, units = "in", w = 8, h =4)
  library(gridExtra)
legend <- get_legend(g2)
g2 <- g2 + theme(legend.position="none")
grid.arrange(g1, g2, legend, ncol=3, widths=c(2.3, 2.3, 0.8))
#dev.off()

}


###################################################
## Figure 6 - Tribe Scaled Accuracy Results 
###################################################
library(ggplot2)
acc_res_list <- list()
for (tooth in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  for (side in 1:2){print(c(tooth, side))
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=5_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k5_M5 <- acc_part
    acc_imputed_k5_M5 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=10_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k5_M10 <- acc_part
    acc_imputed_k5_M10 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=20_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k5_M20 <- acc_part
    acc_imputed_k5_M20 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=5_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k10_M5 <- acc_part
    acc_imputed_k10_M5 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=10_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k10_M10 <- acc_part
    acc_imputed_k10_M10 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=20_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k10_M20 <- acc_part
    acc_imputed_k10_M20 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=5_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k20_M5 <- acc_part
    acc_imputed_k20_M5 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=10_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k20_M10 <- acc_part
    acc_imputed_k20_M10 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=20_tooth=",tooth,"scaled_summaries.RData"))
    acc_part <- acc_part[1:20]
    acc_imputed_k20_M20 <- acc_imputed[1:20]
    
    dat <- data.frame(knn = rep(1:20,10), acc = c(acc_part,acc_imputed_k5_M5,acc_imputed_k5_M10,acc_imputed_k5_M20,acc_imputed_k10_M5,acc_imputed_k10_M10,acc_imputed_k10_M20,acc_imputed_k20_M5,acc_imputed_k20_M10,acc_imputed_k20_M20), k = c(rep("No Imp", 20), rep(c(5,10,20),each = 60)), M = c(rep("No Imp",20),rep(rep(c(5,10,20), each = 20),3)))
    
    #dat <- data.frame(knn = rep(1:20,10), acc = c(acc_part,acc_imputed_k5_M5,acc_imputed_k5_M10,acc_imputed_k5_M20,acc_imputed_k10_M5,acc_imputed_k10_M10,acc_imputed_k10_M20,acc_imputed_k20_M5,acc_imputed_k20_M10,acc_imputed_k20_M20), type = rep(c("no imp","k5 M5","k5 M10","k5 M20","k10 M5","k10 M10", "k10 M20","k20 M5","k20 M10", "k20 M20"),each = 20))
    
    dat$k <- factor(dat$k, levels = c("20", "10", "5", "No Imp"))
    dat$M <- factor(dat$M, levels = c("20", "10", "5", "No Imp"))
    
    library(ggplot2)
    if (side == 1){
      g1 <- ggplot(aes(x = knn, y = acc, colour = k, shape = M), data = dat) + geom_point() + geom_line() + ggtitle(paste0("tooth=",tooth,", side=",side)) + ylim(.4,.86) + theme(legend.position = "none")
    }
    
    if (side == 2){
      g2 <- ggplot(aes(x = knn, y = acc, colour = k, shape = M), data = dat) + geom_point() + geom_line() + ggtitle(paste0("tooth=",tooth,", side=",side))  + ylim(.4,.86)
    }
    
    
    

    
  }
  library(cowplot)
  #png(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/fig/acc_by_tooth_by_knn_tribe_scaled_",tooth,".png"), res = 300, units = "in", w = 8, h =4)
  legend <- get_legend(g2)
  g2 <- g2 + theme(legend.position="none")
  grid.arrange(g1, g2, legend, ncol=3, widths=c(2.3, 2.3, 0.8))
  #dev.off()
  
  
}



##################################################
#Figure 3: Accuracy of tribe classification unscaled with knn = 1
##################################################

   #tooth <- "LM1"
#side <- 1
library(ggplot2)
acc_res_list <- list()
for (tooth in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  for (side in 1:2){#print(c(tooth, side))
load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=5_tooth=",tooth,"_summaries.RData"))
acc_part_k5_M5 <- acc_part
acc_imputed_k5_M5 <- acc_imputed
rm(acc_part)
rm(acc_imputed)

load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=10_tooth=",tooth,"_summaries.RData"))
acc_part_k5_M10 <- acc_part
acc_imputed_k5_M10 <- acc_imputed
rm(acc_part)
rm(acc_imputed)

load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=20_tooth=",tooth,"_summaries.RData"))
acc_part_k5_M20 <- acc_part
acc_imputed_k5_M20 <- acc_imputed
rm(acc_part)
rm(acc_imputed)

load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=5_tooth=",tooth,"_summaries.RData"))
acc_part_k10_M5 <- acc_part
acc_imputed_k10_M5 <- acc_imputed
rm(acc_part)
rm(acc_imputed)

load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=10_tooth=",tooth,"_summaries.RData"))
acc_part_k10_M10 <- acc_part
acc_imputed_k10_M10 <- acc_imputed
rm(acc_part)
rm(acc_imputed)

load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=20_tooth=",tooth,"_summaries.RData"))
acc_part_k10_M20 <- acc_part
acc_imputed_k10_M20 <- acc_imputed
rm(acc_part)
rm(acc_imputed)

load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=5_tooth=",tooth,"_summaries.RData"))
acc_part_k20_M5 <- acc_part
acc_imputed_k20_M5 <- acc_imputed
rm(acc_part)
rm(acc_imputed)

load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=10_tooth=",tooth,"_summaries.RData"))
acc_part_k20_M10 <- acc_part
acc_imputed_k20_M10 <- acc_imputed
rm(acc_part)
rm(acc_imputed)

load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=20_tooth=",tooth,"_summaries.RData"))
acc_part_k20_M20 <- acc_part
acc_imputed_k20_M20 <- acc_imputed
rm(acc_part)
rm(acc_imputed)

# dat <- data.frame(knn = rep(1:60,4), acc = c(acc_part_5,acc_imputed_5,acc_imputed_10,acc_imputed_20), type = rep(c("no imp","k5 M5","k10 M10", "k20 M20"),each = 60))
# library(ggplot2)
# g <- ggplot(aes(x = knn, y = acc, colour = type), data = dat) + geom_point() + geom_line() + ggtitle(paste0("tooth=",tooth,", side=",side))
# print(g)

#print(c(mean(acc_imputed_20[1:20]),mean(acc_imputed_10[1:20]),mean(acc_imputed_5[1:20]),mean(acc_part_5[1:20])))

# acc_res_list[[paste0(tooth,"_",side)]] <- data.frame(
#   accuracy  = c(mean(acc_imputed_k20_M20[1:20]),mean(acc_imputed_k20_M10[1:20]),mean(acc_imputed_k20_M5[1:20]),mean(acc_imputed_k10_M20[1:20]),mean(acc_imputed_k10_M10[1:20]),mean(acc_imputed_k10_M5[1:20]),mean(acc_imputed_k5_M20[1:20]),mean(acc_imputed_k5_M10[1:20]),mean(acc_imputed_k5_M5[1:20]),mean(acc_part_k20_M20[1:20])),
#   tooth_type = rep(paste0(tooth,"_",side),10),  k = c(rep(20,3),rep(10,3),rep(5,3),rep("No Imp",1)), M = c(rep(c(20,10,5),3),rep("No Imp",1)))

#knn = 1
acc_res_list[[paste0(tooth,"_",side)]] <- data.frame(
  accuracy  = c(mean(acc_imputed_k20_M20[1]),mean(acc_imputed_k20_M10[1]),mean(acc_imputed_k20_M5[1]),mean(acc_imputed_k10_M20[1]),mean(acc_imputed_k10_M10[1]),mean(acc_imputed_k10_M5[1]),mean(acc_imputed_k5_M20[1]),mean(acc_imputed_k5_M10[1]),mean(acc_imputed_k5_M5[1]),mean(acc_part_k20_M20[1])),
  tooth_type = rep(paste0(tooth,"_",side),10),  k = c(rep(20,3),rep(10,3),rep(5,3),rep("No Imp",1)), M = c(rep(c(20,10,5),3),rep("No Imp",1)))


  }}

dat <- do.call(rbind, acc_res_list)
dat$k <- factor(dat$k, levels = c("20", "10", "5", "No Imp"))
dat$M <- factor(dat$M, levels = c("20", "10", "5", "No Imp"))

#png(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/fig/acc_by_tooth_knn1_tribe_unscaled.png"), res = 300, units = "in", w = 8, h =4)
ggplot(aes(x = tooth_type, y = accuracy, col = k, shape = M), data = dat) + geom_point()
#dev.off()

############################################################
## Figure 4: Accuracy of tribe classification scaled
############################################################

library(ggplot2)
acc_res_list <- list()
for (tooth in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  for (side in 1:2){#print(c(tooth, side))
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=5_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k5_M5 <- acc_part
    acc_imputed_k5_M5 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=10_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k5_M10 <- acc_part
    acc_imputed_k5_M10 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=20_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k5_M20 <- acc_part
    acc_imputed_k5_M20 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=5_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k10_M5 <- acc_part
    acc_imputed_k10_M5 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=10_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k10_M10 <- acc_part
    acc_imputed_k10_M10 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=20_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k10_M20 <- acc_part
    acc_imputed_k10_M20 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=5_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k20_M5 <- acc_part
    acc_imputed_k20_M5 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=10_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k20_M10 <- acc_part
    acc_imputed_k20_M10 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=20_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k20_M20 <- acc_part
    acc_imputed_k20_M20 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    # dat <- data.frame(knn = rep(1:60,4), acc = c(acc_part_5,acc_imputed_5,acc_imputed_10,acc_imputed_20), type = rep(c("no imp","k5 M5","k10 M10", "k20 M20"),each = 60))
    # library(ggplot2)
    # g <- ggplot(aes(x = knn, y = acc, colour = type), data = dat) + geom_point() + geom_line() + ggtitle(paste0("tooth=",tooth,", side=",side))
    # print(g)
    
    #print(c(mean(acc_imputed_20[1:20]),mean(acc_imputed_10[1:20]),mean(acc_imputed_5[1:20]),mean(acc_part_5[1:20])))
    
    # acc_res_list[[paste0(tooth,"_",side)]] <- data.frame(
    #   accuracy  = c(mean(acc_imputed_k20_M20[1:20]),mean(acc_imputed_k20_M10[1:20]),mean(acc_imputed_k20_M5[1:20]),mean(acc_imputed_k10_M20[1:20]),mean(acc_imputed_k10_M10[1:20]),mean(acc_imputed_k10_M5[1:20]),mean(acc_imputed_k5_M20[1:20]),mean(acc_imputed_k5_M10[1:20]),mean(acc_imputed_k5_M5[1:20]),mean(acc_part_k20_M20[1:20])),
    #   tooth_type = rep(paste0(tooth,"_",side),10),  k = c(rep(20,3),rep(10,3),rep(5,3),rep("No Imp",1)), M = c(rep(c(20,10,5),3),rep("No Imp",1)))
    
    #knn = 1
    acc_res_list[[paste0(tooth,"_",side)]] <- data.frame(
      accuracy  = c(mean(acc_imputed_k20_M20[1]),mean(acc_imputed_k20_M10[1]),mean(acc_imputed_k20_M5[1]),mean(acc_imputed_k10_M20[1]),mean(acc_imputed_k10_M10[1]),mean(acc_imputed_k10_M5[1]),mean(acc_imputed_k5_M20[1]),mean(acc_imputed_k5_M10[1]),mean(acc_imputed_k5_M5[1]),mean(acc_part_k20_M20[1])),
      tooth_type = rep(paste0(tooth,"_",side),10),  k = c(rep(20,3),rep(10,3),rep(5,3),rep("No Imp",1)), M = c(rep(c(20,10,5),3),rep("No Imp",1)))
    
    
  }}

dat <- do.call(rbind, acc_res_list)
dat$k <- factor(dat$k, levels = c("20", "10", "5", "No Imp"))
dat$M <- factor(dat$M, levels = c("20", "10", "5", "No Imp"))

#png(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/fig/acc_by_tooth_knn1_tribe_scaled.png"), res = 300, units = "in", w = 8, h =4)
ggplot(aes(x = tooth_type, y = accuracy, col = k, shape = M), data = dat) + geom_point()
#dev.off()

