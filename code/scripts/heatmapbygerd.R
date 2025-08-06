library(readxl)
library(pheatmap)
library(ComplexHeatmap)
library(ggplot2)
library(ggsci)
library('outliers')
setwd("/data/home/swliu/RProject/GERD/")
dataset <- read.csv('./gerd_2.csv')
# View(dataset)

dataset = dataset[-1]
dataset <- as.data.frame(dataset)
dataset$gender<- ifelse(dataset$gender == "男", 1, 0) 
names(dataset)[names(dataset) == 'age'] <- 'Age'
names(dataset)[names(dataset) == 'gender'] <- 'Gender'
dataset <-dataset[,!names(dataset) %in% c("Name","LES上缘位置cm", "LES下缘位置cm" ,"birthday","检查时间" ,"UES静息压mmHg",
                                "LES位置.距鼻孔.cm","UES残余压mmHg","大缺损收缩.次.","UES上缘位置cm",
                                "小缺损收缩.次.","UES下缘位置cm","UES长度cm" ,"UES位置.距鼻孔.cm" ,"诊断结果")]
dataset$Belching <- 0
dataset$Heartburn <- 0
dataset$Sour_regurgitation <- 0
dataset$Duration <- 0

for(i in 1:925){
  zs <- dataset[i,'主诉']
  if(grepl('反酸', zs)){
    dataset[i,'Sour_regurgitation'] <- 1
  }
  if(grepl('烧心', zs)){
    dataset[i,'Heartburn'] <- 1
  }
  if(grepl('胸骨后疼痛', zs)){
    dataset[i,'Heartburn'] <- 1
  }
  if(grepl('灼烧', zs)){
    dataset[i,'Heartburn'] <- 1
  }
  if(grepl('嗳气', zs)){
    dataset[i,'Belching'] <- 1
  }
  key_words <- c('反酸','烧心','胸骨后疼痛','灼烧','嗳气')
  if(sapply(key_words, grepl, zs)|> any()){
    gsub("一", "1", zs)
    gsub("二", "2", zs)
    gsub("三", "3", zs)
    gsub("四", "4", zs)
    gsub("五", "5", zs)
    gsub("六", "6", zs)
    gsub("七", "7", zs)
    gsub("八", "8", zs)
    gsub("九", "9", zs)
    gsub("十", "10", zs)
    if(grepl(".*?([0-9]+).*",zs)){
      num_d <- as.numeric(gsub(".*?([0-9]+).*", "\\1", zs)) 
      if(grepl('年', zs)){
        dataset[i,'Duration'] <- num_d * 12
      }else if(grepl('月', zs)){
        dataset[i,'Duration'] <- num_d
      }else if(grepl('周', zs)){
        dataset[i,'Duration'] <- num_d/4
      }
    }
  }
}
dataset <- dataset[,!names(dataset) %in% c('主诉')]

col <- colnames(dataset)
#异常数据处理
for(i in 1:41){
  if(col[i] %in% c("Age","label","Belching","Heartburn","Sour_regurgitation", "Duration","HH")){
    print(col[i])
    next
  }
  q1<-quantile(dataset[,i], 0.05)        #取得时1%时的变量值
  q99<-quantile(dataset[,i], 0.95) 
  #replacement has 1 row, data has 0 说明一个没换
  for(j in 1:925){
    if (dataset[j,i] < q1){
      dataset[j,i] <- q1 
    }
    if (dataset[j,i] > q99){
      dataset[j,i] <- q99
    }
  }

}


rownames(dataset) <- paste0('row',1:925)

dataset = dataset[order(dataset[,'label'],dataset[,'Gender'],dataset[,'Belching'],dataset[,'Heartburn'],dataset[,'Sour_regurgitation'],dataset[,'HH'],dataset[,'Age'],dataset[,'Duration']),]

col = colnames(dataset) 
#col[38:41] =c('Duration','Sour_regurgitation','Heartburn','Belching')
colnames(dataset) <- col
#arrange(dataset, label, Gender,Heartburn)
#dataset[dataset[,39]==2,39] =1
dataset$Gender<- ifelse(dataset$Gender == 1, 'Male', 'Female') 
dataset$Belching<- ifelse(dataset$Belching == 1, 'Yes', 'No') 
dataset$HH<- ifelse(dataset$HH == 1, 'Yes', 'No') 
dataset$Heartburn<- ifelse(dataset$Heartburn == 1, 'Yes', 'No') 
dataset$Sour_regurgitation<- ifelse(dataset$Sour_regurgitation == 1, 'Yes', 'No') 
dataset$label<- ifelse(dataset$label == 1, 'Surgery', 'Surgery-free') 



count_data <- as.matrix(count_data)
count_data=apply(count_data,2,as.numeric)


max(count_data[2])
num_data = dataset[,c("WMT" ,"RE" ,"TRE","PUT","PST",
                      "PTT","TRAC","LRE","DMS" ,"RE.L.Ac",
                      "RE.L.Wa","RE.L.Al","RE.L.Ak","RE.M.Ac","RE.M.Wa",
                      "RE.M.Al","RE.M.Ak","RE.T.Ac","RE.T.Wa","RE.T.Al",
                      "RE.T.Ak","CFV","LESP",
                      "LES.IRP","DL","DCI","LES.length","IBP",
                      "PIP","MRS.DCI","PC","SC","IEC")]
num_data[num_data == "-"] = "0" 
count_data =  dataset[,c("Duration","Age",'label',"Gender","Belching","Heartburn","Sour_regurgitation","HH")]
count_data$Age <- as.numeric(count_data$Age)
count_data[count_data == "-"] = "20"
num_data <- as.matrix(num_data)
num_data=apply(num_data,2,as.numeric)
num_data <- as.data.frame(num_data)
num_data[is.na(num_data)]
table(is.na(num_data))

for (i in 1:33){
  x = num_data[i]
  num_data[i] = (x - min(x)) / (max(x) - min(x))
}

num_data=apply(num_data,2,as.numeric)
num_data <- as.data.frame(num_data)
num_data = t(num_data)

HHcolor <- c('#f0a1a8','#8abcd1') 
names(HHcolor) <- c("Yes","No")

Heartburncolor <- c('#c08eaf','#feba07') 
names(Heartburncolor) <- c("Yes","No")

SRcolor <- c(rgb(145/255,211/255,192/255),'#bdaead') 
names(SRcolor) <- c("Yes","No") #类型颜色

Agecolor <- colorRampPalette(c("#F1F2E5","#cccccc","#969696","#525252"))(4)
names(Agecolor) <- c("10","40","70","90")

Durationcolor <- colorRampPalette(c("#F1F2E5","#bdc9e1","#74a9cf","#0570b0"))(4)
names(Durationcolor) <- c("0","200","400","600")

Sexcolor <- c(rgb(243/255,135/255,145/255),rgb(163/255,157/255,247/255)) 
names(Sexcolor) <- c("Male","Female") #类型颜色

labelcolor <- colorRampPalette(c("#b3cde3",'#ffe78f'))(2)
names(labelcolor) <- c("Surgery","Surgery-free")

Belchingcolor <-c('#93b3a6','#f3bb82') 
names(Belchingcolor) <- c("Yes","No")
ann_colors <- list(label=labelcolor,Gender= Sexcolor,Sour_regurgitation=SRcolor,
                    Heartburn= Heartburncolor,HH= HHcolor,Belching=Belchingcolor
                    )
par(pin = c(32,16))
par(mar=c(1,1,1,1))
ht<-pheatmap(
  num_data,
  cluster_cols = F,  clustering_method = "ward.D",
  cellheigh = 10,
  cellwidth = 1.5,
  annotation_col = count_data,
  annotation_colors = ann_colors
)

pdf("heatmap.pdf",width=32,height = 16)
draw(ht)
dev.off()

save_pheatmap_pdf(ht, "test.pdf")

b <- as.matrix(b)
b=apply(b,2,as.numeric)

pheatmap(t(b),)

pheatmap(t(b),)
dev.new()

heatmap(num_data)
