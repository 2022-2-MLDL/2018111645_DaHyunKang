rm(list = ls())
# install.packages("ggfortify")
library(ggplot2)
library(ggfortify)
library(readr)

## tumor data
tumor <- read_delim("BC-TCGA-Tumor.txt", delim = "\t", escape_double = FALSE, trim_ws = TRUE)

## normal data
normal <- read_delim("BC-TCGA-Normal.txt", delim = "\t", escape_double = FALSE, trim_ws = TRUE)


## 행 이름 지정
tumor <- data.frame(tumor, row.names = tumor[,1]$`Hybridization REF`)
tumor <- tumor[,-1]
normal <- data.frame(normal, row.names = normal[,1]$`Hybridization REF`)
normal <- normal[,-1]


## tumor normal 데이터 합치기
dt <- cbind(tumor, normal)


## 각 행의 원소 속성을 반환하는 코드
table(unlist(lapply(sapply(dt, FUN = "class"), FUN = "[", 1)))

## 모든 원소를 numeric으로 변경
dt[, sapply(dt, FUN = "is.character")] = sapply(dt[, sapply(dt, FUN = "is.character")],FUN = "as.numeric")


## 결측값(NA) 제거
table(is.na(dt))
dt <- na.omit(dt)


## dt의 행과 열을 교체
dt <- t(dt)
rownames(dt)=NULL
write.csv(dt,file="bc_data.csv")
## sample labeling
sample.class <- c(rep('Tumor',529), rep('Normal',61))
# sample.colors <- c(rep('red',529), rep('black',61))
dt2 <- cbind(dt,sample.class)


## PCA plot
pca_res <- prcomp(dt,scale.=TRUE)
autoplot(pca_res, data=dt2, colour='sample.class')



