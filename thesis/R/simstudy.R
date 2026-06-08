library(kml)
library(tidyverse)
library(Rtsne)
library(tidymodels)
library(rsample)
library(parsnip)
library(caret)
library(SimilarityMeasures)
library(car)
library(fpc)
library(permute)
library(dtw)
library(SimilarityMeasures)
library(TSdist)
library(trajectories)
library(tictoc)
library(ggplot2)
# relabel sub-function
Relabel=function(clustA,clustB){
  GG=length(unique(clustA))
  cell=table(clustA,clustB)
  if(length(table(clustB)) < length(table(clustA))){
    cell=table(clustA,clustB)
    cell=cbind(cell,matrix(0,length(table(clustA)),length(table(clustA))-length(table(clustB))))
    colnames(cell)=as.character(1:length(table(clustA)))
  }
  pM=rbind(1:GG,allPerms(1:GG))
  pMresult=matrix(0,1,dim(pM)[1])
  for(i in 1:dim(pM)[1]){
    for(j in 1:GG){
      pMresult[i]=pMresult[i]+cell[j,pM[i,j]]
    }
  }
  b=which.max(pMresult)
  clustC=clustB
  for(i in 1:GG){
    wh=which(clustB==pM[b,i])
    clustC[wh]=i
  }
  pMb=pM[b,]
  
  misCell=cell
  for(i in 1:GG){
    misCell[i,pMb[i]]=0
  }
  misCL=sum(misCell)/sum(cell)
  
  newCell=table(clustA,clustC)
  return(list(clust=clustC,label=pMb,miscl=misCL,memM=newCell))
}

tsne_f <- function(num, Bite, t.num) {
  F_stat = E_stat = D_stat = perp = Accuracy = matrix(0, Bite, 3)
  F_stat2 = E_stat2 = D_stat2 = perp2 = Accuracy2 = matrix(0, Bite, 3)
  
  for (ite in 1:Bite) {
    
    cat("작업중: ", ite, "\n")
    
    t.n <- t.num
    o.n <- num
    dat <- matrix(0, nrow = t.n * o.n * 3, ncol = 3) #dat[,1]=id, dat[,2]=obs, dat[,3]=time
    dat[,1] = rep(1:(o.n * 3), each = t.n) # 1~o.n*3 t.n만큼씩 각각 반복생성
    obs <- NULL
    time <- NULL
    
    for (i in 1:o.n) {
      idx <- seq(1, 10, length.out = t.n)
      val <- dnorm(idx, 4 + runif(1, -1, 1), 1) * rnorm(1, 35, 2)
      obs <- c(obs,val)
    }
    for (i in 1:o.n) {
      idx <- seq(1, 10, length.out = t.n)
      val <- dnorm(idx, 7 + runif(1, -1, 1), 1) * rnorm(1, 45, 2)
      obs <- c(obs,val)
    }
    for (i in 1:o.n) {
      val <- (0.4 * dnorm(idx, 3 + runif(1, -1, 1), 1) + 0.6 * dnorm(idx, 7 + runif(1, -1, 1), 1)) * rnorm(1, 40, 2)
      obs <- c(obs,val)
    }
    
    time <- NULL
    
    for (j in 1:3) {
      time <- c(time, rep(idx, o.n * 0.4))
      for (i in 1:(o.n * 0.3)) {
        time <- c(time, idx - runif(t.n))
      }
      for (i in 1:(o.n * 0.3)) {
        time <- c(time, idx + runif(t.n))
      }
    }
    
    dat[,2] <- obs
    dat[,3] <- round(time, 1)
    grp <- c(rep(1, o.n), rep(2, o.n), rep(3, o.n))
    
    ## distance calculation
    id.tab <- table(dat[,1])
    id.cum <- cumsum(id.tab)
    n = length(id.tab)
    distF <- distE <- distDTW <- matrix(0, n, n)
    
    
    
    i = 1
    mat.i <- dat[1:id.cum[i], 2]
    time.i <- dat[1:id.cum[i], 3]
    for (j in (i + 1):n) {
      mat.j <- dat[(id.cum[j - 1] + 1):id.cum[j], 2]
      time.j <- dat[(id.cum[j - 1] + 1):id.cum[j], 3]
      distF[i, j] <- distFrechet(time.i, mat.i, time.j, mat.j, FrechetSumOrMax="max")
      distDTW[i, j] <- dtw(mat.i, mat.j)$distance
      distE[i, j] <- stats::dist(rbind(mat.i, mat.j), method = "euclidean")
    }
    
    
    
    for (i in 2:(n - 1)) {
      mat.i <- dat[(id.cum[i - 1] + 1):id.cum[i], 2]
      time.i <- dat[(id.cum[i - 1] + 1):id.cum[i], 3]
      for (j in (i + 1):n) {
        mat.j <- dat[(id.cum[j - 1] + 1):id.cum[j], 2]
        time.j <- dat[(id.cum[j - 1] + 1):id.cum[j], 3]
        distF[i, j]<-distFrechet(time.i, mat.i, time.j, mat.j, FrechetSumOrMax="max")
        distDTW[i, j]<-dtw(mat.i, mat.j)$distance
        distE[i, j]<-stats::dist(rbind(mat.i, mat.j), method = "euclidean")
      }
    }
    
    
    
    distF[lower.tri(distF)] <- t(distF)[lower.tri(distF)]
    distE[lower.tri(distE)] <- t(distE)[lower.tri(distE)]
    distDTW[lower.tri(distDTW)] <- t(distDTW)[lower.tri(distDTW)]
    
    ## t-sne
    
    perplexity_vec <- seq(5, o.n-1, by = 5)
    iter_vec <- vector(mode = "integer", length = length(perplexity_vec))
    
    ## Using Frechet
    cost_list = cost_list2 = NULL
    
    for (i in seq_along(perplexity_vec)) {
      tsne_tmp <- Rtsne(distF, perplexity = perplexity_vec[i], max_iter = 500, dims = 2, is_distance = T)
      cost_list[i] <- tsne_tmp$itercosts[1]
      cost_list2[i] <- 2 * sum(tsne_tmp$costs) + log(n) * perplexity_vec[i] / n
    }
    
    ansur_tsne <- Rtsne(distF, perplexity = perplexity_vec[which.min(cost_list)],
                        dims = 2, max_iter = 500, is_distance = T)
    # Perplexity KL
    perp[ite, 1] <- perplexity_vec[which.min(cost_list)]
    
    fre_tsne_df <- data.frame(tsne_x = ansur_tsne$Y[, 1],
                              tsne_y = ansur_tsne$Y[, 2],
                              treat = grp)
    # kmeans and Accuracy
    result <- kmeans(ansur_tsne$Y, 3)
    grp2 <- result$cluster
    misclass <- Relabel(grp,grp2)$miscl
    acc <- 1 - misclass
    # Accuracy
    Accuracy[ite,1] <- acc
    
    # Statistics
    B=summary(Anova(lm(cbind(tsne_x,tsne_y)~grp,data=fre_tsne_df)))$multivariate.tests$grp$SSPH
    W=summary(Anova(lm(cbind(tsne_x,tsne_y)~grp,data=fre_tsne_df)))$multivariate.tests$grp$SSPE
    fre_lambda=det(W)/det(B+W)
    fre_pillai=sum(diag(B%*%solve(B+W)))
    fre_hotelling=sum(diag(B%*%solve(W)))
    # F_stat
    F_stat[ite,] = c(fre_lambda, fre_pillai, fre_hotelling)
    
    # S(Perp)
    ansur_tsne <- Rtsne(distF, dims = 2, max_iter = 500,
                        perplexity = perplexity_vec[which.min(cost_list2)], is_distance = T)
    
    perp2[ite,1] <- perplexity_vec[which.min(cost_list2)]
    fre_tsne_df <- data.frame(tsne_x = ansur_tsne$Y[, 1],
                              tsne_y = ansur_tsne$Y[, 2],
                              treat = grp)
    
    result <- kmeans(ansur_tsne$Y, 3)
    grp2 <- result$cluster
    misclass <- Relabel(grp,grp2)$miscl
    acc <- 1 - misclass
    Accuracy2[ite,1] <- acc
    
    B=summary(Anova(lm(cbind(tsne_x,tsne_y)~grp,data=fre_tsne_df)))$multivariate.tests$grp$SSPH
    W=summary(Anova(lm(cbind(tsne_x,tsne_y)~grp,data=fre_tsne_df)))$multivariate.tests$grp$SSPE
    fre_lambda2=det(W)/det(B+W)
    fre_pillai2=sum(diag(B%*%solve(B+W)))
    fre_hotelling2=sum(diag(B%*%solve(W)))
    F_stat2[ite,] = c(fre_lambda2, fre_pillai2, fre_hotelling2)
    
    
    ## Using DTW 
    cost_list = cost_list2 = NULL
    
    for (i in seq_along(perplexity_vec)) {
      tsne_tmp <- Rtsne(distDTW, perplexity = perplexity_vec[i], max_iter = 500, dims = 2, is_distance = T)
      cost_list[i] <- tsne_tmp$itercosts[1]
      cost_list2[i] <- 2 * sum(tsne_tmp$costs) + log(n) * perplexity_vec[i] / n
    }
    
    ansur_tsne <- Rtsne(distDTW, perplexity = perplexity_vec[which.min(cost_list)],
                        dims = 2, max_iter = 500, is_distance = T)
    # Perplexity KL
    perp[ite, 2] <- perplexity_vec[which.min(cost_list)]
    
    DTW_tsne_df <- data.frame(tsne_x = ansur_tsne$Y[, 1],
                              tsne_y = ansur_tsne$Y[, 2],
                              treat = grp)
    # kmeans and Accuracy
    result <- kmeans(ansur_tsne$Y, 3)
    grp2 <- result$cluster
    misclass <- Relabel(grp,grp2)$miscl
    acc <- 1 - misclass
    # Accuracy
    Accuracy[ite,2] <- acc
    
    # Statistics
    B=summary(Anova(lm(cbind(tsne_x,tsne_y)~grp,data=DTW_tsne_df)))$multivariate.tests$grp$SSPH
    W=summary(Anova(lm(cbind(tsne_x,tsne_y)~grp,data=DTW_tsne_df)))$multivariate.tests$grp$SSPE
    dtw_lambda=det(W)/det(B+W)
    dtw_pillai=sum(diag(B%*%solve(B+W)))
    dtw_hotelling=sum(diag(B%*%solve(W)))
    # D_stat
    D_stat[ite,] = c(dtw_lambda, dtw_pillai, dtw_hotelling)
    
    # S(Perp)
    ansur_tsne <- Rtsne(distDTW, dims = 2, max_iter = 500,
                        perplexity = perplexity_vec[which.min(cost_list2)], is_distance = T)
    
    perp2[ite,2] <- perplexity_vec[which.min(cost_list2)]
    DTW_tsne_df <- data.frame(tsne_x = ansur_tsne$Y[, 1],
                              tsne_y = ansur_tsne$Y[, 2],
                              treat = grp)
    
    result <- kmeans(ansur_tsne$Y, 3)
    grp2 <- result$cluster
    misclass <- Relabel(grp,grp2)$miscl
    acc <- 1 - misclass
    Accuracy2[ite,2] <- acc
    
    B=summary(Anova(lm(cbind(tsne_x,tsne_y)~grp,data=DTW_tsne_df)))$multivariate.tests$grp$SSPH
    W=summary(Anova(lm(cbind(tsne_x,tsne_y)~grp,data=DTW_tsne_df)))$multivariate.tests$grp$SSPE
    dtw_lambda2=det(W)/det(B+W)
    dtw_pillai2=sum(diag(B%*%solve(B+W)))
    dtw_hotelling2=sum(diag(B%*%solve(W)))
    D_stat2[ite,] = c(dtw_lambda2, dtw_pillai2, dtw_hotelling2)
    
    
    ## Using Euclidean
    
    cost_list = cost_list2 = NULL
    
    for (i in seq_along(perplexity_vec)) {
      tsne_tmp <- Rtsne(distE, perplexity = perplexity_vec[i], max_iter = 500, dims = 2, is_distance = T)
      cost_list[i] <- tsne_tmp$itercosts[1]
      cost_list2[i] <- 2 * sum(tsne_tmp$costs) + log(n) * perplexity_vec[i] / n
    }
    
    ansur_tsne <- Rtsne(distE, perplexity = perplexity_vec[which.min(cost_list)],
                        dims = 2, max_iter = 500, is_distance = T)
    # Perplexity KL
    perp[ite, 3] <- perplexity_vec[which.min(cost_list)]
    
    E_tsne_df <- data.frame(tsne_x = ansur_tsne$Y[, 1],
                              tsne_y = ansur_tsne$Y[, 2],
                              treat = grp)
    # kmeans and Accuracy
    result <- kmeans(ansur_tsne$Y, 3)
    grp2 <- result$cluster
    misclass <- Relabel(grp,grp2)$miscl
    acc <- 1 - misclass
    # Accuracy
    Accuracy[ite,3] <- acc
    
    # Statistics
    B=summary(Anova(lm(cbind(tsne_x,tsne_y)~grp,data=E_tsne_df)))$multivariate.tests$grp$SSPH
    W=summary(Anova(lm(cbind(tsne_x,tsne_y)~grp,data=E_tsne_df)))$multivariate.tests$grp$SSPE
    E_lambda=det(W)/det(B+W)
    E_pillai=sum(diag(B%*%solve(B+W)))
    E_hotelling=sum(diag(B%*%solve(W)))
    # D_stat
    E_stat[ite,] = c(E_lambda, E_pillai, E_hotelling)
    
    # S(Perp)
    ansur_tsne <- Rtsne(distE, dims = 2, max_iter = 500,
                        perplexity = perplexity_vec[which.min(cost_list2)], is_distance = T)
    
    perp2[ite,3] <- perplexity_vec[which.min(cost_list2)]
    E_tsne_df <- data.frame(tsne_x = ansur_tsne$Y[, 1],
                              tsne_y = ansur_tsne$Y[, 2],
                              treat = grp)
    
    result <- kmeans(ansur_tsne$Y, 3)
    grp2 <- result$cluster
    misclass <- Relabel(grp,grp2)$miscl
    acc <- 1 - misclass
    Accuracy2[ite,3] <- acc
    
    B=summary(Anova(lm(cbind(tsne_x,tsne_y)~grp,data=E_tsne_df)))$multivariate.tests$grp$SSPH
    W=summary(Anova(lm(cbind(tsne_x,tsne_y)~grp,data=E_tsne_df)))$multivariate.tests$grp$SSPE
    E_lambda2=det(W)/det(B+W)
    E_pillai2=sum(diag(B%*%solve(B+W)))
    E_hotelling2=sum(diag(B%*%solve(W)))
    E_stat2[ite,] = c(E_lambda2, E_pillai2, E_hotelling2)
  }
  
  return(cbind(F_stat,D_stat,E_stat,perp,Accuracy,F_stat2,D_stat2,E_stat2,perp2,Accuracy2))
}

n90_t50=tsne_f(num=30, Bite=50, t.num=50) # num: the number of observations per group
n90_t100=tsne_f(num=30,Bite=50,t.num=100)
n180_t50=tsne_f(num=60, Bite=50, t.num=50) 
n180_t100=tsne_f(num=60, Bite=50, t.num=100) 
n270_t50=tsne_f(num=90, Bite=50, t.num=50)
n270_t100=tsne_f(num=90, Bite=50, t.num=100)


res<-apply(n90_t50,2,mean) 
res<-rbind(res,apply(n90_t50,2,sd))
res<-rbind(res,apply(n90_t100,2,mean))
res<-rbind(res,apply(n90_t100,2,sd))

res<-rbind(res,apply(n180_t50,2,mean))
res<-rbind(res,apply(n180_t50,2,sd))
res<-rbind(res,apply(n180_t100,2,mean))
res<-rbind(res,apply(n180_t100,2,sd))

res<-rbind(res,apply(n270_t50,2,mean))
res<-rbind(res,apply(n270_t50,2,sd))
res<-rbind(res,apply(n270_t100,2,mean))
res<-rbind(res,apply(n270_t100,2,sd))

colnames(res)<-c("F_lambda","F_pillai","F_hotelling",
                 "D_lambda","D_pillai","D_hotelling",
                 "E_lambda","E_pillai","E_hotelling",
                 "F_perp","D_perp","E_perp",
                 "F_acc","D_acc","E_acc",
                 "SF_lambda","SF_pillai","SF_hotelling",
                 "SD_lambda","SD_pillai","SD_hotelling",
                 "SE_lambda","SE_pillai","SE_hotelling",
                 "F_Sperp","D_Sperp","E_Sperp",
                 "F_Sacc","D_Sacc","E_Sacc")

rownames(res)<-c("mu_n90_t50","sd_n90_t50","mu_n90_t100","sd_n90_t100","mu_n180_t50","sd_n180_t50","mu_n180_t100","sd_n180_t100","mu_n270_t50","sd_n270_t50","mu_n270_t100","sd_n270_t100")