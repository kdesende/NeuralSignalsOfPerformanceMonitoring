source('randomwalk_w_posterroronly.R')
ntrials <- 500;v <- 1;a <- 1;ter <- .2;z=.5;t2=1;s=1;dt=.001

##########################################################################
#1. Error detection and confidence judgments are equally as accurate in both scenarios
par(mfrow=c(2,2),lwd=2)
for(only_post_err in c(F,T)){
  df <- data.frame(randomwalk_w_posterroronly(v,a,ter,z,t2,s,dt,ntrials,only_post_err))
  names(df) <- c('rt','cor','rt2','cj')
  
  df$cj_cat <- as.numeric(cut(df$cj,quantile(df$cj,seq(0,1,length.out=7)),include.lowest=T))
  CJ <- with(df,aggregate(cor,by=list(cj_cat=cj_cat),mean));CJ_var <- with(df,aggregate(cor,by=list(cj_cat=cj_cat),sd))
  plot(CJ$x~CJ$cj_cat,type='b',frame=F,ylab="Accuracy",xlab="Binned confidence",pch=19,ylim=c(0,1))

  df$error_detection <- ifelse(df$cj<quantile(df$cj,.2),1,0)
  ER <- with(df,aggregate(cor,by=list(e_detect=error_detection),mean))
  plot(ER$x~ER$e_detect,type='b',frame=F,ylab="Accuracy",xlab="Detected error",xaxt='n',pch=19,ylim=c(0,1))
  axis(1,at=c(0,1),labels=c("Yes","No"))
}

############################################################################
#2. We can explain the various confidence effects described in the main text
for(only_post_err in c(F,T)){
  #2A confidence decreases as difficulty increases (two drifts)
  df_a <- data.frame(randomwalk_w_posterroronly(v,a,ter,z,t2,s,dt,ntrials,only_post_err));df_a$cond <- 1
  df_b <- data.frame(randomwalk_w_posterroronly(v+1,a,ter,z,t2,s,dt,ntrials,only_post_err));df_b$cond <- 2
  df <- rbind(df_a,df_b);names(df) <- c('rt','cor','rt2','cj','cond')
  df$cj_cat <- as.numeric(cut(df$cj,quantile(df$cj,seq(0,1,length.out=7)),include.lowest=T))
  
  CJ <- with(df,aggregate(cj_cat,by=list(drift=cond,cor=cor),mean))
  plot(CJ$x[CJ$cor==1]~CJ$drift[CJ$cor==1],type='b',frame=F,pch=19,ylab="Binned confidence",xlab="Drift rate",xaxt='n',ylim=c(1,6))
  lines(CJ$x[CJ$cor==0]~CJ$drift[CJ$cor==0],type='b',col="grey",pch=19)
  axis(1,at=c(1,2),labels=c("Low","High"))
  legend('topleft',inset=.1,legend=c('Correct','Error'),col=c("black","grey"),lty=1,box.lty=0,cex=2,lwd=3)
  
  #2B accuracy of confidence better under speed pressure (two bounds)
  df_a <- data.frame(randomwalk_w_posterroronly(v,a,ter,z,t2,s,dt,ntrials,only_post_err));df_a$cond <- 1
  df_b <- data.frame(randomwalk_w_posterroronly(v,a+2,ter,z,t2,s,dt,ntrials,only_post_err));df_b$cond <- 2
  df <- rbind(df_a,df_b);names(df) <- c('rt','cor','rt2','cj','cond')
  df$cj_cat <- as.numeric(cut(df$cj,quantile(df$cj,seq(0,1,length.out=7)),include.lowest=T))
  
  CJ <- with(df,aggregate(cor,by=list(cj_cat=cj_cat,bound=cond),mean))
  plot(CJ$x[CJ$bound==1]~CJ$cj_cat[CJ$bound==1],type='b',frame=F,ylab="Accuracy",xlab="Binned confidence",ylim=c(0,1))
  lines(CJ$x[CJ$bound==2]~CJ$cj_cat[CJ$bound==2],type='b',col="grey",pch=19)
  legend('bottomright',inset=.1,legend=c('Low boundar','High boundary'),col=c("black","grey"),lty=1,box.lty=0,cex=2,lwd=3)
  
  #2C accuracy of confidence increases with time b/w first decision and conf judgment (two t2s)
  #(quantiles are done seperately, because there's no overlap between cjs here)
  df_a <- data.frame(randomwalk_w_posterroronly(v,a,ter,z,t2,s,dt,ntrials,only_post_err));df_a$cond <- 1
  df_a$cj_cat <- as.numeric(cut(df_a[,4],quantile(df_a[,4],seq(0,1,length.out=7)),include.lowest=T))
  df_b <- data.frame(randomwalk_w_posterroronly(v,a,ter,z,t2-.8,s,dt,ntrials,only_post_err));df_b$cond <- 2
  df_b$cj_cat <- as.numeric(cut(df_b[,4],quantile(df_b[,4],seq(0,1,length.out=7)),include.lowest=T))
  df <- rbind(df_a,df_b);names(df) <- c('rt','cor','rt2','cj','cond','cj_cat')
  
  CJ <- with(df,aggregate(cor,by=list(cj_cat=cj_cat,t2=cond),mean))
  plot(CJ$x[CJ$t2==1]~CJ$cj_cat[CJ$t2==1],type='b',frame=F,ylab="Accuracy",xlab="Binned confidence",ylim=c(0,1))
  lines(CJ$x[CJ$t2==2]~CJ$cj_cat[CJ$t2==2],type='b',col="grey",pch=19)
  legend('bottomright',inset=.1,legend=c('Long post-decision','Short post-decision'),col=c("black","grey"),lty=1,box.lty=0,cex=2,lwd=3)
}

