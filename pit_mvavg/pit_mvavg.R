library(dplyr)
library(pracma)

setwd("C:/Users/kw102/OneDrive/바탕 화면/bigcon/2020빅콘테스트 데이터분석분야-퓨쳐스리그_스포츠투아이_제공데이터(.CSV)_시즌별, 시트별 구분")
pit <- read.csv("2020빅콘테스트_스포츠투아이_제공데이터_팀투수_2020.csv")

table(pit$WLS) # S는 존재 안함
pit[pit$WLS=="L","WLS"] <- 0; pit[pit$WLS=="W","WLS"] <- 1; pit[pit$WLS=="D",'WLS'] <- NA
#WLS 열의 값중 패는 0 승은 1 무승부는 2로 변경함.
pit$WLS <- as.numeric(pit$WLS)


tp <- pit%>%select(GDAY_DS,T_ID,ER,WLS)
#날짜, 팀이름, 실책점, 승패 열 추출



pit_HH <- tp%>%filter(T_ID=="HH")
pit_HT <- tp%>%filter(T_ID=="HT")
pit_KT <- tp%>%filter(T_ID=="KT")
pit_LG <- tp%>%filter(T_ID=="LG")
pit_LT <- tp%>%filter(T_ID=="LT")
pit_NC <- tp%>%filter(T_ID=="NC")
pit_OB <- tp%>%filter(T_ID=="OB")
pit_SK <- tp%>%filter(T_ID=="SK")
pit_SS <- tp%>%filter(T_ID=="SS")
pit_WO <- tp%>%filter(T_ID=="WO")
#각 팀별로 변수를 만듬


a <- c()
for (i in 1:length(pit_HH$WLS)){
  if (i<=18) a[i] <- mean(pit_HH[1:i,"WLS"],na.rm=T)
  else a[i] <- mean(pit_HH[i-17:i,"WLS"],na.rm = T)
}#승률은 승,패의 평균이므로 무승부인 NA값을 제외하고 평균을 구했다.
pit_HH$WLS <- round(a,digits=2)
pit_HH$ER <- round(movavg(pit_HH$ER,n=18),digits=2)# movavg 함수를 이용하여 평균자책점의 이동평균을 구하였다.
write.csv(pit_HH,file="pit_HH.csv")#데이터를 파일에 저장함

a <- c()
for (i in 1:length(pit_HT$WLS)){
  if (i<=18) a[i] <- mean(pit_HT[1:i,"WLS"],na.rm=T)
  else a[i] <- mean(pit_HT[i-17:i,"WLS"],na.rm = T)
}#승률은 승,패의 평균이므로 무승부인 NA값을 제외하고 평균을 구했다.
pit_HT$WLS <- round(a,digits=2)
pit_HT$ER <- round(movavg(pit_KT$ER,n=18),digits=2)# movavg 함수를 이용하여 평균자책점의 이동평균을 구하였다.
write.csv(pit_HT,file="pit_HT.csv")#데이터를 파일에 저장함

a <- c()
for (i in 1:length(pit_KT$WLS)){
  if (i<=18) a[i] <- mean(pit_KT[1:i,"WLS"],na.rm=T)
  else a[i] <- mean(pit_KT[i-17:i,"WLS"],na.rm = T)
}#승률은 승,패의 평균이므로 무승부인 NA값을 제외하고 평균을 구했다.
pit_KT$WLS <- round(a,digits=2)
pit_KT$ER <- round(movavg(pit_KT$ER,n=18),digits=2)# movavg 함수를 이용하여 평균자책점의 이동평균을 구하였다.
write.csv(pit_KT,file="pit_KT.csv")#데이터를 파일에 저장함

a <- c()
for (i in 1:length(pit_LG$WLS)){
  if (i<=18) a[i] <- mean(pit_LG[1:i,"WLS"],na.rm=T)
  else a[i] <- mean(pit_LG[i-17:i,"WLS"],na.rm = T)
}#승률은 승,패의 평균이므로 무승부인 NA값을 제외하고 평균을 구했다.
pit_LG$WLS <- round(a,digits=2)
pit_LG$ER <- round(movavg(pit_LG$ER,n=18),digits=2)# movavg 함수를 이용하여 평균자책점의 이동평균을 구하였다.
write.csv(pit_LG,file="pit_LG.csv")#데이터를 파일에 저장함

a <- c()
for (i in 1:length(pit_LT$WLS)){
  if (i<=18) a[i] <- mean(pit_LT[1:i,"WLS"],na.rm=T)
  else a[i] <- mean(pit_LT[i-17:i,"WLS"],na.rm = T)
}#승률은 승,패의 평균이므로 무승부인 NA값을 제외하고 평균을 구했다.
pit_LT$WLS <- round(a,digits=2)
pit_LT$ER <- round(movavg(pit_LT$ER,n=18),digits=2)# movavg 함수를 이용하여 평균자책점의 이동평균을 구하였다.
write.csv(pit_LT,file="pit_LT.csv")#데이터를 파일에 저장함

a <- c()
for (i in 1:length(pit_NC$WLS)){
  if (i<=18) a[i] <- mean(pit_NC[1:i,"WLS"],na.rm=T)
  else a[i] <- mean(pit_NC[i-17:i,"WLS"],na.rm = T)
}#승률은 승,패의 평균이므로 무승부인 NA값을 제외하고 평균을 구했다.
pit_NC$WLS <- round(a,digits=2)
pit_NC$ER <- round(movavg(pit_NC$ER,n=18),digits=2)# movavg 함수를 이용하여 평균자책점의 이동평균을 구하였다.
write.csv(pit_NC,file="pit_NC.csv")#데이터를 파일에 저장함

a <- c()
for (i in 1:length(pit_OB$WLS)){
  if (i<=18) a[i] <- mean(pit_OB[1:i,"WLS"],na.rm=T)
  else a[i] <- mean(pit_OB[i-17:i,"WLS"],na.rm = T)
}#승률은 승,패의 평균이므로 무승부인 NA값을 제외하고 평균을 구했다.
pit_OB$WLS <- round(a,digits=2)
pit_OB$ER <- round(movavg(pit_OB$ER,n=18),digits=2)# movavg 함수를 이용하여 평균자책점의 이동평균을 구하였다.
write.csv(pit_OB,file="pit_OB.csv")#데이터를 파일에 저장함

a <- c()
for (i in 1:length(pit_SK$WLS)){
  if (i<=18) a[i] <- mean(pit_SK[1:i,"WLS"],na.rm=T)
  else a[i] <- mean(pit_SK[i-17:i,"WLS"],na.rm = T)
}#승률은 승,패의 평균이므로 무승부인 NA값을 제외하고 평균을 구했다.
pit_SK$WLS <- round(a,digits=2)
pit_SK$ER <- round(movavg(pit_SK$ER,n=18),digits=2)# movavg 함수를 이용하여 평균자책점의 이동평균을 구하였다.
write.csv(pit_SK,file="pit_SK.csv")#데이터를 파일에 저장함

a <- c()
for (i in 1:length(pit_SS$WLS)){
  if (i<=18) a[i] <- mean(pit_SS[1:i,"WLS"],na.rm=T)
  else a[i] <- mean(pit_SS[i-17:i,"WLS"],na.rm = T)
}#승률은 승,패의 평균이므로 무승부인 NA값을 제외하고 평균을 구했다.
pit_SS$WLS <- round(a,digits=2)
pit_SS$ER <- round(movavg(pit_SS$ER,n=18),digits=2)# movavg 함수를 이용하여 평균자책점의 이동평균을 구하였다.
write.csv(pit_SS,file="pit_SS.csv")#데이터를 파일에 저장함

a <- c()
for (i in 1:length(pit_WO$WLS)){
  if (i<=18) a[i] <- mean(pit_WO[1:i,"WLS"],na.rm=T)
  else a[i] <- mean(pit_WO[i-17:i,"WLS"],na.rm = T)
}#승률은 승,패의 평균이므로 무승부인 NA값을 제외하고 평균을 구했다.
pit_WO$WLS <- round(a,digits=2)
pit_WO$ER <- round(movavg(pit_WO$ER,n=18),digits=2)# movavg 함수를 이용하여 평균자책점의 이동평균을 구하였다.
write.csv(pit_WO,file="pit_WO.csv")#데이터를 파일에 저장함
