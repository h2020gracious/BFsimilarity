

#### READ in RAW DATA ##### 

radical.data<- read.csv('Daphnia_magna_immobilizationN.csv', sep=",")

class(radical.data)
dim(radical.data)
cols.n<- colnames(radical.data)



#unique materials
#namesU<- as.character(unique(radical.data[,'Name']))




#LIBRARIES NEEDED
library(babar)
library(bestNormalize)
set.seed(102)

#Select the three columns needed
rd.sub<- radical.raw[,c('Name','Concentration','Response')]

#Normilize data
rd.sub[,3]<- scale(rd.sub[,3])

#rd.sub[,3]<- yeojohnson(rd.sub[,3])$x.t
#rd.sub[,3]<- log10(rd.sub[,3])


#Perform the analysis

namesU1<- as.character(unique(rd.sub[,'Name']))

lenU<- length(namesU1)
BF.mat<- matrix(NA,lenU,lenU)
colnames(BF.mat)<- namesU1; rownames(BF.mat)<- namesU1
W.mat<- matrix(NA,lenU,lenU)
colnames(W.mat)<- namesU1; rownames(W.mat)<- namesU1





for(i in 1:(lenU)){
  comp1<- namesU1[i]
  rd.suba<- rd.sub[rd.sub[,'Name']==comp1,]
  rd.suba<- rd.suba[,-1]
  
  
  rd.suba<- rd.suba[!is.na(rd.suba$Response),]
  rd.suba<- rd.suba[order(rd.suba$Concentration),]
 
  mu.a <- mean(rd.suba[,2],na.rm=T)
  sd.a <- sd(rd.suba[,2],na.rm=T)
  
  if(nrow(rd.suba)>40){rd.suba<- rd.suba[1:10,]}
  
  if(prod(dim(rd.suba))!=0){
    print(c(i,comp1))
    for(j in (i):lenU){#i+1
      print(c(j))
      comp2<- namesU1[j]
      print(c(j,comp2))  
      rd.subb<- rd.sub[rd.sub[,'Name']==comp2,]
      rd.subb<- rd.subb[,-1]
     
      rd.subb<- rd.subb[!is.na(rd.subb$Response),] 
      rd.subb<- rd.subb[order(rd.subb$Concentration),] 
      
      mu.b <- mean(rd.subb[,2],na.rm=T)
      sd.b <- sd(rd.subb[,2],na.rm=T)
      
      if(nrow(rd.subb)>40){rd.subb<- rd.subb[1:10,]}
      
      
      print(c(j,comp2))       
      if(prod(dim(rd.subb))!=0){
        w<- 1; w.in<- rep(0,1)
        while(w<2){
          set.seed(11) ## for reproducibility
          
          results_H1 <- Bayescompare(rd.suba, rd.subb, hyp = "H1", model = "Bar3par")
          
          #print(c(j,comp2))       
          set.seed(11) ## for reproducibility
          results_H3 <- Bayescompare(rd.suba, rd.subb, hyp = "H3", model = "Bar3par")
          
          #print(c(j,comp2))     
          logevidence_H1 <- results_H1$logevidence
         
          logevidence_H3 <- results_H3$logevidence
         
          
          
          #Bayes’ factor for hypothesis 1 versus hypothesis 3. 
          Bayes <- exp(logevidence_H1)/exp(logevidence_H3)
          
          w.in[w]<- Bayes
          #Bayes
          w<-w+1
        }
        print(c(i,j,logevidence_H1,logevidence_H3,log10(Bayes)))
        BF.mat[i,j]<- log10(mean(w.in))
        
      }}}
  print(c(i,j))
}

write.table(BF.mat,'BFmat_H1H3.csv',sep=',',col.names=TRUE,row.names=TRUE)



