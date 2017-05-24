# X is a dataframe with four columns: first column - entity ID, second column - rater ID, 
# third column - view, fourth column - score.


  #calculates TA1 average as per Sperrin et al (2013)
  #returns a dataframe X with TA1 appended
  #and a Y - same but collapsed into one row per entity

#  X<-read.csv("/Users/chaoran/Desktop/RadsData/R files/reaquestionaboutapplyingsperrinscorrectingforrate/raterbias code example.csv")
  X<-read.csv("/Users/chaoran/Desktop/RadsData/R files/NewStructure.csv")
  X <- X[complete.cases(X$Score),] # take only rows


  colnames(X)<-c("Entity","Rater","View","Score") #add default colnames to X
  
  ent<-levels(X$Entity) #vector of all entities
  
  n<-dim(X)[1] #number of images read total by all raters
  nEnt<-length(ent) #number of unique images read
  
  RaV<-as.factor(do.call(paste, c(X[c("Rater", "View")], sep = ""))) #we don't have any views so not sure what this does, exactly. Makes some sort of "raterview" variable
  
  rat<-levels(RaV)  #vector of all raters
  R<-length(rat) #number of unique raters
  
  
  X$TA1Score<-X$Score #new column for Transformed scores (sets to X$Score as default value)
  #print(X$TA1Score)
  #construct the overall ECDF
  ordscore<-X$TA1Score[order(X$TA1Score)] #Takes X$TA1Score and makes an ordered integer vector
  ordrater<-RaV[order(X$TA1Score)] #Orders ???
  levels(ordrater)<-1:R #lists raters
  ordrater<-as.numeric(ordrater) #was already numeric??
  
  nRater<-table(RaV) #table of how many images read per rater
  
  ovpdf<-1/(R*nRater[ordrater]) #array with inverse weight for each obs (one weight per rater)
  ecdfall<-cbind(ordscore,cumsum(ovpdf)) #ecdf (weighted) (table with rater, ordered density, and cumulative sum of the weights)
  
  #convert each reader onto Unif[0,1] scale within
  for(j in 1:R){
    #each reader gets weight of (1/#images) for each image read.
    #Score is cumulative.
    X$TA1Score[RaV==rat[j]]<-rank(X$TA1Score[RaV==rat[j]])/(nRater[j])  
    #print(X$TA1Score[j]) #three rater output on [0,1] scale
  }
  
  #print(X$TA1Score)

  #produce TA1
  for(i in 1:n){
    #print(X$TA1Score)
    #score is 0 or 1, depending on whether the cumweight is >= TA1 weight or not
    X$TA1Score[i]<-min(ecdfall[ecdfall[,2]>=X$TA1Score[i],1]) 
    #print(min(ecdfall[ecdfall[,2]>=X$TA1Score[i],1]))
    #print(ecdfall[ecdfall[,2]>=X$TA1Score[i]])
    #print(X$TA1Score[i])
  }
  # X[!complete.cases(X$Score),"Entity"] 
  
  #average score for each entity
  RAScore<-TA1Score<-rep(NA,nEnt)
  #print(X$TA1Score)
  for(i in 1:nEnt){
    RAScore[i]<-mean(X$Score[X$Entity==ent[i]])
    TA1Score[i]<-mean(X$TA1Score[X$Entity==ent[i]])
    #print(mean(X$TA1Score[X$Entity==ent[i]]))
  }
  Y<-data.frame(ent,RAScore,X$TA1Score)

  #####################################################
  ##combine Y
  library(plyr)
  arrange(Y, ent) # Use arrange from plyr package
  Y<-Y[order(Y$ent),] # Sort by ent column and make a new Y
  Y$ent<-unique(Y$ent) # Original data with repeats removed.
  #aggregate(Y[, 2:3], list(Y$ent), mean) 
  #get Mean per group in Y$TA1.Score every 3 rows
  n <- 3;
  nlist <- aggregate(Y,list(rep(1:(nrow(Y)%/%n+1),each=n,len=nrow(Y))),mean)[-1];
  #Y <- merge(Y$ent,nlist,by=0,all=TRUE)
  Y <- cbind(Y$ent,nlist)
  Y$`ent`<-NULL
  Y<- rename(Y, c("Y$ent"="ent"))
  Y<- unique(Y)
  #####################################################
  
  ####################################################
  ##make figure
  #Scoring distributions of individual raters
  boxplot(X$Score ~ X$Rater, xlab="Score", ylab="Rater")
  boxplot(X$TA1Score ~ X$Rater, xlab="TA1Score", ylab="Rater")
  
  #pairwise comparisons of raw scores assigned by raters
  #pairs(cbind(X$Score[X$Rater=="r1"], X$Score[X$Rater=="r2"],X$Score[X$Rater=="r3"]))
  radScores = cbind(X$Score[X$Rater=="r1"], X$Score[X$Rater=="r2"],X$Score[X$Rater=="r3"])
  dimnames(radScores)[[2]] = c("Rad1","Rad2","Rad3")
  my_line <- function(x, y, ...){
    points(x, y, ...)
    abline(a = 0, b = 1)
  }
  par(pty="s")
  pairs(radScores, col = "lightblue", lower.panel = my_line, upper.panel = my_line, asp=1, pch=19)
  
  #boxplot of rater 1's scoring distribution after stage 1 transformation
  boxplot(X$TA1Score[X$Rater=="r1"], xlab="Reader1", ylab="TA1Score")
  #boxplot of rater 2's scoring distribution after stage 1 transformation
  boxplot(X$TA1Score[X$Rater=="r2"], xlab="Reader2", ylab="TA1Score")
  #boxplot of rater 3's scoring distribution after stage 1 transformation
  boxplot(X$TA1Score[X$Rater=="r3"], xlab="Reader3", ylab="TA1Score")
  
  par(pty="m")
  #QQ plots comparing reader 1 and reader 2 scoring distributions
  qqplot(X$Score[X$Rater=="r1"], X$Score[X$Rater=="r2"], xlab="Reader1", ylab="Reader2")
  abline(a = 0, b = 1, col="grey")
  #QQ plots comparing reader 1 and reader 2 TA1scoring distributions
  qqplot(X$TA1Score[X$Rater=="r1"], X$TA1Score[X$Rater=="r2"], xlab="Reader1", ylab="Reader2")
  abline(a = 0, b = 1, col="grey")
  
  #QQ plots comparing reader 1 and reader 3 scoring distributions
  qqplot(X$Score[X$Rater=="r1"], X$Score[X$Rater=="r3"], xlab="Reader1", ylab="Reader3")
  abline(a = 0, b = 1, col="grey")
  #QQ plots comparing reader 1 and reader 3 TA1scoring distributions
  qqplot(X$TA1Score[X$Rater=="r1"], X$TA1Score[X$Rater=="r3"], xlab="Reader1", ylab="Reader3")
  abline(a = 0, b = 1, col="grey")
  
  #QQ plots comparing reader 2 and reader 3 scoring distributions
  qqplot(X$Score[X$Rater=="r2"], X$Score[X$Rater=="r3"], xlab="Reader2", ylab="Reader3")
  abline(a = 0, b = 1, col="grey")
  #QQ plots comparing reader 2 and reader 3 TA1scoring distributions
  qqplot(X$TA1Score[X$Rater=="r2"], X$TA1Score[X$Rater=="r3"], xlab="Reader2", ylab="Reader3")
  abline(a = 0, b = 1, col="grey")
  
  #Readers 1 and 2 assigned scores
  #region above grey line denotes women with Y(RA) >= 46
  #and therefor classified as high density
  par(pty="s") #make the image square
  #x, y, xlabel, ylabel, xlim=ylim, shape of the point, size of the point = .5
  plot(X$Score[X$Rater=="r1"], X$Score[X$Rater=="r2"], xlab="Reader1", ylab="Reader2", asp=1, pch=19, cex= .5)
  abline(a = 100, b = -1, col="grey")
  #x, y, xlabel, ylabel, xlim=ylim, shape of the point, size of the point = .5
  plot(X$TA1Score[X$Rater=="r1"], X$TA1Score[X$Rater=="r2"], xlab="Reader1", ylab="Reader2", asp=1, pch=19, cex= .5)
  abline(a = 100, b = -1, col="grey")

  # transformed and untransformed scores compared.
  # differences between transformed and untransformed scores.
  (X$Score[X$Rater=="r1"] + X$Score[X$Rater=="r2"])/2
  RA = (X$Score[X$Rater=="r1"] + X$Score[X$Rater=="r2"] + X$Score[X$Rater=="r3"])/3
  TA = (X$TA1Score[X$Rater=="r1"] + X$TA1Score[X$Rater=="r2"] + X$TA1Score[X$Rater=="r3"])/3
  plot(RA,TA, xlab="Raw Average", ylab="Transformed Average", asp=1, pch=19, cex= .1)
  hist(RA-TA, xlab="Difference", ylab="Frequency")


  
#example data and output
#Xeg<-read.csv("raterbias code example.csv")
#calcTA1(Xeg)

