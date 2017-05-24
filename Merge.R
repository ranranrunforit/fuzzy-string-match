#########################################################
##Merge data with partial match in r

#############################################

BIRADtotal <- rbind(BIRADStestfeat, BIRADStrainfeat) 
############################################

# Method 1: using the native R adist
#source1.devices<-read.csv('[path_to_your_source1.csv]')
#source2.devices<-read.csv('[path_to_your_source2.csv]')
# To make sure we are dealing with charts
Y$ent<-as.character(Y$ent)
BIRADtotal$StudyID<-as.character(BIRADtotal$StudyID)

# It creates a matrix with the Standard Levenshtein distance between the name fields of both sources
dist.name<-adist(Y$ent,BIRADtotal$StudyID, partial = TRUE, ignore.case = TRUE)

# We now take the pairs with the minimum distance
min.name<-apply(dist.name, 1, min)

match.s1.s2<-NULL 
for(i in 1:nrow(dist.name))
{
  s2.i<-match(min.name[i],dist.name[i,])
  s1.i<-i
  match.s1.s2<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=BIRADtotal[s2.i,]$StudyID, s1name=Y[s1.i,]$ent, adist=min.name[i]),match.s1.s2)
}

#BIRADtotal.pd=BIRADtotal$pd,Y.RAScore=Y$RAScore,Y.X.TA1Score=Y$X.TA1Score,
# and we then can have a look at the results
View(match.s1.s2)
total1 <- merge(match.s1.s2, Y, by.x=c("s1name"),by.y=c("ent"))
total2 <- merge(total1, BIRADtotal, by.x=c("s2name"),by.y=c("StudyID"))
keeps <- c("s2name", "s1name","pd","RAScore","X.TA1Score")
total3 <- total2[keeps]
#################################################



