#read in data
BIRADStrainfeat <- read.csv("/Users/chaoran/Desktop/RadsData/RF and SVM/SiemensMLOTrain_TC0TC3.csv", header = T)
BIRADStestfeat <- read.csv("/Users/chaoran/Desktop/RadsData/RF and SVM/SiemensMLOTest_TC0TC3.csv", header = T)

#build the model
# X <- BIRADStrainfeat[complete.cases(BIRADStrainfeat$StudyID),] # take only rows
SVMtrain <- BIRADStrainfeat[,c("pd","flatness", "skewness", "stdOverMeanIntensity",
                    "compactness0","convexHull2Mask0","dense2convexHull0",         
                    "numObjects0","meanBlobSize0","stdOverMeanBlobSize0",
                    "stdOverDenseAreaBlobSize0","stdOverConvexHullBlobSize0",
                    "meanDenseTissue0","meanFatTissue0","stdDenseTissue0",          
                    "stdFatTissue0","stdOverMeanDense0","stdOverMeanFat0",
                    "compactness3","convexHull2Mask3","dense2convexHull3",        
                    "numObjects3","meanBlobSize3","stdOverMeanBlobSize3",
                    "stdOverDenseAreaBlobSize3","stdOverConvexHullBlobSize3",
                    "meanDenseTissue3","meanFatTissue3","stdDenseTissue3",           
                     "stdFatTissue3","stdOverMeanDense3","stdOverMeanFat3")]

SVMtest <- BIRADStestfeat[,c("pd","flatness", "skewness", "stdOverMeanIntensity",
                             "compactness0","convexHull2Mask0","dense2convexHull0",         
                             "numObjects0","meanBlobSize0","stdOverMeanBlobSize0",
                             "stdOverDenseAreaBlobSize0","stdOverConvexHullBlobSize0",
                             "meanDenseTissue0","meanFatTissue0","stdDenseTissue0",          
                             "stdFatTissue0","stdOverMeanDense0","stdOverMeanFat0",
                             "compactness3","convexHull2Mask3","dense2convexHull3",        
                             "numObjects3","meanBlobSize3","stdOverMeanBlobSize3",
                             "stdOverDenseAreaBlobSize3","stdOverConvexHullBlobSize3",
                             "meanDenseTissue3","meanFatTissue3","stdDenseTissue3",           
                             "stdFatTissue3","stdOverMeanDense3","stdOverMeanFat3")]

library("e1071")
#Y <- X$pd
# X <-subset(X, select=-X$pd)
#create SVM Model and show summary
svm_model <- svm(SVMtrain, SVMtrain$pd, probability=T)
summary(svm_model)


# Run Prediction
pred <- predict(svm_model, SVMtest)
pred-SVMtest[,1]
