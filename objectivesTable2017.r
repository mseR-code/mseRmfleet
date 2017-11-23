#------------------------------------------------------------------------------#
#-- Function to create performance summary table for 2017 MSE                                         --#
#------------------------------------------------------------------------------#

# .calcObjTable (Calculate statistics required for 2017 MSE CSAP report).
# Purpose:       Creates a csv file that includes performance measures related to 2017 MSE objectives
#                 for each combination of MP and OM scenario
# Parameters:    NONE.
# Returns:      ----- *** still need to make csv file location based on current working directory

# Source:        K. Holt


calcObjTable<-function (fName) {


stats<-read.csv(file=paste( fName,"_perfTable2",".csv", sep="" ), header=T)


simList<-unique(stats[,1])

scenario <- rep(NA, length(simList))
MP<-rep(NA,length(simList))
obj1<-rep(NA,length(simList))
obj2AcceptProb<-rep(NA,length(simList))
obj2ObsProb<-rep(NA,length(simList))
obj3a<-rep(NA,length(simList))
obj3b<-rep(NA,length(simList))
obj4<-rep(NA,length(simList))
obj5<-rep(NA,length(simList))
minC<-rep(NA,length(simList))
maxC<-rep(NA,length(simList))
medAAV<-rep(NA,length(simList))
D2016<-rep(NA,length(simList))
C2016<-rep(NA,length(simList))


for (i in 1:length(simList)) {
  
  # Extract scenario and MP

  scenario[i] <-as.character(subset(stats,stats[,1]==simList[i])[1,2])
  MP[i]       <-as.character(subset(stats,stats[,1]==simList[i])[1,3])
  
  # Extract objective 1
  obj1[i]<-subset(stats,stats[,1]==simList[i] & stats[,4]=="Long" & stats[,7] == "pObj1")[[8]]
  
  # Extract objective 2
  obj2AcceptProb[i]<-subset(stats,stats[,1]==simList[i] & stats[,4]=="Short" & stats[,7] == "pDecline")[[8]]
  obj2ObsProb[i]<-subset(stats,stats[,1]==simList[i] & stats[,4]=="Short" & stats[,7] == "obsPdecline")[[8]]
  
  # Extract objective 3a
  obj3a[i]<-subset(stats,stats[,1]==simList[i] & stats[,4]=="Long" & stats[,7] == "pObj3")[[8]]
  
  # Extract objective 3b
  obj3b[i]<-subset(stats,stats[,1]==simList[i] & stats[,4]=="Long" & stats[,7] == "pHealthy")[[8]]
  
  # Extract objective 4
  obj4[i]<-subset(stats,stats[,1]==simList[i] & stats[,4]=="Long" & stats[,7] == "pObj4")[[8]]
  
  # Extract objective 5
  obj5[i]<-subset(stats,stats[,1]==simList[i] & stats[,4]=="Short" & stats[,7] == "medAvgCatch")[[8]]
  
  
  # Extract minimum and maximum catch (medians)
  minC[i]<-subset(stats,stats[,1]==simList[i] & stats[,4]=="Short" & stats[,7] == "medLowCatch")[[8]]
  maxC[i]<-subset(stats,stats[,1]==simList[i] & stats[,4]=="Short" & stats[,7] == "medHighCatch")[[8]]  
  
  # Extract median AAV
  medAAV[i]<-subset(stats,stats[,1]==simList[i] & stats[,4]=="Short" & stats[,7] == "medAAV")[[8]]  
  
  #Extract 2016 catch and depletion
  D2016[i]<-subset(stats,stats[,1]==simList[i] & stats[,4]=="Short" & stats[,7] == "t1AvgDep")[[8]]  
  C2016[i]<-subset(stats,stats[,1]==simList[i] & stats[,4]=="Short" & stats[,7] == "t1AvgCatch")[[8]] 
   
}


output<-data.frame(scenario,MP, obj1, obj2ObsProb, obj2AcceptProb, obj3a, obj3b, obj4, obj5, minC, maxC, medAAV, D2016,C2016)



write.csv(output, file=paste( fName,"_objTable_2017",".csv"), row.names=F)

}

