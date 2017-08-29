#load libraries

library(ggplot2)
library(lme4)

#### specifying paths and various conditions####################

experimentlist<-c("/users/human/Desktop/Tecan/041717_First_Tecan_Run.csv", "/users/human/Desktop/Tecan/042017_Second_Tecan_Run.csv", "/users/human/Desktop/Tecan/042617_Third_Tecan_Run.csv", "/users/human/Desktop/Tecan/051517_Fourth_Tecan_Run.csv", "/users/human/Desktop/Tecan/052317_Fifth_Tecan_Run-3.csv", "/users/human/Desktop/Tecan/060617_Sixth_Tecan_Run.csv", "/users/human/Desktop/Tecan/060717_Seventh_Tecan_Run.csv", "/users/human/Desktop/Tecan/061317_Eighth_Tecan_Run.csv", "/users/human/Desktop/Tecan/071317_Ninth_Tecan_Run.csv", "/users/human/Desktop/Tecan/072017_Tenth_Tecan_Run.csv", "/users/human/Desktop/Tecan/072017_Eleventh_Tecan_Run.csv", "/users/human/Desktop/Tecan/072417_Twelth_Tecan_Run.csv", "/users/human/Desktop/Tecan/072417_Thirteenth_Tecan_Run.csv")

#paths to graph files
#TODO: replace all of this stuff with a for loop with substrings
pdflist<-c(gsub(" ","", paste('/Users/human/Desktop/TecanRunGraphs/',gsub(" ","_", as.character(Sys.time())),'_TecanRun1')), gsub(" ","", paste('/Users/human/Desktop/TecanRunGraphs/',gsub(" ","_", as.character(Sys.time())),'_TecanRun2')), gsub(" ","", paste('/Users/human/Desktop/TecanRunGraphs/',gsub(" ","_", as.character(Sys.time())),'_TecanRun3')), gsub(" ","", paste('/Users/human/Desktop/TecanRunGraphs/',gsub(" ","_", as.character(Sys.time())),'_TecanRun4')),gsub(" ","", paste('/Users/human/Desktop/TecanRunGraphs/',gsub(" ","_", as.character(Sys.time())),'_TecanRun5')),
    gsub(" ","", paste('/Users/human/Desktop/TecanRunGraphs/',gsub(" ","_", as.character(Sys.time())),'_TecanRun6')), gsub(" ","", paste('/Users/human/Desktop/TecanRunGraphs/',gsub(" ","_", as.character(Sys.time())),'_TecanRun7')), gsub(" ","", paste('/Users/human/Desktop/TecanRunGraphs/',gsub(" ","_", as.character(Sys.time())),'_TecanRun8')), gsub(" ","", paste('/Users/human/Desktop/TecanRunGraphs/',gsub(" ","_", as.character(Sys.time())),'_TecanRun9')), gsub(" ","", paste('/Users/human/Desktop/TecanRunGraphs/',gsub(" ","_", as.character(Sys.time())),'_TecanRun10')), gsub(" ","", paste('/Users/human/Desktop/TecanRunGraphs/',gsub(" ","_", as.character(Sys.time())),'_TecanRun11')))

#paths to GR files
#TODO: replace all of these with for loop with substring as well
grfilelist<-c('/Users/human/Desktop/TecanRunGR/GR1.rfile', '/Users/human/Desktop/TecanRunGR/GR2.rfile', '/Users/human/Desktop/TecanRunGR/GR3.rfile', '/Users/human/Desktop/TecanRunGR/GR4.rfile', '/Users/human/Desktop/TecanRunGR/GR5.rfile', '/Users/human/Desktop/TecanRunGR/GR6.rfile', '/Users/human/Desktop/TecanRunGR/GR7.rfile', '/Users/human/Desktop/TecanRunGR/GR8.rfile', '/Users/human/Desktop/TecanRunGR/GR9.rfile', '/Users/human/Desktop/TecanRunGR/GR10.rfile', '/Users/human/Desktop/TecanRunGR/GR11.rfile')

#conditions
C1<-rep(c("1.5% Glucose", "1.7% Glucose", "1.8% Glucose", "2.5% Glucose", "8.5µM GDA", "5µM Rad", "2µg/mL Flu", "1.5% Glucose", "0.5% DMSO", "2µg/mL Flu",  "5µM Rad", "8.5µM GDA"),8) #run 1

C2<-rep(c("10µg/mL Ben", "2µg/mL Ben","0.4µg/mL Ben", "1.5% Glucose", "2µg/mL Flu", "10µg/mL Ben", "2µg/mL Ben", "2µg/mL Flu"),each = 12) #run 2

C3<-rep(rep(c("1.5% Glucose", "1.7% Glucose", "1.8% Glucose", "2.5% Glucose" ), each = 12),2) # run 3

C4<-rep(rep(c("2.5% Glucose", "1.8% Glucose", "1.7% Glucose", "1.5% Glucose" ), each = 12),2) # run 4

C5<-c(rep(c("2.5% Glucose", "1.8% Glucose", "1.7% Glucose", "1.5% Glucose" ), each = 12), rep(c("1.5% Glucose", "1.7% Glucose", "1.8% Glucose", "2.5% Glucose" ), each = 12)) # run 5, bad data with INVALIDS (RUN ON OUR TECAN)

C6<-rep(rep(c("2.5% Glucose", "1.8% Glucose", "1.7% Glucose", "1.5% Glucose" ), each = 12),2)
#run 6, a rerun of the failed experiment on 6/5 with iranon and iramis (HADLY TECAN)


C7<-rep(c("1.5% Glucose"),96) # run 7 the bad data one with INVALIDS (RAN ON OUR TECAN - BROKEN)

C8<-rep(c("1.5% Glucose"),96) # run 8 rerun of run 7 because of bad data, with iranon and iramis and only 1.5% because I wanted to focus on the effect of gene and not really condition (ON COUNTER, HADLY TECAN)

C9<-rep(c("1.5% Glucose"),96) # run 9 (NEW BATCH, HADLY TECAN) 7/13

C10<-rep(c("1.5% Glucose"), 96) # run 10 Hadly Tecan 7_20

C11<-rep(c("1.5% Glucose"), 96) # run 11 Fraiser Tecan 7_20

C12<-rep(c("1.5% Glucose"),96) # run 12 Hadly Tecan 7_24

C13<-rep(c("1.5% Glucose"),96) # run 13 Fraiser Tecan 7_24

conditionlist<-c(C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13)

datelist<-c(042017,042617,042817,051517,051517, 060617, 060717, 061317, 071317, 072017, 072017, 072417, 072417)

sdthreshold<-c(0.33,0.57,0.45, 0.33, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1) #TODO replace w/ actual math

#TODO check the gene lists to make sure that they are correct with the experiment setups in the lab notebook

genelist1<-rep("ANC",96)

genelist2<-rep("ANC",96)

genelist3<-rep(rep(c("ANC", "RAS2", "TOR"), each = 4),8)

genelist4<-rep(rep(c("ANC", "RAS2", "TOR"), each = 1),32)

genelist5<-rep(rep(c("RAS2", "TOR", "ANC", "IRA_NON", "IRA_MIS", "RAS2", "TOR", "ANC", "IRA_NON", "IRA_MIS","IRA_NON", "IRA_MIS"), each = 1),8)

genelist6<-rep(rep(c("RAS2", "TOR", "ANC", "IRA_NON", "IRA_MIS", "RAS2", "TOR", "ANC", "IRA_NON", "IRA_MIS","IRA_NON", "IRA_MIS"), each = 1),8)

genelist7<-c(rep("IRA_NON",12),rep("IRA_MIS",12),rep("RAS2",6), rep("TOR",6),rep("ANC",12),rep("RAS2",12), rep("TOR",12), rep("IRA_NON",12),rep("IRA_MIS",12))

genelist8<-c(rep("IRA_NON", 12), rep("IRA_MIS", 12), rep("RAS2", 12), rep("ANC", 12), rep("TOR", 12), rep("IRA_NON", 12), rep("IRA_MIS", 12), rep("ANC", 12))

genelist9<-c(rep("IRA_NON", 12), rep("IRA_MIS", 12), rep("RAS2", 12), rep("ANC", 12), rep("TOR", 12), rep("IRA_NON", 12), rep("IRA_MIS", 12), rep("ANC", 12))

genelist10<-c(rep("IRA_NON", 12), rep("IRA_MIS", 12), rep("RAS2", 12), rep("ANC", 12), rep("TOR", 12), rep("IRA_NON", 12), rep("IRA_MIS", 12), rep("ANC", 12))

genelist11<-c(rep("IRA_NON", 12), rep("IRA_MIS", 12), rep("RAS2", 12), rep("ANC", 12), rep("TOR", 12), rep("IRA_NON", 12), rep("IRA_MIS", 12), rep("ANC", 12))

genelist12<-rep(c(rep(c("RAS2","ANC","TOR","IRA_NON","IRA_MIS"),2),"TOR","ANC"),8)

genelist13<-rep(c(rep(c("RAS2","ANC","TOR","IRA_NON","IRA_MIS"),2),"TOR","ANC"),8)

genelistmaster<-c(genelist1, genelist2, genelist3, genelist4, genelist5, genelist6, genelist7, genelist8, genelist9, genelist10, genelist11, genelist12, genelist13)

################starting giant for loop through each experiment###########################
#for (path in 1:length(experimentlist)){
path<- 9
while (path<12){
    if (path == 5 | path == 7){
        path<- path+1
    }

    raw.data<-read.table(experimentlist[path], sep=",")

    ####parsing out unused rows and keeping only mean from every well

    GrowthData<-raw.data[as.numeric(which(raw.data[,1]=="Mean")),]
    #head(GrowthData)
    GrowthData<-GrowthData[,-1]
    GrowthData<-GrowthData[,!is.na(GrowthData[1,])] #removing NA



    Rows_to_keep<-as.numeric(which(raw.data[,1]=="Mean"))-3
    rownames(GrowthData)<-raw.data[Rows_to_keep, 1]


    Time<-list()
    start<-0
    Time<-append(Time, start)
    for (i in 1:(ncol(GrowthData)-1)){
        start<-start+900
        Time<-append(Time, start)
    }
    #as.numeric(Time)

    ###changing columns that are factors to numeric
    new.data<-GrowthData
    new.data[,1]<-as.numeric(as.character.factor(new.data[,1]))
    new.data[,4]<-as.numeric(as.character.factor(new.data[,4]))
    new.data[,5]<-as.numeric(as.character.factor(new.data[,5]))
    new.data[,8]<-as.numeric(as.character.factor(new.data[,8]))

    #new.data[new.data=="INVALID"]<-0
    #is.na(new.data) <- sapply(new.data, is.infinite)
    #is.na(new.data) <- sapply(new.data, is.nan)
    #new.data[is.na(new.data)]<-0
    #new.data[,256:280]<-0

    ####  Sliding window linear modeling

    pdf(file  = pdflist[path], width = 16, height = 12)


    par(mfrow = c(3, 4))

    interval=15
    #timeINsec<-raw.data[1,][-1]
    #timeINhrs<-timeINsec/3600
    #t<-as.numeric(timeINhrs[-1])  #timepoints in experiment
    t<-as.numeric(Time)/3600
    info<-matrix(0,length(t)-interval,3)
    ODinfo<-matrix(0,30,2)
    colnames(info)<-c("Intercept","slope","r.square")
    expo<-matrix(NA,96,3)
    GR<-matrix(0,96,11)
    GR<-as.data.frame(GR)
    WellID<-rownames(new.data)
    Growth_Rates<-list()
    print("entering for loop")
    for(j in 1:96){

       print(j)
        y<-as.numeric(log(abs(as.numeric(new.data[j,]))))
        y[which(y[]=="-Inf")]<-0
        #y[y==y[185]]<-0
        #print(y)
        for (k in 1:(length(y)-interval)){
            # lm
            l<-lm(y[k:(k+interval)]~t[k:(k+interval)],na.rm=T)
            info[k,1]<-coef(l)[1] #y-intercept
            info[k,2]<-coef(l)[2] #slope
            info[k,3]<-summary(l)$adj.r.squared
        }

        #z<-lm(info[1:,2]~t[1:20]) #is this actually right?
        9
        horiz<-info[which.min(abs(info[1:20,2])),1] #y-intercept of slope closest to 0
        tophoriz<-info[nrow(info),1] #y-intercept of last point for calculating exp phase

        GR[j,1]<-as.character(WellID[j])
        GR[j,4]<-as.numeric(info[nrow(info),1])
        GR[j,6]<-sd(y,na.rm=TRUE)
        GR[j,5]<-as.numeric(max(info[40:nrow(info),3],na.rm=T))

        if(max(info[40:nrow(info),3],na.rm=T)>=0.8 ){  ###  Find the fit with the highest growth rate and R^2 at least 0.95
            expo[j,1]<-max(info[which(info[40:nrow(info),3]>0.8),2]) #max slope
            slope<-max(info[which(info[40:nrow(info),3]>0.8),2])
            y_int<-info[which(info[,2]==slope),1]
            expo[j,2]<-(horiz-y_int)/expo[j,1] #calculating the x intercept for lag phase
            expo[j,3]<-(tophoriz-y_int)/expo[j,1] #calculating the intersection of exp phase and saturation for length of exp.
            #expo[j,2]<-(-1)*max(info[which(info[40:nrow(info),3]>0.8),2])/expo[j,1] #calculating the intercept

            GR[j,3]<-expo[j,1] #max slope
            GR[j,8]<-expo[j,2] #start of lag phase
            GR[j,9]<-expo[j,3] #start of exponential
            GR[j,10]<-expo[j,3]-expo[j,2] #calculating length of exponential

            print("plotting!!!")
            plot(t,y,pch=10,main=as.character(WellID[j]),ylim=c(min(log(new.data[1:nrow(new.data),2:ncol(new.data)]),na.rm=T),max(log(new.data[1:nrow(new.data),2:ncol(new.data)]),na.rm=T)),xlab='Time (hr)',ylab='log(OD)')


            if (info[nrow(info),2]>0.02) { #seeing if last point is steep, if it is, carrying cap not reached
                legend('bottomright',legend="Carrying Capacity not reached")
                print(path)
                print(j)
                print("Carrying cap not reached")

                GR[j,3]<- 0
                GR[j,8]<- 0
                GR[j,9]<- 0
                GR[j,10]<- 0


            } else if(GR[j,6]<sdthreshold[path]){
                legend('bottomright',legend="Standard Deviation is way off")
                print(path)
                print(j)
                print("STANDARD DEVIATION IS OFF FOR THIS ONE")

                GR[j,3]<- 0
                GR[j,8]<- 0
                GR[j,9]<- 0
                GR[j,10]<- 0



            } else if(expo[j]>.005){
                print("minimum bound")
                abline(info[(match(max(info[which(info[,3]>0.8),2]),info)-dim(info)[1])],max(info[which(info[,3]>0.8),2]))
                abline(v=GR[j,8],col="blue")
                abline(h=horiz, col="red")
                abline(v=GR[j,9],col="blue")
                abline(h=tophoriz,col="red")
                legend('bottomright',legend=expo[j])
            } else {
                print("oh noooo no detectable growthhhh")
                 legend('bottomright',legend="No Detectable Growth")

                 GR[j,3]<- 0
                 GR[j,8]<- 0
                 GR[j,9]<- 0
                 GR[j,10]<- 0
            }


        }else{
            print(path)
            print(j)
            print("no detectable growth")
            plot(t,y,pch=10,main=as.character(WellID[j]),ylim=c(min(log(new.data[1:nrow(new.data),2:ncol(new.data)]),na.rm=T),max(log(new.data[1:nrow(new.data),2:ncol(new.data)]),na.rm=T)),xlab='Time (hr)',ylab='log(OD)')
            legend('bottomright',legend="No Detectable Growth")

            GR[j,3]<- 0
            GR[j,8]<- 0
            GR[j,9]<- 0
            GR[j,10]<- 0
            }
        }
    dev.off()

    GR<-as.data.frame(GR)
    colnames(GR)<-c("Well","Date","FinalRate", "MaxSlope", "CarryingCapacity", "SD", "Condition", "LagPhaseLength", "ExpPhaseStart", "ExpPhaseLength", "Gene")

    GR$Date<-datelist[path]
    
    GR$Gene<-genelistmaster[path]
    GR$Condition<-conditionlist[path]
    assign(gsub(" ","", paste("GR",as.character(path))),GR)

    save(GR, file = grfilelist[path])

    if (path == 1){
        alldata<-GR
    } else {
        alldata<-rbind(alldata,GR)
    }

    print("**********************Done running experiment*******************")
    print(path)
    path<-path+1

}

#alldata$SD<-as.numeric(as.character(alldata$SD))

#GR6<-matrix(0,96,10)
#GR6<-alldata[which(alldata$Date==060617),]
#rownames(GR6)<-c(1:96)
#GR6<-subset(GR6[,1:11])

#GR8<-matrix(0,96,10)
#GR8<-alldata[which(alldata$Date==061317),]
#rownames(GR8)<-c(1:96)
#GR8<-subset(GR8[,1:11])


alldata$Row<-rep(rep(c("A","B","C","D","E","F","G","H"),each=12),6)
alldata$Column<-rep(rep(c(1,2,3,4,5,6,7,8,9,10,11,12),8),6)
alldata$Edge<-FALSE
alldata$Edge[which(alldata$Row=="A"|alldata$Row=="H"|alldata$Column==1|alldata$Column==12)]<-TRUE


mutants<-rbind(GR3,GR4,GR6,GR8)
mutants$Row<-rep(rep(c("A","B","C","D","E","F","G","H"),each=12),4)
mutants$Column<-rep(rep(c(1,2,3,4,5,6,7,8,9,10,11,12),8),4)

############################MODEL CONSTRUCTION#####################################
#GR1<-lmer(MaxSlope~Gene+Condition+Gene:Condition+(1|Well)+(1|Date),data=alldata,REML=F)
#GR2<-lmer(MaxSlope~Gene+Condition+(1|Well)+(1|Date),data=alldata,REML=F)
#GR3<-lmer(LagPhaseLength~Gene+Condition+(1|Well)+(1|Date),data=alldata, REML=F)
#GR4<-lmer(LagPhaseLength~Gene+Condition+Gene:Condition+(1|Well)+(1|Date),data=alldata, REML=F)

#GR5<-lmer(MaxSlope~Condition+(1|Date),data=alldata, REML=F)
#GR6<-lmer(MaxSlope~Gene+Condition+Row+Column+(1|Date),data=alldata, REML=F)
#GR6<-lmer(MaxSlope~Gene+Condition+Row+Column+(1|Date),data=alldata, REML=F)
#GR7<-lmer(MaxSlope~Gene+Condition+(1|Date),data=alldata, REML=F)

#GR8<-lmer(LagPhaseLength~Gene+Condition+(1|Date),data=alldata, REML=F)
#GR9<-lmer(MaxSlope~Condition+(1|Date)+(1|Well),data=alldata, REML=F)


#GR10<-lmer(MaxSlope~Gene+Condition+(1|Date)+(1|Row)+(1|Column),data=alldata, REML=F)

GR6<-lmer(LagPhaseLength~Gene+Condition+Row+Column+(1|Date),data=alldata, REML=F)
GR10<-lmer(LagPhaseLength~Gene+Condition+(1|Date)+(1|Row)+(1|Column),data=alldata, REML=F)

##test terms###
anova(update(GR1,.~.-(1|Date)),GR1) #3.87e-08 ***
anova(update(GR1,.~.-(1|Well)),GR1) #1
anova(update(GR1,.~.-(Gene:Condition)),GR1) #0.8806
anova(GR1,update(GR1,.~.+(1|Gene:Date))) #1
anova(GR1,update(GR1,.~.+(1|Gene:Condition))) #1
anova(GR1,update(GR1,.~.+(1|Condition:Date))) #1
anova(GR1,update(GR1,.~.+(1|Condition:Well))) #1
anova(update(GR1,.~.-(Gene)),GR1) #1
anova(update(GR1,.~.-(Condition)),GR1) #2.2e-16 ***

anova(update(GR2,.~.-(Gene)),GR2) #0.9782
anova(update(GR2,.~.-(Condition)),GR2) #2.177e-15 ***

anova(update(GR3,.~.-(1|Date)),GR3) #1
anova(update(GR3,.~.-(1|Well)),GR3) #1
anova(update(GR3,.~.-(Gene)),GR3) #0.985
anova(update(GR3,.~.-(Condition)),GR3) #0.1374

anova(update(GR4,.~.-(1|Date)),GR4) #1
anova(update(GR4,.~.-(1|Well)),GR4) #1
anova(update(GR4,.~.-(Gene)),GR4) #1
anova(update(GR4,.~.-(Condition)),GR4) #1
anova(update(GR4,.~.-(Gene:Condition)),GR4) #1

anova(update(GR5,.~.+(1|Well)),GR5) #1
anova(update(GR5,.~.+Gene),GR5) #0.9782

anova(update(GR6,.~.-Gene),GR6) #0.7385
anova(update(GR6,.~.-Row),GR6) #7.684e-05 ***
anova(update(GR6,.~.-Column),GR6) #0.03309 * #doesn't this mean that Gene DOES have an effect?
anova(update(GR6,.~.-Condition),GR6) #6.25e-13 ***
anova(update(GR6,.~.+(1|Gene:Date)),GR6) #1
anova(update(GR6,.~.+(1|Gene:Date)),GR6) #1
anova(update(GR6,.~.+(1|Condition:Date)),GR6) #1
anova(GR6,update(GR6,.~.+(1|Gene:Condition))) #1
#but in this case, isn't row the same as condition basically?

anova(update(GR7,.~.-Gene),GR7) #0.9782

anova(update(GR8,.~.-Gene),GR8) #0.658
anova(update(GR8,.~.-Condition),GR8) #2.2e-16 ***
anova(update(GR8,.~.+Row),GR8) #2.2e-16 ***
anova(update(GR8,.~.+Column),GR8) #0.03309 * #0.5603
anova(GR8,update(GR8,.~.+(1|Gene:Condition))) #1
anova(GR8,update(GR8,.~.+(1|Gene:Date))) #1
anova(GR8,update(GR8,.~.+(1|Condition:Date))) #0.00292 **
anova(GR8,update(GR8,.~.+(1|Condition:Well))) #0.9786

anova(update(GR9,.~.+Gene),GR9) #0.9782
anova(update(GR9,.~.-Condition),GR9) #2.148e-15 ***
anova(update(GR9,.~.+Row),GR9) # 0.0001174 ***
anova(update(GR9,.~.+Column),GR9) # 0.0763 .
anova(GR9,update(GR9,.~.+(1|Gene:Condition))) #1
anova(GR9,update(GR9,.~.+(1|Gene:Date))) #1
anova(GR9,update(GR9,.~.+(1|Condition:Date))) #1
anova(GR9,update(GR9,.~.+(1|Condition:Well))) #1
anova(update(GR9,.~.-(1|Date)),GR9) # 2.988e-10 ***
anova(update(GR9,.~.-(1|Well)),GR9) #1

anova(update(GR10,.~.-Gene),GR10) #0.979
anova(update(GR10,.~.-Condition),GR10) #7.751e-13 ***
anova(update(GR10,.~.-(1|Date)),GR10) # 3.713e-08 ***
anova(update(GR10,.~.-(1|Row)),GR10) #0.0006635 ***
anova(update(GR10,.~.-(1|Column)),GR10) # 1
anova(update(GR10,.~.+(1|Well)),GR10) # 1
anova(GR10,update(GR10,.~.+(1|Gene:Condition))) #1
anova(GR10,update(GR10,.~.+(1|Gene:Date))) #1
anova(GR10,update(GR10,.~.+(1|Condition:Date))) #1
anova(GR10,update(GR10,.~.+(1|Condition:Well))) #1

###################################################################

GR10<-lmer(MaxSlope~Gene+Condition+(1|Date)+(1|Row)+(1|Column),data=mutants, REML=F)

anova(update(GR10,.~.-Gene),GR10) #6.255e-05 ***
anova(update(GR10,.~.-Condition),GR10) #2.2e-16 ***
anova(update(GR10,.~.-(1|Date)),GR10) #0.0002657 ***
anova(update(GR10,.~.-(1|Row)),GR10) #0.9759
anova(update(GR10,.~.-(1|Column)),GR10) #0.06958 .
anova(update(GR10,.~.+(1|Well)),GR10) # 1
anova(GR10,update(GR10,.~.+(1|Gene:Condition))) # 0.9339
anova(GR10,update(GR10,.~.+(1|Gene:Date))) #2.512e-08 ***
anova(GR10,update(GR10,.~.+(1|Condition:Date))) #0.7097
#gene:date has a significant effect
#gene has significant effect and date effect is diminished with mutant dataset
#condition is still significant
#column is slightly significant, but row and well are not

GR6<-lmer(MaxSlope~Gene+Condition+Row+Column+(1|Date),data=mutants, REML=F)

anova(update(GR6,.~.-Gene),GR6) #2.204e-05 ***
anova(update(GR6,.~.-Row),GR6) #0.3259
anova(update(GR6,.~.-Column),GR6) #0.1856
anova(update(GR6,.~.-Condition),GR6) #2.2e-16 ***
anova(update(GR6,.~.+(1|Gene:Date)),GR6) #1.279e-09 ***
anova(update(GR6,.~.+(1|Condition:Date)),GR6) #1
anova(GR6,update(GR6,.~.+(1|Gene:Condition))) #1
anova(GR6,update(GR6,.~.+(1|Row:Column))) #1
anova(update(GR6,.~.+(1|Well)),GR6) # 1
anova(update(GR6,.~.+Well),GR6) #0.09553 .
#add gene:date
#well has a little bit of an effect when treated as a fixed effect
#gene has effect when only with experiments 3 and 4, diminished effect if row and column are treated as fixed effects
#condition and date has an effect

#need ?


GR10<-lmer(MaxSlope~Gene+Condition+Edge+(1|Date)+(1|Row)+(1|Column),data=alldata, REML=F)


############################GRAPHING#######################################

ggplot(mutants, aes(x = Condition, y = as.numeric(as.character(LagPhaseLength)))) + geom_jitter(aes(color=Gene)) +geom_boxplot(fill = NA, outlier.size = NA, aes(group = Condition))+theme_bw()+ylab("Lag Phase Length")+xlab("Condition")+facet_grid(.~Date)+theme(axis.text.x=element_text(angle=90))


ggplot(mutants, aes(x = Gene, y = as.numeric(as.character(MaxSlope)))) + geom_jitter(aes(color=Gene)) +geom_boxplot(fill = NA, outlier.size = NA, aes(group = Gene))+theme_bw()+ylab("Maximum exponential growth rate")+xlab("Condition")+facet_grid(.~Condition)+theme(axis.text.x=element_text(angle=90))

ggplot(mutants, aes(x = Condition, y = as.numeric(as.character(ExpPhaseLength)))) + geom_jitter(aes(color=Gene)) +geom_boxplot(fill = NA, outlier.size = NA, aes(group = Condition))+theme_bw()+ylab("Exp Phase Length")+xlab("Condition")+facet_grid(.~Date)+theme(axis.text.x=element_text(angle=90))

ggplot(subset(alldata,alldata$LagPhaseLength != 0), aes(x = Condition, y = as.numeric(as.character(LagPhaseLength)))) + geom_jitter(aes(color=Gene)) +geom_boxplot(fill = NA, outlier.size = NA, aes(group = Condition))+theme_bw()+ylab("Lag Phase Length")+xlab("Condition")+facet_grid(.~Date)+theme(axis.text.x=element_text(angle=90))

###########################################################################





ggplot(GR, aes(x = Condition, y = as.numeric(Rate))) + geom_jitter() +geom_boxplot(fill = NA, outlier.size = NULL, aes(group = Condition))+theme_bw()+ylab("Maximum exponential growth rate")+xlab("Condition")

ggplot(alldata, aes(x = Condition, y = as.numeric(Rate))) + geom_jitter() +geom_boxplot(fill = NA, outlier.size = NULL, aes(group = Condition))+theme_bw()+ylab("Maximum exponential growth rate")+xlab("Condition")

ggplot(GR, aes(x = Condition, y = as.numeric(as.character(Rate)))) + geom_jitter(aes(color=Gene)) +geom_boxplot(fill = NA, outlier.size = NA, aes(group = Condition))+theme_bw()+ylab("Maximum exponential growth rate")+xlab("Condition")+facet_grid(.~Date)+theme(axis.text.x=element_text(angle=90))

#column names are vertical and dates separated by color, with combined data set!
ggplot(subset(alldata, alldata$MaxSlope != 0), aes(x = Condition, y = as.numeric(as.character(Rate)))) + geom_jitter(aes(color=Gene)) +geom_boxplot(fill = NA, outlier.size = NA, aes(group = Condition))+theme_bw()+ylab("Maximum exponential growth rate")+xlab("Condition")+facet_grid(.~Date)+theme(axis.text.x=element_text(angle=90))

ggplot(subset(alldata, alldata$MaxSlope != 0), aes(x = LagPhaseLength, y = as.numeric(as.character(Rate)))) + geom_jitter(aes(color=Gene)) +geom_boxplot(fill = NA, outlier.size = NA, aes(group = Condition))+theme_bw()+ylab("Maximum exponential growth rate")+xlab("Condition")+facet_grid(.~Date)+theme(axis.text.x=element_text(angle=90))

ggplot(GR3, aes(x = Gene, y = as.numeric(as.character(Rate)))) + geom_jitter(aes(color=Gene)) +geom_boxplot(fill = NA, outlier.size = NA, aes(group = Gene))+theme_bw()+ylab("Maximum exponential growth rate")+xlab("Condition")+facet_grid(.~Condition)+theme(axis.text.x=element_text(angle=90))

#removing 0s with subset
ggplot(subset(alldata, alldata$Rate != 0), aes(x = Condition, y = as.numeric(as.character(Rate)))) + geom_jitter(aes(color=Gene)) +geom_boxplot(fill = NA, outlier.size = NA, aes(group = Condition))+theme_bw()+ylab("Maximum exponential growth rate")+xlab("Condition")+facet_grid(.~Date)+theme(axis.text.x=element_text(angle=90))


#Plotting Lag Phase to GR
ggplot(GR, aes(x = Condition, y = as.numeric(as.character(LagPhaseLength)))) + geom_jitter(aes(color=Gene)) +geom_boxplot(fill = NA, outlier.size = NA, aes(group = Condition))+theme_bw()+ylab("Lag Phase Length")+xlab("Condition")+facet_grid(.~Date)+theme(axis.text.x=element_text(angle=90))

######  PLOTTING

par(mfrow=c(1,2))
### Quick plot of raw OD measurements
timeINmin<-timeINsec/60
timeINhrs<-timeINsec/3600

plot(timeINhrs, new.data[1,2:dim(new.data)[2]], ylim=c(min(new.data[,2:dim(new.data)[2]]),max(new.data[,2:dim(new.data)[2]])))

for (i in 2:9){
  points(timeINhrs, new.data[i,2:dim(new.data)[2]], col=rgb(runif(1),runif(1),runif(1)))
}

### Quick plot of log OD measurements
plot(timeINhrs, log(new.data[1,2:dim(new.data)[2]]), ylim=c(min(log(new.data[,2:dim(new.data)[2]])),max(log(new.data[,2:dim(new.data)[2]]))))

for (i in 2:9){
  points(timeINhrs, log(new.data[i,2:dim(new.data)[2]]), col=rgb(runif(1),runif(1),runif(1)))
}


library(ggplot2)

GR<-as.data.frame(GR)
colnames(GR)<-c("Strains", "Conditions", "Growth")
GR$Conditions <- factor(GR$Conditions, levels=c("5uMGdA", "5uMControl", "8.5uMGdA", "8.5uMControl", "25uMGdA", "25uMControl", "50uMGdA", "50uMControl", "100uMGdA", "100uMControl", "200uMGdA", "200uMControl"), ordered=TRUE)
GR$Growth<-as.numeric(as.character(GR$Growth))
GB<-c(rep("Beer", 24), rep("MA", 24), rep("Rec",24), rep("Wild", 24))
GR<-cbind(GR, GB)
cond<-c("GdA", "Control")
Drug<-rep(cond, 48)
GR<-cbind(GR, Drug)


pdf("/Users/Grace/dropbox/Geldanamycin/Tecan_Growth/Tecan_Boxplot_140510.pdf")
ggplot(subset(GR, GR$Growth != 0), aes(Conditions, Growth))+geom_boxplot(aes(fill = GR$Drug))+geom_point(aes(colour = GR$GB))+opts(axis.text.x=theme_text(angle=-45))+scale_fill_manual(values=c("grey", "white"))
dev.off()

pdf("/Users/Grace/dropbox/Geldanamycin/Tecan_Growth/Tecan_Sorted_140510.pdf")
ggplot(data=GR, aes(Conditions, Growth))+geom_point(aes(colour = GR$Strains))
dev.off()


###FInalAwesomeness


ggplot(subset(KillCurve, KillCurve$GB!="Rec" & KillCurve$Numbers!="5"& KillCurve$Numbers!="500"& KillCurve$Numbers!="30"), aes(Numbers, Growth, color = Drug, group = Drug))+geom_point()+geom_smooth(group = 1)+opts(axis.text.x=theme_text(angle=-45))+scale_color_manual(values=c("red", "blue"))+facet_grid(GB~.)


p<-ggplot(alldata, aes(x=SD)) + geom_histogram(binwidth=0.03)  + facet_grid(.~Date)
intercept<-data.frame(Date = c(042017,042617,042817), int = c(0.33,0.57,0.45))
p + geom_vline(aes(xintercept=int),intercept)
