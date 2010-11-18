#Cstart <- "01-jan-2010"
#Cstop  <- "31-jan-2010"


GetDataLandingValueByRegistration <- function(Cstart=Cstart,Cstop=Cstop) {

# This function gets the value of each component of the landings

# Get the total days at sea by trip:

dasbyreg <- GetDataDaysAtSeaWithLandingsByRegistration(Cstart=Cstart,Cstop=Cstop)

print ("Got days at sea by trip")

# Get the price data

prices <- GetDataPrice(Cstart=Cstart,Cstop=Cstop)

#Fudge: if there are no price data use 2009

if(dim(prices)[[1]] ==0){ prices <- GetDataPrice(Cstart='01-Jan-2009',Cstop='31-Dec-2009')}

print ("Got prices")

sv <- tapply(prices$VALUE,list(as.factor(prices$WEEK),as.factor(prices$TXN_ICES_CODE)),sum);
sw <- tapply(prices$WEIGHT,list(as.factor(prices$WEEK),as.factor(prices$TXN_ICES_CODE)),sum);

sp <- sv/sw;


# table with total landings by week
CTAB <- tapply(dasbyreg$WEIGHT,list(as.factor(dasbyreg$WEEK),as.factor(dasbyreg$TXN_ICES_CODE)),sum);
# replace the NAs with zeros
CTAB[is.na(CTAB)] <- 0;


  sp1 <- vectorise(sp)
  dimnames(sp1)[[2]] <- c("PRICE","WEEK","TXN_ICES_CODE")
  
  sp1[is.na(sp1$PRICE),]
  
 # Merge the price data with the landings and effort data

valuebyregistration <- merge(dasbyreg,sp1,all.x=T)

 # For missing prices take the mean for that species

 mean.na <- function(x) { mean(x[!is.na(x)])}

 sp2 <- vectorise(tapply(sp1$PRICE,sp1$TXN_ICES_CODE,mean.na))
 dimnames(sp2)[[2]] <- c("PRICE","TXN_ICES_CODE")
 
 # Where are the prices missing in valuebyregistration ?
 
 ww <- (1:length(valuebyregistration[,1]))[is.na(valuebyregistration$PRICE)]
 
 # Insert the averages
 
 valuebyregistration$PRICE[ww] <- sp2$PRICE[match(as.character(valuebyregistration$TXN_ICES_CODE[ww]),sp2$TXN_ICES_CODE)]

 # Note: some species do not seem to be in the prices table ("FBM","HER","POA","RAZ","RJB","RJH","SBR","SBX","SYC")
 # For the time-being I will make these equal to 1
 
 valuebyregistration$PRICE[is.na(valuebyregistration$PRICE)] <- 1
 
 valuebyregistration$VALUE <- valuebyregistration$WEIGHT*valuebyregistration$PRICE
 
 valuebyregistration
 
 }
 
###############################################################################
##################             Example of DATA EXTRACTION        #########################
###############################################################################

 
# valuebyregistration <- GetDataLandingValueByRegistration(Cstart="01-jan-2009",Cstop="31-jan-2009")
#xx
