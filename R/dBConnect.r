dBConnect<-function(which.database="visstat"){
	library(RODBC)
  odbcCloseAll()
	if(which.database=="visstat"){
   conn <- odbcConnect(dsn="visstatp", uid="doug",pwd="")
   }
  if(which.database=="frisbe"){
  conn<-odbcConnect(dsn="frisbep",uid="doug",pwd="")
  }
	conn
}

