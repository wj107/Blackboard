#------------------------------------------
#------------------------------------------
#------- initialize Blackboard grading
#------------------------------------------
#------------------------------------------

#---create function that inputs gradebook headers and roster

bboard.init<-function(
	#--gradebook csv file... no extension!!!
		file=NULL,
	#--teacher
		teacher="William Johnson"
		){

#---work in the desktop
#	setwd("C:/Users/wjohnson/Desktop/")
	
#---load csv w/gradebook data
	dat<-read.csv(paste0(file,".csv"),header=F,stringsAsFactors=F)
	
#---extract first row for assignments
	assignments<-unlist(dat[1,4:ncol(dat)])
	names(assignments)<-NULL

#---extract student roster
	roster<-dat[,1:3]
	names(roster)<-NULL

#---extract student names
	students<-paste(roster[,2],roster[,1])
	students[1]<-teacher

#---output!!
	out<-list(assignments,roster,students)
	out
	}

	

	
