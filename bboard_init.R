#------------------------------------------
#------------------------------------------
#------- initialize Blackboard grading
#------------------------------------------
#------------------------------------------

######################################
#---create function that inputs gradebook headers and roster

bboard.init<-function(
	#--gradebook csv file... no extension!!!
		file=NULL,
	#--teacher
		teacher="William Johnson",
	#--save... yes or no?
		save=F,
	#--file.out (no RData extension needed)
		file.out=NULL
		){

#####################################
#-------check arguments

#---stop if no filename
	if(is.null(file)) stop("Argument `file' is undefined, with no default")
#---stop if trying to save w/out filename
	if(save && is.null(file.out)) stop("To save course data, argument `file.out' cannot be NULL")


#####################################
#-------extract course info data

#---???? play w/ WD??
	
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


######################################
#----------manage output


#---output!!
	course.info<-list(assignments,roster,students)

#----if save
	if(save) save(course.info, file=paste0(file.out,".RData"))
#---if not save
	if(!save) course.info
		
	}

	

	
