#----------------------------------------------------
#----------------------------------------------------
#------- output grade spreadsheet for missing work
#----------------------------------------------------
#----------------------------------------------------

##########################################
#---create function that creates spreadsheet to upload to bboard

work.missing<-function(
	#--list from initialization
		dat=NULL,
	#--nane of file to upload	
		fname="uploader"
		){

#########################################
#------check arguments

#---do you have data??
	if(is.null(dat)) stop("Argument `dat' is undefined with no default")

########################################
#------MENU: what assignment?

#---select assignment to grade
	print("What assignment are you looking at?")
	menu(dat[[1]])->tmp

#---define assignment
	dat[[1]][tmp]->assignment

########################################
#------MENU: what students?


	
#---who didn't turn in work??
	select.list(dat[[3]][-c(1,length(dat[[3]]))],multiple=T,graphic=F,title="Who didn't turn in work??")->who
	length(who)->N
#---convert answer to index
	sapply(1:N, function(i) which(dat[[3]]==who[i]))->i
#---subset roster who didn't turn in work
	dat[[2]][c(1,i),1:3]->who

########################################
#------Comment.

#---grade column
	grade<-c(assignment, rep(0,N))
#---other, random columns 5,6,8
	#col5
	  c5<-c("Grading Notes",rep("",N))
	  c6<-c("Notes Format",rep("SMART_TEXT",N))
	  c8<-c("Feedback Format",rep("SMART_TEXT",N))

#---what comment to add???
	readline(prompt="Write a comment for students missing the assignment.")->feedback
#---comment column
	c7<-c("Feedback to Learner", rep(feedback, N))

########################################
#--------OUTPUT


#---put it together!!
	uploader<-cbind(who, grade,c5,c6,c7,c8)
	names(uploader)<-NULL
#----write uploader file
	write.csv(
		uploader,
		file=paste0(fname,".csv"),
		#col.names=NA,
		row.names=F
		)
	}
