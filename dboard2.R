#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
#------- output metrics for student performance on discussion board
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------

#---create function that takes course info, dboard info, outputs 'nice' metric data

db.info<-function(
	#--course info list from initialization
		course=NULL,
	#--raw dboard data
		dat=NULL,
	#--output file name
		output="db_info"
		){

#---do you have course info??
	if(is.null(course)) stop("Argument `course' is undefined with no default")
#---do you have dboard data??
	if(is.null(dat)) stop("Argument `dat' is undefined with no default")

#---select d board to grade

#---------would love to grep only the d boards, here!!!
	print("What discussion board are you looking at?")
	menu(course[[1]])->tmp

#---define assignment
	course[[1]][tmp]->assignment
	
#---who didn't turn in work??
#	select.list(dat[[3]][-c(1,length(dat[[3]]))],multiple=T,graphic=F,title="Who didn't turn in work??")->who
#	length(who)->N
#---convert answer to index
#	sapply(1:N, function(i) which(dat[[3]]==who[i]))->i
#---subset roster who didn't turn in work
#	dat[[2]][c(1,i),1:3]->who

#---grade column
#	grade<-c(assignment, rep(0,N))
#---other, random columns 5,6,8
	#col5
#	  c5<-c("Grading Notes",rep("",N))
#	  c6<-c("Notes Format",rep("SMART_TEXT",N))
#	  c8<-c("Feedback Format",rep("SMART_TEXT",N))

#----------------------
#--------COMMENTS!!

#---good work!
	readline(prompt="Write a comment for excellent participation.")->c1
#---good work!
	readline(prompt="Write a comment for excellent participation.")->c2
#---good work!
	readline(prompt="Write a comment for excellent participation.")->c3
#---good work!
	readline(prompt="Write a comment for excellent participation.")->c4

#---compile comments altogether (w/assignment title
	comments<-c(assignment, c1,c2,c3,c4)

#---comment column
#	c7<-c("Feedback to Learner", rep(feedback, N))

#---put it together!!
#	uploader<-cbind(who, grade,c5,c6,c7,c8)
#	names(uploader)<-NULL

#---------old dboard code!  to extract 'student.info'
dat<-trimws(dat)

auth<-which(dat=="Author:")+1
auth<-dat[auth]

length(auth)->N

when<-which(dat=="Posted Date:")+1
when<-as.character(dat[when])

whenn<-strsplit(when,", 2018")
day<-unlist(lapply(whenn,function(x) x[1]))
hour<-unlist(lapply(whenn, function(x) x[2]))

b<-c("(Post is Unread)","(Post is Read)")
post<-which(dat=="Published")+1
post.end<-which(dat%in%b)-1

posts<-lapply(1:N,function(x) c(post[x],post.end[x]))
posts<-lapply(posts, function(x) paste(dat[x[1]:x[2]],collapse=" "))

wc<-lapply(posts, function(x) length(gregexpr("\\W+",x)[[1]]))
wc<-unlist(wc)


post.info<-data.frame(auth,day,wc)

###########summarized student info on d board posts

A<-as.character(levels(post.info$auth))
WC<-tapply(post.info$wc,post.info$auth,sum)
DAYS<-tapply(post.info$day,post.info$auth,function(x) length(unique(x)))
POSTS<-as.numeric(table(auth))

if(length(A)>5) comments<-c(comments,rep("",length(A)-5))

student.info<-data.frame(A,POSTS,DAYS,WC,comments,row.names=NULL)

#----write uploader file
	write.csv(
		student.info,
		file=paste0(output,".csv")
		)
	}
