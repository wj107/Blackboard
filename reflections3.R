#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
#------- output metrics for student performance on reflections
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------

#########################################################
#---create function that takes course info, reflections raw data, outputs 'nice' metric data

ref.info<-function(
	#--course info list from initialization
		course=NULL,
	#--raw reflection data
		dat=NULL,
	#--output file name
		output="ref_info"
		){

#####################################################
#--------Check Arguments.

#---do you have course info??
	if(is.null(course)) stop("Argument `course' is undefined with no default")
#---do you have dboard data??
	if(is.null(dat)) stop("Argument `dat' is undefined with no default")

####################################################
#---MENU: select reflection to grade

#---------would love to grep only the reflections, here!!!
	print("What reflection are you looking at?")
	menu(course[[1]])->tmp
	course[[1]][tmp]->reflection

#---define students
	course[[3]]->students
	

#####################################################
#--------COMMENTS!!

#---you NEED default comments!!

#---good work!
	readline(prompt="Write a comment for excellent participation.")->c1
	readline(prompt="Write a comment for writing enough words, but not enough days.")->c2
	readline(prompt="Write a comment for writing enough days, but not enough words.")->c3
	readline(prompt="Write a comment for minimal participation.")->c4
	readline(prompt="Write a comment for no participation.")->c5

#---compile comments altogether (w/assignment title
	comments<-c(reflection, c1,c2,c3,c4,c5)


######################################
#--------EXTRACT student.info from reflection data


###find all authors, dates, and replies in posting data
	auth<-which(dat %in% students)
	dates<-which(!is.na(strptime(dat,"%H:%M")))
	reply<-which(dat=="Reply")
####remove the names in posting data that are superfluous (ie, first in consecutive occurences)
	auth<-auth[-which(c(diff(auth),11)==1)]
###define post=distance between auth and dates
	post<-dates-auth

####make sure we have the same number of posters & dates
	if(length(dates)!=length(auth)) stop("Number of dates and number of posters doesn't match up.")


########################################
######ORIGINAL POSTS
########################################

###find INDEXES of original posts
	orig<-which(post==1)

###make sure we have the same number of 'replies' and original posts
	if(length(orig)!=length(reply)) stop("Number of replies and original posts doesn't match up.")

####define authors/dates of original posts
	auth.orig<-auth[orig]
	auth.orig<-dat[auth.orig]
	dates.orig<-dates[orig]
	dates.orig<-dat[dates.orig]

####define start/end of original posts
	post.orig.start<-dates[orig]+1
	post.orig.end<-reply-1
######collapse multi-line posts
	posts.orig<-lapply(1:length(orig),function(x) c(post.orig.start[x],post.orig.end[x]))
	posts.orig<-lapply(posts.orig, function(x) paste(dat[x[1]:x[2]],collapse=" "))
#####count words in original posts
	wc.orig<-lapply(posts.orig, function(x) length(gregexpr("\\W+",x)[[1]]))
	wc.orig<-unlist(wc.orig)
####find DAY of original posts
	when.orig<-strsplit(dates.orig,"M ")
	day.orig<-unlist(lapply(when.orig,function(x) x[2]))
	##someday...
	#time.orig<-unlist(lapply(when.orig, function(x) x[1]))

########################################
######REPLIES
########################################

###find INDEXES of replies
	reps<-which(post!=1)

###don't think we need this....
###make sure we have the same number of 'replies' and original posts
##	if(length(orig)!=length(reply)) stop("Number of replies and original posts doesn't match up.")

####define authors/dates of replies
	auth.reps<-auth[reps]
	auth.reps<-dat[auth.reps]
	dates.reps<-dates[reps]
	dates.reps<-dat[dates.reps]
####define start/end of replies
	post.rep.start<-auth[reps]+1
	post.rep.end<-dates[reps]-1
######collapse multi-line posts
	posts.rep<-lapply(1:length(reps),function(x) c(post.rep.start[x],post.rep.end[x]))
	posts.rep<-lapply(posts.rep, function(x) paste(dat[x[1]:x[2]],collapse=" "))
#####count words in replies
	wc.reps<-lapply(posts.rep, function(x) length(gregexpr("\\W+",x)[[1]]))
	wc.reps<-unlist(wc.reps)
####find time/date of original posts
	when.reps<-strsplit(dates.reps,"M ")
	day.reps<-unlist(lapply(when.reps,function(x) x[2]))
	##someday...
	#time.reps<-unlist(lapply(when.reps, function(x) x[1]))

#########################################
#########Collect all RAW data
#########################################

#####define: author, day, wc, type of post:
	auth<-c(auth.orig,auth.reps)
	days<-c(day.orig,day.reps)
	wc<-c(wc.orig,wc.reps)
	type<-c(rep("Post",length(orig)),rep("Reply",length(reps)))
####collect raw data in data.frame
	post.info<-data.frame(auth,days,wc,type,row.names=NULL)

########################################################
#-----------OUTPUT!!! prep student.info

###define unique author info
	AUTH<-as.character(levels(post.info$auth))
##sum word counts across authors
	WC<-tapply(post.info$wc,post.info$auth,sum)
###define distinct posting days
	DAYS<-tapply(post.info$day,post.info$auth,function(x) length(unique(x)))
###define number of posts
	TOTAL.POSTS<-as.numeric(table(auth))
###define number of replies
	REPLIES<-tapply(post.info$type,post.info$auth,function(x) sum(x=="Reply"))

if(length(AUTH)>6) comments<-c(comments,rep("",length(AUTH)-6))

student.info<-data.frame(AUTH,TOTAL.POSTS,DAYS,WC,REPLIES,comments,row.names=NULL)

#----write uploader file
	write.csv(
		student.info,
		file=paste0(output,".csv")
		)
	}
