##########take reflections, extract data on student comments

#####assume dat<-raw clipboard data

dat<-trimws(dat)

dates<-which(!is.na(strptime(dat,"%H:%M")))
reply<-which(dat=="Reply")

replies<-which(dat=="")

if(length(dates)!=length(reply)) {
	ndates<-unlist(lapply(replies,function(x) which(x-dates<0)[1]))
	dat[(dates[ndates])]->dat[replies]
	dat[(dates[ndates])]<-"Reply"
	
	dates<-which(!is.na(strptime(dat,"%H:%M")))

	bad<-c("Marked as resolved","Re-opened")
	bad<-which(dat[dates-1] %in% bad)
	dates<-dates[-bad]
	}

length(dates)->N

auth<-dates-1
auth<-dat[auth]

post<-dates+1
post.end<-reply-1

posts<-lapply(1:N,function(x) c(post[x],post.end[x]))
posts<-lapply(posts, function(x) paste(dat[x[1]:x[2]],collapse=" "))

wc<-lapply(posts, function(x) length(gregexpr("\\W+",x)[[1]]))
wc<-unlist(wc)

when<-dat[dates]
when<-strsplit(when,"M ")
day<-unlist(lapply(when,function(x) x[2]))
#time<-unlist(lapply(whenn, function(x) x[1]))


post.info<-data.frame(auth,day,wc)

###########summarized student info on d board posts

A<-as.character(levels(post.info$auth))
WC<-tapply(post.info$wc,post.info$auth,sum)

DAYS<-tapply(post.info$day,post.info$auth,function(x) length(unique(x)))
POSTS<-as.numeric(table(auth))

student.info<-data.frame(A,POSTS,DAYS,WC,row.names=NULL)


#write.csv(student.info,file.path(wP,"test.csv"))

