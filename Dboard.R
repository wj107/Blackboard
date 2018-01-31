##########take dboard data, extract data on student comments

#####assume dat<-raw clipboard data

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

student.info<-data.frame(A,POSTS,DAYS,WC,row.names=NULL)

dbg<-function(C=101,W=1){

#########GRAPH of student wc's
ggplot(student.info,aes(x=WC,y=1))+
	ggtitle(paste0("KCC",C,": Week #",W),subtitle="Discussion Board stats")+
	geom_point(size=10,alpha=1/5)+
	ylab("")+xlab("Word Count in Discussion Posts")+
	scale_x_continuous(breaks=seq(round(min(WC),-2),round(max(WC),-2),100))+
	scale_y_continuous(breaks=1,labels=NULL)+
	labs(caption="Each circle represents one student")->>g1

#########GRAPH of student post's
ggplot(student.info,aes(x=POSTS,y=1))+
	ggtitle(paste0("KCC",C,": Week #",W),subtitle="Discussion Board stats")+
	geom_point(size=10,position="stack")+
	ylab("")+xlab("Number of Discussion Posts")+
	scale_x_continuous(breaks=seq(1,max(POSTS),1),minor_breaks=NULL)+
	scale_y_continuous(breaks=seq(1,10,1),minor_breaks=NULL,labels=NULL)+
	labs(caption="Each circle represents one student")->>g2
	
#########GRAPH of days posted
ggplot(student.info,aes(x=DAYS,y=1))+
	ggtitle(paste0("KCC",C,": Week #",W),subtitle="Discussion Board stats")+	
	geom_point(size=10,position="stack")+
	ylab("")+xlab("How many different days did you post?")+
	scale_x_continuous(breaks=seq(1,7,1),minor_breaks=NULL)+
	scale_y_continuous(breaks=seq(1,10,1),minor_breaks=NULL,labels=NULL)+
	labs(caption="Each circle represents one student")->>g3

}

#####a good saving function.  (set dimensions ahead of time???))
dbg.s<-function(x="graph") ggsave(file.path(wP,paste0(x,".png")))
	
#####write student info to csv:
##file.create(file.path(wP,"105wk2.csv"))
##write.csv(student.info,file.path(wP,"105wk2.csv"))



