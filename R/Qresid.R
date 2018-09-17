Qresid <-
function(y,X,numperms=1e4,thresh=10){
 #set.seed(myseed)
  n=length(y)
  Xresid=fastresid(X,y)
  Aobs=(t(Xresid)%*%Xresid)^2
  myobs=as.vector(t(y)%*%Aobs%*%y)
  runningsum=0
  trigger=F
  i=0
  while (trigger==F){
   i=i+1
   ynew=sample(y)
   Xresid=fastresid(X,ynew)
   Acurrent=(t(Xresid)%*%Xresid)^2
   qcurrent=as.vector(t(ynew)%*%Acurrent%*%ynew)
   runningsum=runningsum+(qcurrent>=myobs)
   pcurrent=runningsum/i
   pcurrent1=min(pcurrent+1e-10,.99)
   ratio=(pcurrent1)/sqrt(pcurrent1*(1-pcurrent1)/i)
   trigger=((ratio>thresh)|(i==numperms))
   }
   myp=pcurrent
   return(list(myp=myp,i=i))
  }
