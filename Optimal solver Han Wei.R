aps<-list()
mindex<-0
fillaps<-function(cl,index,n){
  for(i in 0:n){
    cl[index]=i
    if(index==6){
      mindex<<-1+mindex
      aps[[mindex]]<<-cl
    }else{
      fillaps(cl,(index+1),(n-i))
    }
  }
}
fillaps(c(0,0,0,0,0,0),1,5) # get all possible strategy

apss<-c()
for(i in 1:mindex){
  apss[i]<-toString(aps[[i]])
}

generateas<-function(num){
  dls<-list()
  dindex<<-0
  filldice<-function(cl,index,n){
    if(index==6){
      cl[index]=n
      dindex<<-1+dindex
      dls[[dindex]]<<-cl
    }else{
      for(i in 0:n){
        cl[index]=i
        filldice(cl,(index+1),(n-i))
      }
    }
  }
  filldice(c(0,0,0,0,0,0),1,num)
  return(dls)
}

pps<-generateas(5) # get alll possible dice combination

ppss<-c()
for(i in 1:252){
  ppss[i]<-toString(pps[[i]])
}

library(hashmap)
apsbook<-hashmap(apss,1:mindex) #apsbook[[toString(c(0,1,0,3,0,1))]] 
ppsbook<-hashmap(ppss,1:252) #ppsbook[[toString(c(0,1,0,3,0,1))]]

apsmx<-matrix(0,length(aps),length(pps)) # transition matrix from strategy to dice combination
rownames(apsmx)<-apss
colnames(apsmx)<-ppss

generateps<-function(n){
  rls<-list()
  rindex<<-0
  filldice<-function(cl,num){
    if(num==0){
      rindex<<-1+rindex
      rls[[rindex]]<<-cl
    }else{
      for(i in 1:6){
        al=cl
        al[i]=al[i]+1
        filldice(al,(num-1))
      }
    }
  }
  filldice(c(0,0,0,0,0,0),n)
  return(rls)
}

addtoApsmx<-function(i){
  currentstate<-aps[[i]]
  d<-5-sum(currentstate)
  ls<-generateps(d)
  for(j in 1:length(ls)){
    endstate<-ls[[j]]+currentstate
    jindex<-ppsbook[[toString(endstate)]]
    apsmx[i,jindex]<<-(apsmx[i,jindex])+1.0
  }
}

fixApsmx<-function(i){
  currentstate<-aps[[i]]
  d<-5-sum(currentstate)
  dm<-6.0 ^ d
  for(j in 1:length(apsmx[i,])){
    apsmx[i,j]<<-(apsmx[i,j])/dm
  }
}

for(i in 1:mindex){
  addtoApsmx(i)
  fixApsmx(i)
}



#Finished dice set up
Boxnumber<-13

generateBoxes<-function(n){ #n total box, mmmax box openning
  rls<-list()
  rindex<<-0
  fillBoxes<-function(cl,num,mx){
    if(mx==0){
      rindex<<- 1+rindex
      rls[[rindex]]<<-cl
    }else{
      if(num>(n)){
        rindex<<- rindex
      }else{
        for(i in 0:1){
          al=cl
          al[num]=i
          fillBoxes(al,(num+1),(mx-i))
        }
      }
    }
  }
  initbox=numeric(n)
  for(k in 1:n){
      fillBoxes(initbox,1,k)
  }
  return(rls)
}

boxes=generateBoxes(Boxnumber)

boxlist<-c()
for(i in 1:rindex){
  boxlist[i]<-toString(boxes[[i]])
}
boxbook<-hashmap(boxlist,1:rindex) #boxbook[[toString(c(1,1,1,1))]]

#1 be Aces,2 be Two, 3 be Threes, 4 be Fours, 5 be Fives, 6 be Sixes
#7 be full house, 8 be small straight, 9 be large straight, 10 be Yahtzee
#11  be Three of A kind, 12 be Four of A kind, 13 be chance

boxesname<-function(k){
  if(k==1){
    return('Aces')
  }else if(k==2){
    return('Twos')
  }else if(k==3){
    return('Threes')
  }else if(k==4){
    return('Fours')
  }else if(k==5){
    return('Fives')
  }else if(k==6){
    return('Sixs')
  }else if(k==7){
    return('Full House')
  }else if(k==8){
    return('Small Straight')
  }else if(k==9){
    return('Large Straight')
  }else if(k==10){
    return('Yahtzee')
  }else if(k==11){
    return('Three Of A kind')
  }else if(k==12){
    return('Four Of A kind')
  }else{
  return('Chance')
  }
}

scoreBasis<-function(k,ls){
  if(k<=6){
    return(k*ls[k])
  }else if(k==7){
    if(max(ls)==3&& (2 %in% ls)){
      return(25)
    }
    return(0)
  }else if(k==8){
    if(ls[3]>=1&&ls[4]>=1){
      if(ls[2]>=1){
        if(ls[1]+ls[5]>=1){
          return(30)
        }
      }else if(ls[5]>=1&&ls[6]>=1){
        return(30)
      }
    }
    return(0)
  }else if(k==9){
    if(ls[1]+ls[6]==1){
      if(ls[2]==1&&ls[3]==1&&ls[4]==1&&ls[5]==1){
        return(40)
      }
    }
    return(0)
  }else if(k==10){
    if(5 %in% ls){
      return(50)
    }
    return(0)
  }else if(k==11){
    s=0;
    if(max(ls)>=3){
      for(j in 1:6){
        s=s+j*ls[j]
      }
    }
    return(s)
  }else if(k==12){
    s=0;
    if(max(ls)>=4){
      for(j in 1:6){
        s=s+j*ls[j]
      }
    }
    return(s)
  }else{
    s=0;
    for(j in 1:6){
      s=s+j*ls[j]
    }
    return(s)
  }
}

calEscoreBasis<-function(boxls){
  sc=matrix(0,length(pps),2)
  if(sum(boxls)==1){
    k = 1
    while(k<=length(boxls)){
      if(boxls[k]==1){
        break()
      }
      k = k+1
    }
    for(i in 1:length(pps)){
      ls=pps[[i]]
      sc[i,1]=scoreBasis(k,ls)
      sc[i,2]=k
    }
    return(sc);
  }
  
  k = numeric(Boxnumber)
  for(i in 1:Boxnumber){
    if(boxls[i]==1){
      temp = boxls
      temp[i]=0
      k[i]<-boxstrategy[[boxbook[[toString(temp)]]]][[4]]
    }
  }
  
  for(i in 1:dindex){
    ls=pps[[i]]
    choicebox<-1
    maxexpscore<-0
    for(j in 1:Boxnumber){
      if(k[j]!=0){
        temp<-scoreBasis(j,ls)+k[j]
        if(temp>maxexpscore){
          choicebox<-j
          maxexpscore=temp
        }
      }
    }
    sc[i,1]=maxexpscore
    sc[i,2]=choicebox
  }
  return(sc)
}

getaps<-function(mpps){
  return(apsmx %*% mpps[,1])
}

getpps<-function(maps){
  res=matrix(0,length(pps),2)
  for(i in 1:dindex){
    ls=pps[[i]]
    expv=0;
    ms=0;
    for(j in 1:mindex){
      if(all(ls>=aps[[j]])){
        if(maps[j]>expv){
          expv=maps[j]
          ms=j
        }
      }
    }
    res[i,1]=expv
    res[i,2]=ms
  }
  return(res)
}

boxstrategy<-list()
for(i in 1:length(boxes)){
  print(i)
  for(j in 1:Boxnumber){
    if(sum(boxes[[i]])==j){
      # start calculating strategy
      pps1=calEscoreBasis(boxes[[i]])
      rownames(pps1)<-ppss
      aps1=getaps(pps1)
      rownames(aps1)<-apss
      pps2=getpps(aps1)
      rownames(pps2)<-ppss
      aps2=getaps(pps2)
      rownames(aps1)<-apss
      pps3=getpps(aps2)
      rownames(pps3)<-ppss
      aps3=getaps(pps3)
      exp3=aps3[1]
      currentstrategy<-list()
      currentstrategy[[1]]<-pps1
      currentstrategy[[2]]<-pps2
      currentstrategy[[3]]<-pps3
      currentstrategy[[4]]<-exp3
      boxstrategy[[i]]<-currentstrategy
    }
  }
}
#wait until 8191 is pop up


#b : boxes opening, eg c(1,0,0,0,0,0,0,0,0,0,0,0,0) means only Aces opening
#d : dice, eg c(5,0,0,0,0,0) means five dice with Aces
#r : number of reroll chance, 0 means no reroll chance remaining

decision<-function(b,d,r){
  idb<-boxbook[[toString(b)]]
  idd<-ppsbook[[toString(d)]]
  
  k<- boxstrategy[[idb]][[r+1]][idd,2]
  if(r!=0){
    return(aps[[k]])
  }else{
    return(boxesname(k))
  }
}

library(sets)
checkbad<-function(){
  #for(i in 1:length(boxes)){
  for(i in 1:13){
    s1<-boxstrategy[[i]][[2]][,2]
    s2<-boxstrategy[[i]][[3]][,2]
    s1<-as.set(s1)
    s2<-as.set(s2)
    if(!set_is_subset(s2, s1)){
      print(i)
    }
  }
  return("Done")
}
#checkbad()

