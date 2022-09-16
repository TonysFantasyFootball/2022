#install.packages('rsconnect')
#rsconnect::setAccountInfo(name='fantasyfootballviz', token='C0875C07BD680870F1C34A41B468344B', secret='D+HtbX0SEMqLfeVOxPWMq5HgAx2cglHAiy00rfYw')
#setwd("C:/Users/chip/OneDrive/Documents/R/shiny- fantasy football")
#runApp("weeklycharts")
#deployApp("weeklycharts",account='fantasyfootballviz')
#terminateApp("weeklycharts",account='fantasyfootballviz')
#input=list(); input$leagueID=1682008834; input$Year=2022; input$Week=1; input$match=F

library(shiny)
library(fflr)

bad="#d12d21"; good="#14b31c"


ui=fluidPage(
  fluidRow(
     column(2,textInput("leagueID", "ESPN Leaguge ID", value = "1682008834", width="240px"))
    ,column(1,uiOutput("yr"))
    ,column(1,uiOutput("wk"))
    ,column(1,checkboxInput("match","Group by Matchups"))
  )#fluidRow
  ,tabsetPanel(
    tabPanel("Projections & Results", plotOutput("plot1","100%","1000px"))
    ,tabPanel("Possible Lineup Scores", plotOutput("plot2","100%","1000px"))
    ,tabPanel("Time Series", plotOutput("plot3","100%","1000px"))
    ,tabPanel("Resources", div(
              HTML("<br><br><p>How to make your league viewable to the public: <a href='https://support.espn.com/hc/en-us/articles/360000064451-Making-a-Private-League-Viewable-to-the-Public'>https://support.espn.com/hc/en-us/articles/360000064451-Making-a-Private-League-Viewable-to-the-Public</a></p>")
              ,img(src="img.png")
              ,HTML("<br><br><p>Be patient. Give the charts some time to load</p>")
              )
              
              )
  )#tabsetPanel
)#fluidPage



server = function(input, output) {
  
  yw=reactive({c(ffl_year(),ffl_week())})
  
  output$yr=renderUI({numericInput("Year", " \nYear", yw()[1], 2022, yw()[1], 1, "90px")})
  output$wk=renderUI({numericInput("Week", " \nWeek", yw()[2]-1, 1, 18, 1, "60px")})
 
tm=reactive({
  ffl_id(input$leagueID)  #### Check Historic
  lt=league_teams(input$leagueID)
  data.frame(teamId=lt$teamId,name=paste(lt$location,lt$nickname))
})
   
d=reactive({
  ffl_id(input$leagueID)
  w1=team_roster(input$leagueID,input$Year!=ffl_year(),input$Week)
  if(input$Year<ffl_year() & length(w1)>0) w1=w1[as.character(input$Year)]
  w=w1[[1]]; for(i in 2:length(w1)) w=rbind(w,w1[[i]])
  w[w$lineupSlot!="IR",c(3,5,6,7,8,10,12,13)]
})

    output$plot1=renderPlot({
    ds=d()[d()$lineupSlot!="BE",]
    ds=ds[order(ds$actualScore),]
    tot=tapply(ds$actualScore,ds$teamId,sum)
    top=tapply(ds$projectedScore,ds$teamId,sum)
    nx=names(tot)
    ord=order(-tot)
    
    if(input$match){
      ffl_id(input$leagueID,T)
      mp=tidy_schedule(input$leagueID)   ###Check Historic
      mp=mp[mp$matchupPeriodId==input$Week,]
      mp=mp[order(mp$teamId),]
      av=ave(tot,mp$matchupId,FUN=max)
      ord=order(-av,-tot)
    }

    mxy=max(c(tot,top))
    mxx=max(c(ds$actualScore,ds$projectedScore))
    sdx=round(ds$actualScore-ds$projectedScore)
    colx1=colorRampPalette(c(bad,gray(.8)))(1-min(sdx))
    colx2=colorRampPalette(c(gray(.8),good))(max(sdx)+1)
    colx=c(colx1,colx2[-1]); ds$sdx=sdx-min(sdx)+1
    sdy=round(tot-top)
    coly1=colorRampPalette(c(bad,gray(.8)))(1-min(sdy))
    coly2=colorRampPalette(c(gray(.8),good))(max(sdy)+1)
    coly=c(coly1,coly2[-1]); sdy=sdy-min(sdy)+1

    cs=max(ord)/2; if(cs%%2!=0) cs=cs+1
    
    par(mar=rep(0,4),mfrow=c(2,cs))

    
ix=input$match+0
for(i in ord){
  a=ds[ds$teamId==nx[i],]
  plot(c(0,10)*(1-2*ix),c(0,10),type="n",xaxt="n",yaxt="n",bty="n")
  text(0,9.5,tm()$name[i],cex=2,adj=ix,font=2)
  rect(.25*(1-2*ix),0,.75*(1-2*ix),9*tot[i]/mxy,col=coly[sdy[i]],border=NA)
  rect(.25*(1-2*ix),0,.75*(1-2*ix),9*top[i]/mxy,lwd=2)
  
  for(j in 1:nrow(a)){
    o=min(0,9*a$actualScore[j]/mxx)
    rect((1-o)*(1-2*ix),j-1,(9*a$actualScore[j]/mxx+1-o)*(1-2*ix),j-.5,col=colx[a$sdx[j]],border=NA)
    rect((1-o)*(1-2*ix),j-1,(9*a$projectedScore[j]/mxx+1-o)*(1-2*ix),j-.5,lwd=2)}
  text(1-2*ix,1:nrow(a)-.25,paste(a$firstName,a$lastName),adj=ix,cex=2)
if(input$match)  ix=(ix+1)%%2
}

    })
  
  
output$plot2=renderPlot({
  dx=d(); tx=tm()
  pos=as.character(dx$lineupSlot[dx$teamId==dx$teamId[1] & dx$lineupSlot!="BE"])
  b=list(); tx$score=0
  for(i in 1:nrow(tx)){
    dxi=dx[dx$teamId==tx$teamId[i] & dx$projectedScore!=0,]
    
    bp=list()
    for(j in 1:length(pos)){
      bp[[j]]=dxi$playerId[dxi$position==pos[j]]
      if(pos[j]=="FLEX") bp[[j]]=dxi$playerId[dxi$position%in%c("RB","WR","TE")]
    }
    g=expand.grid(bp)
    gm=t(apply(g,1,sort))
    gs=unique(gm)
    for(k in 1:nrow(dxi)) gs[gs==dxi$playerId[k]]=dxi$actualScore[k]
    x=rowSums(gs)
    b[[i]]=table(factor(round(x),levels=seq(round(min(x)),round(max(x)),by=1)))
    tx$score[i]=sum(dxi$actualScore[dxi$lineupSlot!="BE"])
  }
  
  nt=nrow(tx)
  mx1=c(); my1=c(); for(i in 1:nt){
    y=as.numeric(names(b[[i]]))
    my1[i]=max(b[[i]]); mx1[i]=max(y)}
  mx=max(mx1); my=max(my1)
  ord=order(-mx1)
  
  if(input$match){
    ffl_id(input$leagueID,T)
    mm=tidy_schedule(input$leagueID)   ###Check Historic
    mm=mm[mm$matchupPeriodId==input$Week,]
    mm=mm[order(mm$teamId),]
    am=ave(mx1,mm$matchupId,FUN=max)
    ord=order(-am,-mx1)
  }
  
  tx=tx[ord,]
  b=b[ord]

  par(mar=rep(0,4))
  plot(c(-4,10),c(.5,nt+.5),type="n",xaxt="n",yaxt="n",bty="n")
  text(rep(-.1,nt),nt:1,tx$name,cex=3,font=2,adj=1)
  for(i in 1:nt){
    z=b[[i]]; nz=as.numeric(names(z))
    lines(c(0,10*(min(nz)-1)/mx),rep(nt+1-i,2))
    for(j in 1:length(z)) lines(10*rep(nz[j],2)/mx,nt+1-i+c(-1,1)*z[j]/my/2,lwd=7,col=ifelse(nz[j]>tx$score[i],bad,good))
    if(input$match & i%%2==1) lines(rep(0,2),c(nt+1-i,nt-i))
  }  
  
 
output$plot3=renderPlot({
  ffl_id(input$leagueID,T)
  hx=data.frame(teamId=NA,wk=NA,actual=NA,proj=NA,best=NA)[-1,]; hn=names(hx)

  for(k in 1:input$Week){
    h=best_roster(input$leagueID,scoringPeriodId=k)
    for(i in 1:length(h)){
      hx=rbind(hx,c(h[[i]]$teamId[1],k
                    ,sum(h[[i]]$actualScore[!h[[i]]$actualSlot%in%c("BE","IR")])
                    ,sum(h[[i]]$projectedScore[!h[[i]]$actualSlot%in%c("BE","IR")])
                    ,sum(h[[i]]$actualScore[!h[[i]]$lineupSlot%in%c("BE","IR")])
                    ))

    }
  }
  names(hx)=hn

  mh=tidy_schedule(input$leagueID)
  mh=mh[mh$matchupPeriodId<=input$Week,]

  hx$win=0; for(i in 1:nrow(hx)){
    h1=as.numeric(mh[mh$teamId==hx[i,1] & mh$matchupPeriodId==hx[i,2],3])
    h2=as.numeric(mh[mh$matchupId==h1 & mh$teamId!=hx[i,1],4])
    h3=hx[hx[,1]==h2 & hx[,2]==hx[i,2],3]
    hx$win[i]=hx[i,3]>h3+0
  }

  ord=order(-tapply(hx$win,hx$teamId,sum))
  mxh=max(hx)

  par(mar=c(1,1,3,1),mfrow=c(ceiling(length(ord)/3),3))
  for(i in ord){
    hxi=hx[hx$teamId==i,]
    plot(c(1,input$Week+1),c(10,mxh),type="n",bty="l",yaxt="n",xaxt="n",xlab="",ylab="", main=tm()$name[i],cex.main=3)
    lines(hxi$wk,hxi$actual)
    points(hxi$wk,hxi$best,pch=15,col=gray(.8),cex=5)
    points(hxi$wk,hxi$actual,pch=15,col=ifelse(hxi$win,good,bad),cex=5)
    points(hxi$wk,hxi$proj,pch=0,lwd=2,cex=5)


  }



})
    
  
  
  #output$plot1=renderPlot({plot(1:10)})
  #output$plot2=renderPlot({plot(10:1)})
  #output$plot3=renderPlot({plot(rep(5,10))})
   
})
  
  
  
}

shinyApp(ui,server)
