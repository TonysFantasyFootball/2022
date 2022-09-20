#setwd("C:/Users/chip/OneDrive/Documents/R/shiny- fantasy football")
#input=list(); input$leagueID=1682008834; input$Year=2022; input$Week=1; input$match=T
#input=list(); input$leagueID=252353; input$Year=2020; input$Week=7; input$match=F


library(shiny)
library(shinycssloaders)
library(fflr)

yr=ffl_year()
wk=ffl_week()
bad="#d12d21"; good="#14b31c"


ui=fluidPage(     tags$head(tags$style(type = "text/css", "a{color: #808080;}")),
  fluidRow(
     column(2,textInput("leagueID", "ESPN League ID", value = "1682008834", width="240px"))
    ,column(1,numericInput("Year", " Year", yr, 2004, yr, 1, "90px"))
    ,column(1,numericInput("Week", " Week", wk-1, 1, 18, 1, "60px"))
    ,column(1,checkboxInput("match","Group by Matchups"))
  )#fluidRow
  ,tabsetPanel(
     tabPanel("Projections & Results", withSpinner(plotOutput("plot1","100%","1000px"),type=1,color=gray(.7),size=2))
    ,tabPanel("Possible Lineup Scores", withSpinner(plotOutput("plot2","100%","1000px"),type=6,color=gray(.5),size=2))
    ,tabPanel("Time Series", withSpinner(plotOutput("plot3","100%","1000px"),type=7,color=gray(.3),size=2))
    ,tabPanel("Resources", div(
              HTML("<br><br><p>How to make your league viewable to the public: <a href='https://support.espn.com/hc/en-us/articles/360000064451-Making-a-Private-League-Viewable-to-the-Public'>https://support.espn.com/hc/en-us/articles/360000064451-Making-a-Private-League-Viewable-to-the-Public</a></p>")
              ,img(src="img.png")
              #,HTML("<br><br><p>Be patient. Give the charts some time to load</p>")
              )
              
              )
  )#tabsetPanel
)#fluidPage



server = function(input, output, session) {
  

  observe({
    query=parseQueryString(session$clientData$url_search)
    if (!is.null(query[['leagueID']])) {
      updateTextInput(session, "leagueID", value = query[['leagueID']])
    }})
  
 
tm=reactive({
  ffl_id(input$leagueID); histo=input$Year!=yr
  lt=league_teams(input$leagueID,histo); if(histo) lt=lt[[as.character(input$Year)]]
  data.frame(teamId=lt$teamId,name=paste(lt$location,lt$nickname))
})
   
d=reactive({
  ffl_id(input$leagueID); histo=input$Year!=yr
  w1=team_roster(input$leagueID,histo,input$Week); if(histo) w1=w1[[as.character(input$Year)]]
  w=w1[[1]]; for(i in 2:length(w1)) w=rbind(w,w1[[i]])
  w[w$lineupSlot!="IR" & !is.na(w$actualScore),c(3,5,6,7,8,10,12,13)]
})

m=reactive({
  ffl_id(input$leagueID,T); histo=input$Year!=yr
  mp=tidy_schedule(input$leagueID,histo)
  if(histo){ mp=do.call("rbind",mp); mp=mp[mp$seasonId==input$Year,]}
  mp=mp[mp$matchupPeriodId==input$Week,]
  mp[order(mp$teamId),]
})

    output$plot1=renderPlot({
    ds=d()[d()$lineupSlot!="BE",]
    ds=ds[order(ds$actualScore),]
    tot=tapply(ds$actualScore,ds$teamId,sum)
    top=tapply(ds$projectedScore,ds$teamId,sum)
    nx=names(tot)
    ord1=order(-tot)
    
    if(input$match){
      av=ave(tot,m()$matchupId,FUN=max)
      ord1=order(-av,-tot)
    }

    mxy=max(tot,top)
    mxx=max(ds$actualScore,ds$projectedScore)
    sdx=round(ds$actualScore-ds$projectedScore)
    colx1=colorRampPalette(c(bad,gray(.8)))(1-min(sdx))
    colx2=colorRampPalette(c(gray(.8),good))(max(sdx)+1)
    colx=c(colx1,colx2[-1]); ds$sdx=sdx-min(sdx)+1
    sdy=round(tot-top)
    coly1=colorRampPalette(c(bad,gray(.8)))(1-min(sdy))
    coly2=colorRampPalette(c(gray(.8),good))(max(sdy)+1)
    coly=c(coly1,coly2[-1]); sdy=sdy-min(sdy)+1

    cs=max(ord1)/2; if(cs%%2!=0) cs=cs+1
    
    par(mar=rep(0,4),mfrow=c(2,cs))

    
ix=input$match+0
for(i in ord1){
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
    rmx=which(sapply(bp,length)==0); if(length(rmx)>0) bp=bp[-rmx]
    g=expand.grid(bp)
    gm=t(apply(g,1,sort))
    gs=unique(gm)
    for(k in 1:nrow(dxi)) gs[gs==dxi$playerId[k]]=dxi$actualScore[k]
    x=round(rowSums(gs))
    b[[i]]=table(factor(x,levels=seq(min(x),max(x),1)))
    tx$score[i]=sum(dxi$actualScore[dxi$lineupSlot!="BE"])
  }
  
  nt=nrow(tx)
  mx1=c(); my1=c(); for(i in 1:nt){
    y=as.numeric(names(b[[i]]))
    my1[i]=max(b[[i]]); mx1[i]=max(y)}
  mx=max(mx1); my=max(my1)
  ord2=order(-mx1)
  
  if(input$match){
    am=ave(mx1,m()$matchupId,FUN=max)
    ord2=order(-am,-mx1)
  }
  
  tx=tx[ord2,]
  b=b[ord2]

  par(mar=rep(0,4))
  plot(c(-4,10),c(.5,nt+.5),type="n",xaxt="n",yaxt="n",bty="n")
  text(rep(-.1,nt),nt:1,tx$name,cex=3,font=2,adj=1)
  for(i in 1:nt){
    z=b[[i]]; nz=as.numeric(names(z))
    lines(c(0,10*(min(nz)-1)/mx),rep(nt+1-i,2))
    points(10*nz[z==0]/mx,rep(nt+1.0-i,sum(z==0)),pch="-",col=ifelse(nz[z==0]>tx$score[i],bad,good))
    for(j in which(z>0)) lines(10*rep(nz[j],2)/mx,nt+1-i+c(-1,1)*z[j]/my/2,lwd=7,col=ifelse(nz[j]>tx$score[i],bad,good))
    if(input$match & i%%2==1) lines(rep(0,2),c(nt+1-i,nt-i))
  }
  

})
  
 
output$plot3=renderPlot({
  ffl_id(input$leagueID,T); histo=input$Year!=yr

  h=tidy_scores(input$leagueID,histo,T); if(histo){ h=do.call("rbind",h); h=h[h$seasonId==input$Year, ] }
  h=h[h$matchupPeriodId<=input$Week,]
  
  if(input$leagueID==1682008834 & input$Year==2022 & yr==2022){
    hc1=team_roster(input$leagueID,F,1)
    hc2=c(); for(i in 1:length(hc1)) hc2[i]=sum(hc1[[i]]$actualScore[!hc1[[i]]$lineupSlot%in%c("BE","IR")])
    h$totalPoints[h$matchupPeriodId==1]=hc2
  }
  
  hx=data.frame(teamId=h$teamId, wk=h$matchupPeriodId, actual=h$totalPoints)
  

  hx$opp=0; for(i in 1:nrow(hx)){
    h1=h$matchupId[h$teamId==hx$teamId[i] & h$matchupPeriodId==hx$wk[i]]
    hx$opp[i]=h$totalPoints[h$matchupId==h1 & h$teamId!=hx$teamId[i]]
  }
  hx$win=hx$actual>hx$opp+0

  ord3=order(-tapply(hx$win,hx$teamId,sum),-tapply(hx$actual,hx$teamId,sum))
  mxh=max(hx$actual,hx$opp)
  mnh=min(hx$actual,hx$opp)
  
  

  par(mar=c(1,1,3,1),mfrow=c(ceiling(length(ord3)/3),3))
  for(i in ord3){
    hxi=hx[hx$teamId==tm()$teamId[i],]
    plot(c(1,input$Week+1),c(mnh-.1*(mxh-mnh),mxh+.1*(mxh-mnh)),type="n",bty="l",yaxt="n",xaxt="n",xlab="",ylab="", main=tm()$name[i],cex.main=3)
    lines(hxi$wk,hxi$actual)
    if(input$match) points(hxi$wk,hxi$opp,pch=15,col=gray(.8),cex=5)
    points(hxi$wk,hxi$actual,pch=15,col=ifelse(hxi$win,good,bad),cex=5)
  }



})
    
  
  
  #output$plot1=renderPlot({plot(1:10)})
  #output$plot2=renderPlot({plot(10:1)})
  #output$plot3=renderPlot({plot(rep(5,10))})
  
  
  
  
}

shinyApp(ui,server)
