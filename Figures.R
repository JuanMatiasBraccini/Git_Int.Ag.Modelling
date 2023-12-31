# ---------- Header-------------------------------------------
#note: code for creating relevant figures
Report.year=2023  #update as appropriate
mypath='C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias/External collaborations/Ag/'#modify path as appropriate
path=paste0(mypath,Report.year)
if(!dir.exists(path))dir.create(path)

in.path=paste(path,'data',sep='/')
if(!dir.exists(in.path))dir.create(in.path)
out.path=paste(path,'outputs',sep='/')
if(!dir.exists(out.path))dir.create(out.path)

fn.in=function(x) paste(in.path,x,sep='/')
fn.out=function(x) paste(out.path,x,sep='/')

# ---------- Load libraries-------------------------------------------
library(tidyverse)
library(extrafont)
library(Hmisc)
library(ggpubr)
library(ggrepel)
library(dplyr, warn.conflicts = FALSE)
options(dplyr.summarise.inform = FALSE)

# install.packages("remotes")
# remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)

# ---------- Load data-------------------------------------------
Dat=read.csv(fn.in('IntAg Modelling.csv'))
gvp=read.csv(fn.in('gvp.csv'))
deltas=read.csv(fn.in('deltas.csv'))
Color.palette=read.csv(paste(mypath,'Color.palette.csv',sep='/'))
  
# ---------- Manipulate data-------------------------------------------
#fix some colnames
id=grep('ï..',names(Dat))
if(length(id)>0) names(Dat)[id]=str_remove(names(Dat)[id], 'ï..')

#create vectors of interest
vec_industry=sort(unique(Dat$industry))
vec_scenario =sort(unique(Dat$scenario))
vec_intag_level1=sort(unique(Dat$intag_level1))
vec_intag_level2=sort(unique(Dat$intag_level2))
vec_ni_level1=sort(unique(Dat$ni_level1))
vec_ni_level2=sort(unique(Dat$ni_level2))
vec_scope=sort(unique(Dat$scope))
vec_gas=sort(unique(Dat$gas))
vec_year=sort(unique(Dat$year))


# ---------- Summary stats of metadata -------------------------------------------
meta.data=list(vec_industry,vec_scenario,vec_intag_level1,vec_intag_level2,
               vec_ni_level1,vec_ni_level2,vec_scope,vec_gas)                           
names(meta.data)=c('industry','scenario','intag_level1','intag_level2',
                   'ni_level1','ni_level2','scope','gas')
do.summary=FALSE
if(do.summary)
{
  for(m in 1:length(meta.data))
  {
    d=Dat%>%
      dplyr::select(year,names(meta.data)[m])%>%
      group_by_at(vars(year,names(meta.data)[m]))%>%  
      tally()%>%
      spread(year,n)
    write.csv(d,fn.out(paste0('Meta.data_summary_',names(meta.data)[m],'.csv')),row.names = F)
    
  }
}

# ---------- Create display theme -------------------------------------------
font_import(pattern="arial.ttf",prompt=FALSE)
loadfonts(device = "postscript")
mytheme<-function(Ttl.siz=18,Sbt.siz=16,str.siz=12,strx.siz=12,cap.siz=10,
                   lgT.siz=14,leg.siz=12,axs.t.siz=10,axs.T.siz=14)
{
  #font<-windowsFonts("Arial" = windowsFont("Arial"))
  #font="Arial"  #"TT Courier New"  "TT Arial"  "TT Times New Roman"
  theme_bw()%+replace% 
    theme(
      #panel
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black", fill=NA, size=1.15),
      
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.line = element_line(colour = "black"),
      #axis.ticks = element_line(),          #strip axis ticks
      
      # strip background
      strip.background = element_rect(
        fill = "grey90",
        colour = "grey90"),
      
      #text elements
      #title
      plot.title = element_text(             
        # family = font,                           
        size = Ttl.siz,                         
        face = 'bold',                           #bold typeface
        hjust = 0,                               #left align
        vjust = 2),                              #raise slightly
      
      #subtitle
      plot.subtitle = element_text(          
        # family = font,                           
        size = Sbt.siz,
        hjust = 0,                               #left align
        vjust = 2),                         
      
      #strip legend
      strip.text = element_text(
        # family = font,
        size = str.siz),
      strip.text.x = element_text(
        # family = font,
        size = strx.siz,
        margin = margin(.1,0,.1,0, "cm")),
      
      
      #caption
      plot.caption = element_text(          
        # family = font,                           
        size = cap.siz,                          
        hjust = 1),                             #right align
      
      #legend
      legend.title=element_text(
        # family = font,
        size=lgT.siz),
      legend.text=element_text(
        # family = font,
        size=leg.siz),
      
      #axis titles
      axis.title = element_text(             
        # family = font,                          
        size = axs.T.siz),                     
      
      #axis text
      axis.text = element_text(              
        # family = font,                          
        size = axs.t.siz)                       
    )
}


# ---------- Share of estimated emissions -------------------------------------------
Variables=c('ni_level2','scope')
Variables.size=c(3,4) #size of inset percentage
Variables.size1=c(8,10) #size of legend labels
Fig.title=c('Source and Industry_','Scope and Industry_')
Scenarios=vec_scenario
Years=c(2035)
share.estim.em=function(data,Variable,yr,Scen,Size,Size1,drop.level=FALSE) 
{
  d=data%>%
    filter(year%in%yr & scenario==Scen)
  if(!isFALSE(drop.level))
  {
    d=d%>%filter(!intag_level2%in%drop.level)
  }
   
  d=d%>%
    group_by_at(vars(industry,Variable))%>% 
    summarise(Sum = sum(ghg)) %>% 
    mutate(perc =Sum/sum(Sum))%>%
    filter(round(100*perc)>0)
  if(Variable=="scope")d$scope=paste('Scope',d$scope)
  p=d%>%
    mutate_at(vars(Variable), factor)%>%
    ggplot(aes_string(x='industry',y='perc', fill=Variable))+
    geom_bar(width=.8, stat="identity")+
    mytheme(leg.siz=Size1,axs.t.siz=15)+xlab('')+ylab('')+
    theme(legend.title = element_blank(),
          legend.position = 'top',
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
    geom_text(aes(x=industry,y=perc,label=paste0(round(100*perc),'%')),
              stat='identity', position=position_stack(0.5),size=Size)
    my.cols=Color.palette%>%
              filter(Legend.Entry%in%unique(d%>%pull((!!sym(Variable)))))%>%
              mutate(Colour=rgb(red, green, blue, maxColorValue = 255))
    my.cols1=my.cols$Colour
    names(my.cols1)=my.cols$Legend.Entry
    p=p+scale_fill_manual(values=my.cols1)
    
    
  #Display legend levels >=5%
  d1=d%>%filter(perc>=0.05)
  g <- ggplot_build(p)
  VALs=unique(g$data[[1]]["fill"]$fill)
  names(VALs)=sort(unique(d%>%pull(Variable)))
  p=p+scale_fill_manual(values=VALs,
                      breaks=sort(unique(d1%>%pull(Variable))))
  
  return(p)
}
for(i in 1:length(Variables))
{
  for(y in 1:length(Years))
  {
    for(s in 1:length(Scenarios))
    {
      share.estim.em(data=Dat,
                     Variable=Variables[i],
                     yr=Years[y],
                     Scen=Scenarios[s],
                     Size=Variables.size[i],
                     Size1=Variables.size1[i],
                     drop.level='Purchased feed (WA)')
      
      figure.title=Fig.title[i]
      xtnsion=paste(Variables[i],Years[y],Scenarios[s],sep='.')
      ggsave(fn.out(paste0('Share of Estimated Emissions by ',figure.title,xtnsion,'.tiff')), 
             width = 10,height = 6,compression = "lzw")
    }
  }
}

# ---------- Estimated Total Emissions by Industry -------------------------------------------
Variables=c('ni_level2','scope','ni_level2.horizontal')
Variables.size1=c(10,12,10) #size of legend labels
Fig.title=c('Industry_','Scope_','Industry_')
Scenarios=vec_scenario
Years=list(c(2020,2035),vec_year)

estim.tot.em=function(data,Variable,yr,Scen,Size1,drop.level=FALSE,horiz=FALSE)
{
  d=data%>%
    filter(year%in%yr & scenario==Scen)
  if(!isFALSE(drop.level))
  {
    d=d%>%filter(!intag_level2%in%drop.level)
  }
  d=d%>%
    group_by_at(vars(year,Variable))%>% 
    summarise(Sum = sum(ghg)/1e6)%>%
    filter(Sum>0)
  if(Variable=="scope")
  {
    d1=data%>%
      filter(year%in%yr & scenario==Scen)%>%
      filter(!intag_level2%in%c('Purchased feed (WA)','Purchased feed (imported)'))%>%
      group_by_at(vars(year))%>% 
      summarise(Total = sum(ghg)/1e6)%>%
      filter(Total>0)
    d=d%>%
      mutate(scope=paste('Scope',scope))
    
    p=d%>%
      left_join(d1,by='year')%>%
      mutate(year=as.character(year))%>%
      mutate_at(vars(Variable), factor)%>%
      ggplot(aes_string(x='year',y='Sum', fill=Variable))+
      geom_bar(stat="identity")+
      mytheme(leg.siz=Size1,axs.t.siz=15)+xlab('')+ylab('MtCo2e')+
      theme(legend.title = element_blank(),
            legend.position = 'top',
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      geom_path(inherit.aes = FALSE,aes(x=year,y=Total,color='Total Emissions'),group=1,size=2)+
      geom_point(inherit.aes = FALSE,aes(x=year,y=Total,color='Total Emissions'),size=3,stroke=2,
                 fill='steelblue', shape=21)+
      scale_color_manual(name="",values=c('Total Emissions'='black'))   
  }else
  {
    p=d%>%
      mutate(year=as.character(year))%>%
      mutate_at(vars(Variable), factor)%>%
      ggplot(aes_string(x='year',y='Sum', fill=Variable))+
      geom_bar(stat="identity")+
      mytheme(leg.siz=Size1,axs.t.siz=15)+xlab('')+ylab('MtCo2e')+
      theme(legend.title = element_blank(),
            legend.position = 'top',
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
  }
  
  my.cols=Color.palette%>%
    filter(Legend.Entry%in%unique(d%>%pull((!!sym(Variable)))))%>%
    mutate(Colour=rgb(red, green, blue, maxColorValue = 255))
  my.cols1=my.cols$Colour
  names(my.cols1)=my.cols$Legend.Entry
  
  p=p+scale_fill_manual(values=my.cols1)
  
  if(horiz) p=p+coord_flip()
  return(p)
  
}
for(i in 1:length(Variables))
{
  for(y in 1:length(Years))
  {
    for(s in 1:length(Scenarios))
    {
      Var=Variables[i]
      HOR=FALSE
      if(grepl('horizontal',Var))
      {
        HOR=TRUE
        Var=gsub("\\..*", "", Var)
      }
        
      estim.tot.em(data=Dat,
                   Variable=Var,
                   yr=Years[[y]],
                   Scen=Scenarios[s],
                   Size1=Variables.size1[i],
                   drop.level='Purchased feed (WA)',
                   horiz=HOR)
      
      figure.title=Fig.title[i]
      yrs=Years[[y]]
      dumi=ifelse(length(yrs)==2,'and','to')
      yrs=paste(yrs[1],dumi,yrs[length(yrs)])
      xtnsion=paste(Variables[i],yrs,Scenarios[s],sep='.')
      WIDTH=10
      if(length(Years[[y]])<=4)  WIDTH=7.5
      ggsave(fn.out(paste0('Estimated Total Emissions by ',figure.title,xtnsion,'.tiff')), 
             width = WIDTH,height = 6,compression = "lzw")
    }
  }
}

# ---------- GHG emissions - WA agriculture Industry -------------------------------------------
Variables=c('ni_level2')
Variables.size1=c(10) #size of legend labels
intag_level1.list=list(All.intag_level1=unique(Dat$intag_level1),Ag.emissions='Ag emissions')
Scenarios="sc1"
Years=list(2020)
Industries=list(All.industries=unique(Dat$industry),Beef="Beef",Sheep="Sheep",Dairy="Dairy",Pork="Pork",
                Chicken="Chicken",Eggs="Eggs",Horticulture="Horticulture",Grain="Grain")

GHG.emissions.donut=function(data,Variable,yr,Scen,Ind,drop.level,intag1,TITLE,Bx.pad,Nudg)  
{
  d=data%>%
    filter(year%in%yr & scenario==Scen & industry%in%Ind)%>%
    filter(ghg>0)%>%
    filter(intag_level1%in%intag1)
  Lvls=unique(d[,match(Variable,names(d))])
  if(!isFALSE(drop.level))
  {
    d=d%>%filter(!intag_level2%in%drop.level)
  }
  d=d%>%
    group_by_at(vars(year,Variable))%>% 
    summarise(Sum = sum(ghg)/1e6)%>%
    filter(Sum>0)%>%
    data.frame()
  iid=match(Variable,names(d))
  d[,iid]=factor(d[,iid],levels=Lvls)
  d=d[rev(order(d[,iid])),]
  
  d=d%>%
    mutate(year=as.character(year))%>%
    mutate(
      fraction = Sum / sum(Sum),  # Compute percentages
      ymax = cumsum(Sum),# Compute the cumulative percentages (top of each rectangle)
      ymin = c(0,lag(ymax,1)[-1]), # Compute the bottom of each rectangle
      labelPosition = (ymax + ymin) / 2,   # Compute label position
      label=as.character(!!sym(Variable)),
      label=ifelse(nchar(label)>15,sub(" ", "\n", label),label),
      label = paste0(label,"\n", '(',round(100*fraction,1),'%)'))# Compute nice label
  
  Xmin=1
  Xmax=2
  
  p=d%>%
    ggplot(aes_string(x=1, y="Sum",fill = Variable))+
    geom_bar(width = 1, stat = "identity")+
    coord_polar(theta="y")+
    theme_void() +
    xlim(c(-Xmin, Xmax))+
    theme(legend.position = "none")+
    geom_text_repel(aes(label=label, x=1, y=labelPosition),force=1,
                    arrow=arrow(angle=0),seed=666,color = "black",
                    size = 5, nudge_x = Nudg,
                    box.padding = Bx.pad,
                    max.overlaps = getOption("ggrepel.max.overlaps", default = 50),
                    min.segment.length=0)+
    annotate(geom = 'text', x = -1, y = 0.5, label = TITLE,size=7,hjust = 0.5)
  
  my.cols=Color.palette%>%
    filter(Legend.Entry%in%unique(d%>%pull((!!sym(Variable)))))%>%
    mutate(Colour=rgb(red, green, blue, maxColorValue = 255))
  my.cols1=my.cols$Colour
  names(my.cols1)=my.cols$Legend.Entry
  
  p=p+scale_fill_manual(values=my.cols1)
  
  return(p)
  
}
for(v in 1:length(Variables))
{
  for(y in 1:length(Years))
  {
    for(i in 1:length(Industries))
    {
      DRP.LVL=FALSE
      if(names(Industries)[i]=='All.industries') DRP.LVL='Purchased feed (WA)'
      
      for(s in 1:length(Scenarios))
      {
        for(l in 1:length(intag_level1.list))
        {
           GHG.emissions.donut(data=Dat,
                              Variable=Variables[v],
                              yr=Years[[y]],
                              Scen=Scenarios[s],
                              Ind=Industries[[i]],
                              drop.level=DRP.LVL,
                              intag1=intag_level1.list[[l]],
                              TITLE=paste0('GHG emissions -','\n','WA agriculture','\n','Industry',
                                           ' (',unlist(Years[[y]]),')'),
                              Bx.pad=1.5,
                              Nudg=0.2)
          
          figure.title='GHG emissions - WA agriculture Industry_'
          yrs=Years[[y]]
          if(length(yrs)>1)
          {
            dumi=ifelse(length(yrs)==2,'and','to')
            yrs=paste(yrs[1],dumi,yrs[length(yrs)])
          }
          xtnsion=paste(Variables[v],yrs,names(Industries)[i],Scenarios[s],names(intag_level1.list)[l],sep='.')
          WIDTH=10
          if(length(Years[[y]])<=4)  WIDTH=7.5
          ggsave(fn.out(paste0(figure.title,xtnsion,'.tiff')),width = WIDTH,height = 6,compression = "lzw")
        }
      }
    }

  }
}

# ---------- Sankey plots -------------------------------------------
Sankey.plt=function(data)
{
  p=data%>%
    ggplot(aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node),
               label = node)) +
    geom_sankey(flow.alpha = 0.5, node.color = 1) +
    geom_sankey_label(size = 3.5, color = 1, fill = "white") +
    scale_fill_viridis_d(alpha = 0.95) +
    theme_sankey(base_size = 16)+
    guides(fill = guide_legend(title = "Title"))+
    theme(legend.position = "none")
  print(p)
}
#Sankey.plt(data=mtcars%>%make_long(cyl, vs, am, gear, carb))
#ggsave(fn.out('sankey.tiff'), width = 6,height = 6,compression = "lzw")
