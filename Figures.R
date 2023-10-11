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

# install.packages("remotes")
# remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)

# ---------- Load data-------------------------------------------
Dat=read.csv(fn.in('IntAg Modelling.csv'))

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


# ---------- Share of estimated emissions by Source and Industry -------------------------------------------
Variables=c('intag_level2','intag_level1','ni_level1','ni_level2','gas')
Variables.size=c(3,5,5,2,5) #size of inset percentage
Variables.size1=c(8,10,10,8,10) #size of legend labels
Scenarios=vec_scenario
Years=vec_year
share.estim.em=function(data,Variable,yr,Scen,Size,Size1)
{
  d=data%>%
    filter(year==yr & scenario==Scen)%>%
    filter(!intag_level2%in%c('Purchased feed (WA)','Purchased feed (imported)'))%>%
    group_by_at(vars(industry,Variable))%>% 
    summarise(Sum = sum(ghg)) %>% 
    mutate(perc =Sum/sum(Sum))%>%
    filter(round(100*perc)>0)
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
                     Size1=Variables.size1[i])
      xtnsion=paste(Variables[i],Years[y],Scenarios[s],sep='.')
      ggsave(fn.out(paste0('Share of Estimated Emissions by Source and Industry_',xtnsion,'.tiff')), 
             width = 10,height = 6,compression = "lzw")
    }
  }
}

# ---------- Share of estimated emissions Scope and Industry  -------------------------------------------
Variables=c('scope')
Variables.size=4 #size of inset percentage
Variables.size1=10
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
                    Size1=Variables.size1[i])
      xtnsion=paste(Variables[i],Years[y],Scenarios[s],sep='.')
      ggsave(fn.out(paste0('Share of Estimated Emissions by Scope and Industry_',xtnsion,'.tiff')), 
             width = 10,height = 6,compression = "lzw")
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
