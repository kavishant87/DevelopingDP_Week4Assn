
library(shiny)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(splines2)
library(MASS)
library(mapproj)
shinyServer(function(input,output){
    
    output$Data<-renderTable({
    x<-as.numeric(USArrests[,as.numeric(input$var)]);
    Urban_Population<-as.numeric(USArrests$UrbanPop);
    if(as.numeric(input$var)==1)
    {Murder <- x; data.frame(Murder,Urban_Population)}
    else if(as.numeric(input$var)==2)
    {Assault <- x; data.frame(Assault,Urban_Population) }
   else  if(as.numeric(input$var)==4)
    {Rape <- x; data.frame(Rape,Urban_Population)}
    })
    
    output$Map<-renderPlot({
    us <- map_data("state")
    if(as.numeric(input$var)==1)
    {Crimedata <- data.frame(state=tolower(rownames(USArrests)),USArrests)
    ggplot(Crimedata,aes(map_id=state,fill=Murder))+
    geom_map(map=us,colour="black")+
    scale_fill_gradientn(colours=c("cyan","thistle2","hotpink","red"))+
    expand_limits(x=us$long,y=us$lat)+
    coord_map("bonne",lat0=50)+
        theme(
            panel.border=element_rect(fill=NA,colour="black"),
            panel.grid.major=element_line(colour="white",size=1),
            panel.grid.minor=element_line(colour="white",size=1),
            axis.line=element_blank(),
            axis.ticks=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="bottom",
            legend.key.size=unit(1.25,"cm"),
            legend.title=element_text(size=rel(1),face="bold",hjust=0,colour="black"),
            legend.text=element_text(size=10,colour="black",angle=45),
            panel.margin=unit(2,"lines"),
            panel.background=element_rect(fill='gray',colour='gray')
        )
    
    }
        
    
   
   else if(as.numeric(input$var)==2)
        {Crimedata <- data.frame(state=tolower(rownames(USArrests)),USArrests)
        ggplot(Crimedata,aes(map_id=state,fill=Assault))+
            geom_map(map=us,colour="black")+
            scale_fill_gradientn(colours=c("cyan","thistle2","hotpink","red"))+
            expand_limits(x=us$long,y=us$lat)+
            coord_map("bonne",lat0=50)+
            theme(
                panel.border=element_rect(fill=NA,colour="black"),
                panel.grid.major=element_line(colour="white",size=1),
                panel.grid.minor=element_line(colour="white",size=1),
                axis.line=element_blank(),
                axis.ticks=element_blank(),
                axis.text.x=element_blank(),
                axis.text.y=element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                legend.position="bottom",
                legend.key.size=unit(1.25,"cm"),
                legend.title=element_text(size=rel(1),face="bold",hjust=0,colour="black"),
                legend.text=element_text(size=10,colour="black",angle=45),
                panel.margin=unit(2,"lines"),
                panel.background=element_rect(fill='gray',colour='gray')
            )
        
        }
        else if(as.numeric(input$var)==4)
        {Crimedata <- data.frame(state=tolower(rownames(USArrests)),USArrests)
        ggplot(Crimedata,aes(map_id=state,fill=Rape))+
            geom_map(map=us,colour="black")+
            scale_fill_gradientn(colours=c("cyan","thistle2","hotpink","red"))+
            expand_limits(x=us$long,y=us$lat)+
            coord_map("bonne",lat0=50)+
            theme(
                panel.border=element_rect(fill=NA,colour="black"),
                panel.grid.major=element_line(colour="white",size=1),
                panel.grid.minor=element_line(colour="white",size=1),
                axis.line=element_blank(),
                axis.ticks=element_blank(),
                axis.text.x=element_blank(),
                axis.text.y=element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                legend.position="bottom",
                legend.key.size=unit(1.25,"cm"),
                legend.title=element_text(size=rel(1),face="bold",hjust=0,colour="black"),
                legend.text=element_text(size=10,colour="black",angle=45),
                panel.margin=unit(2,"lines"),
                panel.background=element_rect(fill='gray',colour='gray')
            )
        
        }
    })
    
    output$Diagnostics<- renderPlot({
        x<-as.numeric(USArrests[,as.numeric(input$var)])
        y<-as.numeric(USArrests$UrbanPop)
        par(mfrow=c(2,3))
        par(cex=0.8)
        lm.out=lm(y~x,data=USArrests)
        plot(lm.out,col="blue")
        plot(cooks.distance(lm.out),col="blue")
        
    })
    })