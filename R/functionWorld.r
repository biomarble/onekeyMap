
checkMatch=function(query,target){
    correctURL="https://biomarble.github.io/mapworldNames.html"
    v=match(query,target)
    notExists=query[is.na(v)]
    if(length(notExists)>0){
        msg=paste0("Countrys NOT found:\n",paste(notExists,collapse = "\n"),"\n\nCorrect country names: ",correctURL,"\n")
        warning(msg)
    }
    return(v)
}
scalemap=function(scale){
    if(scale=="medium"){
        world=worldm
    }else if(scale=="large"){
        world=worldl
    }else if(scale=="small"){
        world=worlds
    }else{
        stop("scale of map must be one of large small medium\n")
    }
    return(world)
}

#' @title mapWorld_fill.
#' @description  draw World map in country.
#' @importFrom magrittr %>%
#' @import sf
#' @import ggplot2
#' @export
mapWorld_fill=function(data,label,colorTrans=NULL,style="round",title="",scale="medium",highColor='red',lowColor='yellow',naColor="white",axis.text.size=9,legend.text.size=10,legend.title.size=11){
    inputType="name"
    world=scalemap(scale)
    colnames(data) = c('id','value')
    if(inputType=="name"){
        data$postal={world[checkMatch(data$id,world$admin),]%>%st_drop_geometry()}[,'postal']
    }
    mapdata=left_join(world,data,by=c('postal'="postal"))
    baseMap=baseMap +geom_sf(data=mapdata,aes(fill=value),linetype = "solid", size = 0.1,color='grey70',show.legend = T)
    
    if(!is.null(colorTrans)){
        if(colorTrans =="percent"){
            br=pretty(mapdata$value,n=5)
            baseMap=baseMap+scale_fill_gradient(high=highColor,low=lowColor,na.value=naColor,breaks=br,labels=paste0(br*100,'%'))
        }else{
            baseMap=baseMap+scale_fill_gradient(high=highColor,low=lowColor,na.value=naColor,trans=colorTrans)
        }
    }else{
        baseMap=baseMap+scale_fill_gradient(high=highColor,low=lowColor,na.value=naColor)
    }
    
    if(is.null(style)){
        coord=round_world
    }else if(style=="round"){
        coord=round_world
    }else if(style=="flat"){
        coord=flat_world
    }else{
        stop("coord must be flat or round\n")
    }
    
    g=baseMap+coord+
        labs(x="",y="",fill=label ,title=title)+
        theme_bw() +
        theme(
            plot.title = element_text(hjust = 0.5),
            axis.text = element_text(colour = "black", size = 9),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 10),
            legend.position = "right",
   #        panel.grid.major =  element_line(color = gray(.5),linetype = "solid",size = 0.3),
            panel.grid.major =  element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank()
        )
    
    #g=g+geom_text_repel(data=mapdata,aes(label = name,geometry = geometry),size=3, stat = "sf_coordinates",na.rm=T,show.legend=F,point.padding = 0,box.padding = 0)
    return(g)
}
#' @title mapWorld_point.
#' @description draw Points in World map.
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr count
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr bind_rows
#' @importFrom RColorBrewer brewer.pal.info
#' @importFrom RColorBrewer brewer.pal
#' @import sf
#' @import ggplot2
#' @export
mapWorld_point=function(data,title="",inputType="coordinate",bwmap=F,style="round",alpha=0.8,colorby=NULL,shapeby=NULL,jitter.factor=0.002,scale="medium",colorTrans="log10",size=2,shape=20,color="black",axis.text.size=9,jitter=F,point.mapcolors="Set1"){
    world=scalemap(scale)
    if(inputType=="name"){
#        data$postal={world[checkMatch(data$id,world$admin),]%>%st_drop_geometry()}[,'postal']
        stop("function not available!\n")
    }else if(inputType=="coordinate"){
        jitterd=data%>%mutate(x=paste(longitude,latitude,sep=":"))%>%group_by(x)%>%count()%>%filter(n!=1)%>%as.data.frame()
        indJitter=paste(data$longitude,data$latitude,sep=":")%in%jitterd$x
        
        if(jitter){
            jitterdata=data[indJitter,]%>%
                        st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant")%>%
                        st_jitter(factor=jitter.factor)
            nojitterdata=data[!indJitter,]%>%
                        st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
            
            data=bind_rows(jitterdata,nojitterdata)
        }else{
            data=data%>%st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
        }
    }
    
    if(!is.null(colorby)){
        colorbyv=data%>%select(colorby)%>%st_drop_geometry()
        colorbyv=as.factor(colorbyv[,1])
        if(length(point.mapcolors)==1){
            if(!(point.mapcolors %in% rownames(brewer.pal.info)) ){
                stop("point.mapcolors not exist in RColorBrewer Package\n\nAvailable palellte:\n",paste0(rownames(brewer.pal.info),collapse="  "))
            }
            colorvalue=colorRampPalette(rev(brewer.pal(8,name=point.mapcolors)))(length(levels(colorbyv)))
        }else{
            #colorvalue=c( "#E41A1C" ,"#377EB8", "#4DAF4A" ,"#984EA3" ,"#FF7F00", "#FFFF33" ,"#A65628", "#F781BF")
            #colorvalue=c(  "#FFFF33" , "#4DAF4A" ,"#984EA3" ,"#E41A1C" ,"#377EB8")
            if(length(point.mapcolors)!=length(levels(colorbyv))){
                stop('\npoint.mapcolors length not match with group number:\nGroups: ',paste0(levels(colorbyv),collapse = "  "),"\nNumber of Colors: ",length(point.mapcolors))
            }
            colorvalue=point.mapcolors
        }
    }
    if(!is.null(shapeby)){
        shapebyv=data%>%select(shapeby)%>%st_drop_geometry()
        shapebyv=as.factor(shapebyv[,1])
    }
    if(bwmap){
        baseMap=baseMap +geom_sf(fill='white',color='grey70',data=world,linetype = "solid", size = 0.1,show.legend = F )+scale_fill_manual(values =defaultColor,na.value="white")
    }else{
        baseMap=baseMap +geom_sf(aes(fill=as.factor(mapcolor13)),color='grey70',data=world,linetype = "solid", size = 0.1,show.legend = F )+scale_fill_manual(values =defaultColor,na.value="white")
    }
    if(!is.null(colorby) && !is.null(shapeby)){
        baseMap=baseMap+geom_sf(data=data,aes(color=colorbyv,shape=shapebyv),alpha=alpha,size = size)
    }else if(!is.null(colorby)){
        baseMap=baseMap+geom_sf(data=data,aes(color=colorbyv),size = size,shape=shape,alpha=alpha,stroke = 0)+
            scale_color_manual(values=colorvalue)
    }else if(!is.null(shapeby)){
        baseMap=baseMap+geom_sf(data=data,aes(shape=shapebyv),size = size,color=color,alpha=alpha,stroke = 0)
    }else{
        baseMap=baseMap+geom_sf(data=data,size = size,color=color,shape=shape,alpha=alpha)
    }
    
    if(is.null(style)){
        coord=round_world
    }else if(style=="round"){
        coord=round_world
    }else if(style=="flat"){
        coord=flat_world
    }else{
        stop("coord must be flat or round\n")
    }
    
    g=baseMap+coord+
        labs(x="",y="" ,color=colorby,shape=shapeby,title=title)+
        theme_bw() +
        theme(
            plot.title = element_text(hjust = 0.5),
            axis.text = element_text(colour = "black", size = 9),
            legend.text = element_text(size = 18),
            legend.title = element_text(size = 20),
            legend.position = "right",
          #  legend.key.size = unit(1, "cm"),
#           panel.grid.major =  element_line(color = gray(.5),linetype = "solid",size = 0.3),
            panel.grid.major =  element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank()
        )+guides(color = guide_legend(override.aes = list(size = 8)))

    #g=g+eom_text_repel(data=mapdata,aes(label = name,geometry = geometry),size=3, stat = "sf_coordinates",na.rm=T,show.legend=F,point.padding = 0,box.padding = 0)
    return(g)
}
