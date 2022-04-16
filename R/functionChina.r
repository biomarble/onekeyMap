#' @title mapChinaPieProvince.
#' @description  draw China map in province.
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom scatterpie geom_scatterpie
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggnewscale new_scale_fill
#' @import sf
#' @import ggplot2
#' @export

mapChina_pie_province=function(data,title="",pieName=NULL,axis.text.size=9,dataCol=NULL,piePalette="Set2",pieSize=1,NameCol=1){
    if(is.null(dataCol)){
        dataCol=colnames(data)[setdiff(1:ncol(data),NameCol)]
    }
    if(length(dataCol)<=7){
        pieColor=brewer.pal(name=piePalette,n=length(dataCol))
    }else{
        pieColor=colorRampPalette(brewer.pal(name=piePalette,n=7))(length(dataCol))
    }
    names(pieColor)=dataCol

    use=mapchinaData[pmatch(data[,NameCol],mapchinaData$name),]%>%
        st_drop_geometry()%>%
        select('center')%>%unlist()%>%
        matrix(ncol=2,byrow = T)%>%as.data.frame()%>%
        cbind(data,.)%>%st_as_sf( coords = c("V1", "V2"), crs = 4326, agr = "constant")
    plotdata=cbind(use%>%st_coordinates(),use%>%st_drop_geometry())
    baseMap=ggplot(mapchinaData) +
            geom_sf(aes(fill=adcode),linetype = "solid", size = 0.1,show.legend = F)+
            scale_fill_manual(values =defaultColor)+new_scale_fill()+
            geom_scatterpie(aes(r=pieSize,x=X, y=Y), data=plotdata,cols=dataCol,color='grey50')+
            scale_fill_manual(pieName,values=pieColor)

    islandMap <- baseMap+coord_sf(crs = st_crs(4326), xlim = c(107.6,122.7 ), ylim = c(3.0,25.0),expand = F, datum = NA)+
        labs(x=NULL,y=NULL)+
        theme_bw()+
        theme(legend.position = 'None',
              panel.border = element_rect(colour = "black", fill = 'transparent' ),
              plot.margin = unit(c(0,0,0,0), "cm")
              )
    g=baseMap+
        annotation_custom(
            grob = ggplotGrob(islandMap),
            xmin =  126,
            xmax =  126+(122.7-107.6)/1.7,
            ymin =  17.0 ,
            ymax =  17+(25.0-3.0)/1.7
        )+chinaMap_coord+labs(x="",y="",title=title)+
        theme_bw()+
        theme(plot.title = element_text(hjust=0.5),
              axis.text = element_text(colour="black", size=axis.text.size),
              axis.text.y = element_text(lineheight = 0.65),
              legend.text=element_text(size=10),
              legend.title=element_text(size=10),
              legend.position="right",
              panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.3),
              panel.background = element_rect(fill = "#f0f8ff")
        )
    #g=g+geom_text_repel(data=mapchinaData,aes(label = name,geometry = geometry),size=3, stat = "sf_coordinates",na.rm=T,show.legend=F,point.padding = 0,box.padding = 0)
    return(g)
}

#' @title mapChinaFillProvince.
#' @description  draw China map in province.
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join
#' @import sf
#' @import ggplot2
#' @export

mapChina_fill_province=function(data,label,colorTrans=NULL,title="",highColor='red',lowColor='yellow',naColor="white",axis.text.size=9,legend.text.size=10,legend.title.size=11){
    colnames(data) = c('id','value')
    data$adcode={mapchinaData[pmatch(data$id,mapchinaData$name),]%>%st_drop_geometry()}[,'adcode']
    mapchinaData=left_join(mapchinaData,data,by=c('adcode'="adcode"))

    baseMap=ggplot(mapchinaData) +geom_sf(aes(fill=value),linetype = "solid", size = 0.1,show.legend = T)
    if(!is.null(colorTrans)){
        if(colorTrans =="percent"){
          br=pretty(mapchinaData$value,n=5)
            baseMap=baseMap+scale_fill_gradient(high=highColor,low=lowColor,na.value=naColor,breaks=br,labels=paste0(br*100,'%'))
        }else{
            baseMap=baseMap+scale_fill_gradient(high=highColor,low=lowColor,na.value=naColor,trans=colorTrans)
        }
    }else{
        baseMap=baseMap+scale_fill_gradient(high=highColor,low=lowColor,na.value=naColor)
    }

    islandMap <- baseMap +coord_sf(crs = st_crs(4326), xlim = c(107.6,122.7 ), ylim = c(3.0,25.0),expand = F, datum = NA)+
        theme_bw()+
        theme(
            legend.position = 'None',
            panel.border = element_rect(colour = "black", fill = 'transparent' ),
            panel.background = element_rect(fill = "#f0f8ff"),
            plot.margin = unit(c(0,0,0,0), "cm")
            )

    g=baseMap+annotation_custom(
            grob = ggplotGrob(islandMap),
            xmin =  126,
            xmax =  126+(122.7-107.6)/1.7,
            ymin =  17.0 ,
            ymax =  17+(25.0-3.0)/1.7
        )+chinaMap_coord+
        labs(x="",y="",fill=label ,title=title)+
        theme_bw()+
        theme(plot.title = element_text(hjust=0.5),
              axis.text = element_text(colour="black", size=axis.text.size),
              axis.text.y = element_text(lineheight = 0.65),
              legend.text=element_text(size=legend.text.size),
              legend.title=element_text(size=legend.title.size),
              legend.position="right",
              panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.3),
              panel.background = element_rect(fill = "#f0f8ff")
        )
    #g=g+geom_text_repel(data=mapchinaData,aes(label = name,geometry = geometry),size=3, stat = "sf_coordinates",na.rm=T,show.legend=F,point.padding = 0,box.padding = 0)
    return(g)
}

#' @title mapChinaPoint.
#' @description  draw China map in Points.
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @import sf
#' @import ggplot2
#' @export
mapChina_point=function(data,title="",colorby=NULL,shapeby=NULL,colorTrans="log10",size=2,shape=19,color="black",axis.text.size=9){
    if(!is.null(colorby)){
        colorbyv=data%>%select(colorby)
        colorbyv=colorbyv[,1]
    }
    if(!is.null(shapeby)){
        shapebyv=data%>%select(shapeby)
        shapebyv=shapebyv[,1]
    }
    data=data%>%st_as_sf( coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
    baseMap=ggplot(mapchinaData) +geom_sf(aes(fill=adcode),linetype = "solid", size = 0.1,show.legend = F)+
        scale_fill_manual(values =defaultColor)
    if(!is.null(colorby) && !is.null(shapeby)){
        baseMap=baseMap+geom_sf(data=data,aes(color=colorbyv,shape=shapebyv),size = size)
    }else if(!is.null(colorby)){
        baseMap=baseMap+geom_sf(data=data,aes(color=colorbyv),size = size,shape=shape)
    }else if(!is.null(shapeby)){
        baseMap=baseMap+geom_sf(data=data,aes(shape=shapebyv),size = size,color=color)
    }else{
        baseMap=baseMap+geom_sf(data=data,size = size,color=color,shape=shape)
    }

    islandMap <- baseMap+coord_sf(crs = st_crs(4326), xlim = c(107.6,122.7 ), ylim = c(3.0,25.0),expand = F, datum = NA)+
         theme_bw()+theme(legend.position = 'None',panel.border = element_rect(colour = "black", fill = 'transparent' ),plot.margin = unit(c(0,0,0,0), "cm"))
    g=baseMap+
        annotation_custom(
        grob = ggplotGrob(islandMap),
        xmin =  126,
        xmax =  126+(122.7-107.6)/1.7,
        ymin =  17.0 ,
        ymax =  17+(25.0-3.0)/1.7
         )+chinaMap_coord+labs(x="",y="",color=colorby,shape=shapeby,title=title)+
        theme_bw()+
        theme(plot.title = element_text(hjust=0.5),
              axis.text = element_text(colour="black", size=axis.text.size),
              axis.text.y = element_text(lineheight = 0.65),
              legend.text=element_text(size=10),
              legend.title=element_text(size=10),
              legend.position="right",
              panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.3),
              panel.background = element_rect(fill = "#f0f8ff")
        )
    #g=g+eom_text_repel(data=mapchinaData,aes(label = name,geometry = geometry),size=3, stat = "sf_coordinates",na.rm=T,show.legend=F,point.padding = 0,box.padding = 0)
    return(g)
}
