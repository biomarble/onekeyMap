# onekeyMap - draw China/world Map easily


## Installation

### General method

```R
if(!requireNamespace('remotes',quietly = T)) install.packages('remotes',update=F)
remotes::install_github('biomarble/onekeyMap',dependencies=T,upgrade = F)
```

### For Users in China

```R
if(!requireNamespace('onekeyMap',quietly = T)){
  tryCatch({
    remotes::install_github('biomarble/onekeyMap',dependencies=T,upgrade = F)
  },error = function(e){
    remotes::install_url('https://download.fastgit.org/biomarble/PlantNGSTools/archive/main.zip',dependencies = T,upgrade = F) 
  })
}
```

**Thanks to [fastgit - the mirror speeder for GitHub.com.](https://fastgit.org/) **

## Functions



|Function| Description | Demo |
|-|-| -|
|mapChina_fill_province| color China map according to data in province scale | <img src="https://blog.ugeneyun.cn/assets/Rpackage/chinaMap1.png" alt="pic" width=300 /> |
|mapChina_point| put dots into China map |  <img src="https://blog.ugeneyun.cn/assets/Rpackage/chinaMap4.png" alt="pic" width=300 /> |
|mapChina_pie_province| put pie chart into China map | <img src="https://blog.ugeneyun.cn/assets/Rpackage/chinaMap5.png" alt="pic" width=300 /> |
|mapWorld_fill| color world map according to data in country scale |  <img src="https://blog.ugeneyun.cn/assets/Rpackage/worldMap1.png" alt="pic" width=300 />|
|mapWorld_point| put dots into world map |  <img src="https://blog.ugeneyun.cn/assets/Rpackage/worldMap2.png" alt="pic" width=300 /> |

## Example

```R
heatFile=system.file('demoChina','Province.heat.csv',package = 'onekeyMap')
data=read.csv(heatFile,header=T,check.names = F,na.strings = "-",encoding = 'UTF-8')
map=mapChina_fill_province(data,label="Production",title="2019 Productions of Cotton",colorTrans="log10")

png('chinaMap1.png',w=3000,h=2000,res=300)
plot(map)
dev.off()
```


## Tips

- Report bugs/suggestions to [Issues](https://github.com/biomarble/onekeyMap/issues)

- 扫码关注微信公众号，不定期发布培训课程：<br>
![qrcode.png](https://raw.githubusercontent.com/biomarble/PlantNGSTools/main/qrcode.png)
