install.packages("remotes")
library("remotes")
remotes::install_github("GuangchuangYu/nCov2019",force = TRUE)
remotes::install_github("GuangchuangYu/chinamap",force = TRUE)
require(nCov2019)
require(chinamap)
################################China
options(digits = 20)
rm(list=ls())
library(jsonlite)
library(chinamap) #github.com/guangchuangyu/chinamap
library(ggplot2)

#爬虫获取疫情数据，并替换某些中间符号
web = 'https://view.inews.qq.com/g2/getOnsInfo?name=disease_h5&callback=1580373566110'
rawdata = readLines(web, encoding="UTF-8")
rawdata = sub("^\\d+", "", rawdata)
rawdata = sub("^\\(", "", rawdata)
rawdata = sub("\\)$", "", rawdata)
tempdata = fromJSON(rawdata)
covdata = fromJSON(tempdata$data)

#数据变量分组
usedata = data.frame(name = covdata$areaTree$children[[1]]$name,
                confirmnum = covdata$areaTree$children[[1]]$total$confirm,
                confirmrange = cut(covdata$areaTree$children[[1]]$total$confirm, 
                              c(1,10,100,500,1000,10000,100000),
                              include.lowest = T, right=F))

#获取中国地图，并将省份名称统一为两个字或三个字的简称
cn = get_map_china()
cn$province = sub("省", "", cn$province)
cn$province = sub("自治区", "", cn$province)
cn$province = sub("市", "", cn$province)
cn$province = sub("特别行政区", "", cn$province)
cn$province = sub("维吾尔", "", cn$province)
cn$province = sub("壮族", "", cn$province)
cn$province = sub("回族", "", cn$province)
#将地图与疫情数据合并
cn2 = merge(cn, usedata, by.x='province', by.y='name', all.x=TRUE)
cn2 = cn2[order(cn2$order), ]


#设定颜色为6种不同深浅的红色
cols = RColorBrewer::brewer.pal(6, 'Reds')
names(cols) = levels(usedata$confirmrange)
#绘制地图
#获取地图数据，并将边界、省界等填充为灰色线
#是否进行球形投影
#设置白色背景、灰色经纬线、字体大小为14的简约主题
#隐藏坐标轴
#设定主标题、副标题和小标题
#设置图例的标题、内容、颜色
cnpic = ggplot(cn2, aes(long,lat)) +
        geom_map(aes(long, lat, map_id=id, group=group, fill=confirmrange), map=cn2, data=cn2, colour='grey') +
        coord_map() +    #或采用coord_map("polyconic")，在球面维度上绘制地图
        theme_minimal(base_size = 14) +
        xlab(NULL) + ylab(NULL) +
        labs(title = "China Epidemic Distribution of COVID-19",
             subtitle = paste('Confirmed:',covdata$chinaTotal$confirm,
                              "Suspected:",covdata$chinaTotal$suspect,
                              "Dead:",covdata$chinaTotal$dead,
                              "Heal:",covdata$chinaTotal$heal),
             caption=paste("Update to:", covdata$lastUpdateTime)) + 
        scale_fill_manual(labels = c("1-9", "10-99","100-499","500-999","1000-9999","≥10000"),
                          values=cols, breaks=names(cols))
#添加省份
labeldata = read.csv("C:/Users/HP/Desktop/R期末/labeldata.csv", header = TRUE, encoding = "UTF-8")
colnames(labeldata)[1] = 'province'
cnpic = ggplot() + geom_text(aes(jd, wd, label = province), data = labeldata)
cnpic
#将计算机虚拟内存调至110000Mb
#申请分配内存空间
memory.limit(160000)
library(plotly)
ggplotly()






################################World
#爬虫获取世界疫情数据
#web2 = 'https://api.inews.qq.com/newsqa/v1/automation/foreign/country/ranklist'
#rawdata = readLines(web2, encoding="UTF-8")
#rawdata = sub("^\\d+", "", rawdata)
#rawdata = sub("^\\(", "", rawdata)
#rawdata = sub("\\)$", "", rawdata)
#tempdata = fromJSON(rawdata)
#covdata2 = fromJSON(tempdata$data)
#通过nCov2019包获取世界疫情数据（绘制中国地图已使用爬虫方法，因此此处使用另一种方法）
library(nCov2019)
library(ggplot2)
timeseris = load_nCov2019(lang = 'en')
glotimeseris = timeseris['global']
todaydata = glotimeseris[glotimeseris$time=="2020-07-03",]

#对照maps包地图信息修改疫情数据国家名
todaydata$country = sub("United\\sStates.*", "USA", todaydata$country)
todaydata$country = sub("Republic\\sof\\sKorea", "South Korea", todaydata$country)
todaydata$country = sub("United\\sKingdom.*", "UK", todaydata$country)
todaydata$country = sub("Republika\\sSeverna\\sMakedonija", "Macedonia", todaydata$country)
usedata = data.frame(name = todaydata$country,
                confirmnum = todaydata$cum_confirm,
                deadnum = todaydata$cum_dead,
                healmnum = todaydata$cum_heal,
                confirm = cut(todaydata$cum_confirm, 
                              c(0,10,50,100,1000,10000,100000,1000000,10000000),
                              include.lowest = T, right=F))
#调用世界地图，并选取不包含南极洲的部分（南极洲目前无人定居，没有新冠肺炎病例，因此略去）
wd = map_data('world')
wd = wd[wd$region != "Antarctica", ]
#将世界疫情数据与地图融合
wd2 = merge(wd, usedata, by.x='region', by.y='name', all.x=TRUE)
wd2 = wd2[order(wd2$order),]
#设定颜色为8种不同深浅的红色
cols = RColorBrewer::brewer.pal(8, 'Reds')
names(cols) = levels(usedata$confirm)
#绘图
wdpic = ggplot(wd2, aes_(~long, ~lat)) + 
        coord_equal() +
        theme_minimal(base_size = 24) +
        xlab(NULL) + ylab(NULL) +
        labs(title = "World Epidemic Distribution of COVID-19", 
             subtitle = paste('Confirmed:',sum(todaydata$cum_confirm),
                              "Dead:",sum(todaydata$cum_dead),
                              "Heal:",sum(todaydata$cum_heal)),
             caption=paste("Update to:", todaydata$time[1]))+  
        geom_map(aes_(~long, ~lat, map_id=~region, group=~group, fill=~confirm), 
                 map=wd2, data=wd2, colour='grey') + 
        scale_fill_manual(labels = c("1-9", "10-49","50-99","100-999","1000-9999","10000-99999","100000-1000000","≥1000000"),
                          values=cols, breaks=names(cols))
wdpic
#将计算机虚拟内存调至70000Mb
#申请分配内存空间
memory.limit(70000)
library(plotly)
ggplotly()

text = paste("Confirmed: ", wd2$confirmnum, "\n",
             "Dead: ", wd2$deadnum, "\n", 
             "Heal: ", wd2$healnum, "\n",
             sep="")
wdpic2 = plotly_build(wdpic)
style(wdpic2, text=text, hoverinfo ="text", traces = c(1,2,3))








###################################China plotly
library("maptools")
library(rgdal)
library(tidyverse)
library(plyr)
library(ggplot2)
library(plotly)
library(jsonlite)

#爬虫获取疫情数据，并替换某些中间符号
web = 'https://view.inews.qq.com/g2/getOnsInfo?name=disease_h5&callback=1580373566110'
rawdata = readLines(web, encoding="UTF-8")
rawdata = sub("^\\d+", "", rawdata)
rawdata = sub("^\\(", "", rawdata)
rawdata = sub("\\)$", "", rawdata)
tempdata = fromJSON(rawdata)
covdata = fromJSON(tempdata$data)
covdata = fromJSON(y$data)
usedata = data.frame(NAME = covdata$areaTree$children[[1]]$name,
                province=covdata$areaTree$children[[1]]$name,
                confirmnum = covdata$areaTree$children[[1]]$total$confirm,
                suspectednum = covdata$areaTree$children[[1]]$total$suspect,
                deadnum = covdata$areaTree$children[[1]]$total$dead,
                healnum = covdata$areaTree$children[[1]]$total$heal)
cn = readOGR("C:/Users/HP/Desktop/R期末/Q9-bou2_4p.shp")
#通过shp文件获取中国地图，并将省份名称统一为两个字或三个字的简称
cn$NAME = sub("省", "", cn$NAME)
cn$NAME = sub("自治区", "", cn$NAME)
cn$NAME = sub("市", "", cn$NAME)
cn$NAME = sub("特别行政区", "", cn$NAME)
cn$NAME = sub("维吾尔", "", cn$NAME)
cn$NAME = sub("壮族", "", cn$NAME)
cn$NAME = sub("回族", "", cn$NAME)
#添加地图ID并合并，查看数据发现，cn@data中共有925个行政信息
cndata = data.frame(cn@data, id=seq(0:924)-1)
cn1 = fortify(cn)
cn2 = join(cn1, cndata, type = "full")
#将地图与疫情数据合并
cn3 = join(cn2, usedata, type="full")
#变量数据分组
cn3$confirmrange = cut(cn3$confirm, breaks = c(0,9,99,499,999,9999,99999))
cn3$confirmrangef = factor(cn3$confirmrange,levels=c('(0,9]','(9,99]','(99,499]','(499,999]','(999,1e+04]','(1e+04,1e+05]'),labels=c('1-9','10-99','100-499','500-999','1000-9999','≥10000'),order=TRUE)
#获取各省份省会的经纬度，以便在图中各省省会的位置标注省份名称
pro = read.csv("C:/Users/HP/Desktop/R期末/Q9-province.csv")
#绘图
cnpic = ggplot(cn3, aes(long,lat)) +
        geom_polygon(aes(group=group,fill=confirmrangef),colour="grey") +
        scale_fill_brewer(palette="Reds") +
        #coord_equal() +
        #geom_map(aes(long, lat, map_id = id, fill=confirmrangef), map=cn3, data=cn3, colour='grey') +
        coord_map() +    #或采用coord_map("polyconic")，在球面维度上绘制地图
        geom_text(aes(x = long, y = lat, label = province), data =pro)+
        guides(fill=guide_legend(reverse=TRUE,title=NULL))+  
        theme_minimal(base_size = 14) +
        xlab(NULL) + ylab(NULL) +
        labs(title = "China Epidemic Distribution of COVID-19",
             subtitle = paste('Confirmed:',covdata$chinaTotal$confirm,
                              "Suspected:",covdata$chinaTotal$suspect,
                              "Dead:",covdata$chinaTotal$dead,
                              "Heal:",covdata$chinaTotal$heal),
             caption=paste("Update to:", covdata$lastUpdateTime))
cnpic
ggplotly()

text = paste("Confirmed: ", cn3$confirmnum, "\n",
             "Suspected: ", cn3$suspectednum, "\n",
             "Dead: ", cn3$deadnum, "\n", 
             "Heal: ", cn3$healnum, "\n",
             sep="")
cnpic2 = plotly_build(cnpic)
style(cnpic2, text=text, hoverinfo ="text", traces = c(1,2,3,4))
