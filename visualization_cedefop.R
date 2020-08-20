# install.packages("sparklyr")
# install.packages("ggplot2")

library(sparklyr)
library(dplyr)
library(reshape2)
library(ggplot2)


# change the hdfs path value according to the actual value
hdfs_path <-  "hdfs:///data/ft_document_essnet.out"

sc <- spark_connect(master = "yarn-client", spark_home = '/usr/lib/spark/')

ft_document <- spark_read_parquet(sc = sc, path = hdfs_path)
summarise(ft_document, summary)


# change paramter accordingly to the desired country
country = "RO"

#### NUmber of job ads present on RO sites for each UE country
number <- ft_document %>% filter(sourcecountry == country) %>% group_by(idcountry) %>% summarise(count = n())
number <- collect(number)

barplot(number$count, names.arg = number$idcountry, main = "Number of job ads from RO sites to EU countries \ present on RO sites")

#### Education level requirements per countries

countryedu <- ft_document %>% filter(sourcecountry == country) %>% select(idcountry, educational_level) %>% group_by(idcountry, educational_level) %>% summarise(count = n())
countryedu <- collect(countryedu)
countryedu[countryedu==""] <- NA
p <- ggplot(countryedu, aes(x = educational_level, count)) 
p <- p + geom_col(aes(fill = educational_level))
p <- p + facet_wrap(~idcountry) 
p <- p + theme(axis.text.x=element_blank())
p <- p + ggtitle(paste0("Number of job ads for EU countries collected in each period \ per educational level \ present for ", country))
p

##### category sectors adds

countrycateg <- ft_document %>% filter(sourcecountry == country) %>% select(idcountry, category_sector) %>% group_by(idcountry, category_sector) %>% summarise(count = n())
countrycateg <- collect(countrycateg)
countrycateg[countrycateg==""] <- NA
p <- ggplot(countrycateg, aes(x = category_sector , count)) 
p <- p + geom_col(aes(fill = category_sector))
p <- p + facet_wrap(~idcountry) 
p <- p + theme(axis.text.x=element_blank())
p <- p + ggtitle(paste0("Number of job ads for EU countries collected in each period \ per category sector \ for ", country))
p


##### breakdown by esco level 1 and level 2

countryesco1 <- ft_document %>% filter(sourcecountry == country) %>% select(idcountry, esco_level_1) %>% group_by(idcountry, esco_level_1) %>% summarise(count = n())
countryesco1 <- collect(countryesco2)
countryesco1[countryesco1==""] <- NA
p <- ggplot(countryesco1, aes(x = esco_level_1 , count)) 
p <- p + geom_col(aes(fill = esco_level_1))
p <- p + facet_wrap(~idcountry) 
p <- p + theme(axis.text.x=element_blank())
p <- p + ggtitle(paste0("Number of job ads for EU countries collected in each period \ per esco level 1 \ for ", country))
p

countryesco2 <- ft_document %>% filter(sourcecountry == country) %>% select(idcountry, esco_level_2) %>% group_by(idcountry, esco_level_2) %>% summarise(count = n())
countryesco2 <- collect(countryesco2)
countryesco2[countryesco2==""] <- NA
p <- ggplot(countryesco2, aes(x = esco_level_2 , count)) 
p <- p + geom_col(aes(fill = esco_level_2))
p <- p + facet_wrap(~idcountry) 
p <- p + theme(axis.text.x=element_blank())
p <- p + ggtitle(paste0("Number of job ads for EU countries collected in each period \ per esco level 2 \ for ", country))
p

######### Number of jobs ads for EU countries per experience required

countryexp <- ft_document %>% filter(sourcecountry == country) %>% select(idcountry, experience) %>% group_by(idcountry, experience) %>% summarise(count = n())
countryexp <- collect(countryexp)
countryexp[countryexp == ""] <- NA
p <- ggplot(countryexp, aes(x = experience , count)) 
p <- p + geom_col(aes(fill = experience))
p <- p + facet_wrap(~idcountry) 
p <- p + theme(axis.text.x=element_blank())
p <- p + ggtitle(paste0("Number of job ads for EU countries collected in each period \ per experience requirements\ for ", country))
p


######### Number of job ads collected in each period 
countrydate <- ft_document %>% filter(sourcecountry == country) %>% select(idcountry, grab_date) %>% group_by(idcountry, grab_date) %>% summarise(count = n())
countrydate <- collect(countrydate)
countrydate$grab_date <- as.Date(countrydate$grab_date, origin="1970-01-01")
p <- ggplot(countrydate, aes(x = grab_date, count)) 
p <- p + geom_line(color = "#00AFBB", size = 1)
p <- p + facet_wrap(~idcountry) 
p <- p + ggtitle(paste0("Number of job ads for EU countries collected\ per grab date \ for ", country))
p

######## Number of job ads for EU countries per each paramter 'country'  site" 
sites <- ft_document %>% filter(sourcecountry == country) %>% select(idcountry, site) %>% group_by(idcountry, site) %>% summarise(count = n())
sites <- collect(countrysites)
sites <- dcast(countrysites, site ~ idcountry)
write.csv(sites, "sites.csv", row.names = FALSE)