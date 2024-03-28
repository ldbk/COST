#' Test data availability
#'
#' @description Test data availability
#' @param info an info object fomr defstock function
#' @param CLr commercial landing
#' @param CSr commercial sampling 
#' @param param the parameters
#' @return table of results
#'
#' @export
teststock<-function(info,CLr,CSr,param){
	if(F){
		setwd("/Users/moi/ifremer/analyses_stock_2024/WGNSSK/mur.27.3a47dtest/")
		library(COSTlite)
		load("./data/CLrall.rdata")
		CLr<-clData(CLr@cl)
		load("./data/CSrall.rdata")
		CSr<-csData(CSr@tr,CSr@hh,CSr@sl,CSr@hl,CSr@ca)
		path2precatch<-"/Users/moi/ifremer/analyses_stock_2024/precatch.csv"
		stock<-"mur.27.3a47d"
		wg<-"WGNSSK"
		year<-2023
		defstock(stock,wg,2023,CLr,CSr,path2precatch)
	}
	#allspecies vector if more than 1
	allspecies<-unlist(strsplit(info$species,","))

	#data test
	clcur<-CLr@cl%>%filter(year==param$currentyear)
	#eval(parse(text=paste0("cscur<-subset(CSr,year==",param$currentyear,",table='sl')")))
	#cscur<-subset(CSr,year==param$currentyear,table="sl")
	nbech<-CSr@sl%>%filter(spp%in%allspecies)%>%
		select(trpCode,staNum,catchCat,year)%>%distinct()%>%
		group_by(year,catchCat)%>%summarise(nb_sampled_haul=n())%>%ungroup()
	nbfish<-CSr@hl%>%filter(spp%in%unlist(strsplit(info$species,split=",")))%>%
		select(lenNum,catchCat,year)%>%group_by(year,catchCat)%>%
		summarise(nb_fish=sum(lenNum,na.rm=T))%>%ungroup()

	nbCL<-nrow(clcur)
	testCL<-nrow(clcur)>0 & any(!is.na(clcur),na.rm=T)
	#nbECH<-nbech%>%filter(year==param$currentyear&catchCat=="DIS")%>%select(nb_sampled_haul)%>%as.numeric(na.rm=T)
	#nbECH<-ifelse(is.na(nbECH),0,nbECH)
	nbECH<-nbech%>%filter(year==param$currentyear&catchCat=="LAN")%>%select(nb_sampled_haul)%>%as.numeric(na.rm=T)
	nbECH<-ifelse(is.na(nbECH),0,nbECH)
	testECH<-nbECH>=param$seuilsample
	nbDIS<-nbech%>%filter(year==param$currentyear&catchCat=="DIS")%>%select(nb_sampled_haul)%>%as.numeric(na.rm=T)
	nbDIS<-ifelse(is.na(nbDIS),0,nbDIS)
	testDIS<-nbDIS>=param$seuilsampledis
	nbfishLAN<-nbfish%>%filter(year==param$currentyear&catchCat=="LAN")%>%select(nb_fish)%>%as.numeric(na.rm=T)
	nbfishLAN<-ifelse(is.na(nbfishLAN),0,nbfishLAN)
	testfishLAN<-nbfishLAN>=param$seuilfish & testECH
	nbfishDIS<-nbfish%>%filter(year==param$currentyear&catchCat=="DIS")%>%select(nb_fish)%>%as.numeric(na.rm=T)
	nbfishDIS<-ifelse(is.na(nbfishDIS),0,nbfishDIS)
	testfishDIS<-nbfishDIS>=param$seuilfish & testDIS
	nbage<-as.numeric(CSr@ca%>%filter(year==param$currentyear & age!=-1 & spp%in%allspecies)%>%filter(!is.na(age))%>%summarise(nb=n()))
	testage<-nbage>=param$seuilage & (testfishLAN|testfishDIS)
	nbmasse<-as.numeric(CSr@ca%>%filter(year==param$currentyear & indWt!=-1 & spp%in%allspecies)%>%filter(!is.na(age))%>%summarise(nb=n()))
	testmasse<-nbmasse>=param$seuilweight & (testfishLAN|testfishDIS)

	tabtest<-data.frame(Type=c("LAN","LAN samples","DIS samples","sizeLAN","sizeDIS","age","weight"),
		Results=c(testCL,testECH,testDIS,testfishLAN,testfishDIS,testage,testmasse),
		Tresholds=c(0,param$seuilsample,param$seuilsampledis,param$seuilfish,param$seuilfish,param$seuilage,param$seuilweight),
		Nb_currentyear=c(paste(nbCL,"rows in CL"),
		     paste(nbECH,"LAN samples"),
		     paste(nbDIS,"DIS samples"),
		     paste(nbfishLAN,"fishes in LAN"),
		     paste(nbfishDIS,"fishes in DIS"),
		     paste(nbage,"ages in CA"),
		     paste(nbmasse,"weight in CA")))
	#tabtest for a period of interest using param info

	#data test
	listyear<-param$currentyear:(param$currentyear-param$nbyear)
	clcur<-CLr@cl%>%filter(year%in%listyear)
	#eval(parse(text=paste0("cscur<-subset(CSr,year==",param$currentyear,",table='sl')")))
	#cscur<-subset(CSr,year==param$currentyear,table="sl")
	nbech<-CSr@sl%>%filter(spp%in%allspecies)%>%
		select(trpCode,staNum,catchCat,year)%>%distinct()%>%
		group_by(year,catchCat)%>%summarise(nb_sampled_haul=n())%>%ungroup()
	#nbfish<-CSr@hl%>%filter(spp%in%info$species)%>%
	nbfish<-CSr@hl%>%filter(spp%in%unlist(strsplit(info$species,split=",")))%>%
		select(lenNum,catchCat,year)%>%group_by(year,catchCat)%>%
		summarise(nb_fish=sum(lenNum,na.rm=T))%>%ungroup()

	nbCL<-nrow(clcur)
	testCL<-nrow(clcur)>0 & any(!is.na(clcur),na.rm=T)
	#nbECH<-nbech%>%filter(year%in%listyear&catchCat=="DIS")%>%pull(nb_sampled_haul)%>%as.numeric(na.rm=T)%>%sum
	#nbECH<-ifelse(is.na(nbECH),0,nbECH)
	nbECH<-nbech%>%filter(year%in%listyear&catchCat=="LAN")%>%pull(nb_sampled_haul)%>%as.numeric(na.rm=T)%>%sum
	nbECH<-ifelse(is.na(nbECH),0,nbECH)
	testECH<-nbECH>=param$seuilsample
	nbDIS<-nbech%>%filter(year%in%listyear&catchCat=="DIS")%>%pull(nb_sampled_haul)%>%as.numeric(na.rm=T)%>%sum
	nbDIS<-ifelse(is.na(nbDIS),0,nbDIS)
	testDIS<-nbDIS>=param$seuilsampledis
	nbfishLAN<-nbfish%>%filter(year%in%listyear&catchCat=="LAN")%>%pull(nb_fish)%>%as.numeric(na.rm=T)%>%sum
	nbfishLAN<-ifelse(is.na(nbfishLAN),0,nbfishLAN)
	testfishLAN<-nbfishLAN>=param$seuilfish & testECH
	nbfishDIS<-nbfish%>%filter(year%in%listyear&catchCat=="DIS")%>%pull(nb_fish)%>%as.numeric(na.rm=T)%>%sum
	nbfishDIS<-ifelse(is.na(nbfishDIS),0,nbfishDIS)
	testfishDIS<-nbfishDIS>=param$seuilfish & testDIS
	nbage<-as.numeric(CSr@ca%>%filter(year%in%listyear& age!=-1 & spp%in%allspecies)%>%filter(!is.na(age))%>%summarise(nb=n()))
	testage<-nbage>=param$seuilage & (testfishLAN|testfishDIS)
	nbmasse<-as.numeric(CSr@ca%>%filter(year%in%listyear& indWt!=-1 & spp%in%allspecies)%>%filter(!is.na(age))%>%summarise(nb=n()))
	testmasse<-nbmasse>=param$seuilweight & (testfishLAN|testfishDIS)


	tabtest2<-data.frame(Type=c("LAN","LAN samples","DIS samples","sizeLAN","sizeDIS","age","weight"),
		Results=c(testCL,testECH,testDIS,testfishLAN,testfishDIS,testage,testmasse),
		Tresholds=c(0,param$seuilsample,param$seuilsampledis,param$seuilfish,param$seuilfish,param$seuilage,param$seuilweight),
		Nb_allyear=c(paste(nbCL,"rows in CL"),
		     paste(nbECH,"LAN samples"),
		     paste(nbDIS,"DIS samples"),
		     paste(nbfishLAN,"fishes in LAN"),
		     paste(nbfishDIS,"fishes in DIS"),
		     paste(nbage,"ages in CA"),
		     paste(nbmasse,"weight in CA")))

	tabtest$Nb_allyear<-tabtest2$Nb_allyear
	tabtest$Results<-tabtest2$Results
	names(tabtest)[4]<-paste0("Nb_",param$currentyear)
	names(tabtest)[5]<-paste0("Nb_",param$currentyear,"to",param$currentyear-param$nbyear)

	#add a test if no sizeLAN then no LAN samples
	if(!tabtest$Results[4]){tabtest$Results[2]<-tabtest$Results[4]}
	#add a test if no lan everything goes FALSE
	if(!tabtest$Results[1]){tabtest$Results[2:7]<-FALSE}

	return(tabtest)
}



#' Stock definition
#'
#' @description Stock definition and information 
#' @param stock,wg stock id and ICES WG
#' @param year processing year
#' @param CLr commercial landing
#' @param CSr commercial sampling 
#' @return list of 2 objects with stock info
#'
#' @export
defstock<-function(stock,wg,year,CLr,CSr,path2precatch="../../precatch.csv"){
	areaciem0<-areaciem[areaciem$newstock==stock,]
	wgparam0<-wgparam[wgparam$newstock==stock&wgparam$wg==wg,]
	#function unit
	if(!grepl(".fu.",tolower(stock))){
		listarea<-paste(gsub("27.","",sort(unique(areaciem0$area[areaciem0$type=="Div"]))),collapse=",")
	}else{
		listarea<-paste(gsub("27.","",sort(unique(areaciem0$area[areaciem0$type=="StatRec"]))),collapse=",")
	}
	commonname0<-paste(reffao[reffao$ESPF_COD==wgparam0$fao,6:5],collapse="/")
	info0<-data.frame(wg=wgparam0$wg,
			  stock=wgparam0$newstock,
			  commonname=commonname0,
			  species=wgparam0$spp,
			  taxon=wgparam0$fao,
			  area=listarea)
	commonname<-paste(reffao[reffao$ESPF_COD%in%unique(CLr@cl$taxon),6:5],collapse="/")
	wyear<-tapply(CLr@cl$landWt,CLr@cl$year,sum,na.rm=T)
	wyear<-data.frame(year=names(wyear),w=wyear/1000)
	#area with fu stuff
		if(!grepl(".fu.",tolower(stock))){
		 	area=paste(eval(parse(text=paste0("unique(sort(c('",paste(gsub("27.","",gsub(",","','",
							     sort(unique(c(CSr@hh$area,CLr@cl$area))))),
							   collapse="','"),"')))"))),collapse=",")
		}else{
		 	area=paste(eval(parse(text=paste0("unique(sort(c('",paste(gsub("27.","",gsub(",","','",
							     sort(unique(c(CSr@hh$rect,CLr@cl$rect))))),
							   collapse="','"),"')))"))),collapse=",")
		}
	nbyear<-length(unique(CLr@cl$year))
	rangeyear<-paste0(range(CLr@cl$year),collapse="-")
	infoyear<-paste0(nbyear,"y (",rangeyear,")")
	info<-data.frame(wg=wg,
		 stock=stock,
		 commonname,
		 species=paste(sort(unique(CSr@sl$spp)),collapse=","),
		 taxon=paste(sort(unique(CLr@cl$taxon)),collapse=","),
		 area=sort(unique(area)),
		 year=infoyear,
		 precatch=findprecatch(path2precatch,CLr,CSr)
	)
	eval(parse(text= paste0("info$catch_t_",year,"<-as.numeric(wyear$w[wyear$year==",year,"])")))
	eval(parse(text= paste0("info$catch_t_",year-1,"<-as.numeric(wyear$w[wyear$year==",year-1,"])")))
	eval(parse(text= paste0("info$catch_t_",year-2,"<-as.numeric(wyear$w[wyear$year==",year-2,"])")))
	#check some stuff and replace
	if(info$species=="NA"){info$species<-info0$species}
	if(info$taxon==""){info$taxon<-info0$taxon}
	if(info$area=="NA"){info$area<-info0$area}
	if(grepl("character",info$commonname)){info$commonname<-info0$commonname}
	if(info$year!=""){
		t1<-info[,c("stock","wg","species","taxon","area")]
		t2<-info0[,c("stock","wg","species","taxon","area")]
	}
	return(list(info,info0))
}

#' Compute precatch
#'
#' @description compute precatch from ICES precatch
#' @param path2precatch path to precatch file
#' @param CLr commercial landing
#' @param CSr commercial sampling 
#' @return precatch value based on CLr area and spp in CSr
#'
#' @export
findprecatch<-function(path2precatch,CLr,CSr){
	if(file.exists(path2precatch)){
		precatch<-read.csv(path2precatch)
		precatch$catch<-apply(precatch[,6:7],1,sum,na.rm=T)
		precatch<-precatch[precatch$Country=="FR",]
		precatch<-data.frame(year=precatch$Year,
				     spp=precatch$Species.Latin.Name,
				     area=tolower(gsub("_",".",precatch$Area)),
				     country=precatch$Country,
				     lan=precatch$catch)
		#species id
		spp0<-unique(CSr@sl$spp)
		area1<-unique(unlist(strsplit(paste0(sort(unique(CLr@cl$area)),collapse=","),",")))
		area2<-unique(unlist(strsplit(paste0(sort(unique(CSr@hh$area)),collapse=","),",")))
		area0<-sort(unique(c(area1,area2)))
		test<-grepl(paste0(spp0,collapse="|"),precatch$spp)
		pretot<-precatch[test,]
		test<-grepl(paste0(area0,collapse="|"),pretot$area)
		pretot<-pretot[test,]
		if(nrow(pretot)==0){pretot<-NA}else{pretot<-sum(pretot$lan,na.rm=T)}
	}else{
		pretot<-NA
	}
	return(pretot)
}

