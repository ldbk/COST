#library(COSTcore)
#library(COSTdbe)
#library(COSTeda)
#library(xtable)
#
###############################################################
####   Part1 -- sub functions for Report() --     #####
###############################################################

#fonction pour créer un répertoire de travail --> setwd -> on insère les images du package -> réinitialise l'ancien répertoire de travail
initReport <- function() {
temp <- getwd()
suppressWarnings(dir.create("Report"))
setwd(paste(temp,"/Report",sep=""))
invisible(file.copy(from=paste(R.home(),"/library/COSTreport/cost1.jpg",sep=""),to="cost1.jpg"))
invisible(file.copy(from=paste(R.home(),"/library/COSTreport/cost2.jpg",sep=""),to="cost2.jpg"))
return(temp)
}                                           

#fonction pour effacer tous les fichiers créés hors .pdf
deleteReport <- function(){                                        #& length(grep("logo",x))==0 
List <- list.files()
ind <- sapply(List,function(x) {if ( (length(grep(".pdf",x))==0 ) | length(grep("-",x))>0 ) return(FALSE) else return(TRUE)})
for (i in 1:length(ind)){
if (!ind[i]) file.remove(List[i])
}
}

## un peu de modification sur dfApply ##
dfApply1 <- function(tab,valueField,rowFields,colField,fun,...){
if (length(colField)!=1) stop("wrong 'rowField' parameter!!")
if (length(valueField)!=1) stop("wrong 'valueField' parameter!!")
mat <- tapply(tab[,valueField],list(apply(tab[,rowFields,drop=FALSE],1,paste,collapse=":-:"),tab[,colField]),fun,...)
mat1 <- do.call("rbind",lapply(rownames(mat),function(x) strsplit(x,":-:")[[1]]))
df1 <- as.data.frame(mat1) ; names(df1) <- rowFields
df2 <- as.data.frame(mat) ; rownames(df2) <- NULL
df <- cbind.data.frame(df1,df2)
return(df)
}

## -- transformer() met les sortie statistiques de disInfo en data.frame (format standart dans Report() ) -- ##
transformer=function(val){
  df <- cbind(expand.grid(dimnames(val)),as.vector(val))
  names(df) <- c("technical","time","space","value")
  dfApply1(df,"value",c("space","technical"),"time",function(x) x)
}

### --- printCOST() permet de transformer un dataframe a 3 dimension en latex --- ###
printCOST <- function(x,caption=NULL,language="FR",...){
    #load("N:/COST/Tian/Finished-Report()/fieldTitle.RData")
    data(fieldTitle)
    sink (...)
    dataset <- x
    colName=colnames(dataset)
    liste=split(dataset[-1],dataset[[1]])
    n=names(liste)
    cat("\n\\begin{table}[bh!]
         \n\\begin{center}
         \n\\begin{tabular}{*{",ncol(liste[[1]])+1,"}{l}}
         \n\\hline")
    cat("\n",as.character(fieldTitle[26,language]),"&",as.character(fieldTitle[27,language]))
    for( i in 3:length(colName) ) {
       cat("&",colName[i]) }
    cat("\\","\\","\n",sep="")
    cat("\\hline")
    for( i in 1:length(liste) ){
      d=dim( liste[[i]] )
      cat("\n\\multirow{",d[1],"}{*}{",n[i],"}")
      for(j in 1:d[1]){
        for(k in 1:d[2]){
          cat("&",liste[[i]][j,k])
        }
        cat("\\","\\","\n",sep="")
      }
      cat("\n\\hline")
    }
    cat("\n\\end{tabular}
         \n\\caption{",caption,"}
         \n\\end{center}
         \n\\end{table}\n")
           sink()
}

################################################################################
################################################################################


###############################################################
####       Part2 -- Report() for DataVal --     #####
###############################################################

disINFO <- function(object,field,by,fun,...,biopar=FALSE,fieldCorr,preChar,postChar="",language,classe,step=0) {   #classe="cs", "ca", "cl" ou "ce"  , preChar is from fieldTitle
                                                                                                                   #step if a 4th field is to be addded in by (eg 'lenCls' or 'age')
  if (classe=="cs") { object@hh$foVal <- "all" ; repl <- "foVal"}
  if (classe=="ca") { object@ca$matMeth <- "all" ; repl <- "matMeth"}
  if (classe=="cl") { object@cl$all <- "all" ; repl <- "all"}
  if (classe=="ce") { object@ce$all <- "all" ; repl <- "all"}

  by[is.na(by)] <- repl

  if (classe%in%c("cs","ca")) {
    res <- disInfo(object,path=".txt",field=field,by=by,fun=fun,biopar=biopar,append=NA)$result
  } else {
    res <- disInfo(object,path=".txt",field=field,by=by,fun=fun,append=NA)$result
  }

  BY <- by

  for (i in (step+c(1:3))) {if (is.null(dimnames(res)[[i]]) | all(dimnames(res)[[i]]%in%"all")) {BY[i] <- NA ; by[i] <- repl}}

  BY <- BY[step+c(1:3)]
  BY <- BY[!is.na(BY)]
  fields <- fieldCorr[match(BY,fieldCorr$Field),language]
  bY <- c("par ","by ") ; names(bY) <- c("FR","EN")
  if (length(fields)>0) fields <- paste(bY[language],paste(fields,collapse=", ",sep=""),sep="")

  if (classe%in%c("cs","ca")) {
    return(disInfo(object,path=".txt",field=field,by=by,fun=fun,biopar=biopar,append=NA,title=paste(preChar,fields,postChar)))
  } else {
    return(disInfo(object,path=".txt",field=field,by=by,fun=fun,append=NA,title=paste(preChar,fields,postChar)))
  }
}

###  ------------------------------------------------------------  ###
# --csValOutput(),clValOutput(),ceValOutput() stockent les statistiques  -- #
###  ------------------------------------------------------------  ###
csValOutput=function(strDef,cs.val,language="FR"){                      #ou language="EN"
#load("N:/COST/Tian/Finished-Report()/fieldCorr.RData")  #à remplacer dans le package par 'data(fieldCorr)'
data(fieldCorr)
#load("N:/COST/Tian/Finished-Report()/fieldTitle.RData")  #à remplacer dans le package par 'data(fieldTitle)'
data(fieldTitle)                    
#market sampling data
  cs.val.M <- COSTcore:::subset(cs.val,sampType%in%c("M","V"))
  cs.valM1 <- disINFO(cs.val.M,path=".txt",field=c("subSampWt"),by=c(strDef@techStrata,strDef@timeStrata,strDef@spaceStrata),function(x) sum(!is.na(x) & (x>0)),
                      fieldCorr=fieldCorr,preChar=fieldTitle[1,language],language=language,classe="cs")
  cs.valM2 <- disINFO(cs.val.M,path=".txt",field=c("subSampWt"),by=c(strDef@techStrata,strDef@timeStrata,strDef@spaceStrata),sum,na.rm=TRUE,
                      fieldCorr=fieldCorr,preChar=fieldTitle[2,language],postChar="(g)",language=language,classe="cs")
  cs.valM3 <- disINFO(cs.val.M,path=".txt",field=c("trpCode"),by=c(strDef@techStrata,strDef@timeStrata,strDef@spaceStrata),function(x) sum(!is.na(x) & (x>0)),
                      fieldCorr=fieldCorr,preChar=fieldTitle[3,language],language=language,classe="cs")
  cs.valM4 <- disINFO(cs.val.M,path=".txt",field=c("lenNum"),by=c(strDef@techStrata,strDef@timeStrata,strDef@spaceStrata),function(x) sum(!is.na(x) & (x>0)),
                      fieldCorr=fieldCorr,preChar=fieldTitle[12,language],language=language,classe="cs")
  cs.valM5 <- disINFO(cs.val.M,path=".txt",field=c("age"),by=c(NA,strDef@timeStrata,strDef@spaceStrata),function(x) sum(!is.na(x)),biopar=TRUE,
                      fieldCorr=fieldCorr,preChar=fieldTitle[5,language],postChar="(age)",language=language,classe="ca")
  cs.valM6 <- disINFO(cs.val.M,path=".txt",field=c("indWt"),by=c(NA,strDef@timeStrata,strDef@spaceStrata),sum,biopar=TRUE,
                      fieldCorr=fieldCorr,preChar=fieldTitle[6,language],postChar="(age)",language=language,classe="ca")
#sea sampling data
  cs.val.S <- COSTcore:::subset(cs.val,sampType%in%"S")
  cs.valS1 <- disINFO(cs.val.S,path=".txt",field=c("trpCode"),by=c(strDef@techStrata,strDef@timeStrata,strDef@spaceStrata),function(x) length(unique(x)),
                      fieldCorr=fieldCorr,preChar=fieldTitle[3,language],postChar=c("(échantillonnage en mer)"[language=="FR"],"(at-sea sampling)"[language=="EN"]),
                      language=language,classe="cs")
  cs.valS2 <- disINFO(cs.val.S,path=".txt",field=c("trpCode","staNum"),by=c(strDef@techStrata,strDef@timeStrata,strDef@spaceStrata),function(x) length(unique(x)),
                      fieldCorr=fieldCorr,preChar=fieldTitle[5,language],language=language,classe="cs")
  cs.valS3 <- disINFO(cs.val.S,path=".txt",field=c("foDur"),by=c(strDef@techStrata,strDef@timeStrata,strDef@spaceStrata),sum,na.rm=TRUE,
                      fieldCorr=fieldCorr,preChar=fieldTitle[8,language],postChar="(mn)",language=language,classe="cs")
  cs.valS4 <- disINFO(cs.val.S,path=".txt",field=c("lenNum"),by=c(strDef@techStrata,strDef@timeStrata,strDef@spaceStrata),sum,na.rm=TRUE,
                      fieldCorr=fieldCorr,preChar=fieldTitle[12,language],postChar=c("(échantillonnage en mer)"[language=="FR"],"(at-sea sampling)"[language=="EN"]),
                      language=language,classe="cs")
#discards
  cs.val.SD <- suppressWarnings(subsetSpp(cs.val.S,catchCat%in%"DIS"))
#landing
  cs.val.SL <- suppressWarnings(subsetSpp(cs.val.S,catchCat%in%"LAN"))
  cs.valS5 <- disINFO(cs.val.SL,path=".txt",field=c("lenNum"),by=c(strDef@techStrata,strDef@timeStrata,strDef@spaceStrata),sum,na.rm=TRUE,
                      fieldCorr=fieldCorr,preChar=fieldTitle[28,language],language=language,classe="cs")
  cs.valS6 <- disINFO(cs.val.SD,path=".txt",field=c("lenNum"),by=c(strDef@techStrata,strDef@timeStrata,strDef@spaceStrata),sum,na.rm=TRUE,
                      fieldCorr=fieldCorr,preChar=fieldTitle[29,language],language=language,classe="cs")
  cs.valS7 <- disINFO(cs.val.S,path=".txt",field=c("lenNum"),by=c(strDef@techStrata,strDef@timeStrata,strDef@spaceStrata),sum,na.rm=TRUE,
                      fieldCorr=fieldCorr,preChar=fieldTitle[30,language],language=language,classe="cs")
  cs.valList=list(cs.valM1,cs.valM2,cs.valM3,cs.valM4,cs.valM5,cs.valM6,cs.valS1,cs.valS2,cs.valS3,cs.valS4,cs.valS5,cs.valS6,cs.valS7)
  return (cs.valList)
}

################################################################################

ceValOutput=function(strDef,ce.val,language="FR"){
#load("N:/COST/Tian/Finished-Report()/fieldCorr.RData")  #à remplacer dans le package par 'data(fieldCorr)'
data(fieldCorr)
#load("N:/COST/Tian/Finished-Report()/fieldTitle.RData")  #à remplacer dans le package par 'data(fieldTitle)'
data(fieldTitle)

  ce.val1 <- disINFO(ce.val,path=".txt",field="trpNum",by=c(strDef@techStrata,strDef@timeStrata,strDef@spaceStrata),sum,na.rm=TRUE,
                     fieldCorr=fieldCorr,preChar=fieldTitle[9,language],language=language,classe="ce")
  ce.val2 <- disINFO(ce.val,path=".txt",field="daysAtSea",by=c(strDef@techStrata,strDef@timeStrata,strDef@spaceStrata),length,
                     fieldCorr=fieldCorr,preChar=fieldTitle[11,language],language=language,classe="ce")
  ce.valList <- list(ce.val1,ce.val2)
  return (ce.valList)
}

################################################################################

clValOutput=function(strDef,cl.val,language="FR"){
#load("N:/COST/Tian/Finished-Report()/fieldCorr.RData")  #à remplacer dans le package par 'data(fieldCorr)'
data(fieldCorr)
#load("N:/COST/Tian/Finished-Report()/fieldTitle.RData")  #à remplacer dans le package par 'data(fieldTitle)'
data(fieldTitle)
  cl.val1 <- disINFO(cl.val,path=".txt",field="landWt",by=c(strDef@techStrata,strDef@timeStrata,strDef@spaceStrata),sum,na.rm=TRUE,
                     fieldCorr=fieldCorr,preChar=fieldTitle[10,language],language=language,classe="cl")
  cl.valList <- list(cl.val1)
  return (cl.valList)
}

##  --- creation de l'objet "statConsOutput" pour stocker les statistiques --- ###
setClass("statValOutput",representation(cs.val="list",cl.val="list",ce.val="list"))

##-- mainVal() sort le .tex final pour csDataVal,clDataVal et ceDataVal --##
mainVal=function (fileList)
{   sink("mainVal.tex")
    cat("\n\\documentclass{article}
         \n\\usepackage[T1]{fontenc}
         \n\\usepackage[a4paper]{geometry}
         \n\\geometry{verbose,tmargin=2cm,bmargin=1.5cm,lmargin=2cm,rmargin=3cm}
         \n\\usepackage{epsfig}
         \n\\usepackage{graphicx}
         \n\\usepackage{multirow}
         \n\\usepackage{hyperref}
         \n\\usepackage{fancyhdr}
         \n\\setlength{\\textheight}{10in}
         \n\\begin{document}\n")
    for (i in fileList) {
        cat("\\input{", i, "}\n", sep = "")
    }
    cat("\n\\clearpage
         \n\\normalsize\\listoftables
         \n\\end{document}")
    sink()
}

## -- firstInfo() et firstPage() pour 1ème page de rapport -- ##
firstInfo=function(dataName){
  stock=dataName@desc
  if ( class(dataName)=="csDataVal" ) {
      vyear=dataName@tr$year
      vcountry=dataName@tr$landCtry }
  if ( class(dataName)=="clDataVal" ) {
      vyear=dataName@cl$year
      vcountry=dataName@cl$landCtry }
  if ( class(dataName)=="ceDataVal" ) {
      vyear=dataName@ce$year
      vcountry=dataName@ce$vslFlgCtry }
  year=toString(unique(vyear))
  country=toString(unique(vcountry))           # pour recuperer les info sur stock,year,country
  info=c(stock,year,country)
  return (info)
}

firstPage=function (x,fileName,language="FR",...)
{   
#load("N:/COST/Tian/Finished-Report()/fieldTitle.RData")
data(fieldTitle)
    sink(fileName)
    v=firstInfo(x)
    cat("\n\\clearpage
         \n\\pagestyle{fancy}
         \n\\fancyhf{}")
    if ( file.exists("logo.jpg")==TRUE) {
     cat("\n\\fancyhead[R]{","\\","includegraphics[height=30pt]{logo.jpg}}",sep="")
     cat("\n\\renewcommand{","\\","headrulewidth}{0.5pt}",sep="")
    }
    if ( file.exists("logo.jpg")==FALSE) {
     cat("\n\\renewcommand{","\\","headrulewidth}{0.4pt}",sep="")
    }
    cat("\n\\lfoot{","\\","LARGE Created by COST}",sep="")
    cat("\n\\rfoot{","\\","LARGE ","\\","today}",sep="")
    cat("\n\\renewcommand{","\\","footrulewidth}{1pt}",sep="")
    cat("\n\\centering
         \n\\Huge")
    if ( class(x)=="csDataVal") cat("\n\\textit{",as.character(fieldTitle[15,language]),"\\","\\","(status : ",as.character(fieldTitle[18,language]),")}",sep="")
    if ( class(x)=="clDataVal") cat("\n\\textit{",as.character(fieldTitle[16,language]),"\\","\\","(status : ",as.character(fieldTitle[18,language]),")}",sep="")
    if ( class(x)=="ceDataVal") cat("\n\\textit{",as.character(fieldTitle[17,language]),"\\","\\","(status : ",as.character(fieldTitle[18,language]),")}",sep="")
    cat("\n\\Huge
         \n\\begin{center}
         \n\\begin{tabular}{lr}
         \n\\hline
         \n stock & ",v[1])
    cat("\\","\\","\n",sep="")
    cat(as.character(fieldTitle[21,language]),"&",v[2])
    cat("\\","\\","\n",sep="")
    cat(as.character(fieldTitle[22,language]),"&",v[3])
    cat("\\","\\","\n",sep="")
    cat("\n\\hline
         \n\\end{tabular}
         \n\\end{center}
         \n\\begin{figure}[bb!]")
    cat("\n\\includegraphics[width=0.35","\\","textwidth]{cost1.jpg}\n",sep="")
    cat("\\hfill
         \\includegraphics[width=0.35","\\","textwidth]{cost2.jpg}",sep="")
    cat("\n\\end{figure}
         \n\\newpage")
    sink()
}

## --- sectionCsVal() separe market,biological et sea sampling --- ##
sectionCsVal=function (section1,section2,section3,language="FR")
{   
#load("N:/COST/Tian/Finished-Report()/fieldTitle.RData")
data(fieldTitle)
    sink("sectionCsVal.tex")
    cat("\n\\centering")
    cat("\n\\section{",as.character(fieldTitle[23,language]),"}")
    cat("\n\\input{",section1,"}\n",sep="")
    cat("\n\\clearpage
         \n\\centering")
    cat("\n\\section{",as.character(fieldTitle[24,language]),"}")
    cat("\n\\input{",section2,"}\n",sep="")
    cat("\n\\clearpage
         \n\\centering")
    cat("\n\\section{",as.character(fieldTitle[25,language]),"}")
    cat("\n\\input{",section3,"}\n",sep="")
    cat("\n\\clearpage\n")
    sink()
}
     

# -- Report() pour l'objet csDataVal -- #
setGeneric("Report",
   function(w,x,y,z,language="FR",logo=as.character(NA),...)
   standardGeneric("Report")
)

setMethod("Report",signature(w="strIni",x="csDataVal",y="missing",z="missing"),
           function(w,x,y,z,language="FR",logo=as.character(NA),...){
           current <- initReport()
           if (!is.na(logo)) invisible(file.copy(from=logo,to="logo.jpg",overwrite=TRUE))
           cs.valStatList=new("statValOutput",cs.val=csValOutput(w,x,language=language))@cs.val
           for( j in 1:4 ) {
              i=cs.valStatList[[j]]
              len=length(dim(i$result))
              res=transformer(i$result)
              res$technical=gsub("_","\\_",res$technical,fixed=T)
              if( len ==3) printCOST(res,caption=i$title,language=language,file="reportCsValM.tex",append=TRUE)
              else print(xtable(i$result,caption=paste("",i$title)),type="latex",caption.placement="top",file="reportCsValM.tex",append=TRUE)
           }
           for( j in 5:6 ) {
              i=cs.valStatList[[j]]
              len=length(dim(i$result))
              res=transformer(i$result)
              res$technical=gsub("_","\\_",res$technical,fixed=T)
              if( len ==3) printCOST(res,caption=i$title,language=language,file="reportCsValB.tex",append=TRUE)
              else print(xtable(i$result,caption=paste("",i$title)),type="latex",caption.placement="top",file="reportCsValM.tex",append=TRUE)
           }
            for( j in 7:10 ) {
              i=cs.valStatList[[j]]
              len=length(dim(i$result))
              res=transformer(i$result)
              res$technical=gsub("_","\\_",res$technical,fixed=T)
              if( len ==3) printCOST(res,caption=i$title,language=language,file="reportCsValS.tex",append=TRUE)
              else print(xtable(i$result,caption=paste("",i$title)),type="latex",caption.placement="top",file="reportCsValS.tex",append=TRUE)
           }
           firstPage(x,"firstCsVal.tex",language=language)
           sectionCsVal("reportCsValM.tex","reportCsValB.tex","reportCsValS.tex",language=language)
           mainVal(c("firstCsVal.tex","sectionCsVal.tex"))
           system("pdflatex mainVal.tex")
           system("pdflatex mainVal.tex")
           deleteReport()
           setwd(current)        
})
################################################################################

# -- Report() pour l'objet clDataVal -- #
setMethod("Report",signature(w="strIni",x="clDataVal",y="missing",z="missing"),
           function(w,x,y,z,language="FR",logo=as.character(NA),...){
           current <- initReport()
           if (!is.na(logo)) invisible(file.copy(from=logo,to="logo.jpg",overwrite=TRUE))
           cl.valStatList=new("statValOutput",cl.val=clValOutput(w,x,language=language))@cl.val
           for( i in cl.valStatList ) {
                res=transformer(i$result)
                res$technical=gsub("_","\\_",res$technical,fixed=T)
                printCOST(res,caption=i$title,language=language,file="reportClVal.tex",append=TRUE)
           }
           firstPage(x,"firstClVal.tex",language=language)
           mainVal(c("firstClVal.tex","reportClVal.tex"))
           system("pdflatex mainVal.tex")
           system("pdflatex mainVal.tex")
           deleteReport()
           setwd(current)
})
################################################################################

# -- Report() pour l'objet ceDataVal -- #
setMethod("Report",signature(w="strIni",x="ceDataVal",y="missing",z="missing"),
           function(w,x,y,z,language="FR",logo=as.characetr(NA),...){
           current <- initReport()
           if (!is.na(logo)) invisible(file.copy(from=logo,to="logo.jpg",overwrite=TRUE))
           ce.valStatList=new("statValOutput",ce.val=ceValOutput(w,x,language=language))@ce.val
           for( i in ce.valStatList ) {
                res=transformer(i$result)
                res$technical=gsub("_","\\_",res$technical,fixed=T)
                printCOST(res,caption=i$title,language=language,file="reportCeVal.tex",append=TRUE)
           }
           firstPage(x,"firstCeVal.tex",language=language)
           mainVal(c("firstCeVal.tex","reportCeVal.tex"))
           system("pdflatex mainVal.tex")
           system("pdflatex mainVal.tex")
           deleteReport()
           setwd(current)
})
################################################################################

# -- Report() pour l'objet csDataVal,clDataVal -- #
setMethod("Report",signature(w="strIni",x="csDataVal",y="clDataVal",z="missing"),
           function(w,x,y,z,language="FR",logo=as.character(NA),...){
           current <- initReport()
           if (!is.na(logo)) invisible(file.copy(from=logo,to="logo.jpg",overwrite=TRUE))
           cs.valStatList=new("statValOutput",cs.val=csValOutput(w,x,language=language))@cs.val
           for( j in 1:4 ) {
              i=cs.valStatList[[j]]
              len=length(dim(i$result))
              res=transformer(i$result)
              res$technical=gsub("_","\\_",res$technical,fixed=T)
              if( len ==3) printCOST(res,caption=i$title,language=language,file="reportCsValM.tex",append=TRUE)
              else print(xtable(i$result,caption=paste("",i$title)),type="latex",caption.placement="top",file="reportCsValM.tex",append=TRUE)
           }
           for( j in 5:6 ) {
              i=cs.valStatList[[j]]
              len=length(dim(i$result))
              res=transformer(i$result)
              res$technical=gsub("_","\\_",res$technical,fixed=T)
              if( len ==3) printCOST(res,caption=i$title,language=language,file="reportCsValB.tex",append=TRUE)
              else print(xtable(i$result,caption=paste("",i$title)),type="latex",caption.placement="top",file="reportCsValM.tex",append=TRUE)
           }
           for( j in 7:10 ) {
              i=cs.valStatList[[j]]
              len=length(dim(i$result))
              res=transformer(i$result)
              res$technical=gsub("_","\\_",res$technical,fixed=T)
              if( len ==3) printCOST(res,caption=i$title,language=language,file="reportCsValS.tex",append=TRUE)
              else print(xtable(i$result,caption=paste("",i$title)),type="latex",caption.placement="top",file="reportCsValS.tex",append=TRUE)
           }

           cl.valStatList=new("statValOutput",cl.val=clValOutput(w,y,language=language))@cl.val
           for( i in cl.valStatList ) {
                res=transformer(i$result)
                res$technical=gsub("_","\\_",res$technical,fixed=T)
                printCOST(res,caption=i$title,language=language,file="reportClVal.tex",append=TRUE)
           }

           firstPage(x,"firstCsVal.tex",language=language)
           sectionCsVal("reportCsValM.tex","reportCsValB.tex","reportCsValS.tex",language=language)
           firstPage(y,"firstClVal.tex",language=language)
           mainVal(c("firstCsVal.tex","sectionCsVal","firstClVal.tex","reportClVal.tex"))
           system("pdflatex mainVal.tex")
           system("pdflatex mainVal.tex")
           deleteReport()
           setwd(current)
})
################################################################################

# -- Report() pour l'objet csDataVal,ceDataVal -- #
setMethod("Report",signature(w="strIni",x="csDataVal",y="ceDataVal",z="missing"),
           function(w,x,y,z,language="FR",logo=as.character(NA),...){
           current <- initReport()
           if (!is.na(logo)) invisible(file.copy(from=logo,to="logo.jpg",overwrite=TRUE))
           cs.valStatList=new("statValOutput",cs.val=csValOutput(w,x,language=language))@cs.val
           for( j in 1:4 ) {
              i=cs.valStatList[[j]]
              len=length(dim(i$result))
              res=transformer(i$result)
              res$technical=gsub("_","\\_",res$technical,fixed=T)
              if( len ==3) printCOST(res,caption=i$title,language=language,file="reportCsValM.tex",append=TRUE)
              else print(xtable(i$result,caption=paste("",i$title)),type="latex",caption.placement="top",file="reportCsValM.tex",append=TRUE)
           }
           for( j in 5:6 ) {
              i=cs.valStatList[[j]]
              len=length(dim(i$result))
              res=transformer(i$result)
              res$technical=gsub("_","\\_",res$technical,fixed=T)
              if( len ==3) printCOST(res,caption=i$title,language=language,file="reportCsValB.tex",append=TRUE)
              else print(xtable(i$result,caption=paste("",i$title)),type="latex",caption.placement="top",file="reportCsValM.tex",append=TRUE)
           }
           for( j in 7:10 ) {
              i=cs.valStatList[[j]]
              len=length(dim(i$result))
              res=transformer(i$result)
              res$technical=gsub("_","\\_",res$technical,fixed=T)
              if( len ==3) printCOST(res,caption=i$title,language=language,file="reportCsValS.tex",append=TRUE)
              else print(xtable(i$result,caption=paste("",i$title)),type="latex",caption.placement="top",file="reportCsValS.tex",append=TRUE)
           }

           ce.valStatList=new("statValOutput",ce.val=ceValOutput(w,y,language=language))@ce.val
           for( i in cl.valStatList ) {
                res=transformer(i$result)
                res$technical=gsub("_","\\_",res$technical,fixed=T)
                printCOST(res,caption=i$title,language=language,file="reportCeVal.tex",append=TRUE)
           }

           firstPage(x,"firstCsVal.tex",language=language)
           section("reportCsValM.tex","reportCsValB.tex","reportCsValS.tex",language=language)
           firstPage(y,"firstCeVal.tex",language=language)
           mainVal(c("firstCsVal.tex","sectionCsVal","firstCeVal.tex","reportCeVal.tex"))
           system("pdflatex mainVal.tex")
           system("pdflatex mainVal.tex")
           deleteReport()
           setwd(current)
})
################################################################################

# -- Report() pour l'objet clDataVal,ceDataVal -- #
setMethod("Report",signature(w="strIni",x="clDataVal",y="ceDataVal",z="missing"),
           function(w,x,y,z,language="FR",logo=as.character(NA),...){
           current <- initReport()
           if (!is.na(logo)) invisible(file.copy(from=logo,to="logo.jpg",overwrite=TRUE))
           cl.valStatList=new("statValOutput",cl.val=clValOutput(w,x,language=language))@cl.val
           ce.valStatList=new("statValOutput",ce.val=ceValOutput(w,y,language=language))@ce.val
           for( i in cl.valStatList ) {
                res=transformer(i$result)
                res$technical=gsub("_","\\_",res$technical,fixed=T)
                printCOST(res,caption=i$title,language=language,file="reportClVal.tex",append=TRUE)
           }
            for( i in ce.valStatList ) {
                res=transformer(i$result)
                res$technical=gsub("_","\\_",res$technical,fixed=T)
                printCOST(res,caption=i$title,language=language,file="reportCeVal.tex",append=TRUE)
           }
           firstPage(x,"firstClVal.tex",language=language)
           firstPage(y,"firstCeVal.tex",language=language)
           mainVal(c("firstClVal.tex","reportClVal.tex","firstCeVal.tex","reportCeVal.tex"))
           system("pdflatex mainVal.tex")
           system("pdflatex mainVal.tex")
           deleteReport()
           setwd(current)
})
################################################################################

# -- Report() pour l'objet csDataVal,clDataVal et ceDataVal-- #
setMethod("Report",signature(w="strIni",x="csDataVal",y="clDataVal",z="ceDataVal"),
           function(w,x,y,z,language="FR",logo=as.character(NA),...){
           current <- initReport()
           if (!is.na(logo)) invisible(file.copy(from=logo,to="logo.jpg",overwrite=TRUE))
           cs.valStatList=new("statValOutput",cs.val=csValOutput(w,x,language=language))@cs.val
           for( j in 1:4 ) {
              i=cs.valStatList[[j]]
              len=length(dim(i$result))
              res=transformer(i$result)
              res$technical=gsub("_","\\_",res$technical,fixed=T)
              if( len ==3) printCOST(res,caption=i$title,language=language,file="reportCsValM.tex",append=TRUE)
              else print(xtable(i$result,caption=paste("",i$title)),type="latex",caption.placement="top",file="reportCsValM.tex",append=TRUE)
           }
           for( j in 5:6 ) {
              i=cs.valStatList[[j]]
              len=length(dim(i$result))
              res=transformer(i$result)
              res$technical=gsub("_","\\_",res$technical,fixed=T)
              if( len ==3) printCOST(res,caption=i$title,language=language,file="reportCsValB.tex",append=TRUE)
              else print(xtable(i$result,caption=paste("",i$title)),type="latex",caption.placement="top",file="reportCsValM.tex",append=TRUE)
           }
           for( j in 7:10 ) {
              i=cs.valStatList[[j]]
              len=length(dim(i$result))
              res=transformer(i$result)
              res$technical=gsub("_","\\_",res$technical,fixed=T)
              if( len ==3) printCOST(res,caption=i$title,language=language,file="reportCsValS.tex",append=TRUE)
              else print(xtable(i$result,caption=paste("",i$title)),type="latex",caption.placement="top",file="reportCsValS.tex",append=TRUE)
           }

           cl.valStatList=new("statValOutput",cl.val=clValOutput(w,y,language=language))@cl.val
           for( i in cl.valStatList ) {
                res=transformer(i$result)
                res$technical=gsub("_","\\_",res$technical,fixed=T)
                printCOST(res,caption=i$title,language=language,file="reportClVal.tex",append=TRUE)
           }

           ce.valStatList=new("statValOutput",ce.val=ceValOutput(w,z,language=language))@ce.val
           for( i in ce.valStatList ) {
                res=transformer(i$result)
                res$technical=gsub("_","\\_",res$technical,fixed=T)
                printCOST(res,caption=i$title,language=language,file="reportCeVal.tex",append=TRUE)
           }

           firstPage(x,"firstCsVal.tex",language=language)
           sectionCsVal("reportCsValM.tex","reportCsValB.tex","reportCsValS.tex",language=language)
           firstPage(y,"firstClVal.tex",language=language)
           firstPage(z,"firstCeVal.tex",language=language)
           mainVal(c("firstCsVal.tex","sectionCsVal","firstClVal.tex","reportClVal.tex","firstCeVal.tex","reportCeVal.tex"))
           system("pdflatex mainVal.tex")
           system("pdflatex mainVal.tex")
           deleteReport()
           setwd(current)
})

################################################################################
################################################################################


###############################################################
####       Part3 -- Report() for DataCons --     #####
###############################################################

disINFOcons <- function(object,field,by,fun,...,preChar,postChar="",language,classe,strDef) {   #classe="cs", "ca", "cl" ou "ce"  , preChar is from fieldTitle
                                                                                                                   #step if a 4th field is to be addded in by (eg 'lenCls' or 'age')
 # load("H:/Finished-Report()/fieldCorr.RData")  #à remplacer dans le package par 'data(fieldCorr)'
 data(fieldCorr)
  biopar <- FALSE
  if (classe=="ca") biopar <- TRUE
  if (classe%in%c("cs","ca")) {
    if (classe=="ca") object@ca$technical <- "all"
    res <- disInfo(object,path=".txt",field=field,by=by,fun=fun,biopar=biopar,append=NA)$result
  } else {
    res <- disInfo(object,path=".txt",field=field,by=by,fun=fun,append=NA)$result
  }
  BY <- c(strDef@techStrata,strDef@timeStrata,strDef@spaceStrata)
  for (i in 1:3) {if (is.null(dimnames(res)[[i]]) | all(dimnames(res)[[i]]%in%"all")) BY[i] <- NA}
  BY <- BY[!is.na(BY)]
  fields <- fieldCorr[match(BY,fieldCorr$Field),language]
  bY <- c("par ","by ") ; names(bY) <- c("FR","EN")
  if (length(fields)>0) fields <- paste(bY[language],paste(fields,collapse=", ",sep=""),sep="")
  if (classe%in%c("cs","ca")) {
    return(disInfo(object,path=".txt",field=field,by=by,fun=fun,biopar=biopar,append=NA,title=paste(preChar,fields,postChar)))
  } else {
    return(disInfo(object,path=".txt",field=field,by=by,fun=fun,append=NA,title=paste(preChar,fields,postChar)))
  }
}

###  csConsOutput(),clConsOutput(),ceConsOutput() stock les statistiques  ###

csConsOutput=function(strDef,cs.cons,language="FR"){
#load("N:/COST/Tian/Finished-Report()/fieldCorr.RData")  #à remplacer dans le package par 'data(fieldCorr)'
data(fieldCorr)
#load("N:/COST/Tian/Finished-Report()/fieldTitle.RData")  #à remplacer dans le package par 'data(fieldTitle)'
data(fieldTitle)
#market sampling data
  cs.cons.M <- COSTcore:::subset(cs.cons,sampType%in%c("M","V"))
  cs.consM1 <- disINFOcons(cs.cons.M,path=".txt",field="subSampWt",by=c("technical","time","space"),function(x) sum(!is.na(x)),
                          preChar=fieldTitle[1,language],language=language,classe="cs",strDef=strDef)
  cs.consM2 <- disINFOcons(cs.cons.M,path=".txt",field="lenNum",by=c("technical","time","space"),sum,na.rm=TRUE,
                              preChar=fieldTitle[12,language],language=language,classe="cs",strDef=strDef)
  cs.consM3 <- disINFOcons(cs.cons.M,path=".txt",field="subSampWt",by=c("technical","time","space"),sum,na.rm=TRUE,
                              preChar=fieldTitle[2,language],postChar="(g)",language=language,classe="cs",strDef=strDef)
  cs.consB1 <- disINFOcons(cs.cons.M,path=".txt",field=c("age"),by=c("technical","time","space"),function(x) sum(!is.na(x)),biopar=TRUE,
                      fieldCorr=fieldCorr,preChar=fieldTitle[5,language],postChar="(age)",language=language,classe="ca",strDef=strDef)
  cs.consB2 <- disINFOcons(cs.cons.M,path=".txt",field=c("indWt"),by=c("technical","time","space"),sum,biopar=TRUE,
                      fieldCorr=fieldCorr,preChar=fieldTitle[6,language],postChar="(age)",language=language,classe="ca",strDef=strDef)
#sea sampling data
  cs.cons.S <- COSTcore:::subset(cs.cons,sampType%in%"S")
  cs.consS1 <- disINFOcons(cs.cons.S,path=".txt",field="trpCode",by=c("technical","time","space"),function(x) sum(!is.na(x)),
                              preChar=fieldTitle[3,language],language=language,classe="cs",strDef=strDef)
  cs.consS2 <- disINFOcons(cs.cons.S,path=".txt",field="daysAtSea",by=c("technical","time","space"),sum,na.rm=TRUE,
                              preChar=fieldTitle[11,language],language=language,classe="cs",strDef=strDef)
  cs.consS3 <- disINFOcons(cs.cons.S,path=".txt",field="foNum",by=c("technical","time","space"),sum,na.rm=TRUE,
                              preChar=fieldTitle[13,language],language=language,classe="cs",strDef=strDef)
  cs.consS4 <- disINFOcons(cs.cons.S,path=".txt",field="foDur",by=c("technical","time","space"),sum,na.rm=TRUE,
                          preChar=fieldTitle[8,language],postChar="(mn)",language=language,classe="cs",strDef=strDef)
  cs.consS5 <- disINFOcons(cs.cons.S,path=".txt",field="subSampWt",by=c("technical","time","space"),sum,na.rm=TRUE,
                              preChar=fieldTitle[2,language],postChar="(g)",language=language,classe="cs",strDef=strDef)

  cs.cons.S_bis <- cs.cons.S
  cs.cons.S_bis@hl$sampType <- sapply(as.character(cs.cons.S_bis@hl$sort),function(x) strsplit(x,"-")[[1]][1])
  cs.cons.S_bis@sl$sampType <- sapply(as.character(cs.cons.S_bis@sl$sort),function(x) strsplit(x,"-")[[1]][1])
  cs.cons.S_bis@tr$sampType <- cs.cons.S_bis@hh$sampType <- "LAN"
  cs.cons.S_lan <- COSTcore:::subset(cs.cons.S_bis,sampType%in%"LAN")
  cs.cons.S_bis@tr$sampType <- cs.cons.S_bis@hh$sampType <- "DIS"
  cs.cons.S_dis <- COSTcore:::subset(cs.cons.S_bis,sampType%in%"DIS")
  cs.consS6 <- disINFOcons(cs.cons.S_lan,path=".txt",field="lenNum",by=c("technical","time","space"),sum,na.rm=TRUE,
                              preChar=fieldTitle[28,language],postChar="",language=language,classe="cs",strDef=strDef)
  cs.consS7 <- disINFOcons(cs.cons.S_dis,path=".txt",field="lenNum",by=c("technical","time","space"),sum,na.rm=TRUE,
                              preChar=fieldTitle[29,language],postChar="",language=language,classe="cs",strDef=strDef)
  cs.consS8 <- disINFOcons(cs.cons.S,path=".txt",field="lenNum",by=c("technical","time","space"),sum,na.rm=TRUE,
                              preChar=fieldTitle[30,language],postChar="",language=language,classe="cs",strDef=strDef)
  cs.consList=list(cs.consM1,cs.consM2,cs.consM3,cs.consB1,cs.consB2,cs.consS1,cs.consS2,cs.consS3,cs.consS4,cs.consS5,cs.consS6,cs.consS7,cs.consS8)
  return (cs.consList)
}
################################################################################

ceConsOutput=function(strDef,ce.cons,language="FR"){
#  load("N:/COST/Tian/Finished-Report()/fieldTitle.RData")
data(fieldTitle)
  ce.cons1 <- disINFOcons(ce.cons,path=".txt",field="trpNum",by=c("technical","time","space"),sum,na.rm=TRUE,
                              preChar=fieldTitle[9,language],language=language,classe="ce",strDef=strDef)
  ce.consList <- list(ce.cons1)
  return (ce.consList)
}
################################################################################

clConsOutput=function(strDef,cl.cons,language="FR"){
#  load("N:/COST/Tian/Finished-Report()/fieldTitle.RData")
data(fieldTitle)
  cl.cons1 <- disINFOcons(cl.cons,path=".txt",field="landWt",by=c("technical","time","space"),sum,na.rm=TRUE,
                              preChar=fieldTitle[10,language],postChar="(kg)",language=language,classe="cl",strDef=strDef)
  cl.consList <- list(cl.cons1)
  return (cl.consList)
}

##  --- creation de l'objet "statConsOutput" pour stocker les listes de statistiques --- ###
setClass("statConsOutput",representation(cs.cons="list",cl.cons="list",ce.cons="list"))

##-- mainCons() sort le .tex final pour clDataCons,ceDataCons --##
mainCons=function (fileList)
{   sink("mainCons.tex")
    cat("\\documentclass{article}
         \n\\usepackage[T1]{fontenc}
         \n\\usepackage[a4paper]{geometry}
         \n\\geometry{verbose,tmargin=2cm,bmargin=1.5cm,lmargin=2cm,rmargin=3cm}
         \n\\usepackage{epsfig}
         \n\\usepackage{graphicx}
         \n\\usepackage{multirow}
         \n\\usepackage{hyperref}
         \n\\usepackage{fancyhdr}
         \n\\setlength{\\textheight}{10in}
         \n\\begin{document}\n")
    for (i in fileList) {
        cat("\\input{", i, "}\n", sep = "")
    }
    cat("\n\\clearpage
         \n\\normalsize\\listoftables
         \n\\end{document}")
    sink()
}

## -- firstInfoCons() et firstPageCons() pour 1ème page de rapport -- ##
firstInfoCons=function(dataName){
  stock=dataName@desc
  if ( class(dataName)=="csDataCons" ) {
      vtime=dataName@tr$time
      vcountry=dataName@tr$landCtry }
  if ( class(dataName)=="clDataCons" ) {
      vtime=dataName@cl$time
      vcountry=dataName@cl$landCtry }
  if ( class(dataName)=="ceDataCons" ) {
      vtime=dataName@ce$time
      vcountry=dataName@ce$vslFlgCtry }
  vyear=strsplit(as.character(unique(vtime)),split=" - " )
  year=character()
  for (i in vyear) {year[i]=i[1]}
  year1=toString(unique(year))
  country=toString(unique(vcountry))           # pour recuperer les info sur stock,year,country
  info=c(stock,year1,country)
  return (info)
}

firstPageCons=function (x,fileName,language="FR",...)
{  
# load("N:/COST/Tian/Finished-Report()/fieldTitle.RData")
data(fieldTitle)
    sink(fileName)
    v=firstInfoCons(x)
    cat("\n\\clearpage
         \n\\pagestyle{fancy}
         \n\\fancyhf{}")
    if ( file.exists("logo.jpg")==TRUE) {
     cat("\n\\fancyhead[R]{","\\","includegraphics[height=30pt]{logo.jpg}}",sep="")
     cat("\n\\renewcommand{","\\","headrulewidth}{0.5pt}",sep="")
    }
    if ( file.exists("logo.jpg")==FALSE) {
     cat("\n\\renewcommand{","\\","headrulewidth}{0.4pt}",sep="")
    }
    cat("\n\\lfoot{","\\","LARGE Created by COST}",sep="")
    cat("\n\\rfoot{","\\","LARGE ","\\","today}",sep="")
    cat("\n\\renewcommand{","\\","footrulewidth}{1pt}",sep="")
    cat("\n\\centering
         \n\\Huge")
    if ( class(x)=="csDataCons") cat("\n\\textit{",as.character(fieldTitle[15,language]),"\\","\\","(status : ",as.character(fieldTitle[19,language]),")}",sep="")
    if ( class(x)=="clDataCons") cat("\n\\textit{",as.character(fieldTitle[16,language]),"\\","\\","(status : ",as.character(fieldTitle[19,language]),")}",sep="")
    if ( class(x)=="ceDataCons") cat("\n\\textit{",as.character(fieldTitle[17,language]),"\\","\\","(status : ",as.character(fieldTitle[19,language]),")}",sep="")
    cat("\n\\Huge
         \n\\begin{center}
         \n\\begin{tabular}{lr}
         \n\\hline
         \n stock & ",v[1])
    cat("\\","\\","\n",sep="")
    cat(as.character(fieldTitle[21,language]),"&",v[2])
    cat("\\","\\","\n",sep="")
    cat(as.character(fieldTitle[22,language]),"&",v[3])
    cat("\\","\\","\n",sep="")
    cat("\n\\hline
         \n\\end{tabular}
         \n\\end{center}
         \n\\begin{figure}[bb!]")
    cat("\n\\includegraphics[width=0.35","\\","textwidth]{cost1.jpg}\n",sep="")
    cat("\\hfill
         \\includegraphics[width=0.35","\\","textwidth]{cost2.jpg}",sep="")
    cat("\n\\end{figure}
         \n\\newpage")
    sink()
}

## --- sectionCsCons() separe market,biological et sea sampling --- ##
sectionCsCons=function (section1,section2,section3,language="FR")
{  
# load("N:/COST/Tian/Finished-Report()/fieldTitle.RData")
data(fieldTitle)
    sink("sectionCsCons.tex")
    cat("\n\\centering")
    cat("\n\\section{",as.character(fieldTitle[23,language]),"}")
    cat("\n\\input{",section1,"}\n",sep="")
    cat("\n\\clearpage
         \n\\centering")
    cat("\n\\section{",as.character(fieldTitle[24,language]),"}")
    cat("\n\\input{",section2,"}\n",sep="")
    cat("\n\\clearpage
         \n\\centering")
    cat("\n\\section{",as.character(fieldTitle[25,language]),"}")
    cat("\n\\input{",section3,"}\n",sep="")
    cat("\n\\clearpage\n")
    sink()
}


# -- Report() pour l'objet csDataCons -- #
setMethod("Report",signature(w="strIni",x="csDataCons",y="missing",z="missing"),
           function(w,x,y,z,language="FR",logo=as.character(NA),...){
           current <- initReport()
           if (!is.na(logo)) invisible(file.copy(from=logo,to="logo.jpg",overwrite=TRUE))
           cs.consStatList=new("statConsOutput",cs.cons=csConsOutput(w,x,language=language))@cs.cons
           for( j in 1:4 ) {
                i=cs.consStatList[[j]]
                res=transformer(i$result)
                res$technical=gsub("_","\\_",res$technical,fixed=T)
                printCOST(res,caption=i$title,language=language,file="reportCsConsM.tex",append=TRUE)
           }
           for( j in 5:6 ) {
                i=cs.consStatList[[j]]
                res=transformer(i$result)
                res$technical=gsub("_","\\_",res$technical,fixed=T)
                printCOST(res,caption=i$title,language=language,file="reportCsConsB.tex",append=TRUE)
           }
           for( j in 7:11 ) {
                i=cs.consStatList[[j]]
                res=transformer(i$result)
                res$technical=gsub("_","\\_",res$technical,fixed=T)
                printCOST(res,caption=i$title,language=language,file="reportCsConsS.tex",append=TRUE)
           }
           firstPageCons(x,"firstCsCons.tex",language=language)
           sectionCsCons("reportCsConsM.tex","reportCsConsB.tex","reportCsConsS.tex",language=language)
           mainCons(c("firstCsCons.tex","sectionCsCons.tex"))
           system("pdflatex mainCons.tex")
           system("pdflatex mainCons.tex")
           deleteReport()
           setwd(current)

})
################################################################################

# -- Report() pour l'objet clDataCons -- #
setMethod("Report",signature(w="strIni",x="clDataCons",y="missing",z="missing"),
           function(w,x,y,z,language="FR",logo=as.character(NA),...){
           current <- initReport()
           if (!is.na(logo)) invisible(file.copy(from=logo,to="logo.jpg",overwrite=TRUE))
           cl.consStatList=new("statConsOutput",cl.cons=clConsOutput(w,x,language=language))@cl.cons
           for( i in cl.consStatList ) {
                res=transformer(i$result)
                res$technical=gsub("_","\\_",res$technical,fixed=T)
                printCOST(res,caption=i$title,language=language,file="reportClCons.tex",append=TRUE)
           }
           firstPageCons(x,"firstClCons.tex",language=language)
           mainCons(c("firstClCons.tex","reportClCons.tex"))
           system("pdflatex mainCons.tex")
           system("pdflatex mainCons.tex")
           deleteReport()
           setwd(current)
})
################################################################################

# -- Report() pour l'objet ceDataCons -- #
setMethod("Report",signature(w="strIni",x="ceDataCons",y="missing",z="missing"),
           function(w,x,y,z,language="FR",logo=as.character(NA),...){
           current <- initReport()
           if (!is.na(logo)) invisible(file.copy(from=logo,to="logo.jpg",overwrite=TRUE))
           if ( w@techStrata != "foCatEu5" ) warning ( "commCat is not available!")
           else {
               ce.consStatList=new("statConsOutput",ce.cons=ceConsOutput(w,x,language=language))@ce.cons
               for( i in ce.consStatList ) {
                  res=transformer(i$result)
                  res$technical=gsub("_","\\_",res$technical,fixed=T)
                  printCOST(res,caption=i$title,language=language,file="reportCeCons.tex",append=TRUE)
               }
               firstPageCons(x,"firstCeCons.tex",language=language)
               mainCons(c("firstCeCons.tex","reportCeCons.tex"))
               system("pdflatex mainCons.tex")
               system("pdflatex mainCons.tex")
           }
           deleteReport()
           setwd(current)
})
################################################################################

# -- Report() pour l'objet csDataCons,clDataCons -- #
setMethod("Report",signature(w="strIni",x="csDataCons",y="clDataCons",z="missing"),
           function(w,x,y,z,language="FR",logo=as.character(NA),...){
           current <- initReport()
           if (!is.na(logo)) invisible(file.copy(from=logo,to="logo.jpg",overwrite=TRUE))
           cs.consStatList=new("statConsOutput",cs.cons=csConsOutput(w,x,language=language))@cs.cons
           for( j in 1:4 ) {
                i=cs.consStatList[[j]]
                res=transformer(i$result)
                res$technical=gsub("_","\\_",res$technical,fixed=T)
                printCOST(res,caption=i$title,language=language,file="reportCsConsM.tex",append=TRUE)
           }
           for( j in 5:6 ) {
                i=cs.consStatList[[j]]
                res=transformer(i$result)
                res$technical=gsub("_","\\_",res$technical,fixed=T)
                printCOST(res,caption=i$title,language=language,file="reportCsConsB.tex",append=TRUE)
           }
           for( j in 7:11 ) {
                i=cs.consStatList[[j]]
                res=transformer(i$result)
                res$technical=gsub("_","\\_",res$technical,fixed=T)
                printCOST(res,caption=i$title,language=language,file="reportCsConsS.tex",append=TRUE)
           }

           cl.consStatList=new("statConsOutput",cl.cons=clConsOutput(w,y,language=language))@cl.cons
           for( i in cl.consStatList ) {
               res=transformer(i$result)
               res$technical=gsub("_","\\_",res$technical,fixed=T)
               printCOST(res,caption=i$title,language=language,file="reportClCons.tex",append=TRUE)
           }

           firstPageCons(x,"firstCsCons.tex",language=language)
           sectionCsCons("reportCsConsM.tex","reportCsConsB.tex","reportCsConsS.tex",language=language)
           firstPageCons(y,"firstClCons.tex",language=language)
           mainCons(c("firstCsCons.tex","sectionCsCons.tex","firstClCons.tex","reportClCons.tex"))
           system("pdflatex mainCons.tex")
           system("pdflatex mainCons.tex")
           deleteReport()
           setwd(current)
})
################################################################################

# -- Report() pour l'objet csDataCons,ceDataCons -- #
setMethod("Report",signature(w="strIni",x="csDataCons",y="ceDataCons",z="missing"),
           function(w,x,y,z,language="FR",logo=as.character(NA),...){
           current <- initReport()
           if (!is.na(logo)) invisible(file.copy(from=logo,to="logo.jpg",overwrite=TRUE))
           cs.consStatList=new("statConsOutput",cs.cons=csConsOutput(w,x,language=language))@cs.cons
           for( j in 1:4 ) {
                i=cs.consStatList[[j]]
                res=transformer(i$result)
                res$technical=gsub("_","\\_",res$technical,fixed=T)
                printCOST(res,caption=i$title,language=language,file="reportCsConsM.tex",append=TRUE)
           }
           for( j in 5:6 ) {
                i=cs.consStatList[[j]]
                res=transformer(i$result)
                res$technical=gsub("_","\\_",res$technical,fixed=T)
                printCOST(res,caption=i$title,language=language,file="reportCsConsB.tex",append=TRUE)
           }
           for( j in 7:11 ) {
                i=cs.consStatList[[j]]
                res=transformer(i$result)
                res$technical=gsub("_","\\_",res$technical,fixed=T)
                printCOST(res,caption=i$title,language=language,file="reportCsConsS.tex",append=TRUE)
           }

           ce.consStatList=new("statConsOutput",ce.cons=ceConsOutput(w,y,language=language))@ce.cons
           for( i in ce.consStatList ) {
               res=transformer(i$result)
               res$technical=gsub("_","\\_",res$technical,fixed=T)
               printCOST(res,caption=i$title,language=language,file="reportCeCons.tex",append=TRUE)
           }

           firstPageCons(x,"firstCsCons.tex",language=language)
           sectionCsCons("reportCsConsM.tex","reportCsConsB.tex","reportCsConsS.tex",language=language)
           firstPageCons(y,"firstCeCons.tex",language=language)
           mainCons(c("firstCsCons.tex","sectionCsCons.tex","firstCeCons.tex","reportCeCons.tex"))
           system("pdflatex mainCons.tex")
           system("pdflatex mainCons.tex")
           deleteReport()
           setwd(current)
})
################################################################################

# -- Report() pour l'objet clDataCons,ceDataCons -- #
setMethod("Report",signature(w="strIni",x="clDataCons",y="ceDataCons",z="missing"),
           function(w,x,y,z,language="FR",logo=as.character(NA),...){
           current <- initReport()
           if (!is.na(logo)) invisible(file.copy(from=logo,to="logo.jpg",overwrite=TRUE))
           cl.consStatList=new("statConsOutput",cl.cons=clConsOutput(w,x,language=language))@cl.cons
           for( i in cl.consStatList ) {
                res=transformer(i$result)
                res$technical=gsub("_","\\_",res$technical,fixed=T)
                printCOST(res,caption=i$title,language=language,file="reportClCons.tex",append=TRUE)
           }

           ce.consStatList=new("statConsOutput",ce.cons=ceConsOutput(w,y,language=language))@ce.cons
           for( i in ce.consStatList ) {
               res=transformer(i$result)
               res$technical=gsub("_","\\_",res$technical,fixed=T)
               printCOST(res,caption=i$title,language=language,file="reportCeCons.tex",append=TRUE)
           }
           firstPageCons(x,"firstClCons.tex",language=language)
           firstPageCons(y,"firstCeCons.tex",language=language)
           mainCons(c("firstClCons.tex","reportClCons.tex","firstCeCons.tex","reportCeCons.tex"))
           system("pdflatex mainCons.tex")
           system("pdflatex mainCons.tex")
           deleteReport()
           setwd(current)
})
################################################################################

# -- Report() pour l'objet csDataCons,clDataCons,ceDataCons -- #
setMethod("Report",signature(w="strIni",x="csDataCons",y="clDataCons",z="ceDataCons"),
           function(w,x,y,z,language="FR",logo=as.character(NA),...){
           current <- initReport()
           if (!is.na(logo)) invisible(file.copy(from=logo,to="logo.jpg",overwrite=TRUE))
           cs.consStatList=new("statConsOutput",cs.cons=csConsOutput(w,x,language=language))@cs.cons
           for( j in 1:4 ) {
                i=cs.consStatList[[j]]
                res=transformer(i$result)
                res$technical=gsub("_","\\_",res$technical,fixed=T)
                printCOST(res,caption=i$title,language=language,file="reportCsConsM.tex",append=TRUE)
           }
           for( j in 5:6 ) {
                i=cs.consStatList[[j]]
                res=transformer(i$result)
                res$technical=gsub("_","\\_",res$technical,fixed=T)
                printCOST(res,caption=i$title,language=language,file="reportCsConsB.tex",append=TRUE)
           }
           for( j in 7:11 ) {
                i=cs.consStatList[[j]]
                res=transformer(i$result)
                res$technical=gsub("_","\\_",res$technical,fixed=T)
                printCOST(res,caption=i$title,language=language,file="reportCsConsS.tex",append=TRUE)
           }

           cl.consStatList=new("statConsOutput",cl.cons=clConsOutput(w,y,language=language))@cl.cons
           for( i in cl.consStatList ) {
               res=transformer(i$result)
               res$technical=gsub("_","\\_",res$technical,fixed=T)
               printCOST(res,caption=i$title,language=language,file="reportClCons.tex",append=TRUE)
           }

           ce.consStatList=new("statConsOutput",ce.cons=ceConsOutput(w,z,language=language))@ce.cons
           for( i in ce.consStatList ) {
              res=transformer(i$result)
              res$technical=gsub("_","\\_",res$technical,fixed=T)
              printCOST(res,caption=i$title,language=language,file="reportCeCons.tex",append=TRUE)
           }

           firstPageCons(x,"firstCsCons.tex",language=language)
           sectionCsCons("reportCsConsM.tex","reportCsConsB.tex","reportCsConsS.tex",language=language)
           firstPageCons(y,"firstClCons.tex",language=language)
           firstPageCons(z,"firstCeCons.tex",language=language)
           mainCons(c("firstCsCons.tex","sectionCsCons.tex","firstClCons.tex","reportClCons.tex","firstCeCons.tex","reportCeCons.tex"))
           system("pdflatex mainCons.tex")
           system("pdflatex mainCons.tex")
           deleteReport()
           setwd(current)
})

################################################################################
################################################################################


###############################################################
####       Part4 -- Report() for dbeObject --     #####
###############################################################

### --- printDBE() can make a longtable in latex --- ###
printDBE <- function(Text,dataset,language="FR",caption=NULL,...){
    con=file(Text,"r")
    sc=readLines(con)
    sc=sc[12:length(sc)-3]
    close(con)
    sink (...)
    colName=colnames(dataset)
    po="l"
    for( i in 1:ncol(dataset) ){
      po=paste(po,"l",sep="")}
    cat("\n\\centering")
    cat("\n\\tablecaption{",caption,"}",sep="")
    cat("\n\\tablehead{\\hline")
    for( i in 1:length(colName) ) {
       cat(" &",colName[i]) }
    cat("\\","\\"," \\hline}",sep="")
    cat("\n\\tabletail{\\hline \\multicolumn{",ncol(dataset)+1,"}{r}{\\emph{Continued on next page}}")
    cat("\\","\\","}",sep="")
    cat("\n\\tablelasttail{\\hline}")
    cat("\n\\begin{supertabular}{",po,"}\n",sep="")
    for( i in sc){
      cat(i,"\n",sep="")
    }
    cat("\n\\hline
         \n\\end{supertabular}
         \n\\vspace{1.5cm}\n\n")
    sink()
}

maindbe=function (fileList,dbe)
{   sink("maindbe.tex")
    cat("\n\\documentclass{article}
         \n\\usepackage[T1]{fontenc}
         \n\\usepackage[a4paper]{geometry}
         \n\\geometry{verbose,tmargin=2cm,bmargin=1.5cm,lmargin=2cm,rmargin=3cm}
         \n\\usepackage{epsfig}
         \n\\usepackage{graphicx}
         \n\\usepackage{multirow}
         \n\\usepackage{supertabular}
         \n\\usepackage{hyperref}
         \n\\usepackage{fancyhdr}
         \n\\pagestyle{fancy}")
    if ( file.exists("logo.jpg")==TRUE) {
     cat("\n\\fancyhead[R]{","\\","includegraphics[height=30pt]{logo.jpg}}",sep="")
     cat("\n\\renewcommand{","\\","headrulewidth}{0.5pt}",sep="")
    }
    if ( file.exists("logo.jpg")==FALSE) {
     cat("\n\\renewcommand{","\\","headrulewidth}{0.4pt}",sep="")
    }
    cat("\n\\lfoot{","\\","LARGE Created by COST}",sep="")
    cat("\n\\rfoot{","\\","LARGE ","\\","today}",sep="")
    cat("\n\\renewcommand{","\\","footrulewidth}{1pt}",sep="")
    cat("\n\\title{",dbe@species," estimate information}",sep="")
    cat("\n\\setlength{\\textheight}{10in}
         \n\\begin{document}
         \n\\maketitle
         \n\\thispagestyle{fancy}
         \n\\begin{center}
         \n\\begin{tabular}{lr}
         \n\\hline
         \n Desc & ",dbe@desc)
    cat("\\","\\","\n",sep="")
    cat("Species &",dbe@species)
    cat("\\","\\","\n",sep="")
    cat("catchCat &",dbe@catchCat)
    cat("\\","\\","\n",sep="")
    cat("param &",dbe@param)
    cat("\\","\\","\n",sep="")
    cat("time &",dbe@strataDesc@timeStrata)
    cat("\\","\\","\n",sep="")
    cat("space &",dbe@strataDesc@spaceStrata)
    cat("\\","\\","\n",sep="")
    cat("technical &",dbe@strataDesc@techStrata)
    cat("\\","\\","\n",sep="")
    cat("\n\\hline
         \n\\end{tabular}
         \n\\end{center}
         \n\\begin{figure}[bb!]")
    cat("\n\\includegraphics[width=0.35","\\","textwidth]{cost1.jpg}\n",sep="")
    cat("\\hfill
         \\includegraphics[width=0.35","\\","textwidth]{cost2.jpg}",sep="")
    cat("\n\\end{figure}
         \n\\newpage\n")
    for (i in fileList) {
        cat("\\input{", i, "}\n", sep = "")
    }
    cat("\n\\clearpage
         \n\\normalsize\\listoftables
         \n\\end{document}")
    sink()
}

## --- dbeLength() dbeAge() stock the infomations of a dbeObject --- ##
dbeLength= function (w,language="FR"){
  #load("N:/COST/Tian/Finished-Report()/fieldTitle.RData")
  data(fieldTitle)
  w <- dbeCalc(w,type="CV",vrbl="l")
  w <- dbeCalc(w,type="CI",vrbl="l")
  #totalLen=cbind(w@lenNum$ci[,4:7],w@lenVar$value,w@lenNum$cv$value)
#  label=paste(w@lenVar$time,w@lenVar$space,w@lenVar$technical,sep="/")
#  totalLen=cbind(label,totalLen)
#  colnames(totalLen)=c(as.character(fieldTitle[31:37,language]))
  totalLen=cbind(w@lenNum$ci,w@lenVar$value,w@lenNum$cv$value)
  colnames(totalLen)=c(as.character(fieldTitle[39,language]),as.character(fieldTitle[26,language]),as.character(fieldTitle[40,language]),as.character(fieldTitle[32:37,language]))
  generalLen=merge(w@nSamp$len,w@nMeas$len, by=c("time","space","technical"), suffix=c(".nSamp",".nMeas"),all=TRUE)
  colnames(generalLen)=c(as.character(fieldTitle[39,language]),as.character(fieldTitle[26,language]),as.character(fieldTitle[40:42,language]))
  total=list()
  total$title=as.character(fieldTitle[43,language])
  total$result=totalLen
  general=list()
  general$title=as.character(fieldTitle[44,language])
  general$result=generalLen
  fileList=list(total,general)
  return (fileList)
}

dbeAge= function (w,language="FR"){
 # load("N:/COST/Tian/Finished-Report()/fieldTitle.RData")
 data(fieldTitle)
  w <- dbeCalc(w,type="CV",vrbl="a")
  w <- dbeCalc(w,type="CI",vrbl="a")
  #totalAge=cbind(w@ageNum$ci[,4:7],w@ageVar$value,w@ageNum$cv$value)
#  label=paste(w@ageVar$time,w@ageVar$space,w@ageVar$technical,1:nrow(w@ageVar),sep="/")
#  totalAge=cbind(label,totalAge)
#  colnames(totalAge)=c(as.character(fieldTitle[31,language]),as.character(fieldTitle[38,language]),as.character(fieldTitle[33:37,language]))
  totalAge=cbind(w@ageNum$ci,w@ageVar$value,w@ageNum$cv$value)
  colnames(totalAge)=c(as.character(fieldTitle[39,language]),as.character(fieldTitle[26,language]),as.character(fieldTitle[40,language]),as.character(fieldTitle[38,language]),as.character(fieldTitle[33:37,language]))
  generalAge=merge(w@nSamp$age,w@nMeas$age, by=c("time","space"), suffix=c(".nSamp",".nMeas"),all=TRUE)
  colnames(generalAge)=c(as.character(fieldTitle[39,language]),as.character(fieldTitle[26,language]),as.character(fieldTitle[41:42,language]))
  total=list()
  total$title=as.character(fieldTitle[45,language])
  total$result=totalAge
  general=list()
  general$title=as.character(fieldTitle[46,language])
  general$result=generalAge
  fileList=list(total,general)
  return (fileList)
}

## --- Report() for dbeObject--- ##
setMethod("Report",signature(w="dbeOutput",x="missing",y="missing",z="missing"),
           function(w,x,y,z,language="FR",logo=as.character(NA),...){
           current <- initReport()
           if (!is.na(logo)) invisible(file.copy(from=logo,to="logo.jpg",overwrite=TRUE))
           if ( all(is.na(w@nMeas$age$value)) == TRUE & all(is.na(w@nMeas$len$value)) == TRUE ) {
               warning(" use RaiseLgth() to fill the dbeObject !")}
           if ( all(is.na(w@nMeas$age$value)) == TRUE ) fileList=dbeLength(w,language=language)
           if ( all(is.na(w@nMeas$len$value)) == TRUE ) fileList=dbeAge(w,language=language)
           if ( any(!is.na(w@nMeas$age$value)) == TRUE & any(!is.na(w@nMeas$len$value)) == TRUE ){
                fileList=dbeLength(w,language=language)
                fileList=append(fileList,dbeAge(w))
                }
           for( i in fileList ) {
               print(xtable(i$result),file="text.txt")
               printDBE("text.txt",i$result,caption=paste("",i$title),file="dbeReport.tex",append=TRUE)}
           maindbe("dbeReport.tex",w)
           system("pdflatex maindbe.tex")
           system("pdflatex maindbe.tex")
           deleteReport()
           setwd(current)
})


################################################################################
################################################################################


###############################################################
####       Part5 -- Report() for script of .R --     #####
###############################################################

####  --- sub function ---   #####
script=function(textName){
  con=file(textName,"r")
  sc=readLines(con,warn=FALSE)
  close(con)
  sc=sc[sc!=""]
  block=sc[1]
  for(i in 2:length(sc)){
    block=paste(block,sc[i],sep="\n")
  }
  block=paste("<<fig=T>>=",block,"@\n",sep="\n")
  return(block)
}

# --     Report() for R script      -- #    
setMethod("Report",signature(w="character",x="missing",y="missing",z="missing"),
           function(w,x,y,z,language="FR",logo=as.character(NA),...){
           current <- initReport()
           invisible(file.copy(from=paste(R.home(),"/library/COSTreport/Sweave.sty",sep=""),to="Sweave.sty"))
           code=script(w)
           Split=strsplit(w,"/")
           Title=Split[[1]][length(Split[[1]])]
           sink("mainScript.Rnw")
           cat("\n\\documentclass[english,french]{article}
                \n\\usepackage[T1]{fontenc}
                \n\\usepackage[a4paper]{geometry}
                \n\\geometry{verbose,tmargin=2cm,bmargin=1.5cm,lmargin=2cm,rmargin=3cm}
                \n\\usepackage{Sweave}
                \n\\usepackage{graphicx}
                \n\\usepackage{babel}")
           cat("\n\\title{",Title,"}")
           cat("\n\\SweaveOpts{echo=T,fig=T,keep.source=TRUE}
                \n\\begin{document}
                \n\\maketitle\n")
           cat(code,"\n")
           cat("\n\\end{document}")
           sink()
           temp <- options()$width
           options(width=90)
           Sweave("mainScript.Rnw")
           system("pdflatex mainScript.tex")
           deleteReport()
           setwd(current)
           options(width=temp)
})

##################           ---  OVER  ---      ###############################

################################################################################
################################################################################