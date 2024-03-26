.cost.ageLenMulti <-
function(){


try(require(tcltk))
try(require(tcltk2))
try(library(Rcmdr))
try(require(tkrplot))
try(tclRequire("BWidget"))
try(library(COSTcore))
try(library(COSTeda));try(library(COSTgui));try(library(COSTdbe))

tdspinner <- function(parent, ...) {tkwidget(parent, "spinbox", ...)}

check<-function(){
		if ( as.character(GetIt(lechoixcs))!="" &  as.character(GetIt(lechoixstrini))!=""){
		tkconfigure(gen.but,state="normal")
		tkconfigure(entry.agelen.plot,state="normal")
		if (tclvalue(tvargrps)!="0"){
		tkconfigure(refr.but,state="normal")
		}
		}
}

slotNamesdata<-function(data){
ploom<-slotNames(data)
out<-c()
for (p in ploom){
if(is(slot(data, p))[1] == "data.frame"){
out<-c(out,p)
}
}

return(out)
}
choixdataset2<-function(id){
#verbose#print(id)
try(tkdestroy(tt2),silent=T)
# try(tkdestroy(tt2),silent=T) # c ne fonctionne pas vraiment, je voudrais que ca ferme la fenetre, ou vrifie avant d'en ouvrir une novuelle
tt2<-tktoplevel()
tkwm.geometry(tt2, "-100+50")     
# listdata <- listDataSets()
# listdata <- c(malist("csData"),malist("ceData"),malist("clData"))
listdata <- malist(paste(id,"DataVal",sep=""))

if ( id == "strini"){
listdata <- malist("strIni")

}


dataSetsBox <- variableListBox(tt2, listdata, title=gettext(domain="R-COSTgui","Select one dataset"))# voir initialselection
tkgrid(getFrame(dataSetsBox), sticky="nwes",column=1,row=1)
  tkgrid.rowconfigure(getFrame(dataSetsBox), 1, weight = 1)
  tkgrid.rowconfigure(tt2, 1, weight = 1)  					
						
	
	
	
	

onOK <- function(){	

out<-getSelection(dataSetsBox)
	
		#verbose#print("voila2:")
		#verbose#print(GetIt((eval(as.name(paste("lechoix",id,sep=""))))))
		
		#verbose#print("id")
		#verbose#print(id)
		if ( length(out) != 0){
		
		
		# DoItmini(paste("tclvalue(choix",id,")<-out",sep=""),env=e1)
		
		
		eval(parse(text=paste("tclvalue(lechoix",id,")<-out",sep="")))
		#verbose#print("bon")
		

		# # DoItmini(paste("tkconfigure(choice.",id,"Data.but, text=out)",sep=""))
		tkconfigure(eval(as.name(paste("but.",id,sep=""))),text=out)
		# tkconfigure(but.csData.subset,state="normal")
		#verbose#print(GetIt((eval(as.name(paste("lechoix",id,sep=""))))))
		# tkconfigure( names.list.quali, state="disabled" )	
		# tkconfigure( names.list.quanti, state="disabled" )
		
		
		if ( id=="cs"){
		
		#verbose#print("on met a jour la liste des especes")

truc<-do.call("slot",args =list(as.name(as.character(GetIt(lechoixcs))),"hl"))
# tkdelete(list.especes,"0","end")
		# for (var in levels(as.factor(truc$spp))) {tkinsert(list.especes, "end", var)}
		
		# elvar<-as.name(as.character(tkget(names.list.quali,tkcurselection(names.list.quali))))
		
		
		
		# on va prechager temporal technical et spacial
		
		listtemp<-unique(
		c("all",
levels(as.factor(do.call("slot",args =list(as.name(as.character(GetIt(lechoixcs))),"tr"))$year))
,
levels(as.factor(do.call("slot",args =list(as.name(as.character(GetIt(lechoixcs))),"hh"))$year))
,
levels(as.factor(do.call("slot",args =list(as.name(as.character(GetIt(lechoixcs))),"sl"))$year))
,
levels(as.factor(do.call("slot",args =list(as.name(as.character(GetIt(lechoixcs))),"ca"))$quarter))
)
)

listtech<-unique(c("all",
levels(as.factor(do.call("slot",args =list(as.name(as.character(GetIt(lechoixcs))),"hh"))$foCatNat))
,
levels(as.factor(do.call("slot",args =list(as.name(as.character(GetIt(lechoixcs))),"hh"))$foCatEU5))
,
levels(as.factor(do.call("slot",args =list(as.name(as.character(GetIt(lechoixcs))),"hh"))$foCatEU6))
))

listspac<-unique(c("all",levels(as.factor(do.call("slot",args =list(as.name(as.character(GetIt(lechoixcs))),"hh"))$area))))
		
		
		tkconfigure(list.bonusTP,state="normal")
		tkconfigure(list.bonusTC,state="normal")
		tkconfigure(list.bonusSP,state="normal")
		
		
for (var in listtemp) {tkinsert(list.bonusTP, "end", var)}
for (var in listtech) {tkinsert(list.bonusTC, "end", var)}
for (var in listspac) {tkinsert(list.bonusSP, "end", var)}


		
		
		
		
		
		
		
		}
		
		
		if (id=="strini"){
		#on charge la liste.. mais ou aller chercher les infos
		
		# c'estpas pas dans strini qu'il faut faire ca l'info doit etre dans csdataval.. mais ou
		

		
		}
		
		
		# on va checker
check()
		
		
		
		
		}
			tkdestroy(tt2)	
		
}
	
OK.but <-tkbutton(tt2,text=gettext(domain="R-COSTgui","   OK   "),command=onOK)
tkgrid(OK.but,column=1,row=2)
tkfocus(tt2)
}
#fonctions pratique pour tk
malist<-function (pat) {

# retrouve dans ls les element de type pat
    Vars <- ls(all.names = TRUE,env=.GlobalEnv)
	# print(Vars)
    if (length(Vars) == 0) {return(Vars)}
    names(which(sapply(Vars, function(.x) {	is(get(.x))[1]==pat	}	)	)	)
	}

getcontentlistbox<-function(listbox){
out<-c()
i<-0		
while (length(as.character(tkget(listbox,i)))!=0){
out<-c(out,as.character(tkget(listbox,i)))
i<-i+1
}
return(out)
}
GetIt<-function(objet){
if (attr(objet,"class")=="tkwin"){
		if (as.character(tkwinfo("class",objet))=="Listbox"){
		return(getcontentlistbox(objet))
		}
}
if (attr(objet,"class")=="tclVar"){
return(tclvalue(objet))

}
}
DoItmini<-function(comm,env=.GlobalEnv){
  result <- parse(text = paste(comm))
  exprs<-result
  i<-1
  #for (i in seq_along(exprs)) {
  ei <- exprs[i]
  #verbose#print(ei)
  result <- eval(ei, envir = env)
  
}


plotagelen<-function(auto=F){

#verbose#print("plotagelen")
#verbose#print(auto)
#verbose#print(tclvalue(cbValue.auto))
# print("ert")
if ( auto==T & tclvalue(cbValue.auto)=="0"){return(NULL)}

 if(!exists("monagelen",e2)){
 
 
 print("pas de monagelent dans e2")
 Message(message = gettext(domain="R-COSTgui","You have to select strate1 and strat2"),  type = "error");return(NULL)

 
 }

if(exists("monagelen",e2)){
#verbose#print("monagelen existe dans e2")
# str1<-str2<-NULL
# try(silent=T,str1<-as.character(paste(as.character(tkget(list.strate1,as.character(tkcurselection(list.strate1)))),collapse=" ")))
# try(silent=T,str2<-as.character(paste(as.character(tkget(list.strate2,as.character(tkcurselection(list.strate2)))),collapse=" ")))
# str1
# str2
# if (length(str1)==0 | length(str2)==0){
# Message(message = gettext(domain="R-COSTgui","You have to select strate1 and strat2"),  type = "error");return(NULL)}

# # # letp<-"all"
# # # if (length(as.character(tkcurselection(list.bonusTP)))!=0){
# # # letp<-c()
# # # for ( rg in as.character(tkcurselection(list.bonusTP))){
# # # letp<-c(letp,as.character(tkget(list.bonusTP,rg)))
# # # }
# # # }


# # # letc<-"all"
# # # if (length(as.character(tkcurselection(list.bonusTC)))!=0){
# # # letc<-c()
# # # for ( rg in as.character(tkcurselection(list.bonusTC))){
# # # letc<-c(letc,as.character(tkget(list.bonusTC,rg)))
# # # }
# # # }
# # # lesp<-"all"
# # # if (length(as.character(tkcurselection(list.bonusSP)))!=0){
# # # lesp<-c()
# # # for ( rg in as.character(tkcurselection(list.bonusSP))){
# # # lesp<-c(lesp,as.character(tkget(list.bonusSP,rg)))
# # # }
# # # }
# # # # list(tp = "all", sp = c("27E2","28E5"), tc = "all")
# # # argelmnt <- as.list(quote(list()))
# # # argelmnt <- c(argelmnt,list(tp=quote(letp),tc=quote(letc),sp=quote(lesp)))
# # # leselements <- c(as.list(call("list")), eval(as.call(argelmnt))   );leselements
#verbose# cat(deparse(as.call(leselements)), "\n")

#verbose#print('ok')
largs <- as.list(quote(list()))
largs <- c(largs,list(x=quote(get("monagelen",e2)),

grps=quote(tclvalue(tvargrps))
));largs
tmp <- c(as.list(call("plot")), eval(as.call(largs))   );tmp
   #verbose# cat(deparse(as.call(tmp)), "\n")
# eval(as.call(tmp))
   
 
   
if ( tclvalue(cbValue.simu) == "1" & auto==T){

#on affiche rien mais on met quand meme a jour le graph
eval(as.call(tmp))


}  
   
   
if ( tclvalue(cbValue.simu) == "1" & auto==F){

# si tclvalue(val.agelen) n 'xiste pas , il faut ptet forcer sa generation ici dans rcmdr

as.name(tclvalue(val.agelen.plot))
if(!exists(as.character(tclvalue(val.agelen.plot)))){genagelen(flag=T)}


largs <- as.list(quote(list()))
largs <- c(largs,list(x=quote(as.name(tclvalue(val.agelen.plot))),
grps=quote(tclvalue(tvargrps))
));largs
tmp <- c(as.list(call("plot")), eval(as.call(largs))   );tmp

X11()
doItAndPrint(
paste(sep="",paste(deparse(as.call(tmp)),collapse=""))
)



 # paste(sep="",tclvalue(val.agelen),"<-",deparse(as.call(tmp)))

}
if ( tclvalue(cbValue.simu) == "0"){
if (!(as.logical(as.numeric(tclvalue(cbValue.simu))) & !auto)){
print(as.call(tmp))
eval(as.call(tmp))
}
if ((as.logical(as.numeric(tclvalue(cbValue.simu))) & !auto)){
# eval(as.call(tmp))
# doItAndPrint(paste("plot(",tclvalue(val.agelen.plot),")",sep=""))
print(as.call(tmp))
AEF<-eval(as.call(tmp))
X11()
plot(AEF)
# justDoIt("plot(eval(as.call(tmp)))")



}

}
}


}


genagelen<-function(flag=F){
#verbose#print('genagelen')
leCS<-NULL
lestr<-NULL
lessp<-NULL

try(leCS<- as.name(as.character(GetIt(lechoixcs))),silent=T)
try(lestr<-as.name(as.character(GetIt(lechoixstrini))),silent=T)
# try(lessp<-as.character(paste(as.character(tkget(list.especes,as.character(tkcurselection(list.especes)))),collapse=" ")),silent=T)

if(length(leCS)==0){Message(message = gettext(domain="R-COSTgui","You have to select a csDataVal type object"),  type = "error");return(NULL)}
if(length(lestr)==0){Message(message = gettext(domain="R-COSTgui","You have to select a strIni type object"),  type = "error");return(NULL)}
# if(length(lessp)==0){Message(message = gettext(domain="R-COSTgui","You have to choose a species"),  type = "error");return(NULL)}



# leCS
# lestr
# lessp

# tkconfigure(gen.but,text=gettext(domain="R-COSTgui","wait..."),state="disabled")

# on va controler que tout est pret






#verbose#print("on recalcul agelens")
# leagelen<-call("ageLenMulti",data=call("eval",quote(leCS)),strDef=call("eval",quote(lestr)),species=call("eval",quote(lessp)))
# # leagelen<-call("ageLenMulti",data=call("eval",quote(as.name(as.character(GetIt(lechoixcs))))),strDef=call("eval",quote(as.name(as.character(GetIt(lechoixstrini))))),species=call("eval",quote(as.character(paste(as.character(tkget(list.especes,as.character(tkcurselection(list.especes)))),collapse=" ")))))
# # leagelen
# # agelens<-eval(leagelen)  # ne devrai pas etre fait à chauqe fois. faudra verifier si les parametre sot novueau
# # deparse(leagelen)


largs <- as.list(quote(list()))


letp<-"all"
if (length(as.character(tkcurselection(list.bonusTP)))!=0){
letp<-c()
for ( rg in as.character(tkcurselection(list.bonusTP))){
letp<-c(letp,as.character(tkget(list.bonusTP,rg)))
}
}


letc<-"all"
if (length(as.character(tkcurselection(list.bonusTC)))!=0){
letc<-c()
for ( rg in as.character(tkcurselection(list.bonusTC))){
letc<-c(letc,as.character(tkget(list.bonusTC,rg)))
}
}
lesp<-"all"
if (length(as.character(tkcurselection(list.bonusSP)))!=0){
lesp<-c()
for ( rg in as.character(tkcurselection(list.bonusSP))){
lesp<-c(lesp,as.character(tkget(list.bonusSP,rg)))
}
}
# list(tp = "all", sp = c("27E2","28E5"), tc = "all")
argelmnt <- as.list(quote(list()))
argelmnt <- c(argelmnt,list(tp=quote(letp),tc=quote(letc),sp=quote(lesp)))
leselements <- c(as.list(call("list")), eval(as.call(argelmnt))   );leselements
#verbose# cat(deparse(as.call(leselements)), "\n")



largs <- c(largs,list(data=quote(as.name(as.character(GetIt(lechoixcs)))),age.plus=quote(as.numeric(tclvalue(lechoix.age.plus))),elmts=quote(as.call(leselements)),strDef=quote(as.name(as.character(GetIt(lechoixstrini))))))
# largs <- c(largs,list(data=quote(as.name(as.character(GetIt(lechoixcs)))),age.plus=quote(-123),elmts=quote(as.call(leselements)),strDef=quote(as.name(as.character(GetIt(lechoixstrini))))))
tmp <- c(as.list(call("ageLenMulti")), eval(as.call(largs))   );tmp
   #verbose# cat(deparse(as.call(tmp)), "\n")
   

 


 
   
   if ( tclvalue(cbValue.simu) == "1"){
   doItAndPrint(  
    paste(sep="",tclvalue(val.agelen.plot),"<-",paste(deparse(as.call(tmp)),collapse=""))
   )
   assign("monagelen",eval(as.name(tclvalue(val.agelen.plot))),envir=e2)
   #ptet un assign ici
   }
   if ( tclvalue(cbValue.simu) == "0"){
   # .monagelens<-eval(as.call(tmp))}
   assign("monagelen",eval(as.call(tmp)),envir=e2)
   # tclvalue(val.agelen)
        # flush.console()

}


# tkconfigure(rbseloui,state="normal")
# tkconfigure(rbselnon,state="normal")
# tkconfigure(refr.but,state="normal")
tkconfigure(entry.agelen.plot,state="normal")
tkconfigure(cb.simu,state="normal")
tkconfigure(cb.auto,state="normal")
# tkconfigure(list.strate2,state="normal")
# tkconfigure(list.strate1,state="normal")

# # # choix.strate<-c("techStrata","timeStrata","spaceStrata")

# # # tkdelete(list.bonusTP,"0","end")
# # # tkdelete(list.bonusTC,"0","end")
# # # tkdelete(list.bonusSP,"0","end")
# # # tkconfigure(list.bonusTP,state="normal")
# # # tkconfigure(list.bonusTC,state="normal")
# # # tkconfigure(list.bonusSP,state="normal")



# il faut récuperer agelens2@outPut$SampagelenMat
if ( tclvalue(cbValue.simu) == "1"){

truc<-eval(as.name(tclvalue(val.agelen.plot)))
# truc2<-truc@outPut$SampagelenMat
truc2<-truc


}

if ( tclvalue(cbValue.simu) == "0"){
#verbose#print("par ici")
truc<-get("monagelen",e2)
truc2<-truc
# # truc2<-truc@outPut$SampagelenMat
}

# # if(is.element("tp",names(truc2))){
# # for (var in unique(truc2$tp)) {tkinsert(list.bonusTP, "end", var)}
# # }
# # if(is.element("tc",names(truc2))){
# # for (var in unique(truc2$tc)) {tkinsert(list.bonusTC, "end", var)}
# # }
# # if(is.element("sp",names(truc2))){
# # for (var in unique(truc2$sp)) {tkinsert(list.bonusSP, "end", var)}
# # }



# # if (flag==F){
# # tkdelete(list.strate1,"0","end")
# # tkdelete(list.strate2,"0","end")
# # for (var in choix.strate) {tkinsert(list.strate2, "end", var);tkinsert(list.strate1, "end", var)}
# # # tkconfigure(gen.but,text="create",state="normal")		
# # }



}






# # faitsubset<-function(envi){
# # #verbose#print("faitsubset")


# # res<-.cost.subset.virtual(virtual=T,sour=c(1,0,0),dataname=tclvalue(lechoixcs),LENV=envi,parr=tt,label="(Comparaison des échantillons en taille)")

# # tclvalue(cbValue.csData.subset)<-"1"
# # tkconfigure(cb.csData.subset,state="normal")

# # }








dejaplot<-F
choix.strate<-c("techStrata","timeStrata","spaceStrata")
lechoixcs <- tclVar("")
lechoixstrini <- tclVar("")
lechoix.age.plus <- tclVar("")
# lechoixce <- tclVar("")
e2 <- new.env(parent = .GlobalEnv)
assign("lechoixcs",lechoixcs,envir=e2)
.envi.ageLenMulti <- new.env(parent = .GlobalEnv)

assign(".envi.ageLenMulti",.envi.ageLenMulti,pos=1)

# assign("lechoixcl",lechoixcl,envir=e2)
tt <- tktoplevel(borderwidth=10)
tkwm.geometry(tt, "-100+50")
font2<-tkfont.create(family="Times",size=12,weight="bold")
font3<-tkfont.create(family="Times",size=12)
fontheading<-tkfont.create(family="Times",size=18,weight="bold")  
tkwm.title(tt,gettext(domain="R-COSTgui","COST - Figure : ALK Multinomial analysis"))
tkgrid(tklabel(tt, text=gettext(domain="R-COSTgui","ALK Multinomial analysis"),font=fontheading))
tkgrid(tklabel(tt,text="")) # Ligne de blanc  
tkgrid(tklabel(tt,text="")) # Ligne de blanc
frameTOP<- tkframe(tt, borderwidth=2, relief="groove")
# frameD<- tkframe(tt, borderwidth=2, relief="groove")
# frameE<- tkframe(tt, borderwidth=2, relief="groove")
frameF<- tkframe(tt, borderwidth=2, relief="groove")
frameG<- tkframe(tt, borderwidth=2, relief="groove")
frameBONUS<- tkframe(tt, borderwidth=2, relief="groove")
frameA<- tkframe(tt, borderwidth=2, relief="groove")
frameAa<- tkframe(frameA, borderwidth=2, relief="groove")
frameAb<- tkframe(frameA, borderwidth=2, relief="groove")
# frameA2<- tkframe(frameA, borderwidth=2, relief="groove")
# listCS<-malist("csData")
but.strini <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<select>"),state="active",command=function(...) choixdataset2("strini"),foreground="red",relief="ridge")
but.newstrini <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","New strIni type object"),state="active",command=function(...) .cost.strIni())#,foreground="red",relief="ridge")
but.cs <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset2("cs"),foreground="red",relief="ridge")
# XX
# but.csData.subset <- tkbutton(frameTOP,text=gettext"subset",state="disabled",command=function(...) faitsubset(.envi.ageLenMulti))
# cbValue.csData.subset<- tclVar("0")
# cb.csData.subset<-tkcheckbutton(frameTOP,variable=cbValue.csData.subset,state="disabled")
text.simu<-tklabel(frameAa,text=gettext(domain="R-COSTgui","save the script :"))
cbValue.simu<-tclVar("0")
cb.simu<-tkcheckbutton(frameAa,variable=cbValue.simu,state="normal")




# but.ceData <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset("ce"),foreground="red",relief="ridge")

tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","CsDataVal : "),font=font2),but.cs)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","StrIni : "),font=font2),but.strini,but.newstrini)
# tkgrid(tklabel(frameTOP,text=" "),tklabel(frameTOP,text=gettext(domain="R-COSTgui","use subset :")),cb.csData.subset)

# tkgrid(tklabel(frameTOP,text=gettext"CsData : ",font=font2),but.cs)
# tkgrid(tklabel(frameTOP,text="CeData : ",font=font2),but.ceData)
tkgrid(frameTOP)#,frameD)
# tkgrid(frameE)


tkgrid(tklabel(tt,text=" "))

tkgrid(frameBONUS)
# tkgrid(frameF)
tkgrid(frameA)

# # # # # tkgrid(frameG) # ARJOUTER PLUS TARD PTET
# list.especes <-tklistbox(frameE,selectmode="unique",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.especes.scroll,...))
# list.especes.scroll<-tkscrollbar(frameE,repeatinterval=5,command=function(...)tkyview(list.especes,...))
# tkgrid(list.especes,list.especes.scroll,sticky="ns")

# on va faire des menu déroulant



#les spin
spin.age.plus<-tdspinner(frameAa,from=-1,to=100,inc=1,width=4,textvariable = lechoix.age.plus,state="normal")



# # combo.strate1 <- tkwidget(frameG,"ComboBox",editable=FALSE,values=choix.strate,state="normal",command=function(...) {prhint('coucou')})
# # combo.strate2 <- tkwidget(frameG,"ComboBox",editable=FALSE,values=choix.strate,state="normal")


# tkgrid(tklabel(frameG,text=gettext(domain="R-COSTgui","strate1")),tklabel(frameG,text=" "),tklabel(frameG,text=gettext(domain="R-COSTgui","strate2")),tklabel(frameG,text="  "))



list.strate1 <-tklistbox(frameG,selectmode="unique",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.strate1.scroll,...),state="disabled")
list.strate1.scroll<-tkscrollbar(frameG,repeatinterval=5,command=function(...)tkyview(list.strate1,...))
list.strate2 <-tklistbox(frameG,selectmode="unique",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.strate2.scroll,...),state="disabled")
list.strate2.scroll<-tkscrollbar(frameG,repeatinterval=5,command=function(...)tkyview(list.strate2,...))



tkgrid(list.strate1,list.strate1.scroll,list.strate2,list.strate2.scroll,sticky="ns")







# un tb pour selection
tvargrps <- tclVar(FALSE)
rbtime <- tkradiobutton(frameAb,state="disabled",command=function()check())
rbspace <- tkradiobutton(frameAb,state="disabled",command=function()check())
rbtech<- tkradiobutton(frameAb,state="disabled",command=function()check())
tkconfigure(rbtime,variable=tvargrps,value="timeStrata",state="normal")
tkconfigure(rbspace,variable=tvargrps,value="spaceStrata",state="normal")
tkconfigure(rbtech,variable=tvargrps,value="techStrata",state="normal")





gen.but <- tkbutton(frameA,text=gettext(domain="R-COSTgui","create"),state="disabled",command=function(...) genagelen())
refr.but <- tkbutton(frameA,text=gettext(domain="R-COSTgui","plot"),state="disabled",command=function(...) plotagelen())


# text.agelen<-tklabel(frameF,text="agelen : ")
# val.agelen <- tclVar("agelens")
# entry.agelen <-tkentry(frameF,width="20",textvariable=val.agelen,state="normal")

text.agelen.plot<-tklabel(frameA,text=gettext(domain="R-COSTgui","agelen plot : "))
val.agelen.plot <- tclVar("agelens")
entry.agelen.plot <-tkentry(frameA,width="20",textvariable=val.agelen.plot,state="disabled")

cbValue.simu<-tclVar("0")
cb.simu <- tkcheckbutton(frameAa,variable=cbValue.simu,state="normal")

cbValue.auto<-tclVar("0")
cb.auto <- tkcheckbutton(frameAa,variable=cbValue.auto,state="normal")
# tkgrid(text.agelen,entry.agelen,gen.but)

tkgrid(tklabel(frameAa,text=gettext(domain="R-COSTgui","age plus : ")),spin.age.plus)
tkgrid(tklabel(frameAa,text=gettext(domain="R-COSTgui","save the script : ")),cb.simu)
# tkgrid(tklabel(frameAa,text=gettext(domain="R-COSTgui","automatic :")),cb.auto)
tkgrid(frameAa,frameAb)
tkgrid(tklabel(frameAb,text=gettext(domain="R-COSTgui","Temporal strata :")),rbtime)
tkgrid(tklabel(frameAb,text=gettext(domain="R-COSTgui","Space strata :")),rbspace)
tkgrid(tklabel(frameAb,text=gettext(domain="R-COSTgui","Technical  strata :")),rbtech)

tkgrid(text.agelen.plot,entry.agelen.plot,refr.but,gen.but)
# tkgrid()

# on met les 3 dans frame BONUS



tkgrid(tklabel(frameBONUS,text=gettext(domain="R-COSTgui","Temporal")),tklabel(frameBONUS,text=" "),tklabel(frameBONUS,text=gettext(domain="R-COSTgui","Technical")),tklabel(frameBONUS,text="  "),tklabel(frameBONUS,text=gettext(domain="R-COSTgui","Spacial")),tklabel(frameBONUS,text="  "))

list.bonusSP <-tklistbox(frameBONUS,selectmode="multiple",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.bonusSP.scroll,...),state="disabled")
list.bonusSP.scroll<-tkscrollbar(frameBONUS,repeatinterval=5,command=function(...)tkyview(list.bonusSP,...))
list.bonusTP <-tklistbox(frameBONUS,selectmode="multiple",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.bonusTP.scroll,...),state="disabled")
list.bonusTP.scroll<-tkscrollbar(frameBONUS,repeatinterval=5,command=function(...)tkyview(list.bonusTP,...))
list.bonusTC <-tklistbox(frameBONUS,selectmode="multiple",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.bonusTC.scroll,...),state="disabled")
list.bonusTC.scroll<-tkscrollbar(frameBONUS,repeatinterval=5,command=function(...)tkyview(list.bonusTC,...))



tkgrid(list.bonusTP,list.bonusTP.scroll,list.bonusTC,list.bonusTC.scroll,list.bonusSP,list.bonusSP.scroll,sticky="ns")






tkbind(list.strate1, "<ButtonRelease-1>", function(...)plotagelen(auto=T))
tkbind(list.strate2, "<ButtonRelease-1>", function(...)plotagelen(auto=T))
tkbind(list.bonusTP, "<ButtonRelease-1>", function(...)plotagelen(auto=T))
tkbind(list.bonusTC, "<ButtonRelease-1>", function(...)plotagelen(auto=T))
tkbind(list.bonusSP, "<ButtonRelease-1>", function(...)plotagelen(auto=T))
tkfocus(tt)
}
.cost.ALK <-
function(){

try(require(tcltk))
try(require(tcltk2))
try(library(Rcmdr))
try(require(tkrplot))
try(tclRequire("BWidget"))
try(library(COSTcore))
try(library(COSTeda));try(library(COSTgui));try(library(COSTdbe));try(library(COSTdbe))

tdspinner <- function(parent, ...) {tkwidget(parent, "spinbox", ...)}

valid<-function(){

if ( tclvalue(tvarmethode)=="fill"){

if(length(as.character(tkcurselection(list.species)))==0){
Message(message = gettext(domain="R-COSTgui","You have to select a species"), type = "error")
}

if (tclvalue(val.out)==""){
Message(message = gettext(domain="R-COSTgui","You have to specify an out object"), type = "error")

}


if (length(as.character(tkcurselection(list.species)))!=0){
# fillALKmult(csObject,"Solea solea",p=15)
tkconfigure(but.view.fill,state="normal")

cmd1<-paste(tclvalue(val.out),"<-fillALKmult(object=",tclvalue(lechoix.but.csdataCons),",spp='",paste(as.character(tkget(list.species,as.character(tkcurselection(list.species)))),collapse=" "),"',p=",tclvalue(lechoix.p),",TRACE=",
as.logical(as.numeric(tclvalue(cbValue.TRACE))),")",sep="")
print(cmd1)
doItAndPrint(cmd1)

}

}
if ( tclvalue(tvarmethode)=="alk"){


if (tclvalue(val.out2)==""){
Message(message = gettext(domain="R-COSTgui","You have to specify an out object"), type = "error")

}

if (tclvalue(val.out2)!=""){
tkconfigure(but.view.alk,state="normal")
cmd1<-paste(tclvalue(val.out2),"<-alkLgthRec(object=",tclvalue(lechoix.but.csdataCons),",type='stepIncr',value=",tclvalue(lechoix.value),",preview=",
as.logical(as.numeric(tclvalue(cbValue.preview)))
,",update=",
as.logical(as.numeric(tclvalue(cbValue.update)))
,",postview=",
as.logical(as.numeric(tclvalue(cbValue.postview)))
,")",sep="")
print(cmd1)
doItAndPrint(cmd1)
}

}

}


slotNamesdata<-function(data){
ploom<-slotNames(data)
out<-c()
for (p in ploom){
if(is(slot(data, p))[1] == "data.frame"){
out<-c(out,p)
}
}

return(out)
}


choixdataset.multi<-function(id,nom.but){
print(id)
try(tkdestroy(tt2),silent=T)
tt2<-tktoplevel()
tkwm.geometry(tt2, "-100+50")     
listdata <-NULL
for ( i in id){listdata <- c(listdata,malist(i))}
dataSetsBox <- variableListBox(tt2, listdata, title=gettext(domain="R-COSTgui","Select one dataset"))# voir initialselection
tkgrid(getFrame(dataSetsBox), sticky="nwes",column=1,row=1)
  tkgrid.rowconfigure(getFrame(dataSetsBox), 1, weight = 1)
  tkgrid.rowconfigure(tt2, 1, weight = 1)  
onOK <- function(){	
out<-getSelection(dataSetsBox)

		if ( length(out) != 0){
		eval(parse(text=paste("tclvalue(lechoix.",nom.but,")<-out",sep="")))
		tkconfigure(eval(as.name(nom.but)),text=out)
		# print(GetIt((eval(as.name(paste("lechoix.",nom.but,sep=""))))))
		# tkconfigure( list.species, state="disabled" )	
		# tkconfigure( names.list.quanti, state="disabled" )
		
# on va récuperer la liste des especes		
# araj<-c("ert","etrt")

  listslot<-slotNamesdata(eval(as.name(out)))
araj<-c()
for ( i in 1:length(listslot)){	
truc<-do.call("slot",args =list(as.name(out),listslot[i]))
araj<-unique(c(araj,levels(as.factor(truc[["spp"]]))));araj
}

tkdelete(list.species,"0","end");
for (var in araj) {tkinsert(list.species, "end", var)}
tkconfigure(but.view.source,state="normal")
tkconfigure(but.view.alk,state="normal")
tkconfigure(but.view.fill,state="normal")
tkconfigure(entry.out2,state="normal")
tkconfigure(entry.out,state="normal")

# on change le nom des objets de sotie
tclvalue(val.out)<-paste(out,"_mult",sep="")
tclvalue(val.out2)<-paste(out,"_rec",sep="")



if (tclvalue(tvarmethode)!="NULL"){
tkconfigure(valid.but,state="normal")
	
		}
}
tkdestroy(tt2)	
		
}
	
OK.but <-tkbutton(tt2,text="   OK   ",command=onOK)
tkgrid(OK.but,column=1,row=2)
tkfocus(tt2)
}


ViewGasp<-function(a){

cmd1<-paste("viewGapsAlkCons(",tclvalue(a),")",sep="")
doItAndPrint(cmd1)


}

ViewGasp2<-function(a,typ=""){

print("gasp2")
print(a)
print(typ)
# si a existe alors on fait a sinon on le simule
if (exists(a)){
print("ca existe")
cmd1<-paste("viewGapsAlkCons(",a,")",sep="")
doItAndPrint(cmd1)
}

if (!exists(a)){
print("ca existe pas")
# on va creer un environnement

etemp <- new.env(parent = .GlobalEnv)


if(typ=="mult"){
print("dans mult")
if (length(as.character(tkcurselection(list.species)))!=0 & tclvalue(val.out)!=""){
print("yeah")
cmd1<-paste(tclvalue(val.out),"<-fillALKmult(object=",tclvalue(lechoix.but.csdataCons),",spp='",paste(as.character(tkget(list.species,as.character(tkcurselection(list.species)))),collapse=" "),"',p=",tclvalue(lechoix.p),",TRACE=",
as.logical(as.numeric(tclvalue(cbValue.TRACE))),")",sep="")
print(cmd1)
# doItAndPrint(cmd1)
DoItmini(cmd1,etemp)
atemp<-get(tclvalue(val.out),etemp)
assign(".atemp.cost.314.XXXX",get(tclvalue(val.out),etemp),envir=.GlobalEnv)# pas tres propre de faire ca
print("lalala")
print(is(atemp))
# print(viewGapsAlkCons(atemp))
cmd2<-paste("viewGapsAlkCons(",".atemp.cost.314.XXXX",")",sep="")
cmd3<-paste("rm(",".atemp.cost.314.XXXX",")",sep="")
# DoItmini(cmd2)# devrait s'afficher dans Rcmdr...
# justDoIt(cmd2)# devrait s'afficher dans Rcmdr...
doItAndPrint(cmd2,F)
doItAndPrint(cmd3,F)
# rm(.atemp.cost.314.XXXX,envir=.GlobalEnv)
}
}

if(typ=="rec"){
if ( tclvalue(val.out2)!=""){


cmd1<-paste(tclvalue(val.out2),"<-alkLgthRec(object=",tclvalue(lechoix.but.csdataCons),",type='stepIncr',value=",tclvalue(lechoix.value),",preview=",
as.logical(as.numeric(tclvalue(cbValue.preview)))
,",update=",
as.logical(as.numeric(tclvalue(cbValue.update)))
,",postview=",
as.logical(as.numeric(tclvalue(cbValue.postview)))
,")",sep="")

DoItmini(cmd1,etemp)
# # atemp<-get(tclvalue(val.out2),etemp)
# # cmd2<-paste("viewGapsAlkCons(","atemp",")",sep="")
# # doItAndPrint(cmd2,F)



assign(".atemp.cost.314.XXXX",get(tclvalue(val.out2),etemp),envir=.GlobalEnv)# pas tres propre de faire ca tout ce qui a ete prepare avant n'est pas utlise

cmd2<-paste("viewGapsAlkCons(",".atemp.cost.314.XXXX",")",sep="")
cmd3<-paste("rm(",".atemp.cost.314.XXXX",")",sep="")
# DoItmini(cmd2)# devrait s'afficher dans Rcmdr...
# justDoIt(cmd2)# devrait s'afficher dans Rcmdr...
doItAndPrint(cmd2,F)
doItAndPrint(cmd3,F)







}
}


# rm(etemp)


}



}




#fonctions pratique pour tk
malist<-function (pat) {

# retrouve dans ls les element de type pat
    Vars <- ls(all.names = TRUE,env=.GlobalEnv)
	# print(Vars)
    if (length(Vars) == 0) {return(Vars)}
    names(which(sapply(Vars, function(.x) {	is(get(.x))[1]==pat	}	)	)	)
	}
	
getcontentlistbox<-function(listbox){
out<-c()
i<-0		
while (length(as.character(tkget(listbox,i)))!=0){
out<-c(out,as.character(tkget(listbox,i)))
i<-i+1
}
return(out)
}

GetIt<-function(objet){
if (attr(objet,"class")=="tkwin"){
		if (as.character(tkwinfo("class",objet))=="Listbox"){
		return(getcontentlistbox(objet))
		}
}
if (attr(objet,"class")=="tclVar"){
return(tclvalue(objet))

}
}

DoItmini<-function(comm,env=.GlobalEnv){
  result <- parse(text = paste(comm))
  exprs<-result
  i<-1
  #for (i in seq_along(exprs)) {
  ei <- exprs[i]
  print(ei)
  result <- eval(ei, envir = env)
  
}


reset.strIni<-function(){
tkconfigure(but.strini,text=gettext(domain="R-COSTgui","<select>"))
tclvalue(lechoix.but.strini)<-""
}


choixrb<-function(){
print("choixrb")


if(tclvalue(tvarmethode)=="fill"){

aact<-c("cb.TRACE","entry.out","spin.p")
ades<-c("entry.out2","cb.update","cb.preview","cb.postview","spin.value")


for ( i in aact){

# tkconfigure(i,state="normal")
eval(parse(text=paste("tkconfigure(",i,",state='normal')",sep="")))
}
for ( i in ades){
eval(parse(text=paste("tkconfigure(",i,",state='disabled')",sep="")))
}
}
if(tclvalue(tvarmethode)=="alk"){
ades<-c("cb.TRACE","entry.out","spin.p")
aact<-c("entry.out2","cb.update","cb.preview","cb.postview","spin.value")

for ( i in aact){

# tkconfigure(i,state="normal")
eval(parse(text=paste("tkconfigure(",i,",state='normal')",sep="")))
}
for ( i in ades){
eval(parse(text=paste("tkconfigure(",i,",state='disabled')",sep="")))
}

}

print(tclvalue(lechoix.but.csdataCons))
if (tclvalue(lechoix.but.csdataCons) != ""){

tkconfigure(valid.but,state="normal")

}

}


lechoix.p <- tclVar(10)
lechoix.value <- tclVar(20)
lechoix.but.csdataCons <- tclVar("")
tt <- tktoplevel(borderwidth=10)
tkwm.geometry(tt, "-100+50")
font2<-tkfont.create(family="Times",size=12,weight="bold")
font3<-tkfont.create(family="Times",size=12)
fontheading<-tkfont.create(family="Times",size=18,weight="bold")  
tkwm.title(tt,gettext(domain="R-COSTgui","COST - ALK post treatment"))
tkgrid(tklabel(tt, text=gettext(domain="R-COSTgui","ALK post treatment"),font=fontheading),row=0,column=1,columnspan=4)
tkgrid(tklabel(tt,text="")) # Ligne de blanc
frameTOP<- tkframe(tt, borderwidth=2, relief="groove")
# frameA<- tkframe(tt, borderwidth=2, relief="groove")
# frameB<- tkframe(tt, borderwidth=2, relief="groove")
# frameC<- tkframe(tt, borderwidth=2, relief="groove")
frameD<- tkframe(tt, borderwidth=2, relief="groove")
frameE<- tkframe(frameD, borderwidth=2, relief="groove")
frameF<- tkframe(frameD, borderwidth=2, relief="groove")

frameG<- tkframe(tt, borderwidth=2, relief="groove")
framesel<- tkframe(tt, borderwidth=2, relief="groove")
frameH<- tkframe(frameG, borderwidth=2, relief="groove")

 # " les list
list.species <-tklistbox(frameE,selectmode="unique",exportselection="FALSE", height=4,width=30, yscrollcommand=function(...)tkset(list.species.scroll,...))
list.species.scroll<-tkscrollbar(frameE,repeatinterval=5,command=function(...)tkyview(list.species,...))



# les entry
# val.desc <- tclVar("")
# entry.desc <-tkentry(frameTOP,width="20",textvariable=val.desc)
# val.species <- tclVar("")
# entry.species <-tkentry(frameTOP,width="20",textvariable=val.species)
val.out <- tclVar("newObject_mult")
entry.out <-tkentry(frameD,width="20",textvariable=val.out,state="disabled")
val.out2 <- tclVar("newObject_rec")
entry.out2 <-tkentry(frameG,width="20",textvariable=val.out2,state="disabled")
# val.param <- tclVar("")
# entry.param <-tkentry(frameTOP,width="20",textvariable=val.param)

# # les radio
tvarmethode <- tclVar("NULL")
rbfill <- tkradiobutton(framesel,command=function()choixrb())
rbalk <- tkradiobutton(framesel,command=function()choixrb())
tkconfigure(rbfill,variable=tvarmethode,value="fill",state="normal")
tkconfigure(rbalk,variable=tvarmethode,value="alk",state="normal")


# tvarmethodDesc <- tclVar("NULL")
# rbanal <- tkradiobutton(frameC,command=function()choixrb())
# rbboot <- tkradiobutton(frameC,command=function()choixrb())
# tkconfigure(rbanal,variable=tvarmethodDesc,value="analytic",state="normal")
# tkconfigure(rbboot,variable=tvarmethodDesc,value="bootstrap",state="normal")
#les bouton



but.csdataCons <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset.multi(c("csDataCons"),"but.csdataCons"),foreground="red",relief="ridge")
valid.but <- tkbutton(tt,text=gettext(domain="R-COSTgui","Validate"),state="disabled",command=function(...) valid())

but.view.source<-tkbutton(frameTOP,text=gettext(domain="R-COSTgui","View"),state="disabled",command=function(...) ViewGasp(lechoix.but.csdataCons))
but.view.fill<-tkbutton(frameD,text=gettext(domain="R-COSTgui","Preview"),state="disabled",command=function(...) ViewGasp2(tclvalue(val.out),typ="mult"))
but.view.alk<-tkbutton(frameG,text=gettext(domain="R-COSTgui","Preview"),state="disabled",command=function(...) ViewGasp2(tclvalue(val.out2),typ="rec"))


# but.strini <- tkbutton(frameA,text=gettext(domain="R-COSTgui","<select>"),state="active",command=function(...) choixdataset.multi("strIni","but.strini"),foreground="red",relief="ridge")
# but.newstrini <- tkbutton(frameA,text=gettext(domain="R-COSTgui","New strIni type object"),state="active",command=function(...) .cost.strIni())#,foreground="red",relief="ridge")
# but.resetstrini <- tkbutton(frameA,text="reset",state="active",command=function(...) reset.strIni())#,foreground="red",relief="ridge")


#text

text.valid<-tklabel(frameD,text=gettext(domain="R-COSTgui","out :"))

#check
cbValue.TRACE<-tclVar("0")
cb.TRACE <- tkcheckbutton(frameF,variable=cbValue.TRACE,state="disabled")

# value=20, preview=FALSE,
  # postview=TRUE, update=TRUE)
cbValue.preview<-tclVar("0")
cb.preview <- tkcheckbutton(frameH,variable=cbValue.preview,state="disabled")
cbValue.postview<-tclVar("0")
cb.postview <- tkcheckbutton(frameH,variable=cbValue.postview,state="disabled")
cbValue.update<-tclVar("1")
cb.update <- tkcheckbutton(frameH,variable=cbValue.update,state="disabled")

#spin

spin.p<-tdspinner(frameF,from=1,to=1000,width=4,textvariable = lechoix.p,state="disabled")
spin.value<-tdspinner(frameH,from=1,to=1000,width=4,textvariable = lechoix.value,state="disabled")



tkgrid(frameTOP,row=1,column=2,columnspan=2)
tkgrid(framesel,row=2,column=1,columnspan=4)
tkgrid(frameD,row=3,column=1,columnspan=2,sticky="ns")
tkgrid(frameG,row=3,column=3,columnspan=2,sticky="ns")

tkgrid(tklabel(framesel,text=gettext(domain="R-COSTgui","Multinomial model:")),rbfill)
tkgrid(tklabel(framesel,text=gettext(domain="R-COSTgui","step size modification:")),rbalk)
tkgrid(tklabel(frameG,text=gettext(domain="R-COSTgui",""),font=font2),row=1,column=2)
tkgrid(frameH,sticky="ew",row=2,column=1,columnspan=3)
# tkgrid(tklabel(frameH,text=gettext(domain="R-COSTgui","preview:")),cb.preview)
# tkgrid(tklabel(frameH,text=gettext(domain="R-COSTgui","postview:")),cb.postview)
tkgrid(tklabel(frameH,text=gettext(domain="R-COSTgui","update:")),cb.update)
tkgrid(tklabel(frameH,text=gettext(domain="R-COSTgui","Step:")),spin.value)
tkgrid(tklabel(frameG,text=gettext(domain="R-COSTgui","out:")),row=3,column=1)
tkgrid(entry.out2,row=3,column=2)
tkgrid(but.view.alk,row=3,column=3)



tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","CsDataCons :"),font=font2),but.csdataCons,but.view.source)
tkgrid(tklabel(frameD,text=gettext(domain="R-COSTgui","species : "),font=font2),row=1,column=1,columnspan=3)
tkgrid(frameE,columnspan=3,row=2,column=1,sticky="ew")

tkgrid(frameF,row=3,column=1,columnspan=3)
tkgrid(text.valid,row=4,column=1)
tkgrid(entry.out,row=4,column=2)
tkgrid(but.view.fill,row=4,column=3)

tkgrid(list.species,sticky="nswe",column=1,row=1,columnspan=4)
tkgrid(list.species.scroll,sticky="nswe",column=5,row=1)




tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","Number of (virtual) individuals : ")),spin.p)
# tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","TRACE : ")),cb.TRACE)

tkgrid(valid.but,row=4,column=2,columnspan=2)
tkfocus(tt)

}
.cost.bioPar.plot <-
function(){


try(require(tcltk))
try(require(tcltk2))
try(library(Rcmdr))
try(require(tkrplot))
try(tclRequire("BWidget"))
try(library(COSTcore))
try(library(COSTeda));try(library(COSTgui))

valid<-function(){
lCS<-tclvalue(lechoixcs)
lechoixtype<-as.character(choixtype[as.numeric(tclvalue(tcl(combo.choixtype,"getvalue")))+1])

if (lCS == ""  | nchar(tclvalue(val.valid)) ==0 | length(lechoixtype)==0){
Message(message = gettext(domain="R-COSTgui","You have to specify a CsData, a type and an out name's objetc"), type = "error")
}





if (lCS !="" & nchar(tclvalue(val.valid)) !=0 & length(lechoixtype)!=0){




XX<-c("sl","ml","wl")[which(lechoixtype==choixtype)]

if (tclvalue(cbValue.simu)=="1"){
if(tclvalue(cbValue.csData.subset)=="0" ){lCS<-tclvalue(lechoixcs)}

if(tclvalue(cbValue.csData.subset)=="1" ){
lesub<-get("formule",.envi.bioparplot)
for ( i in 1:length(lesub)){
doItAndPrint(lesub[[i]])
}
lCS<-strsplit(lesub[[1]],"<-")[[1]][1]
}


if(tclvalue(tvar)=="1"){cmd1<-paste(tclvalue(val.valid),"<-bioPar.plot(",lCS,",type='",XX,"',selection=",as.logical(as.numeric(tclvalue(tvar))),");",tclvalue(val.valid),sep="")}

if(tclvalue(tvar)=="0"){cmd1<-paste("bioPar.plot(",lCS,",type='",XX,"',selection=",as.logical(as.numeric(tclvalue(tvar))),")",sep="")}
doItAndPrint(cmd1)
}



if (tclvalue(cbValue.simu)=="0"){
if(tclvalue(cbValue.csData.subset)=="0" ){lCS<-tclvalue(lechoixcs)}

if(tclvalue(cbValue.csData.subset)=="1" ){
lesub<-get("formule",.envi.bioparplot)[[1]];lesub
lCS<-paste("get('",strsplit(lesub,"<-")[[1]][1],"',.envi.bioparplot)",sep="")
# XXX 
}


if(tclvalue(tvar)=="1"){cmd1<-paste(tclvalue(val.valid),"<-bioPar.plot(",lCS,",type='",XX,"',selection=",as.logical(as.numeric(tclvalue(tvar))),");",tclvalue(val.valid),sep="")}

if(tclvalue(tvar)=="0"){cmd1<-paste("bioPar.plot(",lCS,",type='",XX,"',selection=",as.logical(as.numeric(tclvalue(tvar))),")",sep="")}
#verbose#print(cmd1)
# print("avant")
# X11()
 # eval(parse(text=cmd1))
# print("apres")
justDoIt(cmd1)
# DoItmini(cmd1)


}




}
}

#fonctions pratique pour tk
malist<-function (pat) {
# retrouve dans ls les element de type pat
    Vars <- ls(all.names = TRUE,env=.GlobalEnv)
	# print(Vars)
    if (length(Vars) == 0) {return(Vars)}
    names(which(sapply(Vars, function(.x) {	is(get(.x))[1]==pat	}	)	)	)
	}
	
getcontentlistbox<-function(listbox){
out<-c()
i<-0		
while (length(as.character(tkget(listbox,i)))!=0){
out<-c(out,as.character(tkget(listbox,i)))
i<-i+1
}
return(out)
}

GetIt<-function(objet){
if (attr(objet,"class")=="tkwin"){
		if (as.character(tkwinfo("class",objet))=="Listbox"){
		return(getcontentlistbox(objet))
		}
}
if (attr(objet,"class")=="tclVar"){
return(tclvalue(objet))

}
}

DoItmini<-function(comm,env=.GlobalEnv){
  result <- parse(text = paste(comm))
  exprs<-result
  i<-1
  #for (i in seq_along(exprs)) {
  ei <- exprs[i]
  #verbose#print(ei)
  result <- eval(ei, envir = env)
  
}

choixdataset<-function(id){
#verbose#print(id)
try(tkdestroy(tt2),silent=T)
tt2<-tktoplevel()
tkwm.geometry(tt2, "-100+50")     
listdata <- malist(paste(id,"Data",sep=""))
dataSetsBox <- variableListBox(tt2, listdata, title=gettext(domain="R-COSTgui","Select one dataset"))# voir initialselection
tkgrid(getFrame(dataSetsBox), sticky="nwes",column=1,row=1)
		  tkgrid.rowconfigure(getFrame(dataSetsBox), 1, weight = 1)
  tkgrid.rowconfigure(tt2, 1, weight = 1)  			


onOK <- function(){	
e1 <- new.env(parent = e2)
assign("out", getSelection(dataSetsBox), envir=e1)
out<-getSelection(dataSetsBox)

if (length(out)!=0){

# DoItmini("print(out)",e1)
DoItmini(paste("tclvalue(lechoix",id,")<-out",sep=""),env=e1)
		# # # tkconfigure(		paste("but.",id,"Data",sep="")		,text=out)
		tkconfigure(eval(as.name(paste("but.",id,"Data",sep=""))),text=out)
		tkdestroy(tt2)	
		
		
		# si tout est OK:
		tkconfigure(rbselnon,state="normal")
		tkconfigure(rbseloui,state="normal")
		tkconfigure(combo.choixtype,state="normal")
		tkconfigure(but.csData.subset,state="normal")
		
}

}
OK.but <-tkbutton(tt2,text="   OK   ",command=onOK)
tkgrid(OK.but,column=1,row=2)
tkfocus(tt2)

}

choixfraction<-function(){
#verbose#print("yopyop")
#verbose#print(tclvalue(tvar))
# € on active simplement le bouton
tkconfigure(valid.but,state="active")

if (tclvalue(tvar)=="0"){tkconfigure(entry.valid,state="disabled")}
if (tclvalue(tvar)=="1"){tkconfigure(entry.valid,state="normal")}

}


faitsubset<-function(envi){
#verbose#print("faitsubset")


res<-.cost.subset.virtual(virtual=T,sour=c(1,0,0),dataname=tclvalue(lechoixcs),LENV=envi,parr=tt,label="(Paramètres biologiques)")

tclvalue(cbValue.csData.subset)<-"1"
tkconfigure(cb.csData.subset,state="normal")

}

#
#le GUI

lechoixcs <- tclVar("")
# # lechoixcl <- tclVar("")
# # lechoixce <- tclVar("")
e2 <- new.env(parent = .GlobalEnv)
.envi.bioparplot <- new.env(parent = .GlobalEnv)
assign("lechoixcs",lechoixcs,envir=e2)
assign(".envi.bioparplot",.envi.bioparplot,pos=1)
# # assign("lechoixcl",lechoixcl,envir=e2)
tt <- tktoplevel(borderwidth=10)
tkwm.geometry(tt, "-100+50")
font2<-tkfont.create(family="Times",size=12,weight="bold")
font3<-tkfont.create(family="Times",size=12)
fontheading<-tkfont.create(family="Times",size=18,weight="bold")  
tkwm.title(tt,gettext(domain="R-COSTgui","COST - Figure : Biological parameters"))
tkgrid(tklabel(tt, text=gettext(domain="R-COSTgui","Biological parameters"),font=fontheading))
tkgrid(tklabel(tt,text="")) # Ligne de blanc
frameTOP<- tkframe(tt, borderwidth=2, relief="groove")
frameD<- tkframe(tt, borderwidth=2, relief="groove")
frameE<- tkframe(tt, borderwidth=2, relief="groove")
listCS<-malist("csData")
# # but.clData <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset("cl"),foreground="red",relief="ridge")
but.csData <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset("cs"),foreground="red",relief="ridge")
but.csData.subset <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","subset"),state="disabled",command=function(...) faitsubset(.envi.bioparplot))
cbValue.csData.subset<- tclVar("0")
cb.csData.subset<-tkcheckbutton(frameTOP,variable=cbValue.csData.subset,state="disabled")

# # but.ceData <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset("ce"),foreground="red",relief="ridge")
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","CsData : "),font=font2),but.csData,but.csData.subset)
tkgrid(tklabel(frameTOP,text=" "),tklabel(frameTOP,text=gettext(domain="R-COSTgui","use subset :")),cb.csData.subset)
# # tkgrid(tklabel(frameTOP,text="ClData : ",font=font2),but.clData)
# # tkgrid(tklabel(frameTOP,text="CeData : ",font=font2),but.ceData)
tkgrid(frameTOP)
tkgrid(frameD)
tkgrid(frameE)
tvar <- tclVar("ZER")
rbseloui <- tkradiobutton(frameD,command=function()choixfraction())
rbselnon <- tkradiobutton(frameD,command=function()choixfraction())
tkconfigure(rbseloui,variable=tvar,value=TRUE,state="disabled")
tkconfigure(rbselnon,variable=tvar,value=FALSE,state="disabled")
tkgrid(tklabel(frameD,text=gettext(domain="R-COSTgui","Selection - ")),tklabel(frameD,text=gettext(domain="R-COSTgui","No : ")),rbselnon,tklabel(frameD,text=gettext(domain="R-COSTgui","Yes : ")),rbseloui)
# liste.spp <-tklistbox(frameE,selectmode="multiple",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(liste.spp.scroll,...))
# liste.spp.scroll<-tkscrollbar(frameE,repeatinterval=5,command=function(...)tkyview(liste.spp,...))
# tkgrid(liste.spp,liste.spp.scroll)
# on met un menu déroulant

# choixtype<-c("sl","ml","wl")
choixtype<-c(gettext(domain="R-COSTgui","Sexratio"),gettext(domain="R-COSTgui","maturity"),gettext(domain="R-COSTgui","weight"))
combo.choixtype <- tkwidget(frameE,"ComboBox",editable=FALSE,values=choixtype,state="disabled")

tkgrid(tklabel(frameE,text=gettext(domain="R-COSTgui","Type :")),combo.choixtype)

frameA<- tkframe(tt, borderwidth=2, relief="groove")
valid.but <- tkbutton(frameA,text=gettext(domain="R-COSTgui","Validate"),state="disabled",command=function(...) valid())
tkgrid(frameA)
text.valid<-tklabel(frameA,text=gettext(domain="R-COSTgui","out :"))
text.simu<-tklabel(frameA,text=gettext(domain="R-COSTgui","save the script :"))
cbValue.simu<-tclVar("1")
cb.simu<-tkcheckbutton(frameA,variable=cbValue.simu,state="normal")


val.valid <- tclVar("mySel")
entry.valid <-tkentry(frameA,width="20",textvariable=val.valid)
tkgrid(text.valid,entry.valid,valid.but)
tkgrid(tklabel(frameA,text=""),text.simu,cb.simu)
tkfocus(tt)
}
.cost.controlmap <-
function(){


try(require(tcltk))
try(require(tcltk2))
try(library(Rcmdr))
try(require(tkrplot))
try(tclRequire("BWidget"))
try(library(COSTcore))
try(library(COSTeda));try(library(COSTgui))

moncontr<-function(){
lCL<-tclvalue(lechoixcl)
lCS<-tclvalue(lechoixcs)
lCE<-tclvalue(lechoixce)
# controlMap(sole_cs,sole_cl,sole_ce)
if (lCS == ""  | nchar(tclvalue(val.moncontr)) ==0){
Message(message = gettext(domain="R-COSTgui","You have to select a CsData and an out name's objetc"), type = "error")
}

if (lCS !="" & nchar(tclvalue(val.moncontr)) !=0){

if (lCL =="" & lCE ==""){
cmd1<-paste(tclvalue(val.moncontr),"<-controlMap(",lCS,");",tclvalue(val.moncontr),sep="")
}

if (lCL !="" & lCE ==""){
cmd1<-paste(tclvalue(val.moncontr),"<-controlMap(",lCS,",",lCL,");",tclvalue(val.moncontr),sep="")
}

if (lCL =="" & lCE !=""){
cmd1<-paste(tclvalue(val.moncontr),"<-controlMap(",lCS,",",lCE,");",tclvalue(val.moncontr),sep="")
}

if (lCL !="" & lCE !=""){
cmd1<-paste(tclvalue(val.moncontr),"<-controlMap(",lCS,",",lCL,",",lCE,");",tclvalue(val.moncontr),sep="")
}

doItAndPrint(cmd1)


}


}

#fonctions pratique pour tk
malist<-function (pat) {

# retrouve dans ls les element de type pat
    Vars <- ls(all.names = TRUE,env=.GlobalEnv)
	# print(Vars)
    if (length(Vars) == 0) {return(Vars)}
    names(which(sapply(Vars, function(.x) {	is(get(.x))[1]==pat	}	)	)	)
	}
	
getcontentlistbox<-function(listbox){
out<-c()
i<-0		
while (length(as.character(tkget(listbox,i)))!=0){
out<-c(out,as.character(tkget(listbox,i)))
i<-i+1
}
return(out)
}

GetIt<-function(objet){
if (attr(objet,"class")=="tkwin"){
		if (as.character(tkwinfo("class",objet))=="Listbox"){
		return(getcontentlistbox(objet))
		}
}
if (attr(objet,"class")=="tclVar"){
return(tclvalue(objet))

}
}


	DoItmini<-function(comm,env=.GlobalEnv){
  result <- parse(text = paste(comm))
  exprs<-result
  i<-1
  #for (i in seq_along(exprs)) {
  ei <- exprs[i]
  #verbose#print(ei)
  result <- eval(ei, envir = env)
  
}

choixdataset<-function(id){
#verbose#print(id)
try(tkdestroy(tt2),silent=T)
tt2<-tktoplevel()
tkwm.geometry(tt2, "-100+50")     
listdata <- malist(paste(id,"Data",sep=""))
dataSetsBox <- variableListBox(tt2, listdata, title=gettext(domain="R-COSTgui","Select one dataset"))# voir initialselection
tkgrid(getFrame(dataSetsBox), sticky="nwes",column=1,row=1)
  tkgrid.rowconfigure(getFrame(dataSetsBox), 1, weight = 1)
  tkgrid.rowconfigure(tt2, 1, weight = 1)  					


onOK <- function(){	
e1 <- new.env(parent = e2)
assign("out", getSelection(dataSetsBox), envir=e1)
out<-getSelection(dataSetsBox)
# DoItmini("print(out)",e1)

if(length(out)!=0){
DoItmini(paste("tclvalue(lechoix",id,")<-out",sep=""),env=e1)
		# # # tkconfigure(		paste("but.",id,"Data",sep="")		,text=out)
		tkconfigure(eval(as.name(paste("but.",id,"Data",sep=""))),text=out)
		tkdestroy(tt2)	
		}
		
}
OK.but <-tkbutton(tt2,text="   OK   ",command=onOK)
tkgrid(OK.but,column=1,row=2)
tkfocus(tt2)
}

#le GUI

lechoixcs <- tclVar("")
lechoixcl <- tclVar("")
lechoixce <- tclVar("")
e2 <- new.env(parent = .GlobalEnv)
assign("lechoixcs",lechoixcs,envir=e2)
assign("lechoixcl",lechoixcl,envir=e2)
assign("lechoixce",lechoixce,envir=e2)
tt <- tktoplevel(borderwidth=10)
tkwm.geometry(tt, "-100+50")
font2<-tkfont.create(family="Times",size=12,weight="bold")
font3<-tkfont.create(family="Times",size=12)
fontheading<-tkfont.create(family="Times",size=18,weight="bold")  
tkwm.title(tt,gettext(domain="R-COSTgui","COST - Control table"))
tkgrid(tklabel(tt, text=gettext(domain="R-COSTgui","Control table"),font=fontheading))
tkgrid(tklabel(tt,text="")) # Ligne de blanc  
tkgrid(tklabel(tt,text="")) # Ligne de blanc
frameTOP<- tkframe(tt, borderwidth=2, relief="groove")
frameD<- tkframe(tt, borderwidth=2, relief="groove")
listCS<-malist("csData")
but.clData <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset("cl"),foreground="red",relief="ridge")
but.csData <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset("cs"),foreground="red",relief="ridge")
but.ceData <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset("ce"),foreground="red",relief="ridge")
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","CsData : "),font=font2),but.csData)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","ClData : "),font=font2),but.clData)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","CeData : "),font=font2),but.ceData)
tkgrid(frameTOP,frameD)
frameA<- tkframe(tt, borderwidth=2, relief="groove")
moncontr.but <- tkbutton(frameA,text=gettext(domain="R-COSTgui","Control"),state="active",command=function(...) moncontr())
tkgrid(frameA)
text.moncontr<-tklabel(frameA,text=gettext(domain="R-COSTgui","out :"))
val.moncontr <- tclVar("ctrl")
entry.moncontr<-entry.chemin.rdata <-tkentry(frameA,width="20",textvariable=val.moncontr)
tkgrid(text.moncontr,entry.moncontr,moncontr.but)
tkfocus(tt)
}
.cost.DataCons <-
function(){


try(require(tcltk))
try(require(tcltk2))
try(library(Rcmdr))
try(require(tkrplot))
try(tclRequire("BWidget"))
try(library(COSTcore))
try(library(COSTeda));try(library(COSTgui))

reset<-function(){
#verbose#print("on purge tout")
# lechoixcs <- tclVar("")
# lechoixcl <- tclVar("")
# lechoixce <- tclVar("")

tclvalue(lechoixcl)<-""
tclvalue(lechoixcs)<-""
tclvalue(lechoixce)<-""

tkconfigure(but.clData,text=gettext(domain="R-COSTgui","<dataset>"))
tkconfigure(but.ceData,text=gettext(domain="R-COSTgui","<dataset>"))
tkconfigure(but.csData,text=gettext(domain="R-COSTgui","<dataset>"))

tclvalue(val.cs)<-"Cons_cs"
tclvalue(val.cl)<-"Cons_cl"
tclvalue(val.ce)<-"Cons_ce"
}

laval<-function(){
lCL<-tclvalue(lechoixcl)
lCS<-tclvalue(lechoixcs)
lCE<-tclvalue(lechoixce)
lstrini<-tclvalue(lechoixstrini)


#verbose#print(lCL)
#verbose#print(lCS)
#verbose#print(lCE)
#verbose#print(lstrini)


if (lstrini !="" & lCE !="" & nchar(tclvalue(val.ce)) !=0){
#verbose#print("on fait CE")
cmd3<-paste(tclvalue(val.ce),"<-ceDataCons(",tclvalue(lechoixce),",strIni=",tclvalue(lechoixstrini),")",sep="")
doItAndPrint(cmd3)
}



if (lstrini !="" & lCS !="" & nchar(tclvalue(val.cs)) !=0){
#verbose#print("on fait CS")
cmd1<-paste(tclvalue(val.cs),"<-csDataCons(",tclvalue(lechoixcs),",strIni=",tclvalue(lechoixstrini),")",sep="")
doItAndPrint(cmd1)
}
if (lstrini !="" & lCL !="" & nchar(tclvalue(val.cs)) !=0){
#verbose#print("on fait CL")
cmd2<-paste(tclvalue(val.cl),"<-clDataCons(",tclvalue(lechoixcl),",strIni=",tclvalue(lechoixstrini),")",sep="")
doItAndPrint(cmd2)
}




}


choixdataset2<-function(id){
#verbose#print(id)
try(tkdestroy(tt2),silent=T)
# try(tkdestroy(tt2),silent=T) # c ne fonctionne pas vraiment, je voudrais que ca ferme la fenetre, ou vrifie avant d'en ouvrir une novuelle
tt2<-tktoplevel()
tkwm.geometry(tt2, "-100+50")     
# listdata <- listDataSets()
# listdata <- c(malist("csData"),malist("ceData"),malist("clData"))
listdata <- malist(paste(id,"Data",sep=""))

if ( id == "strini"){
listdata <- malist("strIni")

}


dataSetsBox <- variableListBox(tt2, listdata, title=gettext(domain="R-COSTgui","Select one dataset"))# voir initialselection
tkgrid(getFrame(dataSetsBox), sticky="nwes",column=1,row=1)
  tkgrid.rowconfigure(getFrame(dataSetsBox), 1, weight = 1)
  tkgrid.rowconfigure(tt2, 1, weight = 1)  					
						
	
	
	
	

onOK <- function(){	

out<-getSelection(dataSetsBox)
	
		#verbose#print("voila2:")
		#verbose#print(GetIt((eval(as.name(paste("lechoix",id,sep=""))))))
		
		#verbose#print("id")
		#verbose#print(id)
		if ( length(out) != 0){
		
		
		# DoItmini(paste("tclvalue(choix",id,")<-out",sep=""),env=e1)
		
		
		eval(parse(text=paste("tclvalue(lechoix",id,")<-out",sep="")))
		#verbose#print("bon")
		

		# # DoItmini(paste("tkconfigure(choice.",id,"Data.but, text=out)",sep=""))
		tkconfigure(eval(as.name(paste("but.",id,sep=""))),text=out)
		
		#verbose#print(GetIt((eval(as.name(paste("lechoix",id,sep=""))))))
		# tkconfigure( names.list.quali, state="disabled" )	
		# tkconfigure( names.list.quanti, state="disabled" )
		
		
		if ( id=="cs"){
		
		#verbose#print("on met a jour la liste des especes")

truc<-do.call("slot",args =list(as.name(as.character(GetIt(lechoixcs))),"hl"))
tkdelete(list.especes,"0","end")
		for (var in levels(as.factor(truc$spp))) {tkinsert(list.especes, "end", var)}
		
		# elvar<-as.name(as.character(tkget(names.list.quali,tkcurselection(names.list.quali))))
		
		
		
		
		}
		
		
		
		
		}
			tkdestroy(tt2)	
		
}
	
OK.but <-tkbutton(tt2,text="   OK   ",command=onOK)
tkgrid(OK.but,column=1,row=2)
tkfocus(tt2)
}



#fonctions pratique pour tk
malist<-function (pat) {

# retrouve dans ls les element de type pat
    Vars <- ls(all.names = TRUE,env=.GlobalEnv)
	# print(Vars)
    if (length(Vars) == 0) {return(Vars)}
    names(which(sapply(Vars, function(.x) {	is(get(.x))[1]==pat	}	)	)	)
	}
	
getcontentlistbox<-function(listbox){
out<-c()
i<-0		
while (length(as.character(tkget(listbox,i)))!=0){
out<-c(out,as.character(tkget(listbox,i)))
i<-i+1
}
return(out)
}

GetIt<-function(objet){
if (attr(objet,"class")=="tkwin"){
		if (as.character(tkwinfo("class",objet))=="Listbox"){
		return(getcontentlistbox(objet))
		}
}
if (attr(objet,"class")=="tclVar"){
return(tclvalue(objet))

}
}


	DoItmini<-function(comm,env=.GlobalEnv){
  result <- parse(text = paste(comm))
  exprs<-result
  i<-1
  #for (i in seq_along(exprs)) {
  ei <- exprs[i]
  #verbose#print(ei)
  result <- eval(ei, envir = env)
  
}





choixdataset3<-function(id){
#verbose#print(id)
try(tkdestroy(tt2),silent=T)
tt2<-tktoplevel()
tkwm.geometry(tt2, "-100+50")     
listdata <- malist(paste(id,"DataVal",sep=""))
dataSetsBox <- variableListBox(tt2, listdata, title=gettext(domain="R-COSTgui","Select one dataset"))# voir initialselection
tkgrid(getFrame(dataSetsBox), sticky="nwes",column=1,row=1)
		  tkgrid.rowconfigure(getFrame(dataSetsBox), 1, weight = 1)
  tkgrid.rowconfigure(tt2, 1, weight = 1)  			


onOK <- function(){	
e1 <- new.env(parent = e2)
assign("out", getSelection(dataSetsBox), envir=e1)
out<-getSelection(dataSetsBox)

if(length(out)!=0){
# DoItmini("print(out)",e1)
DoItmini(paste("tclvalue(lechoix",id,")<-out",sep=""),env=e1)
		# # # tkconfigure(		paste("but.",id,"Data",sep="")		,text=out)
		tkconfigure(eval(as.name(paste("but.",id,"Data",sep=""))),text=out)
		tkdestroy(tt2)	
		}
}
OK.but <-tkbutton(tt2,text="   OK   ",command=onOK)
tkgrid(OK.but,column=1,row=2)
tkfocus(tt2)
}

#le GUI

lechoixcs <- tclVar("")
lechoixcl <- tclVar("")
lechoixce <- tclVar("")
lechoixstrini <- tclVar("")
e2 <- new.env(parent = .GlobalEnv)
assign("lechoixcs",lechoixcs,envir=e2)
assign("lechoixcl",lechoixcl,envir=e2)
assign("lechoixce",lechoixce,envir=e2)
tt <- tktoplevel(borderwidth=10)
tkwm.geometry(tt, "-100+50")
font2<-tkfont.create(family="Times",size=12,weight="bold")
font3<-tkfont.create(family="Times",size=12)
fontheading<-tkfont.create(family="Times",size=18,weight="bold")  
tkwm.title(tt,gettext(domain="R-COSTgui","COST - Consolidation of the dataset"))
tkgrid(tklabel(tt, text=gettext(domain="R-COSTgui","Consolidation of the dataset"),font=fontheading))
tkgrid(tklabel(tt,text="")) # Ligne de blanc  
tkgrid(tklabel(tt,text="")) # Ligne de blanc
frameTOP<- tkframe(tt, borderwidth=2, relief="groove")
frameTOP2<- tkframe(tt, borderwidth=2, relief="groove")
frameD<- tkframe(tt, borderwidth=2, relief="groove")
listCS<-malist("csData")
but.clData <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset3("cl"),foreground="red",relief="ridge")
but.csData <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset3("cs"),foreground="red",relief="ridge")
but.ceData <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset3("ce"),foreground="red",relief="ridge")

but.strini <- tkbutton(frameTOP2,text=gettext(domain="R-COSTgui","<select>"),state="active",command=function(...) choixdataset2("strini"),foreground="red",relief="ridge")
but.newstrini <- tkbutton(frameTOP2,text=gettext(domain="R-COSTgui","New strIni type object"),state="active",command=function(...) .cost.strIni())#,foreground="red",relief="ridge")



tkgrid(frameTOP,frameD)
tkgrid(frameTOP2)
tkgrid(tklabel(frameTOP2,text=gettext(domain="R-COSTgui","StrIni : "),font=font2),but.strini,but.newstrini)
frameA<- tkframe(tt, borderwidth=2, relief="groove")
laval.but <- tkbutton(frameA,text=gettext(domain="R-COSTgui","Validate"),state="active",command=function(...) laval())
reset.but <- tkbutton(frameA,text=gettext(domain="R-COSTgui","Reset"),state="active",command=function(...) reset())
tkgrid(frameA)
tkgrid(laval.but,tklabel(frameA,text="   "),reset.but)

val.cs <- tclVar("Cons_cs")
val.cl <- tclVar("Cons_cl")
val.ce <- tclVar("Cons_ce")
entry.cs<-entry.chemin.rdata <-tkentry(frameTOP,width="20",textvariable=val.cs)
entry.cl<-entry.chemin.rdata <-tkentry(frameTOP,width="20",textvariable=val.cl)
entry.ce<-entry.chemin.rdata <-tkentry(frameTOP,width="20",textvariable=val.ce)



tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","CsData : "),font=font2),but.csData,tklabel(frameTOP,text=" -> "),entry.cs)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","ClData : "),font=font2),but.clData,tklabel(frameTOP,text=" -> "),entry.cl)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","CeData : "),font=font2),but.ceData,tklabel(frameTOP,text=" -> "),entry.ce)

tkfocus(tt)
}
.cost.DataVal <-
function(){

try(require(tcltk))
try(require(tcltk2))
try(library(Rcmdr))
try(require(tkrplot))
try(tclRequire("BWidget"))
try(library(COSTcore))
try(library(COSTeda));try(library(COSTgui))


reset<-function(){
#verbose#print("on purge tout")
# lechoixcs <- tclVar("")
# lechoixcl <- tclVar("")
# lechoixce <- tclVar("")

tclvalue(lechoixcl)<-""
tclvalue(lechoixcs)<-""
tclvalue(lechoixce)<-""

tkconfigure(but.clData,text=gettext(domain="R-COSTgui","<dataset>"))
tkconfigure(but.ceData,text=gettext(domain="R-COSTgui","<dataset>"))
tkconfigure(but.csData,text=gettext(domain="R-COSTgui","<dataset>"))

tclvalue(val.cs)<-"Val_cs"
tclvalue(val.cl)<-"Val_cl"
tclvalue(val.ce)<-"Val_ce"
}

laval<-function(){
lCL<-tclvalue(lechoixcl)
lCS<-tclvalue(lechoixcs)
lCE<-tclvalue(lechoixce)
# controlMap(sole_cs,sole_cl,sole_ce)

#verbose#print(lCL)
#verbose#print(lCS)
#verbose#print(lCE)


if (lCE !="" & nchar(tclvalue(val.ce)) !=0){
#verbose#print("on fait CE")
cmd3<-paste(tclvalue(val.ce),"<-ceDataVal(",tclvalue(lechoixce),")",sep="")
doItAndPrint(cmd3)
}



if (lCS !="" & nchar(tclvalue(val.cs)) !=0){
#verbose#print("on fait CS")
cmd1<-paste(tclvalue(val.cs),"<-csDataVal(",tclvalue(lechoixcs),")",sep="")
doItAndPrint(cmd1)
}
if (lCL !="" & nchar(tclvalue(val.cs)) !=0){
#verbose#print("on fait CL")
cmd2<-paste(tclvalue(val.cl),"<-clDataVal(",tclvalue(lechoixcl),")",sep="")
doItAndPrint(cmd2)
}




}

#fonctions pratique pour tk
malist<-function (pat) {

# retrouve dans ls les element de type pat
    Vars <- ls(all.names = TRUE,env=.GlobalEnv)
	# #verbose#print(Vars)
    if (length(Vars) == 0) {return(Vars)}
    names(which(sapply(Vars, function(.x) {	is(get(.x))[1]==pat	}	)	)	)
	}
	
getcontentlistbox<-function(listbox){
out<-c()
i<-0		
while (length(as.character(tkget(listbox,i)))!=0){
out<-c(out,as.character(tkget(listbox,i)))
i<-i+1
}
return(out)
}

GetIt<-function(objet){
if (attr(objet,"class")=="tkwin"){
		if (as.character(tkwinfo("class",objet))=="Listbox"){
		return(getcontentlistbox(objet))
		}
}
if (attr(objet,"class")=="tclVar"){
return(tclvalue(objet))

}
}


	DoItmini<-function(comm,env=.GlobalEnv){
  result <- parse(text = paste(comm))
  exprs<-result
  i<-1
  #for (i in seq_along(exprs)) {
  ei <- exprs[i]
  #verbose#print(ei)
  result <- eval(ei, envir = env)
  
}

choixdataset<-function(id){
#verbose#print(id)
try(tkdestroy(tt2),silent=T)
tt2<-tktoplevel()
tkwm.geometry(tt2, "-100+50")     
listdata <- malist(paste(id,"Data",sep=""))
dataSetsBox <- variableListBox(tt2, listdata, title=gettext(domain="R-COSTgui","Select one dataset"))# voir initialselection
tkgrid(getFrame(dataSetsBox), sticky="nwes",column=1,row=1)
		  tkgrid.rowconfigure(getFrame(dataSetsBox), 1, weight = 1)
  tkgrid.rowconfigure(tt2, 1, weight = 1)  			


onOK <- function(){	
e1 <- new.env(parent = e2)
assign("out", getSelection(dataSetsBox), envir=e1)
out<-getSelection(dataSetsBox)
if(length(out)!=0){

# # DoItmini("print(out)",e1)
DoItmini(paste("tclvalue(lechoix",id,")<-out",sep=""),env=e1)
		# # # tkconfigure(		paste("but.",id,"Data",sep="")		,text=out)
		tkconfigure(eval(as.name(paste("but.",id,"Data",sep=""))),text=out)
		tkdestroy(tt2)	
		}
}
OK.but <-tkbutton(tt2,text="   OK   ",command=onOK)
tkgrid(OK.but,column=1,row=2)
tkfocus(tt2)
}

#le GUI

lechoixcs <- tclVar("")
lechoixcl <- tclVar("")
lechoixce <- tclVar("")
e2 <- new.env(parent = .GlobalEnv)
assign("lechoixcs",lechoixcs,envir=e2)
assign("lechoixcl",lechoixcl,envir=e2)
assign("lechoixce",lechoixce,envir=e2)
tt <- tktoplevel(borderwidth=10)
tkwm.geometry(tt, "-100+50")
font2<-tkfont.create(family="Times",size=12,weight="bold")
font3<-tkfont.create(family="Times",size=12)
fontheading<-tkfont.create(family="Times",size=18,weight="bold")  
tkwm.title(tt,gettext(domain="R-COSTgui","COST - Validation of the dataset"))
tkgrid(tklabel(tt, text=gettext(domain="R-COSTgui","Validation of the dataset"),font=fontheading))
tkgrid(tklabel(tt,text="")) # Ligne de blanc  
tkgrid(tklabel(tt,text="")) # Ligne de blanc
frameTOP<- tkframe(tt, borderwidth=2, relief="groove")
frameD<- tkframe(tt, borderwidth=2, relief="groove")
listCS<-malist("csData")
but.clData <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset("cl"),foreground="red",relief="ridge")
but.csData <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset("cs"),foreground="red",relief="ridge")
but.ceData <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset("ce"),foreground="red",relief="ridge")

tkgrid(frameTOP,frameD)
frameA<- tkframe(tt, borderwidth=2, relief="groove")
laval.but <- tkbutton(frameA,text=gettext(domain="R-COSTgui","Validate"),state="active",command=function(...) laval())
reset.but <- tkbutton(frameA,text=gettext(domain="R-COSTgui","Reset"),state="active",command=function(...) reset())
tkgrid(frameA)
tkgrid(laval.but,tklabel(frameA,text="   "),reset.but)

val.cs <- tclVar("Val_cs")
val.cl <- tclVar("Val_cl")
val.ce <- tclVar("Val_ce")
entry.cs<-entry.chemin.rdata <-tkentry(frameTOP,width="20",textvariable=val.cs)
entry.cl<-entry.chemin.rdata <-tkentry(frameTOP,width="20",textvariable=val.cl)
entry.ce<-entry.chemin.rdata <-tkentry(frameTOP,width="20",textvariable=val.ce)



tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","CsData : "),font=font2),but.csData,tklabel(frameTOP,text=" -> "),entry.cs)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","ClData : "),font=font2),but.clData,tklabel(frameTOP,text=" -> "),entry.cl)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","CeData : "),font=font2),but.ceData,tklabel(frameTOP,text=" -> "),entry.ce)


tkfocus(tt)
}
.cost.dbePlot <-
function(){




try(require(tcltk))
try(require(tcltk2))
try(library(Rcmdr))
try(require(tkrplot))
try(tclRequire("BWidget"))
try(library(COSTcore))
try(library(COSTeda));try(library(COSTgui));try(library(COSTdbe));try(library(COSTdbe))





tdspinner <- function(parent, ...) {tkwidget(parent, "spinbox", ...)}


checkcb<-function(){

if (tclvalue(cbValue.auto) == "1" & tclvalue(flag)=="1"){

valid(auto=T)

}

}


valid<-function(auto=F){


if (length(as.character(listelmt[as.numeric(tclvalue(tcl(combo.elmt,"getvalue")))+1]))==0){
Message(message = gettext(domain="R-COSTgui","You have to specify an elmt"), type = "error")
}


if (length(as.character(listelmt[as.numeric(tclvalue(tcl(combo.elmt,"getvalue")))+1]))!=0){

cmd1<-paste("dbePlot(object=", tclvalue(lechoix.but.dbeOutput),",elmt='",as.character(listelmt[as.numeric(tclvalue(tcl(combo.elmt,"getvalue")))+1]),"',dispKey=",as.logical(as.numeric(tclvalue(cbValue.dispKey))),",indScale=",as.logical(as.numeric(tclvalue(cbValue.indScale))),sep="")


if (length(as.character(listtype[as.numeric(tclvalue(tcl(combo.type,"getvalue")))+1]))!=0){
cmd1<-paste(cmd1,",type='",as.character(listtype[as.numeric(tclvalue(tcl(combo.type,"getvalue")))+1]),"'",sep="")
}

if (length(as.character(listXstratum[as.numeric(tclvalue(tcl(combo.Xstratum,"getvalue")))+1]))!=0 
)
{

if (as.character(listXstratum[as.numeric(tclvalue(tcl(combo.Xstratum,"getvalue")))+1]) != "NULL"){

cmd1<-paste(cmd1,",Xstratum='",as.character(listXstratum[as.numeric(tclvalue(tcl(combo.Xstratum,"getvalue")))+1]),"'",sep="")
}
}


if (tclvalue(cbValue.step)=="1"){
cmd1<-paste(cmd1,",step=",as.numeric(tclvalue(lechoix.step)),sep="")
}

if (tclvalue(val.ylab)!=""){
cmd1<-paste(cmd1,",ylab='",tclvalue(val.ylab),"'",sep="")
}

if (tclvalue(val.titre)!=""){
cmd1<-paste(cmd1,",main='",tclvalue(val.titre),"'",sep="")
}

if (tclvalue(val.layout1)!="1" | tclvalue(val.layout2)!="1"){
cmd1<-paste(cmd1,",layout=c(",tclvalue(val.layout1),",",tclvalue(val.layout2),")",sep="")
}

cmd1<-paste(cmd1,")",sep="")

print(cmd1)


doItAndPrint(cmd1,log=!auto & as.logical(as.numeric(tclvalue(cbValue.simu))))

# puis on fait RaiseLgthBoot


}

}

choixdataset.multi<-function(id,nom.but){
print(id)
try(tkdestroy(tt2),silent=T)
tt2<-tktoplevel()
tkwm.geometry(tt2, "-100+50")     
listdata <-NULL
for ( i in id){listdata <- c(listdata,malist(i))}
dataSetsBox <- variableListBox(tt2, listdata, title=gettext(domain="R-COSTgui","Select one dataset"))# voir initialselection
tkgrid(getFrame(dataSetsBox), sticky="nwes",column=1,row=1)
  tkgrid.rowconfigure(getFrame(dataSetsBox), 1, weight = 1)
  tkgrid.rowconfigure(tt2, 1, weight = 1)  
onOK <- function(){	
out<-getSelection(dataSetsBox)

		if ( length(out) != 0){
		eval(parse(text=paste("tclvalue(lechoix.",nom.but,")<-out",sep="")))
		tkconfigure(eval(as.name(nom.but)),text=out)
		# print(GetIt((eval(as.name(paste("lechoix.",nom.but,sep=""))))))
		# tkconfigure( names.list.quali, state="disabled" )	
		# tkconfigure( names.list.quanti, state="disabled" )
		
		

		# print("on met a jour la liste des variables")

		# a ce niveau on a le nom du cs objet, on va lister toute les variaible exploitable pour le spaceplot
	if (nom.but == "but.dbeOutput"){

	
	# on charge la description
	tclvalue(val.desc)<-eval(as.name(out))@desc
	tclvalue(val.ylab)<-eval(as.name(out))@param
	tkconfigure(entry.ylab ,state="normal")
	tkconfigure(entry.titre ,state="normal")
	tkconfigure(valid.but ,state="normal")
	tclvalue(flag)<-"1"
	
  # listslot<-slotNamesdata(eval(as.name(out)))
# araj<-c()
# for ( i in 1:length(listslot)){	
# truc<-do.call("slot",args =list(as.name(out),listslot[i]))
# araj<-unique(c(araj,levels(as.factor(truc[["spp"]]))));araj
# }
# tkdelete(list.species,"0","end");
# for (var in araj) {tkinsert(list.species, "end", var)}

# on check les LAN et DIS

	# LD<-unique(substr(eval(as.name(out))@hl$sort,1,3))
	# print(LD)
# if(!is.element("DIS",LD)){
# print("pas de DIS")
# tkconfigure(rbdis,state="disabled")
# tclvalue(tvarcatchCat)<-"LAN"

# }
# if(!is.element("LAN",LD)){
# print("pas de LAN")
# tkconfigure(rblan,state="disabled")
# tclvalue(tvarcatchCat)<-"DIS"
# }

		}
	
		
# aactiver<-list(entry.ylab,entry.xlab,list.time,list.tech,list.space,but.refr,entry.fonction,entry.main,cb.simu,cb.auto,list.time,list.tech,spin.x1,spin.x2,spin.y1,spin.y2,rbsel.image,rbsel.contour,rbsel.bubble,rbsel.value)
# # for ( i in 1:length(aactiver)){tkconfigure(aactiver[[i]],state="normal")}

# lapply(aactiver,tkconfigure,state="normal")

# choixtime<-c("year","semester","quarter","month")
# choixspace<-c("rect","subrect","area")
# choixtech<-c("commCat","commCatScl", "foCatEu5", "foCatEu6", "foCatNat","catchCat")
# tkdelete(list.time,"0","end");for (var in choixtime) {tkinsert(list.time, "end", var)}
# tkdelete(list.tech,"0","end");for (var in choixtech) {tkinsert(list.tech, "end", var)}
# tkdelete(list.space,"0","end");for (var in choixspace) {tkinsert(list.space, "end", var)}


	
		
}
tkdestroy(tt2)	
		
}
	
OK.but <-tkbutton(tt2,text=gettext(domain="R-COSTgui","   OK   "),command=onOK)
tkgrid(OK.but,column=1,row=2)
tkfocus(tt2)
}

#fonctions pratique pour tk
malist<-function (pat) {

# retrouve dans ls les element de type pat
    Vars <- ls(all.names = TRUE,env=.GlobalEnv)
	# print(Vars)
    if (length(Vars) == 0) {return(Vars)}
    names(which(sapply(Vars, function(.x) {	is(get(.x))[1]==pat	}	)	)	)
	}
	
checkstep<-function(){
tkconfigure(spin.step,state=c("disabled","normal")[as.numeric(tclvalue(cbValue.step))+1])
checkcb()
}

lechoix.but.dbeOutput <- tclVar("")
lechoix.but.clObject <- tclVar("")
lechoix.but.strini <- tclVar("")
flag <- tclVar("1")
# lechoix.p1 <- tclVar(0.025)
# lechoix.p2 <- tclVar(0.975)
lechoix.step <- tclVar("") # a voir si c 'est bien par defaut

tt <- tktoplevel(borderwidth=10)
tkwm.geometry(tt, "-100+50")
font2<-tkfont.create(family="Times",size=12,weight="bold")
font3<-tkfont.create(family="Times",size=12)
fontheading<-tkfont.create(family="Times",size=18,weight="bold")  
tkwm.title(tt,gettext(domain="R-COSTgui","COST - Figure : dbe"))
tkgrid(tklabel(tt, text=gettext(domain="R-COSTgui","Figure : dbe"),font=fontheading))
tkgrid(tklabel(tt,text="")) # Ligne de blanc
frameTOP<- tkframe(tt, borderwidth=2, relief="groove")
frameBONUS<- tkframe(tt, borderwidth=2, relief="groove")
# frameA<- tkframe(tt, borderwidth=2, relief="groove")
# frameB<- tkframe(tt, borderwidth=2, relief="groove")
# frameC<- tkframe(tt, borderwidth=2, relief="groove")
# frameD<- tkframe(tt, borderwidth=2, relief="groove")
# frameE<- tkframe(frameTOP, borderwidth=2, relief="groove")
frameF<- tkframe(tt, borderwidth=2, relief="groove")
framelayout<- tkframe(frameF, borderwidth=2, relief="groove")



# les entry
val.desc <- tclVar("")
entry.desc <-tkentry(frameTOP,width="20",textvariable=val.desc,state="disabled")
val.titre <- tclVar("")
entry.titre <-tkentry(frameTOP,width="20",textvariable=val.titre,state="disabled")
val.ylab <- tclVar("")
entry.ylab <-tkentry(frameTOP,width="20",textvariable=val.ylab,state="disabled")
val.species <- tclVar("")
entry.species <-tkentry(frameTOP,width="20",textvariable=val.species)

val.layout1 <- tclVar("1")
entry.layout1 <-tkentry(framelayout,width="2",textvariable=val.layout1)
val.layout2 <- tclVar("1")
entry.layout2 <-tkentry(framelayout,width="2",textvariable=val.layout2)

# val.out <- tclVar("out_dbe.boot")
# entry.out <-tkentry(frameD,width="20",textvariable=val.out)
# val.param <- tclVar("")
# entry.param <-tkentry(frameTOP,width="20",textvariable=val.param)

#les combo

listelmt<-unique(c("lenStruc$estim", "ageVar","totalNnum$cv","ageNum$cv","ageNum$ci","ageStruc$estim","lenNum$cv","lenNum$ci","lenStruc$estim"))
combo.elmt <- tkwidget(frameTOP,"ComboBox",editable=FALSE,values=listelmt,state="normal")


listtype<-c("bar", "point","line")
combo.type <- tkwidget(frameTOP,"ComboBox",editable=FALSE,values=listtype,state="normal")


listXstratum<-c("Time", "space", "Technical","NULL")
combo.Xstratum <- tkwidget(frameTOP,"ComboBox",editable=FALSE,values=listXstratum,state="normal")

#les spin
# spin.p1<-tdspinner(frameF,from=0,to=1,inc=0.001,width=5,textvariable = lechoix.p1,state="normal",command=function(...)proba(1))
# spin.p2<-tdspinner(frameF,from=0,to=1,inc=0.001,width=5,textvariable = lechoix.p2,state="disabled",command=function(...)proba(2))
spin.step<-tdspinner(frameF,from=0,to=100,inc=1,width=4,textvariable = lechoix.step,state="disabled")


# les radio
# tvarcatchCat <- tclVar("NULL")
# rbdis <- tkradiobutton(frameB,command=function()choixrb())
# rblan <- tkradiobutton(frameB,command=function()choixrb())
# tkconfigure(rbdis,variable=tvarcatchCat,value="DIS",state="normal")
# tkconfigure(rblan,variable=tvarcatchCat,value="LAN",state="normal")


# tvarmethodDesc <- tclVar("NULL")
# rbanal <- tkradiobutton(frameC,command=function()choixrb())
# rbboot <- tkradiobutton(frameC,command=function()choixrb())
# tkconfigure(rbanal,variable=tvarmethodDesc,value="analytic",state="normal")
# tkconfigure(rbboot,variable=tvarmethodDesc,value="bootstrap",state="normal")
#les bouton


# but.strini <- tkbutton(frameA,text=gettext(domain="R-COSTgui","<select>"),state="active",command=function(...) choixdataset.multi("strIni","but.strini"),foreground="red",relief="ridge")
# but.newstrini <- tkbutton(frameA,text=gettext(domain="R-COSTgui","New strIni type object"),state="active",command=function(...) .cost.strIni())#,foreground="red",relief="ridge")
# but.resetstrini <- tkbutton(frameA,text="reset",state="active",command=function(...) reset.strIni())#,foreground="red",relief="ridge")
valid.but <- tkbutton(tt,text=gettext(domain="R-COSTgui","Validate"),state="disabled",command=function(...) valid())

but.dbeOutput <- tkbutton(frameBONUS,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset.multi(c("dbeOutput"),"but.dbeOutput"),foreground="red",relief="ridge")

# but.clObject <- tkbutton(frameBONUS,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset.multi(c("clDataCons"),"but.clObject"),foreground="red",relief="ridge")

# les check

cbValue.indScale<-tclVar("0")
cb.indScale <- tkcheckbutton(frameF,variable=cbValue.indScale,state="normal",command=function()checkcb())


cbValue.auto<-tclVar("0")
cb.auto <- tkcheckbutton(frameF,variable=cbValue.auto,state="normal")

cbValue.simu<-tclVar("1")
cb.simu <- tkcheckbutton(frameF,variable=cbValue.simu,state="normal")


cbValue.dispKey<-tclVar("1")
cb.dispKey <- tkcheckbutton(frameF,variable=cbValue.dispKey,state="normal",command=function()checkcb())


cbValue.step<-tclVar("0")
cb.step <- tkcheckbutton(frameF,variable=cbValue.step,state="normal",command=function()checkstep())

#text

# text.valid<-tklabel(frameD,text=gettext(domain="R-COSTgui","out :"))
#list

# list.species <-tklistbox(frameE,selectmode="unique",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.species.scroll,...))
# list.species.scroll<-tkscrollbar(frameE,repeatinterval=5,command=function(...)tkyview(list.species,...))

tkgrid(frameBONUS)
tkgrid(frameTOP)
# tkgrid(frameA)
# tkgrid(frameB)
# tkgrid(frameC)
tkgrid(frameF)

# tkgrid(frameD)

tkgrid(tklabel(frameBONUS,text=gettext(domain="R-COSTgui","dbeOutput:"),font=font2),but.dbeOutput)
# tkgrid(tklabel(frameBONUS,text="clDataCons:",font=font2),but.clObject)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Description : ")),entry.desc)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Title : ")),entry.titre)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","ylab : ")),entry.ylab)
# tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","species : "),font=font2),entry.species)
# tkgrid(tklabel(frameE,text=gettext(domain="R-COSTgui","Species"),font=font2))
# tkgrid(list.species,list.species.scroll,sticky="nswe")




tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Type : ")),combo.type)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Element : ")),combo.elmt)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","X stratum : ")),combo.Xstratum)
# tkgrid(frameE,columnspan=2)#,sticky="ew")

# tkgrid(tklabel(frameA,text=gettext(domain="R-COSTgui","StrIni : "),font=font2),but.strini,but.newstrini,but.resetstrini)
# tkgrid(tklabel(frameB,text="LAN : ",font=font2),rblan,tklabel(frameB,text="DIS : ",font=font2),rbdis)
# tkgrid(tklabel(frameC,text="analytic : ",font=font2),rbanal,tklabel(frameC,text="bootstrap : ",font=font2),rbboot)

tkgrid(frameF)

tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","Legend : ")),row=1,column=1,columnspan=2,sticky="w")
tkgrid(cb.dispKey,row=1,column=3)
tkgrid(cb.step,row=2,column=1)
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","Step : ")),row=2,column=2,sticky="w")
tkgrid(spin.step,row=2,column=3)


tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","indScale : ")),row=3,column=1,columnspan=2,sticky="w")
tkgrid(cb.indScale,row=3,column=3)
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","layout : ")),row=4,column=1,columnspan=2,sticky="w")
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","auto :")),row=5,column=1,columnspan=2,sticky="w")
tkgrid(cb.auto,row=5,column=3,columnspan=1,sticky="w")

tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","save the script :")),row=6,column=1,columnspan=2,sticky="w")
tkgrid(cb.simu,row=6,column=3,columnspan=1,sticky="w")

tkgrid(framelayout,row=4,column=3)
tkgrid(entry.layout1,entry.layout2)
# tkgrid(tklabel(frameF,text="probas : "),spin.p1,spin.p2)
# tkgrid(text.valid,entry.out,valid.but)
tkgrid(valid.but)

tkfocus(tt)

}
.cost.deltcalc <-
function(){



try(require(tcltk))
try(require(tcltk2))
try(library(Rcmdr))
try(require(tkrplot))
try(tclRequire("BWidget"))
try(library(COSTcore))
try(library(COSTeda));try(library(COSTgui))


slotNamesdata<-function(data){
ploom<-slotNames(data)
out<-c()
for (p in ploom){
if(is(slot(data, p))[1] == "data.frame"){
out<-c(out,p)
}
}

return(out)
}
choixdataset2<-function(id){
#verbose#print(id)
try(tkdestroy(tt2),silent=T)
# try(tkdestroy(tt2),silent=T) # c ne fonctionne pas vraiment, je voudrais que ca ferme la fenetre, ou vrifie avant d'en ouvrir une novuelle
tt2<-tktoplevel()
tkwm.geometry(tt2, "-100+50")     
# listdata <- listDataSets()
# listdata <- c(malist("csData"),malist("ceData"),malist("clData"))
listdata <- malist(paste(id,"Data",sep=""))

if ( id == "strini"){
listdata <- malist("strIni")

}


dataSetsBox <- variableListBox(tt2, listdata, title=gettext(domain="R-COSTgui","Select one dataset"))# voir initialselection
tkgrid(getFrame(dataSetsBox), sticky="nwes",column=1,row=1)
  tkgrid.rowconfigure(getFrame(dataSetsBox), 1, weight = 1)
  tkgrid.rowconfigure(tt2, 1, weight = 1)  					
						
	
	
	
	

onOK <- function(){	

out<-getSelection(dataSetsBox)
	
		#verbose#print("voila2:")
		#verbose#print(GetIt((eval(as.name(paste("lechoix",id,sep=""))))))
		
		#verbose#print("id")
		#verbose#print(id)
		if ( length(out) != 0){
		
		
		# DoItmini(paste("tclvalue(choix",id,")<-out",sep=""),env=e1)
		
		
		eval(parse(text=paste("tclvalue(lechoix",id,")<-out",sep="")))
		#verbose#print("bon")
		

		# # DoItmini(paste("tkconfigure(choice.",id,"Data.but, text=out)",sep=""))
		tkconfigure(eval(as.name(paste("but.",id,sep=""))),text=out)
		tkconfigure(but.csData.subset,state="normal")
		#verbose#print(GetIt((eval(as.name(paste("lechoix",id,sep=""))))))
		# tkconfigure( names.list.quali, state="disabled" )	
		# tkconfigure( names.list.quanti, state="disabled" )
		
		
		if ( id=="cs"){
		
		#verbose#print("on met a jour la liste des especes")

truc<-do.call("slot",args =list(as.name(as.character(GetIt(lechoixcs))),"hl"))
tkdelete(list.especes,"0","end")
		for (var in levels(as.factor(truc$spp))) {tkinsert(list.especes, "end", var)}
		
		# elvar<-as.name(as.character(tkget(names.list.quali,tkcurselection(names.list.quali))))
		
		
		
		
		}
		
		
		
		
		}
			tkdestroy(tt2)	
		
}
	
OK.but <-tkbutton(tt2,text="   OK   ",command=onOK)
tkgrid(OK.but,column=1,row=2)
tkfocus(tt2)
}
#fonctions pratique pour tk
malist<-function (pat) {

# retrouve dans ls les element de type pat
    Vars <- ls(all.names = TRUE,env=.GlobalEnv)
	# print(Vars)
    if (length(Vars) == 0) {return(Vars)}
    names(which(sapply(Vars, function(.x) {	is(get(.x))[1]==pat	}	)	)	)
	}

getcontentlistbox<-function(listbox){
out<-c()
i<-0		
while (length(as.character(tkget(listbox,i)))!=0){
out<-c(out,as.character(tkget(listbox,i)))
i<-i+1
}
return(out)
}
GetIt<-function(objet){
if (attr(objet,"class")=="tkwin"){
		if (as.character(tkwinfo("class",objet))=="Listbox"){
		return(getcontentlistbox(objet))
		}
}
if (attr(objet,"class")=="tclVar"){
return(tclvalue(objet))

}
}
DoItmini<-function(comm,env=.GlobalEnv){
  result <- parse(text = paste(comm))
  exprs<-result
  i<-1
  #for (i in seq_along(exprs)) {
  ei <- exprs[i]
  #verbose#print(ei)
  result <- eval(ei, envir = env)
  
}


plotdelta<-function(auto=F){

#verbose#print("plotdelta")
#verbose#print(auto)
#verbose#print(tclvalue(cbValue.auto))
# print("ert")
if ( auto==T & tclvalue(cbValue.auto)=="0"){return(NULL)}

# if(!exists("mondelta",e2)){print("pas de mondeltat dans e2")}

if(exists("mondelta",e2)){
#verbose#print("mondelta existe dans e2")
str1<-str2<-NULL
try(silent=T,str1<-as.character(paste(as.character(tkget(list.strate1,as.character(tkcurselection(list.strate1)))),collapse=" ")))
try(silent=T,str2<-as.character(paste(as.character(tkget(list.strate2,as.character(tkcurselection(list.strate2)))),collapse=" ")))
str1
str2
if (length(str1)==0 | length(str2)==0){
Message(message = gettext(domain="R-COSTgui","You have to select strate1 and strat2"),  type = "error");return(NULL)}

letp<-"all"
if (length(as.character(tkcurselection(list.bonusTP)))!=0){
letp<-c()
for ( rg in as.character(tkcurselection(list.bonusTP))){
letp<-c(letp,as.character(tkget(list.bonusTP,rg)))
}
}


letc<-"all"
if (length(as.character(tkcurselection(list.bonusTC)))!=0){
letc<-c()
for ( rg in as.character(tkcurselection(list.bonusTC))){
letc<-c(letc,as.character(tkget(list.bonusTC,rg)))
}
}
lesp<-"all"
if (length(as.character(tkcurselection(list.bonusSP)))!=0){
lesp<-c()
for ( rg in as.character(tkcurselection(list.bonusSP))){
lesp<-c(lesp,as.character(tkget(list.bonusSP,rg)))
}
}
# list(tp = "all", sp = c("27E2","28E5"), tc = "all")
argelmnt <- as.list(quote(list()))
argelmnt <- c(argelmnt,list(tp=quote(letp),tc=quote(letc),sp=quote(lesp)))
leselements <- c(as.list(call("list")), eval(as.call(argelmnt))   );leselements
#verbose# cat(deparse(as.call(leselements)), "\n")

#verbose#print('ok')
largs <- as.list(quote(list()))
largs <- c(largs,list(x=quote(get("mondelta",e2)),
strat1=quote(as.character(paste(as.character(tkget(list.strate1,as.character(tkcurselection(list.strate1)))),collapse=" "))), 
strat2=quote(as.character(paste(as.character(tkget(list.strate2,as.character(tkcurselection(list.strate2)))),collapse=" "))),
selection=(as.logical(as.numeric(tclvalue(tvar))) & !auto),
elmts=quote(as.call(leselements))
));largs
tmp <- c(as.list(call("plot")), eval(as.call(largs))   );tmp
   #verbose# cat(deparse(as.call(tmp)), "\n")
# eval(as.call(tmp))
   
 
   
if ( tclvalue(cbValue.simu) == "1" & auto==T){

#on affiche rien mais on met quand meme a jour le graph
eval(as.call(tmp))


}  
   
   
if ( tclvalue(cbValue.simu) == "1" & auto==F){

# si tclvalue(val.delta) n 'xiste pas , il faut ptet forcer sa generation ici dans rcmdr

as.name(tclvalue(val.delta))
if(!exists(as.character(tclvalue(val.delta)))){gendelta(flag=T)}


largs <- as.list(quote(list()))
largs <- c(largs,list(x=quote(as.name(tclvalue(val.delta))),
strat1=quote(as.character(paste(as.character(tkget(list.strate1,as.character(tkcurselection(list.strate1)))),collapse=" "))), 
strat2=quote(as.character(paste(as.character(tkget(list.strate2,as.character(tkcurselection(list.strate2)))),collapse=" "))),
selection=quote((as.logical(as.numeric(tclvalue(tvar))) & !auto)),
elmts=quote(as.call(leselements))
));largs
tmp <- c(as.list(call("plot")), eval(as.call(largs))   );tmp


doItAndPrint(paste(sep="",tclvalue(val.delta.plot),"<-",paste(deparse(as.call(tmp)),collapse="")))
if ((as.logical(as.numeric(tclvalue(tvar))) & !auto)){
X11()
doItAndPrint(paste("plot(",tclvalue(val.delta.plot),")",sep=""))

}


 # paste(sep="",tclvalue(val.delta),"<-",deparse(as.call(tmp)))

}
if ( tclvalue(cbValue.simu) == "0"){
if (!(as.logical(as.numeric(tclvalue(tvar))) & !auto)){
eval(as.call(tmp))
}
if ((as.logical(as.numeric(tclvalue(tvar))) & !auto)){
# eval(as.call(tmp))
# doItAndPrint(paste("plot(",tclvalue(val.delta.plot),")",sep=""))

AEF<-eval(as.call(tmp))
X11()
plot(AEF)
# justDoIt("plot(eval(as.call(tmp)))")



}

}
}


}


gendelta<-function(flag=F){
#verbose#print('gendelta')
leCS<-NULL
lestr<-NULL
lessp<-NULL

try(leCS<- as.name(as.character(GetIt(lechoixcs))),silent=T)
try(lestr<-as.name(as.character(GetIt(lechoixstrini))),silent=T)
try(lessp<-as.character(paste(as.character(tkget(list.especes,as.character(tkcurselection(list.especes)))),collapse=" ")),silent=T)

if(length(leCS)==0){Message(message = gettext(domain="R-COSTgui","You have to choose a Csdata"),  type = "error");return(NULL)}
if(length(lestr)==0){Message(message = gettext(domain="R-COSTgui","You have to select a strIni type object"),  type = "error");return(NULL)}
if(length(lessp)==0){Message(message = gettext(domain="R-COSTgui","You have to choose a species"),  type = "error");return(NULL)}



leCS
lestr
lessp

# tkconfigure(gen.but,text=gettext(domain="R-COSTgui","wait..."),state="disabled")

# on va controler que tout est pret






#verbose#print("on recalcul deltas")
# ledelta<-call("deltCalc",data=call("eval",quote(leCS)),strDef=call("eval",quote(lestr)),species=call("eval",quote(lessp)))
# # ledelta<-call("deltCalc",data=call("eval",quote(as.name(as.character(GetIt(lechoixcs))))),strDef=call("eval",quote(as.name(as.character(GetIt(lechoixstrini))))),species=call("eval",quote(as.character(paste(as.character(tkget(list.especes,as.character(tkcurselection(list.especes)))),collapse=" ")))))
# # ledelta
# # deltas<-eval(ledelta)  # ne devrai pas etre fait à chauqe fois. faudra verifier si les parametre sot novueau
# # deparse(ledelta)

if (tclvalue(cbValue.csData.subset)=="0"){

largs <- as.list(quote(list()))
largs <- c(largs,list(data=quote(as.name(as.character(GetIt(lechoixcs)))),strDef=quote(as.name(as.character(GetIt(lechoixstrini)))),species=quote(paste(as.character(paste(as.character(tkget(list.especes,as.character(tkcurselection(list.especes)))))),sep=" ",collapse=" "))))
tmp <- c(as.list(call("deltCalc")), eval(as.call(largs))   );tmp
   #verbose# cat(deparse(as.call(tmp)), "\n")
   
   }
 


if (tclvalue(cbValue.csData.subset)=="1"){

if ( tclvalue(cbValue.simu) == "0"){

lesub<-get("formule",.envi.deltcalc)[[1]];lesub
lCStmp<-as.list(quote(list()))
lCStmp <- c(lCStmp,list(x=quote(strsplit(lesub,"<-")[[1]][1]),envir=quote(as.name(".envi.deltcalc"))))
tmp2 <- c(as.list(call("get")), eval(as.call(lCStmp))   );tmp2
lCS<-as.call(tmp2);lCS



largs <- as.list(quote(list()))
largs <- c(largs,list(data=quote(lCS),strDef=quote(as.name(as.character(GetIt(lechoixstrini)))),species=quote(paste(as.character(paste(as.character(tkget(list.especes,as.character(tkcurselection(list.especes)))))),sep=" ",collapse=" "))))
tmp <- c(as.list(call("deltCalc")), eval(as.call(largs))   );tmp
   #verbose# cat(deparse(as.call(tmp)), "\n")
   

}



if ( tclvalue(cbValue.simu) == "1"){
lesub<-get("formule",.envi.deltcalc)
for ( i in 1:length(lesub)){
doItAndPrint(lesub[[i]])
}

lCS<-strsplit(lesub[[1]],"<-")[[1]][1]

largs <- as.list(quote(list()))
largs <- c(largs,list(data=quote(as.name(as.character(lCS))),strDef=quote(as.name(as.character(GetIt(lechoixstrini)))),species=quote(paste(as.character(paste(as.character(tkget(list.especes,as.character(tkcurselection(list.especes)))))),sep=" ",collapse=" "))))
tmp <- c(as.list(call("deltCalc")), eval(as.call(largs))   );tmp
   #verbose# cat(deparse(as.call(tmp)), "\n")
   

}



   }
   

 
   
   if ( tclvalue(cbValue.simu) == "1"){
   doItAndPrint(  
    paste(sep="",tclvalue(val.delta),"<-",deparse(as.call(tmp)))
   )
   assign("mondelta",eval(as.name(tclvalue(val.delta))),envir=e2)
   #ptet un assign ici
   }
      if ( tclvalue(cbValue.simu) == "0"){
   # .mondeltas<-eval(as.call(tmp))}
   assign("mondelta",eval(as.call(tmp)),envir=e2)
tclvalue(val.delta)
        # flush.console()

}


tkconfigure(rbseloui,state="normal")
tkconfigure(rbselnon,state="normal")
tkconfigure(refr.but,state="normal")
tkconfigure(entry.delta.plot,state="normal")
tkconfigure(cb.simu,state="normal")
tkconfigure(cb.auto,state="normal")
tkconfigure(list.strate2,state="normal")
tkconfigure(list.strate1,state="normal")

choix.strate<-c("techStrata","timeStrata","spaceStrata")

tkdelete(list.bonusTP,"0","end")
tkdelete(list.bonusTC,"0","end")
tkdelete(list.bonusSP,"0","end")
tkconfigure(list.bonusTP,state="normal")
tkconfigure(list.bonusTC,state="normal")
tkconfigure(list.bonusSP,state="normal")



# il faut récuperer deltas2@outPut$SampDeltaMat
if ( tclvalue(cbValue.simu) == "1"){

truc<-eval(as.name(tclvalue(val.delta)))
truc2<-truc@outPut$SampDeltaMat



}

if ( tclvalue(cbValue.simu) == "0"){
#verbose#print("par ici")
truc<-get("mondelta",e2)
truc2<-truc@outPut$SampDeltaMat
}

if(is.element("tp",names(truc2))){
for (var in unique(truc2$tp)) {tkinsert(list.bonusTP, "end", var)}
}
if(is.element("tc",names(truc2))){
for (var in unique(truc2$tc)) {tkinsert(list.bonusTC, "end", var)}
}
if(is.element("sp",names(truc2))){
for (var in unique(truc2$sp)) {tkinsert(list.bonusSP, "end", var)}
}



if (flag==F){
tkdelete(list.strate1,"0","end")
tkdelete(list.strate2,"0","end")
for (var in choix.strate) {tkinsert(list.strate2, "end", var);tkinsert(list.strate1, "end", var)}
# tkconfigure(gen.but,text="create",state="normal")		
}



}






faitsubset<-function(envi){
#verbose#print("faitsubset")


res<-.cost.subset.virtual(virtual=T,sour=c(1,0,0),dataname=tclvalue(lechoixcs),LENV=envi,parr=tt,label="(Comparaison des échantillons en taille)")

tclvalue(cbValue.csData.subset)<-"1"
tkconfigure(cb.csData.subset,state="normal")

}








dejaplot<-F
choix.strate<-c("techStrata","timeStrata","spaceStrata")
lechoixcs <- tclVar("")
lechoixstrini <- tclVar("")
# lechoixce <- tclVar("")
e2 <- new.env(parent = .GlobalEnv)
assign("lechoixcs",lechoixcs,envir=e2)
.envi.deltcalc <- new.env(parent = .GlobalEnv)

assign(".envi.deltcalc",.envi.deltcalc,pos=1)

# assign("lechoixcl",lechoixcl,envir=e2)
tt <- tktoplevel(borderwidth=10)
tkwm.geometry(tt, "-100+50")
font2<-tkfont.create(family="Times",size=12,weight="bold")
font3<-tkfont.create(family="Times",size=12)
fontheading<-tkfont.create(family="Times",size=18,weight="bold")  
tkwm.title(tt,gettext(domain="R-COSTgui","COST - Figure : Samples size comparison"))
tkgrid(tklabel(tt, text=gettext(domain="R-COSTgui","Samples size comparison"),font=fontheading))
tkgrid(tklabel(tt,text="")) # Ligne de blanc  
tkgrid(tklabel(tt,text="")) # Ligne de blanc
frameTOP<- tkframe(tt, borderwidth=2, relief="groove")
# frameD<- tkframe(tt, borderwidth=2, relief="groove")
frameE<- tkframe(tt, borderwidth=2, relief="groove")
frameF<- tkframe(tt, borderwidth=2, relief="groove")
frameG<- tkframe(tt, borderwidth=2, relief="groove")
frameBONUS<- tkframe(tt, borderwidth=2, relief="groove")
frameA<- tkframe(tt, borderwidth=2, relief="groove")
frameA2<- tkframe(frameA, borderwidth=2, relief="groove")
# listCS<-malist("csData")
but.strini <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<select>"),state="active",command=function(...) choixdataset2("strini"),foreground="red",relief="ridge")
but.newstrini <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","New strIni type object"),state="active",command=function(...) .cost.strIni())#,foreground="red",relief="ridge")
but.cs <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset2("cs"),foreground="red",relief="ridge")

but.csData.subset <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","subset"),state="disabled",command=function(...) faitsubset(.envi.deltcalc))
cbValue.csData.subset<- tclVar("0")
cb.csData.subset<-tkcheckbutton(frameTOP,variable=cbValue.csData.subset,state="disabled")
text.simu<-tklabel(frameA,text=gettext(domain="R-COSTgui","save the script :"))
cbValue.simu<-tclVar("0")
cb.simu<-tkcheckbutton(frameA,variable=cbValue.simu,state="normal")




# but.ceData <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset("ce"),foreground="red",relief="ridge")

tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","CsData : "),font=font2),but.cs,but.csData.subset)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","StrIni : "),font=font2),but.strini,but.newstrini)
tkgrid(tklabel(frameTOP,text=" "),tklabel(frameTOP,text=gettext(domain="R-COSTgui","use subset :")),cb.csData.subset)

# tkgrid(tklabel(frameTOP,text=gettext"CsData : ",font=font2),but.cs)
# tkgrid(tklabel(frameTOP,text="CeData : ",font=font2),but.ceData)
tkgrid(frameTOP)#,frameD)
tkgrid(frameE)
tkgrid(frameF)

tkgrid(tklabel(tt,text=" "))
tkgrid(frameG)
tkgrid(frameBONUS)
tkgrid(frameA)
list.especes <-tklistbox(frameE,selectmode="unique",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.especes.scroll,...))
list.especes.scroll<-tkscrollbar(frameE,repeatinterval=5,command=function(...)tkyview(list.especes,...))
tkgrid(list.especes,list.especes.scroll,sticky="ns")

# on va faire des menu déroulant






# # combo.strate1 <- tkwidget(frameG,"ComboBox",editable=FALSE,values=choix.strate,state="normal",command=function(...) {prhint('coucou')})
# # combo.strate2 <- tkwidget(frameG,"ComboBox",editable=FALSE,values=choix.strate,state="normal")


tkgrid(tklabel(frameG,text=gettext(domain="R-COSTgui","strate1")),tklabel(frameG,text=" "),tklabel(frameG,text=gettext(domain="R-COSTgui","strate2")),tklabel(frameG,text="  "))



list.strate1 <-tklistbox(frameG,selectmode="unique",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.strate1.scroll,...),state="disabled")
list.strate1.scroll<-tkscrollbar(frameG,repeatinterval=5,command=function(...)tkyview(list.strate1,...))
list.strate2 <-tklistbox(frameG,selectmode="unique",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.strate2.scroll,...),state="disabled")
list.strate2.scroll<-tkscrollbar(frameG,repeatinterval=5,command=function(...)tkyview(list.strate2,...))



tkgrid(list.strate1,list.strate1.scroll,list.strate2,list.strate2.scroll,sticky="ns")







# un tb pour selection
tvar <- tclVar(FALSE)
rbseloui <- tkradiobutton(frameA2,state="disabled",command=function()ls())
rbselnon <- tkradiobutton(frameA2,state="disabled",command=function()ls())
tkconfigure(rbseloui,variable=tvar,value=TRUE,state="disabled")
tkconfigure(rbselnon,variable=tvar,value=FALSE,state="disabled")
tkgrid(tklabel(frameA,text=gettext(domain="R-COSTgui","Selection : ")),frameA2)
tkgrid(tklabel(frameA2,text=gettext(domain="R-COSTgui","Yes :")),rbseloui,tklabel(frameA2,text=gettext(domain="R-COSTgui","No :")),rbselnon)


gen.but <- tkbutton(frameF,text=gettext(domain="R-COSTgui","create"),state="normal",command=function(...) gendelta())
refr.but <- tkbutton(frameA,text=gettext(domain="R-COSTgui","plot"),state="disabled",command=function(...) plotdelta())


text.delta<-tklabel(frameF,text=gettext(domain="R-COSTgui","delta : "))
val.delta <- tclVar("deltas")
entry.delta <-tkentry(frameF,width="20",textvariable=val.delta,state="normal")

text.delta.plot<-tklabel(frameA,text=gettext(domain="R-COSTgui","delta.plot : "))
val.delta.plot <- tclVar("deltas.plot")
entry.delta.plot <-tkentry(frameA,width="20",textvariable=val.delta.plot,state="disabled")

cbValue.simu<-tclVar("0")
cb.simu <- tkcheckbutton(frameA,variable=cbValue.simu,state="normal")

cbValue.auto<-tclVar("1")
cb.auto <- tkcheckbutton(frameA,variable=cbValue.auto,state="disabled")
tkgrid(text.delta,entry.delta,gen.but)
tkgrid(text.delta.plot,entry.delta.plot)
tkgrid(tklabel(frameA,text=gettext(domain="R-COSTgui","save the script : ")),cb.simu)
tkgrid(tklabel(frameA,text=gettext(domain="R-COSTgui","automatic :")),cb.auto)
tkgrid(refr.but)

# on met les 3 dans frame BONUS



tkgrid(tklabel(frameBONUS,text=gettext(domain="R-COSTgui","Temporal")),tklabel(frameBONUS,text=" "),tklabel(frameBONUS,text=gettext(domain="R-COSTgui","Technical")),tklabel(frameBONUS,text="  "),tklabel(frameBONUS,text=gettext(domain="R-COSTgui","Spacial")),tklabel(frameBONUS,text="  "))

list.bonusSP <-tklistbox(frameBONUS,selectmode="multiple",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.bonusSP.scroll,...),state="disabled")
list.bonusSP.scroll<-tkscrollbar(frameBONUS,repeatinterval=5,command=function(...)tkyview(list.bonusSP,...))
list.bonusTP <-tklistbox(frameBONUS,selectmode="multiple",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.bonusTP.scroll,...),state="disabled")
list.bonusTP.scroll<-tkscrollbar(frameBONUS,repeatinterval=5,command=function(...)tkyview(list.bonusTP,...))
list.bonusTC <-tklistbox(frameBONUS,selectmode="multiple",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.bonusTC.scroll,...),state="disabled")
list.bonusTC.scroll<-tkscrollbar(frameBONUS,repeatinterval=5,command=function(...)tkyview(list.bonusTC,...))



tkgrid(list.bonusTP,list.bonusTP.scroll,list.bonusTC,list.bonusTC.scroll,list.bonusSP,list.bonusSP.scroll,sticky="ns")






tkbind(list.strate1, "<ButtonRelease-1>", function(...)plotdelta(auto=T))
tkbind(list.strate2, "<ButtonRelease-1>", function(...)plotdelta(auto=T))
tkbind(list.bonusTP, "<ButtonRelease-1>", function(...)plotdelta(auto=T))
tkbind(list.bonusTC, "<ButtonRelease-1>", function(...)plotdelta(auto=T))
tkbind(list.bonusSP, "<ButtonRelease-1>", function(...)plotdelta(auto=T))
tkfocus(tt)
}
.cost.disCorrPlot <-
function(){


try(require(tcltk))
try(require(tcltk2))
try(library(Rcmdr))
try(require(tkrplot))
try(tclRequire("BWidget"))
try(library(COSTcore))
try(library(COSTeda));try(library(COSTgui));try(library(COSTdbe));try(library(COSTdbe))



tdspinner <- function(parent, ...) {tkwidget(parent, "spinbox", ...)}


valid<-function(){
print("ici")

# cmd2<-paste(tclvalue(val.out),"<-dbeObject(desc='",tclvalue(val.desc),"',species='",paste(as.character(tkget(list.species,as.character(tkcurselection(list.species)))),collapse=" "),"',strataDesc=",tclvalue(lechoix.but.strini),",param='",as.character(listparam[as.numeric(tclvalue(tcl(combo.param,"getvalue")))+1]),"',catchCat='",tclvalue(tvarcatchCat),"',methodDesc='",tclvalue(tvarmethodDesc),"')",sep="")


# puis on fait disCorrPlot

cmd2<-paste(tclvalue(val.out),"<-disCorrPlot(object=",tclvalue(lechoix.but.csObject),sep="")




cmd2<-paste(cmd2,",timeStrata=",as.logical(as.numeric(tclvalue(cbValue.timeStrata))),
",techStrata=",as.logical(as.numeric(tclvalue(cbValue.techStrata))),
",spaceStrata=",as.logical(as.numeric(tclvalue(cbValue.spaceStrata))),
",sampPar=",as.logical(as.numeric(tclvalue(cbValue.sampPar))),
",reg=",as.logical(as.numeric(tclvalue(cbValue.reg))),
",show.legend=",as.logical(as.numeric(tclvalue(cbValue.show.legend))),
",val='",tclvalue(tvarval),"'",

sep="")

# on rajoute species

if ( length(as.character(tkcurselection(list.species)))!=0){
lesspecies<-NULL 
for (rg in as.character(tkcurselection(list.species))){
lesspecies<-c(lesspecies,as.name(paste(as.character(tkget(list.species,rg)),collapse=" ")))
}  
XX<-"c("
for ( i in 1:length(lesspecies)){XX<-paste(XX,"'",as.character(lesspecies[[i]]),"'",sep="")
if ( i !=length(lesspecies)){XX<-paste(XX,",",sep="")}}
XX<-paste(XX,")",sep="")

cmd2<-paste(cmd2,
",species=",XX,


sep="")

}

if ( length(as.character(tkcurselection(list.landSpp)))!=0){
leslandSpp<-NULL 
for (rg in as.character(tkcurselection(list.landSpp))){
leslandSpp<-c(leslandSpp,as.name(paste(as.character(tkget(list.landSpp,rg)),collapse=" ")))
}  
XX2<-"c("
for ( i in 1:length(leslandSpp)){XX2<-paste(XX2,"'",as.character(leslandSpp[[i]]),"'",sep="")
if ( i !=length(leslandSpp)){XX2<-paste(XX2,",",sep="")}}
XX2<-paste(XX2,")",sep="")

cmd2<-paste(cmd2,

",landSpp=",XX2,

sep="")
}











cmd2<-paste(cmd2,")",sep="")
print(cmd2)
doItAndPrint(cmd2)



}


slotNamesdata<-function(data){
ploom<-slotNames(data)
out<-c()
for (p in ploom){
if(is(slot(data, p))[1] == "data.frame"){
out<-c(out,p)
}
}

return(out)
}



choixdataset.multi<-function(id,nom.but){
print(id)
try(tkdestroy(tt2),silent=T)
tt2<-tktoplevel()
tkwm.geometry(tt2, "-100+50")     
listdata <-NULL
for ( i in id){listdata <- c(listdata,malist(i))}
dataSetsBox <- variableListBox(tt2, listdata, title=gettext(domain="R-COSTgui","Select one dataset"))# voir initialselection
tkgrid(getFrame(dataSetsBox), sticky="nwes",column=1,row=1)
  tkgrid.rowconfigure(getFrame(dataSetsBox), 1, weight = 1)
  tkgrid.rowconfigure(tt2, 1, weight = 1)  
onOK <- function(){	
out<-getSelection(dataSetsBox)

		if ( length(out) != 0){
		eval(parse(text=paste("tclvalue(lechoix.",nom.but,")<-out",sep="")))
		tkconfigure(eval(as.name(nom.but)),text=out)
		# print(GetIt((eval(as.name(paste("lechoix.",nom.but,sep=""))))))
		# tkconfigure( names.list.quali, state="disabled" )	
		# tkconfigure( names.list.quanti, state="disabled" )
		
		

		# print("on met a jour la liste des variables")

		# a ce niveau on a le nom du cs objet, on va lister toute les variaible exploitable pour le spaceplot
	if (nom.but == "but.csObject"){

	
	# on charge la description
	# tclvalue(val.desc)<-eval(as.name(out))@desc
	
	
  listslot<-slotNamesdata(eval(as.name(out)))
araj<-c()
for ( i in 1:length(listslot)){	
truc<-do.call("slot",args =list(as.name(out),listslot[i]))
araj<-unique(c(araj,levels(as.factor(truc[["spp"]]))));araj
}
tkdelete(list.species,"0","end");
tkdelete(list.landSpp,"0","end");
for (var in araj) {tkinsert(list.species, "end", var)}
for (var in araj) {tkinsert(list.landSpp, "end", var)}

# on check les LAN et DIS

	# LD<-unique(substr(eval(as.name(out))@hl$sort,1,3))
	# print(LD)
# if(!is.element("DIS",LD)){
# print("pas de DIS")
# tkconfigure(rbdis,state="disabled")
# tclvalue(tvarcatchCat)<-"LAN"

# }
# if(!is.element("LAN",LD)){
# print("pas de LAN")
# tkconfigure(rblan,state="disabled")
# tclvalue(tvarcatchCat)<-"DIS"
# }

		}
	
		
# aactiver<-list(entry.ylab,entry.xlab,list.time,list.tech,list.space,but.refr,entry.fonction,entry.main,cb.simu,cb.auto,list.time,list.tech,spin.x1,spin.x2,spin.y1,spin.y2,rbsel.image,rbsel.contour,rbsel.bubble,rbsel.value)
# # for ( i in 1:length(aactiver)){tkconfigure(aactiver[[i]],state="normal")}

# lapply(aactiver,tkconfigure,state="normal")

# choixtime<-c("year","semester","quarter","month")
# choixspace<-c("rect","subrect","area")
# choixtech<-c("commCat","commCatScl", "foCatEu5", "foCatEu6", "foCatNat","catchCat")
# tkdelete(list.time,"0","end");for (var in choixtime) {tkinsert(list.time, "end", var)}
# tkdelete(list.tech,"0","end");for (var in choixtech) {tkinsert(list.tech, "end", var)}
# tkdelete(list.space,"0","end");for (var in choixspace) {tkinsert(list.space, "end", var)}


	
	

tkconfigure(valid.but,state="normal")
	
}
tkdestroy(tt2)	
		
}
	
OK.but <-tkbutton(tt2,text=gettext(domain="R-COSTgui","   OK   "),command=onOK)
tkgrid(OK.but,column=1,row=2)
tkfocus(tt2)
}

#fonctions pratique pour tk
malist<-function (pat) {

# retrouve dans ls les element de type pat
    Vars <- ls(all.names = TRUE,env=.GlobalEnv)
	# print(Vars)
    if (length(Vars) == 0) {return(Vars)}
    names(which(sapply(Vars, function(.x) {	is(get(.x))[1]==pat	}	)	)	)
	}
	
getcontentlistbox<-function(listbox){
out<-c()
i<-0		
while (length(as.character(tkget(listbox,i)))!=0){
out<-c(out,as.character(tkget(listbox,i)))
i<-i+1
}
return(out)
}

GetIt<-function(objet){
if (attr(objet,"class")=="tkwin"){
		if (as.character(tkwinfo("class",objet))=="Listbox"){
		return(getcontentlistbox(objet))
		}
}
if (attr(objet,"class")=="tclVar"){
return(tclvalue(objet))

}
}

DoItmini<-function(comm,env=.GlobalEnv){
  result <- parse(text = paste(comm))
  exprs<-result
  i<-1
  #for (i in seq_along(exprs)) {
  ei <- exprs[i]
  print(ei)
  result <- eval(ei, envir = env)
  
}

reset.strIni<-function(){
tkconfigure(but.strini,text=gettext(domain="R-COSTgui","<select>"))
tclvalue(lechoix.but.strini)<-""
}

choixrb<-function(){
if("NULL" != tclvalue(tvarmethodDesc) & "NULL" != tclvalue(tvarcatchCat)){

tkconfigure(valid.but,state="active")

}

}

proba<-function(val){
tclvalue(lechoix.p2)<- round(as.numeric(1-as.numeric(tclvalue(lechoix.p1))),3)
}

checkind<-function(){
tkconfigure(spin.p1,state=c("disabled","normal")[as.numeric(tclvalue(cbValue.incl.precision))+1])}

lechoix.but.csObject <- tclVar("")
# lechoix.but.clObject <- tclVar("")
lechoix.but.strini <- tclVar("")
# lechoix.p1 <- tclVar(0.025)
lechoix.nboot <- tclVar(1000)

tt <- tktoplevel(borderwidth=10)
tkwm.geometry(tt, "-100+50")
font2<-tkfont.create(family="Times",size=12,weight="bold")
font3<-tkfont.create(family="Times",size=12)
fontheading<-tkfont.create(family="Times",size=18,weight="bold")  
tkwm.title(tt,gettext(domain="R-COSTgui","COST - Figure : Searching for discards~Variable correlation"))
tkgrid(tklabel(tt, text=gettext(domain="R-COSTgui","Figure : Searching for discards~Variable correlation"),font=fontheading))
tkgrid(tklabel(tt,text="")) # Ligne de blanc
frameTOP<- tkframe(tt, borderwidth=2, relief="groove")
frameBONUS<- tkframe(tt, borderwidth=2, relief="groove")
# frameA<- tkframe(tt, borderwidth=2, relief="groove")
frameB<- tkframe(tt, borderwidth=2, relief="groove")
frameC<- tkframe(tt, borderwidth=2, relief="groove")
frameD<- tkframe(tt, borderwidth=2, relief="groove")
frameE<- tkframe(frameTOP, borderwidth=2, relief="groove")
frameF<- tkframe(tt, borderwidth=2, relief="groove")



# les entry
# val.desc <- tclVar("")
# entry.desc <-tkentry(frameTOP,width="20",textvariable=val.desc)
# val.species <- tclVar("")
# entry.species <-tkentry(frameTOP,width="20",textvariable=val.species)
val.out <- tclVar("res")
entry.out <-tkentry(frameD,width="20",textvariable=val.out)
# val.param <- tclVar("")
# entry.param <-tkentry(frameTOP,width="20",textvariable=val.param)

#les combo

# listparam<-c("weight","number", "maturity", "sexratio")
# combo.param <- tkwidget(frameTOP,"ComboBox",editable=FALSE,values=listparam,state="normal")

#les spin
# spin.p1<-tdspinner(frameF,from=0,to=1,inc=0.001,width=5,textvariable = lechoix.p1,state="normal",command=function(...)proba(1))
spin.nboot<-tdspinner(frameF,from=1,to=1000,inc=1,width=5,textvariable = lechoix.nboot,state="normal")


# les radio
tvaraux <- tclVar("Time")
rbtime <- tkradiobutton(frameB)
rblandings <- tkradiobutton(frameB)
tkconfigure(rbtime,variable=tvaraux,value="Time",state="normal")
tkconfigure(rblandings,variable=tvaraux,value="Landings",state="normal")


tvarval <- tclVar("weight")
rbweight <- tkradiobutton(frameC,command=function()choixrb())
rbnumber <- tkradiobutton(frameC,command=function()choixrb())
tkconfigure(rbweight,variable=tvarval,value="weight",state="normal")
tkconfigure(rbnumber,variable=tvarval,value="number",state="normal")
#les bouton


# but.strini <- tkbutton(frameA,text=gettext(domain="R-COSTgui","<select>"),state="active",command=function(...) choixdataset.multi("strIni","but.strini"),foreground="red",relief="ridge")
# but.newstrini <- tkbutton(frameA,text=gettext(domain="R-COSTgui","New strIni type object"),state="active",command=function(...) .cost.strIni())#,foreground="red",relief="ridge")
# but.resetstrini <- tkbutton(frameA,text="reset",state="active",command=function(...) reset.strIni())#,foreground="red",relief="ridge")
valid.but <- tkbutton(frameD,text=gettext(domain="R-COSTgui","Validate"),state="disabled",command=function(...) valid())

but.csObject <- tkbutton(frameBONUS,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset.multi(c("csDataCons"),"but.csObject"),foreground="red",relief="ridge")

# but.clObject <- tkbutton(frameBONUS,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset.multi(c("clDataCons"),"but.clObject"),foreground="red",relief="ridge")

# les check

# cbValue.incl.precision<-tclVar("1")
# cb.incl.precision <- tkcheckbutton(frameF,variable=cbValue.incl.precision,state="normal",command=function()checkind())

# cbValue.sample.boot<-tclVar("1")
# cb.sample.boot <- tkcheckbutton(frameF,variable=cbValue.sample.boot,state="normal")


cbValue.timeStrata<-tclVar("0")
cb.timeStrata <- tkcheckbutton(frameF,variable=cbValue.timeStrata,state="normal")

cbValue.techStrata<-tclVar("0")
cb.techStrata <- tkcheckbutton(frameF,variable=cbValue.techStrata,state="normal")

cbValue.spaceStrata<-tclVar("0")
cb.spaceStrata <- tkcheckbutton(frameF,variable=cbValue.spaceStrata,state="normal")

cbValue.sampPar<-tclVar("1")
cb.sampPar <- tkcheckbutton(frameF,variable=cbValue.sampPar,state="normal")


cbValue.reg<-tclVar("1")
cb.reg <- tkcheckbutton(frameF,variable=cbValue.reg,state="normal")


cbValue.show.legend<-tclVar("0")
cb.show.legend <- tkcheckbutton(frameF,variable=cbValue.show.legend,state="normal")



#text

text.valid<-tklabel(frameD,text=gettext(domain="R-COSTgui","out :"))
#list

list.species <-tklistbox(frameE,selectmode="multiple",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.species.scroll,...))
list.species.scroll<-tkscrollbar(frameE,repeatinterval=5,command=function(...)tkyview(list.species,...))

list.landSpp <-tklistbox(frameE,selectmode="multiple",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.landSpp.scroll,...))
list.landSpp.scroll<-tkscrollbar(frameE,repeatinterval=5,command=function(...)tkyview(list.landSpp,...))

tkgrid(frameBONUS)
tkgrid(frameTOP)
# tkgrid(frameA)
tkgrid(frameB)
tkgrid(frameC)
tkgrid(frameF)

tkgrid(frameD)

tkgrid(tklabel(frameBONUS,text=gettext(domain="R-COSTgui","csDataCons:"),font=font2),but.csObject)
# tkgrid(tklabel(frameBONUS,text="clDataCons:",font=font2),but.clObject)
# tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Description : ")),entry.desc)
# tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","species : "),font=font2),entry.species)
tkgrid(tklabel(frameE,text=gettext(domain="R-COSTgui","Species"),font=font2))
tkgrid(list.species,list.species.scroll,sticky="nswe")

tkgrid(tklabel(frameE,text=gettext(domain="R-COSTgui","Landing species"),font=font2))
tkgrid(list.landSpp,list.landSpp.scroll,sticky="nswe")




# tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Parameter : ")),combo.param)
tkgrid(frameE,columnspan=2)#,sticky="ew")

# tkgrid(tklabel(frameA,text=gettext(domain="R-COSTgui","StrIni : "),font=font2),but.strini,but.newstrini,but.resetstrini)
tkgrid(tklabel(frameB,text=gettext(domain="R-COSTgui","Time : "),font=font2),rbtime,tklabel(frameB,text=gettext(domain="R-COSTgui","Landings : "),font=font2),rblandings)
tkgrid(tklabel(frameC,text=gettext(domain="R-COSTgui","Weight : "),font=font2),rbweight,tklabel(frameC,text=gettext(domain="R-COSTgui","Number : "),font=font2),rbnumber)

tkgrid(frameF)

tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","Partial sampling : ")),cb.sampPar)
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","Regression line : ")),cb.reg)
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","Show legend : ")),cb.show.legend)
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","Temporal strata : ")),cb.timeStrata)
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","Technical Strata : ")),cb.techStrata)
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","Space strata : ")),cb.spaceStrata)
# tkgrid(tklabel(frameF,text="incl.precision : "),cb.incl.precision)
# tkgrid(tklabel(frameF,text="probas : "),spin.p1,spin.p2)
tkgrid(text.valid,entry.out,valid.but)


tkfocus(tt)

}
.cost.disInfo <-
function(){

try(require(tcltk))
try(require(tcltk2))
try(library(Rcmdr))
try(require(tkrplot))
try(tclRequire("BWidget"))
try(library(COSTcore))
try(library(COSTeda));try(library(COSTgui))

valid<-function(){
#verbose#print("valid")
lCSobj<-tclvalue(lechoix.but.csOBJECT)
# # lespp<-GetIt(list.variables)
lesby<-NULL 
 for (rg in as.character(tkcurselection(list.criteres))){
 lesby<-c(lesby,as.name(paste(as.character(tkget(list.criteres,rg)),collapse=" ")))
 }  

 lesvari<-NULL 
 for (rg in as.character(tkcurselection(list.variables))){
 lesvari<-c(lesvari,as.name(paste(as.character(tkget(list.variables,rg)),collapse=" ")))
 }  

if (lCSobj == ""  | nchar(tclvalue(val.file)) ==0 | nchar(tclvalue(val.fun)) ==0 | is.null(lesby) | is.null(lesvar)){
Message(message = gettext(domain="R-COSTgui","all parameters are mandatory"), type = "error")
}


if (lCSobj !="" & nchar(tclvalue(val.file)) !=0 & nchar(tclvalue(val.fun)) !=0 & !is.null(lesvar) & !is.null(lesby)){
XX<-"c("
for ( i in 1:length(lesby)){XX<-paste(XX,"'",as.character(lesby[[i]]),"'",sep="")
if ( i !=length(lesby)){XX<-paste(XX,",",sep="")}}
XX<-paste(XX,")",sep="")

cmd1<-paste("disInfo(",lCSobj,",'",tclvalue(val.file),"','", as.character(lesvari[[1]]),"',",XX,",fun=",tclvalue(val.fun),",append=",as.logical(as.numeric(tclvalue(cbValue.append))),")",sep='')




# cmd2<-paste("boxplot(",tclvalue(val.valid),")")
doItAndPrint(cmd1)
# doItAndPrint(cmd2)

# disInfo(sole.cs.val,Path,"lenNum","lenCls",sum,na.rm=TRUE,
       # # title="Measured numbers at length",append=FALSE)



}
}
#fonctions pratique pour tk


slotNamesdata<-function(data){
ploom<-slotNames(data)
out<-c()
for (p in ploom){
if(is(slot(data, p))[1] == "data.frame"){
out<-c(out,p)
}
}

return(out)
}


malist<-function (pat) {

# retrouve dans ls les element de type pat
    Vars <- ls(all.names = TRUE,env=.GlobalEnv)
	# #verbose#print(Vars)
    if (length(Vars) == 0) {return(Vars)}
    names(which(sapply(Vars, function(.x) {	is(get(.x))[1]==pat	}	)	)	)
	}
getcontentlistbox<-function(listbox){
out<-c()
i<-0		
while (length(as.character(tkget(listbox,i)))!=0){
out<-c(out,as.character(tkget(listbox,i)))
i<-i+1
}
return(out)
}
GetIt<-function(objet){
if (attr(objet,"class")=="tkwin"){
		if (as.character(tkwinfo("class",objet))=="Listbox"){
		return(getcontentlistbox(objet))
		}
}
if (attr(objet,"class")=="tclVar"){
return(tclvalue(objet))

}
}
DoItmini<-function(comm,env=.GlobalEnv){
  result <- parse(text = paste(comm))
  exprs<-result
  i<-1
  #for (i in seq_along(exprs)) {
  ei <- exprs[i]
  #verbose#print(ei)
  result <- eval(ei, envir = env)
  
}
typevar<-function(dataset,filtre=F){
	# prend un jeux de donn?e et ressort dans une list les nom de variable en focntion de leur type
#filtre permet de ne pas sortir les variables non exploitables (NA ou NULL)	
if(!filtre){
jegarde<-function(vec){return(TRUE)}
}

if (filtre){
jegarde<-function(vec){
out<-TRUE
if(sum(is.na(vec))==length(vec) ){out<-FALSE}
if(sum(vec=="",na.rm=T)==length(vec) ){out<-FALSE}
return(out)
}
}
	out.quanti<-c()
		out.quali<-c()
    
		for ( i in 1:dim(dataset)[2]){

		if (is(dataset[,i])[1]=="numeric" & jegarde(dataset[,i])){out.quanti<-c(out.quanti,names(dataset)[i])}
		if (is(dataset[,i])[1]=="integer" & jegarde(dataset[,i])){out.quanti<-c(out.quanti,names(dataset)[i])}
		if (is(dataset[,i])[1]=="factor" & jegarde(dataset[,i])){out.quali<-c(out.quali,names(dataset)[i])}
		if (is(dataset[,i])[1]=="character" & jegarde(dataset[,i])){out.quali<-c(out.quali,names(dataset)[i])}
				}
		return(list(quanti=out.quanti,quali=out.quali))
		}
comblist2<-function(test){
# on va la combinier et en faire une fonction
out<-list()
out$quanti<-unique(as.vector(unlist(test[names(test)=="quanti"])))
out$quali<-unique(as.vector(unlist(test[names(test)=="quali"])))
return(out)
}
comblist<-function(lalist){
return(comblist2(do.call("c",lalist)))
}	
lesvar<-function(cho,rewrite=F){
variableList<-list()
listslot<-slotNamesdata(eval(as.name(tclvalue(cho))))

if (rewrite==T){
#on met en as.character year month et quarter
for ( i in 1:length(listslot)){
pom<-do.call("slot",args =list(as.name(as.character(GetIt(cho))),listslot[i]))
names(pom)
for (p in c("year","month","quarter")){
if(is.element(p,names(pom))){pom[[p]]<-as.character(pom[[p]])}
}
# on met pom ou il faut
eval(parse(text=paste(as.name(as.character(GetIt(cho))),"@",listslot[i],"<-pom",sep="")))
}
}




for ( i in 1:length(listslot)){
variableList[[i]]<-
typevar(do.call("slot",args =list(as.name(as.character(GetIt(cho))),listslot[i])),filtre=T)
}
# typevar(hakeVal_cl@cl)
return(comblist(variableList))
}
choixdataset.multi<-function(id,nom.but){
#verbose#print(id)
try(tkdestroy(tt2),silent=T)
tt2<-tktoplevel()
tkwm.geometry(tt2, "-100+50")     
listdata <-NULL
for ( i in id){listdata <- c(listdata,malist(i))}
dataSetsBox <- variableListBox(tt2, listdata, title=gettext(domain="R-COSTgui","Select one dataset"))# voir initialselection
tkgrid(getFrame(dataSetsBox), sticky="nwes",column=1,row=1)
  tkgrid.rowconfigure(getFrame(dataSetsBox), 1, weight = 1)
  tkgrid.rowconfigure(tt2, 1, weight = 1)  
onOK <- function(){	
out<-getSelection(dataSetsBox)
		if ( length(out) != 0){
		eval(parse(text=paste("tclvalue(lechoix.",nom.but,")<-out",sep="")))
		tkconfigure(eval(as.name(nom.but)),text=out)

		araj<-sort(lesvar(lechoix.but.csOBJECT)$quanti)
		tkdelete(list.variables,"0","end");for (var in araj) {tkinsert(list.variables, "end", var)}
		tkdelete(list.criteres,"0","end");for (var in c("year","semester","quarter","month","rect","subrect","area","commCat","commCatScl", "foCatEu5", "foCatEu6", "foCatNat","catchCat")
		) {tkinsert(list.criteres, "end", var)}
		tkconfigure(but.valid,state="normal")

	
		
}
tkdestroy(tt2)	
		
}
	
OK.but <-tkbutton(tt2,text="   OK   ",command=onOK)
tkgrid(OK.but,column=1,row=2)
tkfocus(tt2)
}
choixfraction<-function(){
# € on active simplement le bouton
tkconfigure(but.valid,state="active")


}

#le GUI

lechoix.but.csOBJECT <- tclVar("")
# # lechoixcl <- tclVar("")
# # lechoixce <- tclVar("")
# e2 <- new.env(parent = .GlobalEnv)
# assign("lechoixcs",lechoixcs,envir=e2)
# # assign("lechoixcl",lechoixcl,envir=e2)
tt <- tktoplevel(borderwidth=10)
tkwm.geometry(tt, "-100+50")
font2<-tkfont.create(family="Times",size=12,weight="bold")
font3<-tkfont.create(family="Times",size=12)
fontheading<-tkfont.create(family="Times",size=18,weight="bold")  
tkwm.title(tt,gettext(domain="R-COSTgui","COST - Reporting"))
frameTOP<- tkframe(tt, borderwidth=2, relief="groove")
frameD<- tkframe(tt, borderwidth=2, relief="groove")
frameE<- tkframe(tt, borderwidth=2, relief="groove")
framebot<- tkframe(tt, borderwidth=2, relief="groove")
but.csOBJECT <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset.multi(c("csDataCons","clDataCons","ceDataCons","clDataVal","ceDataVal","csDataVal"),"but.csOBJECT"),foreground="red",relief="ridge")
list.variables <-tklistbox(frameE,selectmode="unique",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.variables.scroll,...))
list.variables.scroll<-tkscrollbar(frameE,repeatinterval=5,command=function(...)tkyview(list.variables,...))
list.criteres <-tklistbox(frameE,selectmode="multiple",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.criteres.scroll,...))
list.criteres.scroll<-tkscrollbar(frameE,repeatinterval=5,command=function(...)tkyview(list.criteres,...))
frameA<- tkframe(tt, borderwidth=2, relief="groove")
but.valid <- tkbutton(framebot,text=gettext(domain="R-COSTgui","Validate"),state="disabled",command=function(...) valid())
val.fun <- tclVar("sum")
entry.fun<-tkentry(frameA,width="20",textvariable=val.fun)
val.file <- tclVar(paste(getwd(),"out.txt",sep="/"))
entry.file<-tkentry(frameA,width="20",textvariable=val.file)
cbValue.append <- tclVar("1")
cb.append <- tkcheckbutton(framebot,variable=cbValue.append)



tkgrid(tklabel(tt, text=gettext(domain="R-COSTgui","Reporting"),font=fontheading),row=1,columnspan=3)
tkgrid(tklabel(tt,text=""),row=2) # Ligne de blanc
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","csObject : "),font=font2),but.csOBJECT,columnspan=3)
tkgrid(frameTOP,row=3,columnspan=3)
tkgrid(frameE,row=4,columnspan=3)
tkgrid(tklabel(frameE,text=gettext(domain="R-COSTgui","Field")),row=1,column=0,columnspan=3)
tkgrid(tklabel(frameE,text=gettext(domain="R-COSTgui","By")),row=1,column=6,columnspan=3)
tkgrid(list.variables,list.variables.scroll,list.criteres,list.criteres.scroll,sticky="ns",columnspan=3)
tkgrid(frameA,row=5,columnspan=3)
tkgrid(tklabel(frameA,text=gettext(domain="R-COSTgui","File : ")),entry.file,sticky="w",columnspan=3)
tkgrid(tklabel(frameA,text=gettext(domain="R-COSTgui","Function : ")),entry.fun,sticky="w",columnspan=3)
tkgrid(framebot,row=6,columnspan=3)
tkgrid(but.valid,tklabel(framebot,text=gettext(domain="R-COSTgui","         append :")),cb.append,sticky="e")

tkfocus(tt)
}
.cost.help <-
function() {
	browseURL(
paste(file.path(.path.package(package="COSTgui")[1], "doc"),"/", gettextRcmdr("help-cost"), ".pdf", sep="")

)
}
.cost.importCSV <-
function(){

try(require(tcltk))
try(require(tcltk2))
try(library(Rcmdr))
try(require(tkrplot))
try(tclRequire("BWidget"))
try(library(COSTcore))
try(library(COSTeda));try(library(COSTgui))


#fonctions spécifiques
parcour<-function(ert){
  
 
  chemin<-"NULL"
  # chemin<-file.choose()
chemin<-tclvalue(tkgetOpenFile(filetypes = "{{csv} {.csv}}"))
  
  
  
  
  if (chemin!="NULL"){
  tempo<-eval(as.name(paste("chemin",ert,sep=".")))
  tclvalue(tempo)<-paste(strsplit(chemin,"\\\\")[[1]],collapse="/")
  
}
}

moncontrole<-function(valeur,info,label=NULL){
flag<-0


if(tclvalue(valeur) != ""){
if ( !is.null(label)){
# print(label)
tkconfigure(label,foreground="black")
}}

if(tclvalue(valeur) == ""){
Message(message = paste(gettext(domain="R-COSTgui","You have to specify"),info), type = "error")
flag<-1

# on va placer le texte en rouge pour aider l'utilisateur

if ( !is.null(label)){
# print(label)
tkconfigure(label,foreground="red")
}





}
return(flag)
}

valid<-function(){
  
  # on desactive le bouton.. pour evter les soucis
  tkconfigure(valid.but,state="disabled",text=gettext(domain="R-COSTgui","in progress..."))
  
  
  # on va lancer un cerain nombre de controle et de message d'erreur en fonction de ce qu'a rntré l utilisateur
  flag<-0
  flag<-moncontrole(val.CsData,"CsData",text.csData) +  moncontrole(chemin.tr,"tr",text.tr) +   moncontrole(chemin.hh,"hh",text.hh) +   moncontrole(chemin.sl,"sl",text.sl) + moncontrole(chemin.hl,"hl",text.hl)

  
  if (flag==0){
  
  if ( tclvalue(chemin.ca) !=""){
    c1<-paste(tclvalue(val.CsData)," <- csData(desc='",tclvalue(desc),"',tr ='", tclvalue(chemin.tr),"',hh ='", tclvalue(chemin.hh),"',sl ='", tclvalue(chemin.sl),"',hl ='", tclvalue(chemin.hl),"',ca ='", tclvalue(chemin.ca),"',check = TRUE,sep='",tclvalue(lesep),"')",sep="")
  }
  
  if ( tclvalue(chemin.ca) ==""){
    c1<-paste(tclvalue(val.CsData)," <- csData(desc='",tclvalue(desc),"',tr ='", tclvalue(chemin.tr),"',hh ='", tclvalue(chemin.hh),"',sl ='", tclvalue(chemin.sl),"',hl ='", tclvalue(chemin.hl),"',check = TRUE,sep='",tclvalue(lesep),"')",sep="")
  }
  
  
  doItAndPrint(c1)
    if ( tclvalue(chemin.ce) !=""){
	moncontrole(val.CeData,"CeData",text.ceData)
	
    if ( tclvalue(val.CeData) !=""){
	
  c2<-paste(tclvalue(val.CeData)," <- ceData(ce ='",tclvalue(chemin.ce), "',check = TRUE,sep='",tclvalue(lesep),"')",sep="")
  doItAndPrint(c2)
  }}
  
  if ( tclvalue(chemin.cl) !=""){ 
  	moncontrole(val.ClData,"ClData",text.clData)
    if ( tclvalue(val.ClData) !=""){
  c3<-paste(tclvalue(val.ClData)," <- clData(cl ='",tclvalue(chemin.cl), "',check = TRUE,sep='",tclvalue(lesep),"')",sep="")
  doItAndPrint(c3)
  }
  }
  }
  
  # on reactive le bouton
   tkconfigure(valid.but,state="active",text=gettext(domain="R-COSTgui","Validate"))
  
}

#gui
tt <- tktoplevel(borderwidth=10)
tkwm.geometry(tt, "-100+50")                                                   
font2<-tkfont.create(family="Times",size=12,weight="bold")
font3<-tkfont.create(family="Times",size=12)
fontheading<-tkfont.create(family="Times",size=18,weight="bold")  
tkwm.title(tt,gettext(domain="R-COSTgui","COST - CSV import"))
tkgrid(tklabel(tt, text=gettext(domain="R-COSTgui","CSV import"),font=fontheading))
tkgrid(tklabel(tt,text="")) # Ligne de blanc  
tkgrid(tklabel(tt,text="")) # Ligne de blanc


frameTOP<- tkframe(tt, borderwidth=2, relief="groove")



# on va generer les case de chemin et le bouton
chemin.tr <- tclVar("")
entry.chemin.tr <-tkentry(frameTOP,width="20",textvariable=chemin.tr)
choix.tr.but <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","Browse"),state="active",command=function(...) parcour("tr"))

chemin.hh <- tclVar("")
entry.chemin.hh <-tkentry(frameTOP,width="20",textvariable=chemin.hh)
choix.hh.but <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","Browse"),state="active",command=function(...) parcour("hh"))


chemin.sl <- tclVar("")
entry.chemin.sl <-tkentry(frameTOP,width="20",textvariable=chemin.sl)
choix.sl.but <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","Browse"),state="active",command=function(...) parcour("sl"))


chemin.hl <- tclVar("")
entry.chemin.hl <-tkentry(frameTOP,width="20",textvariable=chemin.hl)
choix.hl.but <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","Browse"),state="active",command=function(...) parcour("hl"))

chemin.ca <- tclVar("")
entry.chemin.ca <-tkentry(frameTOP,width="20",textvariable=chemin.ca)
choix.ca.but <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","Browse"),state="active",command=function(...) parcour("ca"))


chemin.ce <- tclVar("")
entry.chemin.ce <-tkentry(frameTOP,width="20",textvariable=chemin.ce)
choix.ce.but <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","Browse"),state="active",command=function(...) parcour("ce"))


chemin.cl <- tclVar("")
entry.chemin.cl <-tkentry(frameTOP,width="20",textvariable=chemin.cl)
choix.cl.but <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","Browse"),state="active",command=function(...) parcour("cl"))


desc <- tclVar("ma description")
entry.desc <-tkentry(frameTOP,width="20",textvariable=desc)

val.CsData <- tclVar("SIBM_cs")
entry.CsData <-tkentry(frameTOP,width="20",textvariable=val.CsData)

val.CeData <- tclVar("SIBM_ce")
entry.CeData <-tkentry(frameTOP,width="20",textvariable=val.CeData)

val.ClData <- tclVar("SIBM_cl")
entry.ClData <-tkentry(frameTOP,width="20",textvariable=val.ClData)



 fontitalic <- tkfont.create(slant="italic")

tkgrid(frameTOP)
text.csData<-tklabel(frameTOP,text=gettext(domain="R-COSTgui","CsData"))
tkgrid(text.csData,entry.CsData)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Description")),entry.desc)
text.tr<-tklabel(frameTOP,text=gettext(domain="R-COSTgui","tr"))
tkgrid(text.tr,entry.chemin.tr,choix.tr.but)
text.hh<-tklabel(frameTOP,text=gettext(domain="R-COSTgui","hh"))
tkgrid(text.hh,entry.chemin.hh,choix.hh.but)
text.sl<-tklabel(frameTOP,text=gettext(domain="R-COSTgui","sl"))
tkgrid(text.sl,entry.chemin.sl,choix.sl.but)
text.hl<-tklabel(frameTOP,text=gettext(domain="R-COSTgui","hl"))
tkgrid(text.hl,entry.chemin.hl,choix.hl.but)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","ca"),foreground="darkgrey",font=fontitalic),entry.chemin.ca,choix.ca.but)# on met en gris car pas obligatoire
text.ceData<-tklabel(frameTOP,text=gettext(domain="R-COSTgui","CeData"))
tkgrid(text.ceData,entry.CeData)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","ce"),foreground="darkgrey",font=fontitalic),entry.chemin.ce,choix.ce.but)# on met en gris car pas obligatoire
text.clData<-tklabel(frameTOP,text=gettext(domain="R-COSTgui","ClData"))
tkgrid(text.clData,entry.ClData)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","cl"),foreground="darkgrey",font=fontitalic),entry.chemin.cl,choix.cl.but)# on met en gris car pas obligatoire
# on va rajouter un bouton pour valider le tout


frameA<- tkframe(tt, borderwidth=2, relief="groove")
valid.but <- tkbutton(frameA,text=gettext(domain="R-COSTgui","Validate"),state="active",command=function(...) valid())
tkgrid(frameA)

lesep <- tclVar(",")# séparateur par defaut
entry.sep<-tkentry(frameA,width="2",textvariable=lesep)




tkgrid(valid.but,tklabel(frameA,text=gettext(domain="R-COSTgui","sep :")),entry.sep)
tkfocus(tt)
}
.cost.importdataset <-
function(){



try(require(tcltk))
try(require(tcltk2))
try(library(Rcmdr))
try(require(tkrplot))
try(tclRequire("BWidget"))
try(library(COSTcore))
try(library(COSTeda));try(library(COSTgui))


#fonctions pratique pour tk

  DoItmini<-function(comm,env=.GlobalEnv){
  result <- parse(text = paste(comm))
  exprs<-result
  i<-1
  #for (i in seq_along(exprs)) {
  ei <- exprs[i]
  #verbose#print(ei)
  result <- eval(ei, envir = env)
  
}

malist<-function (pat) {

# retrouve dans ls les element de type pat
    Vars <- ls(all.names = TRUE,env=.GlobalEnv)
	# print(Vars)
    if (length(Vars) == 0) {return(Vars)}
    names(which(sapply(Vars, function(.x) {	is(get(.x))[1]==pat	}	)	)	)
	}

	
	#fonctions spécifiques

choixdataset<-function(id){
#verbose#print(id)
try(tkdestroy(tt2),silent=T)
tt2<-tktoplevel()
tkwm.geometry(tt2, "-100+50")     
listdata <- malist("data.frame")
dataSetsBox <- variableListBox(tt2, listdata, title=gettext(domain="R-COSTgui","Select one dataset"))# voir initialselection
tkgrid(getFrame(dataSetsBox), sticky="nwes",column=1,row=1)
	  tkgrid.rowconfigure(getFrame(dataSetsBox), 1, weight = 1)
  tkgrid.rowconfigure(tt2, 1, weight = 1)  				


onOK <- function(){	

# assign("out", getSelection(dataSetsBox), envir=e1)
out<-getSelection(dataSetsBox)
#verbose#print("test ici")
#verbose#print(monchoixtr)
	# DoItmini("print(out)",e1)
		if (length(out)!=0){
		# DoItmini(paste("tclvalue(monchoix",id,")<-out",sep=""),env=e1)
		
		
		
		eval(parse(text=paste("tclvalue(monchoix",id,")<-out",sep="")))
		tkconfigure(eval(as.name(paste("but.chemin.",id,sep=""))),text=out)
		}
		
		
		tkdestroy(tt2)	
}
OK.but <-tkbutton(tt2,text="   OK   ",command=onOK)
tkgrid(OK.but,column=1,row=2)
tkfocus(tt2)
}

moncontrole<-function(valeur,info,label=NULL){
flag<-0


if(tclvalue(valeur) != ""){
if ( !is.null(label)){
# print(label)
tkconfigure(label,foreground="black")
}}

if(tclvalue(valeur) == ""){
Message(message = paste(gettext(domain="R-COSTgui","You have to specify"),info), type = "error")
flag<-1

# on va placer le texte en rouge pour aider l'utilisateur

if ( !is.null(label)){
# print(label)
tkconfigure(label,foreground="red")
}





}
return(flag)
}

valid<-function(){
  
  # on desactive le bouton.. pour evter les soucis
  tkconfigure(valid.but,state="disabled",text=gettext(domain="R-COSTgui","in progress..."))
  
  
  # on va lancer un cerain nombre de controle et de message d'erreur en fonction de ce qu'a rntré l utilisateur
  flag<-0
  flag<-moncontrole(val.CsData,"CsData",text.csData) +  moncontrole(monchoixtr,"tr",text.tr) +   moncontrole(monchoixhh,"hh",text.hh) +   moncontrole(monchoixsl,"sl",text.sl) + moncontrole(monchoixhl,"hl",text.hl)
  
  
  
  
  # # sole_cs <- csData(tr = tr[,-1], hh = hh[,-1], sl = sl[,-1], hl = hl[,-1], ca = ca[,-1], check = TRUE)
# # sole_ce <- ceData(ce = ce[,-1], check = TRUE)
# # sole_cl <- clData(cl = cl[,-1], check = TRUE)

  
  
  
    
  if (flag==0){
  
  if ( tclvalue(monchoixca) !=""){
  #verbose#print("on met ca")
  # sole_cs <- csData(tr = tr[,-1], hh = hh[,-1], sl = sl[,-1], hl = hl[,-1], ca = ca[,-1], check = TRUE)
  
  
    c1<-paste(tclvalue(val.CsData)," <- csData(desc='",tclvalue(desc),"',tr =", tclvalue(monchoixtr),"[,-1]",",hh =", tclvalue(monchoixhh),"[,-1]",",sl =", tclvalue(monchoixsl),"[,-1]",",hl =", tclvalue(monchoixhl),"[,-1]",",ca =", tclvalue(monchoixca),"[,-1]",",check = TRUE)",sep="")
  }
  
  if ( tclvalue(monchoixca) ==""){
  #verbose#print("PAS ca")
     c1<-paste(tclvalue(val.CsData)," <- csData(desc='",tclvalue(desc),"',tr =", tclvalue(monchoixtr),"[,-1]",",hh =", tclvalue(monchoixhh),"[,-1]",",sl =", tclvalue(monchoixsl),"[,-1]",",hl =", tclvalue(monchoixhl),"[,-1]",",check = TRUE)",sep="")
	 }
  
  
  doItAndPrint(c1)
    if ( tclvalue(monchoixce) !=""){
	moncontrole(val.CeData,"CeData",text.ceData)
	
    if ( tclvalue(val.CeData) !=""){
	
	# # sole_ce <- ceData(ce = ce[,-1], check = TRUE)

	
	
  c2<-paste(tclvalue(val.CeData)," <- ceData(ce =",tclvalue(monchoixce),"[,-1]", ",check = TRUE)",sep="")
  doItAndPrint(c2)
  }}
  
  if ( tclvalue(monchoixcl) !=""){ 
  	moncontrole(val.ClData,"ClData",text.clData)
    if ( tclvalue(val.ClData) !=""){
	c3<-paste(tclvalue(val.ClData)," <- clData(cl =",tclvalue(monchoixcl),"[,-1]", ",check = TRUE)",sep="")
	
  doItAndPrint(c3)
  }
  }
  }
  
  # on reactive le bouton
   tkconfigure(valid.but,state="active",text=gettext(domain="R-COSTgui","Validate"))
  
}

#gui
# # e1 <- new.env(parent = .GlobalEnv)
monchoixtr <- tclVar("")
monchoixhh <- tclVar("")
monchoixsl <- tclVar("")
monchoixhl <- tclVar("")
monchoixca <- tclVar("")
monchoixce <- tclVar("")
monchoixcl <- tclVar("")

tt <- tktoplevel(borderwidth=10)
tkwm.geometry(tt, "-100+50")                                                   
font2<-tkfont.create(family="Times",size=12,weight="bold")
font3<-tkfont.create(family="Times",size=12)
fontheading<-tkfont.create(family="Times",size=18,weight="bold")  
tkwm.title(tt,gettext(domain="R-COSTgui","COST - dataset import"))
tkgrid(tklabel(tt, text=gettext(domain="R-COSTgui","dataset import"),font=fontheading))
tkgrid(tklabel(tt,text="")) # Ligne de blanc  
tkgrid(tklabel(tt,text="")) # Ligne de blanc


frameTOP<- tkframe(tt, borderwidth=2, relief="groove")



# on va generer les case de chemin et le bouton
chemin.tr <- tclVar("")
but.chemin.tr <-tkbutton(frameTOP,text=gettext(domain="R-COSTgui","< data tr >"),state="active",command=function(...) choixdataset("tr"),relief="ridge",foreground="red")

chemin.hh <- tclVar("")
but.chemin.hh <-tkbutton(frameTOP,text=gettext(domain="R-COSTgui","< data hh >"),state="active",command=function(...) choixdataset("hh"),relief="ridge",foreground="red")


chemin.sl <- tclVar("")
but.chemin.sl <-tkbutton(frameTOP,text=gettext(domain="R-COSTgui","< data sl >"),state="active",command=function(...) choixdataset("sl"),relief="ridge",foreground="red")


chemin.hl <- tclVar("")
but.chemin.hl <-tkbutton(frameTOP,text=gettext(domain="R-COSTgui","< data hl >"),state="active",command=function(...) choixdataset("hl"),relief="ridge",foreground="red")

chemin.ca <- tclVar("")
but.chemin.ca <-tkbutton(frameTOP,text=gettext(domain="R-COSTgui","< data ca >"),state="active",command=function(...) choixdataset("ca"),relief="ridge",foreground="red")


chemin.ce <- tclVar("")
but.chemin.ce <-tkbutton(frameTOP,text=gettext(domain="R-COSTgui","< data ce >"),state="active",command=function(...) choixdataset("ce"),relief="ridge",foreground="red")


chemin.cl <- tclVar("")
but.chemin.cl <-tkbutton(frameTOP,text=gettext(domain="R-COSTgui","< cl >"),state="active",command=function(...) choixdataset("cl"),relief="ridge",foreground="red")


desc <- tclVar("ma description")
entry.desc <-tkentry(frameTOP,width="10",textvariable=desc)

val.CsData <- tclVar("SIBM_cs")
entry.CsData <-tkentry(frameTOP,width="10",textvariable=val.CsData)

val.CeData <- tclVar("SIBM_ce")
entry.CeData <-tkentry(frameTOP,width="10",textvariable=val.CeData)

val.ClData <- tclVar("SIBM_cl")
entry.ClData <-tkentry(frameTOP,width="10",textvariable=val.ClData)



 fontitalic <- tkfont.create(slant="italic")

tkgrid(frameTOP)
text.csData<-tklabel(frameTOP,text=gettext(domain="R-COSTgui","CsData"))
tkgrid(text.csData,entry.CsData)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Description")),entry.desc)
text.tr<-tklabel(frameTOP,text=gettext(domain="R-COSTgui","tr"))
tkgrid(text.tr,but.chemin.tr)
text.hh<-tklabel(frameTOP,text=gettext(domain="R-COSTgui","hh"))
tkgrid(text.hh,but.chemin.hh)
text.sl<-tklabel(frameTOP,text=gettext(domain="R-COSTgui","sl"))
tkgrid(text.sl,but.chemin.sl)
text.hl<-tklabel(frameTOP,text=gettext(domain="R-COSTgui","hl"))
tkgrid(text.hl,but.chemin.hl)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","ca"),foreground="darkgrey",font=fontitalic),but.chemin.ca)# on met en gris car pas obligatoire
text.ceData<-tklabel(frameTOP,text=gettext(domain="R-COSTgui","CeData"))
tkgrid(text.ceData,entry.CeData)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","ce"),foreground="darkgrey",font=fontitalic),but.chemin.ce)# on met en gris car pas obligatoire
text.clData<-tklabel(frameTOP,text=gettext(domain="R-COSTgui","ClData"))
tkgrid(text.clData,entry.ClData)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","cl"),foreground="darkgrey",font=fontitalic),but.chemin.cl)# on met en gris car pas obligatoire
# on va rajouter un bouton pour valider le tout


frameA<- tkframe(tt, borderwidth=2, relief="groove")
valid.but <- tkbutton(frameA,text=gettext(domain="R-COSTgui","Validate"),state="active",command=function(...) valid())
tkgrid(frameA)


tkgrid(valid.but)
tkfocus(tt)
}
.cost.importRdata <-
function(){


try(require(tcltk))
try(require(tcltk2))
try(library(Rcmdr))
try(require(tkrplot))
try(tclRequire("BWidget"))
try(library(COSTcore))
try(library(COSTeda));try(library(COSTgui))

#fonction spécifique
valid2<-function(){
# on va récuperer la liste des object selectionn

tout<-GetIt(names.list)
agarder<-as.numeric(as.character(tkcurselection(names.list)))+1
# print("on charge :")
# print(tout[agarder])

if(length(tout[agarder])!=0){
doItAndPrint(paste("#",gettext(domain="R-COSTgui","loading of"),paste(tout[agarder],collapse=", "),gettext(domain="R-COSTgui","form file")))
doItAndPrint(paste("#",tclvalue(chemin.rdata),sep=""))
doItAndPrint(paste("loadRdata('",tclvalue(chemin.rdata),"',",paste("c('",paste(tout[agarder],collapse="','"),"'))",sep=""),sep=""))
}


}

contr<-function(){
#verbose#print("contr")

#verbose#print(tclvalue(chemin.rdata))

.nv <- new.env(parent = baseenv())  # this one has enclosure package:base.
.ls<-print(load(tclvalue(chemin.rdata),.nv))
.ls


	tkdelete(names.list,"0","end")
		for (var in .ls) {tkinsert(names.list, "end", var)}
rm(.nv)
tkconfigure(valid2.but,state="active")
# tout selectionner par defaut

for ( i in 0:(length(.ls)-1)){
tkselection.set(names.list,i)
}


}

parcour2<-function(ert){
  
 
  chemin<-"NULL"
  ##chemin<-file.choose()

  chemin<-tclvalue(tkgetOpenFile(filetypes = "{{Rdata} {.Rdata}}"))
  
  
  
  if (chemin!="NULL"){
  tempo<-eval(as.name(paste("chemin",ert,sep=".")))
  tclvalue(tempo)<-paste(strsplit(chemin,"\\\\")[[1]],collapse="/")
  tkdelete(names.list,"0","end")
  tkconfigure(valid2.but,state="disabled")
  contr()
}
}

#fonction pratique pour tk
getcontentlistbox<-function(listbox){
out<-c()
i<-0		
while (length(as.character(tkget(listbox,i)))!=0){
out<-c(out,as.character(tkget(listbox,i)))
i<-i+1
}
return(out)
}

GetIt<-function(objet){


if (attr(objet,"class")=="tkwin"){
		if (as.character(tkwinfo("class",objet))=="Listbox"){
		return(getcontentlistbox(objet))
		}
}

if (attr(objet,"class")=="tclVar"){
return(tclvalue(objet))

}
}

#gui



tt <- tktoplevel(borderwidth=10)
tkwm.geometry(tt, "-100+50")
font2<-tkfont.create(family="Times",size=12,weight="bold")
font3<-tkfont.create(family="Times",size=12)
fontheading<-tkfont.create(family="Times",size=18,weight="bold")  
tkwm.title(tt,gettext(domain="R-COSTgui","COST - Rdata import"))
tkgrid(tklabel(tt, text=gettext(domain="R-COSTgui","Rdata import"),font=fontheading))
tkgrid(tklabel(tt,text="")) # Ligne de blanc  
tkgrid(tklabel(tt,text="")) # Ligne de blanc


frameTOP<- tkframe(tt, borderwidth=2, relief="groove")

chemin.rdata <- tclVar("")
entry.chemin.rdata <-tkentry(frameTOP,width="20",textvariable=chemin.rdata)
choix.rdata.but <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","Browse"),state="active",command=function(...) parcour2("rdata"))
tkgrid(frameTOP)
text.rdata<-tklabel(frameTOP,text=gettext(domain="R-COSTgui","Rdata Path"))
tkgrid(text.rdata,entry.chemin.rdata,choix.rdata.but)

frameA<- tkframe(tt, borderwidth=2, relief="groove")
contr.but <- tkbutton(frameA,text=gettext(domain="R-COSTgui","Control"),state="active",command=function(...) contr())
tkgrid(frameA)
tkgrid(contr.but)


# # "on va mettre un menu déroulant avcec le choix de ce que vous oulez importer







frameinfo<- tkframe(tt, borderwidth=2, relief="groove") #mettre un padding



names.list <-tklistbox(frameinfo,selectmode="multiple",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(names.list.scroll,...))
names.list.scroll<-tkscrollbar(frameinfo,repeatinterval=5,command=function(...)tkyview(names.list,...))
tkgrid(names.list,names.list.scroll,sticky="nswe")


tkgrid(frameinfo)
frameB<- tkframe(tt, borderwidth=2, relief="groove")
valid2.but <- tkbutton(frameB,text=gettext(domain="R-COSTgui","Import selection"),state="disabled",command=function(...) valid2())
tkgrid(frameB)
tkgrid(valid2.but)
tkfocus(tt)

}
.cost.landisVol <-
function(){

try(require(tcltk))
try(require(tcltk2))
try(library(Rcmdr))
try(require(tkrplot))
try(tclRequire("BWidget"))
try(library(COSTcore))
try(library(COSTeda));try(library(COSTgui))

valid<-function(){

lesespeces<-NULL 
for (rg in as.character(tkcurselection(liste.spp))){
lesespeces<-c(lesespeces,as.name(paste(as.character(tkget(liste.spp,rg)),collapse=" ")))
}  
XX<-"c("
for ( i in 1:length(lesespeces)){XX<-paste(XX,"'",as.character(lesespeces[[i]]),"'",sep="")
if ( i !=length(lesespeces)){XX<-paste(XX,",",sep="")}}
XX<-paste(XX,")",sep="")


if (tclvalue(cbValue.simu)=="1"){
if(tclvalue(cbValue.csData.subset)=="0" ){lCS<-tclvalue(lechoixcs)}
if(tclvalue(cbValue.csData.subset)=="1" ){
lesub<-get("formule",.envi.landisvol)
for ( i in 1:length(lesub)){
doItAndPrint(lesub[[i]])
}

# #verbose#print("a")
lCS<-strsplit(lesub[[1]],"<-")[[1]][1]
# #verbose#print(lCS)
# faut récupeer le truc de lesub


}
}

if (tclvalue(cbValue.simu)=="0"){
# on fait sans ecrire
if(tclvalue(cbValue.csData.subset)=="0" ){lCS<-tclvalue(lechoixcs)}
if(tclvalue(cbValue.csData.subset)=="1" ){
# ls(.envi.landisvol)
# #verbose#print("XXX")
lesub<-get("formule",.envi.landisvol)[[1]];lesub
# achercher<-strsplit(lesub,"<-")[[1]][1];achercher
# #verbose#print(achercher)

# .lesubAeffacer<-get(achercher,.envi.landisvol);.lesubAeffacer
# #verbose#print("b")
lCS<-paste("get('",strsplit(lesub,"<-")[[1]][1],"',.envi.landisvol)",sep="")
# XXX 

# faut ptet l'assigner au premier niveau

}
}
  

if (lCS == ""  | nchar(tclvalue(val.valid)) ==0 | is.null(lesespeces)){
Message(message = gettext(domain="R-COSTgui","You have to specify a CsData, a fraction, some species and an out name's objetc"), type = "error")
}


if (lCS !="" & nchar(tclvalue(val.valid)) !=0 & !is.null(lesespeces)){



if(tclvalue(cbValue.simu)=="1" ){
cmd1<-paste(tclvalue(val.valid),"<-landisVol(",lCS,",species=",XX,",fraction='",tclvalue(tvar),"')",sep="")

if( tclvalue(tvarplot)=="boxplot"){
cmd2<-paste(sep="","boxplot(",tclvalue(val.valid),")")
}

if( tclvalue(tvarplot)=="plot"){
cmd2<-paste(sep="","plot(",tclvalue(val.valid),")")
}


doItAndPrint(cmd1)
doItAndPrint(cmd2)
}



if(tclvalue(cbValue.simu)=="0" ){
cmd<-paste(tclvalue(tvarplot),"(","landisVol(",lCS,",species=",XX,",fraction='",tclvalue(tvar),"'))",sep="")
#verbose#print(cmd)
justDoIt(cmd)
}





}

if (exists(".lesubAeffacer")){rm(.lesubAeffacer);gc()}


}








#fonctions pratique pour tk
malist<-function (pat) {

# retrouve dans ls les element de type pat
    Vars <- ls(all.names = TRUE,env=.GlobalEnv)
	# #verbose#print(Vars)
    if (length(Vars) == 0) {return(Vars)}
    names(which(sapply(Vars, function(.x) {	is(get(.x))[1]==pat	}	)	)	)
	}
	
getcontentlistbox<-function(listbox){
out<-c()
i<-0		
while (length(as.character(tkget(listbox,i)))!=0){
out<-c(out,as.character(tkget(listbox,i)))
i<-i+1
}
return(out)
}

GetIt<-function(objet){
if (attr(objet,"class")=="tkwin"){
		if (as.character(tkwinfo("class",objet))=="Listbox"){
		return(getcontentlistbox(objet))
		}
}
if (attr(objet,"class")=="tclVar"){
return(tclvalue(objet))

}
}

DoItmini<-function(comm,env=.GlobalEnv){
  result <- parse(text = paste(comm))
  exprs<-result
  i<-1
  #for (i in seq_along(exprs)) {
  ei <- exprs[i]
  #verbose#print(ei)
  result <- eval(ei, envir = env)
  
}

choixdataset<-function(id){
#verbose#print(id)
try(tkdestroy(tt2),silent=T)
tt2<-tktoplevel()
tkwm.geometry(tt2, "-100+50")     
listdata <- malist(paste(id,"Data",sep=""))
dataSetsBox <- variableListBox(tt2, listdata, title=gettext(domain="R-COSTgui","Select one dataset"))# voir initialselection
tkgrid(getFrame(dataSetsBox), sticky="nwes",column=1,row=1)
  tkgrid.rowconfigure(getFrame(dataSetsBox), 1, weight = 1)
  tkgrid.rowconfigure(tt2, 1, weight = 1)  					


onOK <- function(){	
e1 <- new.env(parent = e2)
assign("out", getSelection(dataSetsBox), envir=e1)
out<-getSelection(dataSetsBox)

if (length(out)!=0){

# # DoItmini("print(out)",e1)
DoItmini(paste("tclvalue(lechoix",id,")<-out",sep=""),env=e1)
		# # # tkconfigure(		paste("but.",id,"Data",sep="")		,text=out)
		tkconfigure(eval(as.name(paste("but.",id,"Data",sep=""))),text=out)
		tkdestroy(tt2)	
		
		
		# si tout est OK:
		tkconfigure(rblan,state="normal")
		tkconfigure(rbdis,state="normal")
		tkconfigure(rbplot,state="normal")
		tkconfigure(rbboxplot,state="normal")
		tkconfigure(but.csData.subset,state="normal")
		

lesspp<-NULL
sn<-slotNames(eval(as.name(out)))
for ( p in sn){
truc<-do.call("slot",args =list(as.name(out),p))
if(sum(names(truc)=="spp")>0){lesspp<-unique(c(lesspp,as.character(levels(as.factor(as.character(truc$spp))))))}
}
		
		
		tkdelete(liste.spp,"0","end")
		
		for (hop in lesspp){tkinsert(liste.spp, "end", hop)}
		if(exists("e1")){rm(e1)}
		
		}
		
}
OK.but <-tkbutton(tt2,text="   OK   ",command=onOK)
tkgrid(OK.but,column=1,row=2)
tkfocus(tt2)

}

choixfraction<-function(){
# € on active simplement le bouton
tkconfigure(valid.but,state="active")


}



faitsubset<-function(envi){
#verbose#print("faitsubset")


.cost.subset.virtual(virtual=T,sour=c(1,0,0),dataname=tclvalue(lechoixcs),LENV=envi,parr=tt,label="(Trip volume of catches)")

tclvalue(cbValue.csData.subset)<-"1"
tkconfigure(cb.csData.subset,state="normal")

}

#le GUI

lechoixcs <- tclVar("")
# # lechoixcl <- tclVar("")
# # lechoixce <- tclVar("")
e2 <- new.env(parent = .GlobalEnv)
.envi.landisvol <- new.env(parent = .GlobalEnv)
assign("lechoixcs",lechoixcs,envir=e2)
assign(".envi.landisvol",.envi.landisvol,pos=1)
# # assign("lechoixcl",lechoixcl,envir=e2)
tt <- tktoplevel(borderwidth=10)
tkwm.geometry(tt, "-100+50")
font2<-tkfont.create(family="Times",size=12,weight="bold")
font3<-tkfont.create(family="Times",size=12)
fontheading<-tkfont.create(family="Times",size=18,weight="bold")  
tkwm.title(tt,gettext(domain="R-COSTgui","COST - Figure : Trip volume of catches"))
tkgrid(tklabel(tt, text=gettext(domain="R-COSTgui","Trip volume of catches"),font=fontheading))
tkgrid(tklabel(tt,text="")) # Ligne de blanc
frameTOP<- tkframe(tt, borderwidth=2, relief="groove")
frameD<- tkframe(tt, borderwidth=2, relief="groove")
frameE<- tkframe(tt, borderwidth=2, relief="groove")
frameF<- tkframe(tt, borderwidth=2, relief="groove")
listCS<-malist("csData")
# # but.clData <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset("cl"),foreground="red",relief="ridge")
but.csData <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset("cs"),foreground="red",relief="ridge")
but.csData.subset <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","subset"),state="disabled",command=function(...) faitsubset(.envi.landisvol))
cbValue.csData.subset<- tclVar("0")
cb.csData.subset<-tkcheckbutton(frameTOP,variable=cbValue.csData.subset,state="disabled")

 

# # but.ceData <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset("ce"),foreground="red",relief="ridge")
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","CsData : "),font=font2),but.csData,but.csData.subset)
tkgrid(tklabel(frameTOP,text=" "),tklabel(frameTOP,text=gettext(domain="R-COSTgui","use subset :")),cb.csData.subset)
# # tkgrid(tklabel(frameTOP,text="ClData : ",font=font2),but.clData)
# # tkgrid(tklabel(frameTOP,text="CeData : ",font=font2),but.ceData)
tkgrid(frameTOP)
tkgrid(frameD)
tkgrid(frameF)
tkgrid(frameE)
tvar <- tclVar("NULL")
rbdis <- tkradiobutton(frameD,command=function()choixfraction())
rblan <- tkradiobutton(frameD,command=function()choixfraction())
tkconfigure(rbdis,variable=tvar,value="DIS",state="disabled")
tkconfigure(rblan,variable=tvar,value="LAN",state="disabled")
tkgrid(tklabel(frameD,text=gettext(domain="R-COSTgui","Landings : "),font=font2),rblan,tklabel(frameD,text=gettext(domain="R-COSTgui","Discards : "),font=font2),rbdis)


tvarplot <- tclVar("boxplot")
rbplot <- tkradiobutton(frameF,command=function()choixfraction())
rbboxplot <- tkradiobutton(frameF,command=function()choixfraction())
tkconfigure(rbplot,variable=tvarplot,value="plot",state="disabled")
tkconfigure(rbboxplot,variable=tvarplot,value="boxplot",state="disabled")
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","plot : "),font=font2),rbplot,tklabel(frameF,text=gettext(domain="R-COSTgui","boxplot : "),font=font2),rbboxplot)


liste.spp <-tklistbox(frameE,selectmode="multiple",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(liste.spp.scroll,...))
liste.spp.scroll<-tkscrollbar(frameE,repeatinterval=5,command=function(...)tkyview(liste.spp,...))
tkgrid(liste.spp,liste.spp.scroll,sticky="ns")
frameA<- tkframe(tt, borderwidth=2, relief="groove")
valid.but <- tkbutton(frameA,text=gettext(domain="R-COSTgui","Validate"),state="disabled",command=function(...) valid())
tkgrid(frameA)
text.valid<-tklabel(frameA,text=gettext(domain="R-COSTgui","out :"))
text.simu<-tklabel(frameA,text=gettext(domain="R-COSTgui","save the script :"))
cbValue.simu<-tclVar("0")
cb.simu<-tkcheckbutton(frameA,variable=cbValue.simu,state="normal")

val.valid <- tclVar("wTrip")
entry.valid<-entry.chemin.rdata <-tkentry(frameA,width="20",textvariable=val.valid)


tkgrid(text.valid,entry.valid,valid.but)
tkgrid(tklabel(frameA,text=""),text.simu,cb.simu)

tkfocus(tt)

}
.cost.lenDisPlot <-
function(){


try(require(tcltk))
try(require(tcltk2))
try(library(Rcmdr))
try(require(tkrplot))
try(tclRequire("BWidget"))
try(library(COSTcore))
try(library(COSTeda));try(library(COSTgui))

validOLD<-function(){
lCS<-tclvalue(lechoixcs)
# # lespp<-GetIt(liste.spp)
lesespeces<-NULL 
for (rg in as.character(tkcurselection(liste.spp))){
lesespeces<-c(lesespeces,as.name(paste(as.character(tkget(liste.spp,rg)),collapse=" ")))
}  
    

if (lCS == ""   | is.null(lesespeces)){
Message(message = gettext(domain="R-COSTgui","You have to specify a CsData, a fraction and some species"), type = "error")
}


if (lCS !=""  & !is.null(lesespeces)){
XX<-"c("
for ( i in 1:length(lesespeces)){XX<-paste(XX,"'",as.character(lesespeces[[i]]),"'",sep="")
if ( i !=length(lesespeces)){XX<-paste(XX,",",sep="")}}
XX<-paste(XX,")",sep="")
cmd1<-paste("lenDisPlot(",lCS,",species=",XX,",fraction='",tclvalue(tvar),"')",sep="")

doItAndPrint(cmd1)

}
}


valid<-function(){

lesespeces<-NULL 
for (rg in as.character(tkcurselection(liste.spp))){
lesespeces<-c(lesespeces,as.name(paste(as.character(tkget(liste.spp,rg)),collapse=" ")))
}  


XX<-"c("
for ( i in 1:length(lesespeces)){XX<-paste(XX,"'",as.character(lesespeces[[i]]),"'",sep="")
if ( i !=length(lesespeces)){XX<-paste(XX,",",sep="")}}
XX<-paste(XX,")",sep="")


if (tclvalue(cbValue.simu)=="1"){
if(tclvalue(cbValue.csData.subset)=="0" ){

lCS<-tclvalue(lechoixcs)
}
if(tclvalue(cbValue.csData.subset)=="1" ){
lesub<-get("formule",.envi.lenDisPlot)
for ( i in 1:length(lesub)){
doItAndPrint(lesub[[i]])
}

lCS<-strsplit(lesub[[1]],"<-")[[1]][1]

}
}

if (tclvalue(cbValue.simu)=="0"){
# on fait sans ecrire
if(tclvalue(cbValue.csData.subset)=="0" ){lCS<-tclvalue(lechoixcs)}
if(tclvalue(cbValue.csData.subset)=="1" ){
# ls(.envi.landisvol)
# print("XXX")
lesub<-get("formule",.envi.lenDisPlot)[[1]];lesub
# achercher<-strsplit(lesub,"<-")[[1]][1];achercher
# print(achercher)

# .lesubAeffacer<-get(achercher,.envi.landisvol);.lesubAeffacer
# print("b")
lCS<-paste("get('",strsplit(lesub,"<-")[[1]][1],"',.envi.lenDisPlot)",sep="")
# XXX 

# faut ptet l'assigner au premier niveau

}
}
  

if (lCS == ""   | is.null(lesespeces)){
Message(message = gettext(domain="R-COSTgui","You have to specify a CsData, a fraction and some species"), type = "error")
}


if (lCS !=""  & !is.null(lesespeces)){



if(tclvalue(cbValue.simu)=="1" ){
cmd1<-paste("lenDisPlot(",lCS,",species=",XX,",fraction='",tclvalue(tvar),"')",sep="")
doItAndPrint(cmd1)
}



if(tclvalue(cbValue.simu)=="0" ){
cmd<-paste("lenDisPlot(",lCS,",species=",XX,",fraction='",tclvalue(tvar),"')",sep="")

# cmd<-paste(tclvalue(tvarplot),"(","landisVol(",lCS,",species=",XX,",fraction='",tclvalue(tvar),"'))",sep="")
#verbose#print(cmd)
justDoIt(cmd)
}





}

if (exists(".lesubAeffacer")){rm(.lesubAeffacer);gc()}


}












#fonctions pratique pour tk
malist<-function (pat) {

# retrouve dans ls les element de type pat
    Vars <- ls(all.names = TRUE,env=.GlobalEnv)
	# print(Vars)
    if (length(Vars) == 0) {return(Vars)}
    names(which(sapply(Vars, function(.x) {	is(get(.x))[1]==pat	}	)	)	)
	}
	
getcontentlistbox<-function(listbox){
out<-c()
i<-0		
while (length(as.character(tkget(listbox,i)))!=0){
out<-c(out,as.character(tkget(listbox,i)))
i<-i+1
}
return(out)
}

GetIt<-function(objet){
if (attr(objet,"class")=="tkwin"){
		if (as.character(tkwinfo("class",objet))=="Listbox"){
		return(getcontentlistbox(objet))
		}
}
if (attr(objet,"class")=="tclVar"){
return(tclvalue(objet))

}
}

DoItmini<-function(comm,env=.GlobalEnv){
  result <- parse(text = paste(comm))
  exprs<-result
  i<-1
  #for (i in seq_along(exprs)) {
  ei <- exprs[i]
  #verbose#print(ei)
  result <- eval(ei, envir = env)
  
}

choixdataset<-function(id){
#verbose#print(id)
try(tkdestroy(tt2),silent=T)
tt2<-tktoplevel()
tkwm.geometry(tt2, "-100+50")     
listdata <- malist(paste(id,"Data",sep=""))
dataSetsBox <- variableListBox(tt2, listdata, title=gettext(domain="R-COSTgui","Select one dataset"))# voir initialselection
tkgrid(getFrame(dataSetsBox), sticky="nwes",column=1,row=1)
					
  tkgrid.rowconfigure(getFrame(dataSetsBox), 1, weight = 1)
  tkgrid.rowconfigure(tt2, 1, weight = 1)  

onOK <- function(){	
e1 <- new.env(parent = e2)
assign("out", getSelection(dataSetsBox), envir=e1)
out<-getSelection(dataSetsBox)


if( length(out)!=0){
# # DoItmini("print(out)",e1)
DoItmini(paste("tclvalue(lechoix",id,")<-out",sep=""),env=e1)
		# # # tkconfigure(		paste("but.",id,"Data",sep="")		,text=out)
		tkconfigure(eval(as.name(paste("but.",id,"Data",sep=""))),text=out)
		tkdestroy(tt2)	
		
		
		# si tout est OK:
		tkconfigure(rblan,state="normal")
		tkconfigure(rbdis,state="normal")
		tkconfigure(but.csData.subset,state="normal")

lesspp<-NULL
sn<-slotNames(eval(as.name(out)))
for ( p in sn){
truc<-do.call("slot",args =list(as.name(out),p))
if(sum(names(truc)=="spp")>0){lesspp<-unique(c(lesspp,as.character(levels(as.factor(as.character(truc$spp))))))}
}
		
		for (hop in lesspp){tkinsert(liste.spp, "end", hop)}
		rm(e1)
		
		}
		
}
OK.but <-tkbutton(tt2,text="   OK   ",command=onOK)
tkgrid(OK.but,column=1,row=2)
tkfocus(tt2)

}


choixdatasetAEF<-function(id){
#verbose#print(id)
try(tkdestroy(tt2),silent=T)
tt2<-tktoplevel()
tkwm.geometry(tt2, "-100+50")     
listdata <- malist(paste(id,"Data",sep=""))
dataSetsBox <- variableListBox(tt2, listdata, title=gettext(domain="R-COSTgui","Select one dataset"))# voir initialselection
tkgrid(getFrame(dataSetsBox), sticky="nwes",column=1,row=1)
  tkgrid.rowconfigure(getFrame(dataSetsBox), 1, weight = 1)
  tkgrid.rowconfigure(tt2, 1, weight = 1)  					


onOK <- function(){	
e1 <- new.env(parent = e2)
assign("out", getSelection(dataSetsBox), envir=e1)
out<-getSelection(dataSetsBox)

if (length(out)!=0){

# # DoItmini("print(out)",e1)
DoItmini(paste("tclvalue(lechoix",id,")<-out",sep=""),env=e1)
		# # # tkconfigure(		paste("but.",id,"Data",sep="")		,text=out)
		tkconfigure(eval(as.name(paste("but.",id,"Data",sep=""))),text=out)
		tkdestroy(tt2)	
		
		
		# si tout est OK:
		tkconfigure(rblan,state="normal")
		tkconfigure(rbdis,state="normal")
		tkconfigure(rbplot,state="normal")
		tkconfigure(rbboxplot,state="normal")
		
		

lesspp<-NULL
sn<-slotNames(eval(as.name(out)))
for ( p in sn){
truc<-do.call("slot",args =list(as.name(out),p))
if(sum(names(truc)=="spp")>0){lesspp<-unique(c(lesspp,as.character(levels(as.factor(as.character(truc$spp))))))}
}
		
		
		tkdelete(liste.spp,"0","end")
		
		for (hop in lesspp){tkinsert(liste.spp, "end", hop)}
		rm(e1)
		
		}
		
}
OK.but <-tkbutton(tt2,text="   OK   ",command=onOK)
tkgrid(OK.but,column=1,row=2)
tkfocus(tt2)

}


choixfraction<-function(){
# € on active simplement le bouton
tkconfigure(valid.but,state="active")


}



faitsubset<-function(envi){
#verbose#print("faitsubset")


res<-.cost.subset.virtual(virtual=T,sour=c(1,0,0),dataname=tclvalue(lechoixcs),LENV=envi,parr=tt,label="(Size structure by Trip)")

tclvalue(cbValue.csData.subset)<-"1"
tkconfigure(cb.csData.subset,state="normal")

}


#le GUI

lechoixcs <- tclVar("")
# # lechoixcl <- tclVar("")
# # lechoixce <- tclVar("")
e2 <- new.env(parent = .GlobalEnv)

.envi.lenDisPlot <- new.env(parent = .GlobalEnv)
assign("lechoixcs",lechoixcs,envir=e2)
assign(".envi.lenDisPlot",.envi.lenDisPlot,pos=1)
# # assign("lechoixcl",lechoixcl,envir=e2)
tt <- tktoplevel(borderwidth=10)
tkwm.geometry(tt, "-100+50")
font2<-tkfont.create(family="Times",size=12,weight="bold")
font3<-tkfont.create(family="Times",size=12)
fontheading<-tkfont.create(family="Times",size=18,weight="bold")  
tkwm.title(tt,gettext(domain="R-COSTgui","COST - Figure : Size structure by Trip"))
tkgrid(tklabel(tt, text=gettext(domain="R-COSTgui","Size structure by Trip"),font=fontheading))
tkgrid(tklabel(tt,text="")) # Ligne de blanc
frameTOP<- tkframe(tt, borderwidth=2, relief="groove")
frameD<- tkframe(tt, borderwidth=2, relief="groove")
frameE<- tkframe(tt, borderwidth=2, relief="groove")
frameA<- tkframe(tt, borderwidth=2, relief="groove")
listCS<-malist("csData")
# # but.clData <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset("cl"),foreground="red",relief="ridge")
but.csData <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset("cs"),foreground="red",relief="ridge")
# # but.ceData <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset("ce"),foreground="red",relief="ridge")

but.csData.subset <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","subset"),state="disabled",command=function(...) faitsubset(.envi.lenDisPlot))
cbValue.csData.subset<- tclVar("0")
cb.csData.subset<-tkcheckbutton(frameTOP,variable=cbValue.csData.subset,state="disabled")
text.simu<-tklabel(frameA,text=gettext(domain="R-COSTgui","save the script :"))
cbValue.simu<-tclVar("0")
cb.simu<-tkcheckbutton(frameA,variable=cbValue.simu,state="normal")


tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","CsData : "),font=font2),but.csData,but.csData.subset)
tkgrid(tklabel(frameTOP,text=" "),tklabel(frameTOP,text=gettext(domain="R-COSTgui","use subset :")),cb.csData.subset)

# # tkgrid(tklabel(frameTOP,text="ClData : ",font=font2),but.clData)
# # tkgrid(tklabel(frameTOP,text="CeData : ",font=font2),but.ceData)
tkgrid(frameTOP)
tkgrid(frameD)
tkgrid(frameE)
tvar <- tclVar("NULL")
rbdis <- tkradiobutton(frameD,command=function()choixfraction())
rblan <- tkradiobutton(frameD,command=function()choixfraction())
tkconfigure(rbdis,variable=tvar,value="DIS",state="disabled")
tkconfigure(rblan,variable=tvar,value="LAN",state="disabled")
tkgrid(tklabel(frameD,text=gettext(domain="R-COSTgui","Landings : "),font=font2),rblan,tklabel(frameD,text=gettext(domain="R-COSTgui","Discards : "),font=font2),rbdis)
liste.spp <-tklistbox(frameE,selectmode="unique",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(liste.spp.scroll,...))
liste.spp.scroll<-tkscrollbar(frameE,repeatinterval=5,command=function(...)tkyview(liste.spp,...))
tkgrid(liste.spp,liste.spp.scroll,sticky="ns")

valid.but <- tkbutton(frameA,text=gettext(domain="R-COSTgui","Validate"),state="disabled",command=function(...) valid())
tkgrid(frameA)
# text.valid<-tklabel(frameA,text=gettext(domain="R-COSTgui","out :"))
# val.valid <- tclVar("wTrip")
# entry.valid<-entry.chemin.rdata <-tkentry(frameA,width="20",textvariable=val.valid)
# tkgrid(text.valid,entry.valid,valid.but)
tkgrid(valid.but)
tkgrid(text.simu,cb.simu)

tkfocus(tt)
}
.cost.makeICdf <-
function(){




try(require(tcltk))
try(require(tcltk2))
try(library(Rcmdr))
try(require(tkrplot))
try(tclRequire("BWidget"))
try(library(COSTcore))
try(library(COSTeda));try(library(COSTgui));try(library(COSTdbe));try(library(COSTdbe))



tdspinner <- function(parent, ...) {tkwidget(parent, "spinbox", ...)}

#fonctions spécifiques
parcour<-function(ert){
  
 
  chemin<-"NULL"
  # chemin<-file.choose()
chemin<-tclvalue(tkgetSaveFile(filetypes = "{{csv} {.csv}}"))

#onregarde si ca fini bien par csv, sinon on rajoute
  
  
if (substr(chemin,nchar(chemin)-3,nchar(chemin))!=".csv"){
chemin<-paste(chemin,".csv",sep="")
}
  
  
  if (chemin!="NULL"){
  tempo<-eval(as.name(paste("chemin",ert,sep=".")))
  tclvalue(tempo)<-paste(strsplit(chemin,"\\\\")[[1]],collapse="/")
  
}
}

valid<-function(){



# puis on fait makeICdf

cmd2<-paste(tclvalue(val.out),"<-makeICdf(dbeOutput=",tclvalue(lechoix.but.dbeOutput),sep="")


cmd2<-paste(cmd2,",filename='",tclvalue(chemin.filename),"'",
",output.df=",as.logical(as.numeric(tclvalue(cbValue.output.df))),
",append=",as.logical(as.numeric(tclvalue(cbValue.append))),
sep="")



if (length(as.character(listCANUMtype[as.numeric(tclvalue(tcl(combo.CANUMtype,"getvalue")))+1]) != 0)){

cmd2<-paste(cmd2,",CANUMtype='",as.character(listCANUMtype[as.numeric(tclvalue(tcl(combo.CANUMtype,"getvalue")))+1]),"'",sep="")

}

if (tclvalue(val.Country)!=""){
cmd2<-paste(cmd2,",Country='",tclvalue(val.Country),"'",sep="")
}
if (tclvalue(val.ReportingCategory)!=""){
cmd2<-paste(cmd2,",ReportingCategory='",tclvalue(val.ReportingCategory),"'",sep="")
}
if (tclvalue(val.SamplesOrigin)!=""){
cmd2<-paste(cmd2,",SamplesOrigin='",tclvalue(val.SamplesOrigin),"'",sep="")
}



cmd2<-paste(cmd2,")",sep="")
print(cmd2)



doItAndPrint(cmd2)



}

slotNamesdata<-function(data){
ploom<-slotNames(data)
out<-c()
for (p in ploom){
if(is(slot(data, p))[1] == "data.frame"){
out<-c(out,p)
}
}

return(out)
}

choixdataset.multi<-function(id,nom.but){
print(id)
try(tkdestroy(tt2),silent=T)
tt2<-tktoplevel()
tkwm.geometry(tt2, "-100+50")     
listdata <-NULL
for ( i in id){listdata <- c(listdata,malist(i))}
dataSetsBox <- variableListBox(tt2, listdata, title=gettext(domain="R-COSTgui","Select one dataset"))# voir initialselection
tkgrid(getFrame(dataSetsBox), sticky="nwes",column=1,row=1)
  tkgrid.rowconfigure(getFrame(dataSetsBox), 1, weight = 1)
  tkgrid.rowconfigure(tt2, 1, weight = 1)  
onOK <- function(){	
out<-getSelection(dataSetsBox)

		if ( length(out) != 0){
		eval(parse(text=paste("tclvalue(lechoix.",nom.but,")<-out",sep="")))
		tkconfigure(eval(as.name(nom.but)),text=out)
		# print(GetIt((eval(as.name(paste("lechoix.",nom.but,sep=""))))))
		# tkconfigure( names.list.quali, state="disabled" )	
		# tkconfigure( names.list.quanti, state="disabled" )
		
		

		# print("on met a jour la liste des variables")

		# a ce niveau on a le nom du cs objet, on va lister toute les variaible exploitable pour le spaceplot
	if (nom.but == "but.dbeOutput"){

	
	# on charge la description
	tclvalue(val.desc)<-eval(as.name(out))@desc
	tclvalue(val.param)<-eval(as.name(out))@param
	# tclvalue(val.param)<-
	tclvalue(val.methodDesc)<-eval(as.name(out))@methodDesc	
	tclvalue(val.catchCat)<-eval(as.name(out))@catchCat
	tclvalue(val.species)<-eval(as.name(out))@species
	tclvalue(val.timeStrata)<-eval(as.name(out))@strataDesc@timeStrata
	tclvalue(val.spaceStrata)<-eval(as.name(out))@strataDesc@spaceStrata
	tclvalue(val.techStrata)<-eval(as.name(out))@strataDesc@techStrata
	
	
	
	# [1] "quarter"

# Slot "spaceStrata":
# [1] NA

# Slot "techStrata":
	
	
  # listslot<-slotNamesdata(eval(as.name(out)))
# araj<-c()
# for ( i in 1:length(listslot)){	
# truc<-do.call("slot",args =list(as.name(out),listslot[i]))
# araj<-unique(c(araj,levels(as.factor(truc[["spp"]]))));araj
# }
# tkdelete(list.species,"0","end");
# for (var in araj) {tkinsert(list.species, "end", var)}

# on check les LAN et DIS

	# LD<-unique(substr(eval(as.name(out))@hl$sort,1,3))
	# print(LD)
# if(!is.element("DIS",LD)){
# print("pas de DIS")
# tkconfigure(rbdis,state="disabled")
# tclvalue(tvarcatchCat)<-"LAN"

# }
# if(!is.element("LAN",LD)){
# print("pas de LAN")
# tkconfigure(rblan,state="disabled")
# tclvalue(tvarcatchCat)<-"DIS"
# }

		}
	
		
		
		
# if ( 		tclvalue(lechoix.but.csObject)!= "" & tclvalue(lechoix.but.dbeOutput)!= ""){

tkconfigure(valid.but,state="normal")
tkconfigure(but.filename,state="normal")

# }
		
# aactiver<-list(entry.ylab,entry.xlab,list.time,list.tech,list.space,but.refr,entry.fonction,entry.main,cb.simu,cb.auto,list.time,list.tech,spin.x1,spin.x2,spin.y1,spin.y2,rbsel.image,rbsel.contour,rbsel.bubble,rbsel.value)
# # for ( i in 1:length(aactiver)){tkconfigure(aactiver[[i]],state="normal")}

# lapply(aactiver,tkconfigure,state="normal")

# choixtime<-c("year","semester","quarter","month")
# choixspace<-c("rect","subrect","area")
# choixtech<-c("commCat","commCatScl", "foCatEu5", "foCatEu6", "foCatNat","catchCat")
# tkdelete(list.time,"0","end");for (var in choixtime) {tkinsert(list.time, "end", var)}
# tkdelete(list.tech,"0","end");for (var in choixtech) {tkinsert(list.tech, "end", var)}
# tkdelete(list.space,"0","end");for (var in choixspace) {tkinsert(list.space, "end", var)}


	
		
}
tkdestroy(tt2)	
		
}
	
OK.but <-tkbutton(tt2,text="   OK   ",command=onOK)
tkgrid(OK.but,column=1,row=2)
tkfocus(tt2)
}

#fonctions pratique pour tk
malist<-function (pat) {

# retrouve dans ls les element de type pat
    Vars <- ls(all.names = TRUE,env=.GlobalEnv)
	# print(Vars)
    if (length(Vars) == 0) {return(Vars)}
    names(which(sapply(Vars, function(.x) {	is(get(.x))[1]==pat	}	)	)	)
	}
	
getcontentlistbox<-function(listbox){
out<-c()
i<-0		
while (length(as.character(tkget(listbox,i)))!=0){
out<-c(out,as.character(tkget(listbox,i)))
i<-i+1
}
return(out)
}

GetIt<-function(objet){
if (attr(objet,"class")=="tkwin"){
		if (as.character(tkwinfo("class",objet))=="Listbox"){
		return(getcontentlistbox(objet))
		}
}
if (attr(objet,"class")=="tclVar"){
return(tclvalue(objet))

}
}

DoItmini<-function(comm,env=.GlobalEnv){
  result <- parse(text = paste(comm))
  exprs<-result
  i<-1
  #for (i in seq_along(exprs)) {
  ei <- exprs[i]
  print(ei)
  result <- eval(ei, envir = env)
  
}

reset.strIni<-function(){
tkconfigure(but.strini,text=gettext(domain="R-COSTgui","<select>"))
tclvalue(lechoix.but.strini)<-""
}

choixrb<-function(){
if("NULL" != tclvalue(tvarmethodDesc) & "NULL" != tclvalue(tvarcatchCat)){

tkconfigure(valid.but,state="active")

}

}

proba<-function(val){
tclvalue(lechoix.p2)<- round(as.numeric(1-as.numeric(tclvalue(lechoix.p1))),3)
}

checkind<-function(){
tkconfigure(spin.p1,state=c("disabled","normal")[as.numeric(tclvalue(cbValue.incl.precision))+1])}

checkfill<-function(){

tkconfigure(spin.p,state=c("disabled","normal")[as.numeric(tclvalue(cbValue.fillGaps))+1])}


# lechoix.but.csObject <- tclVar("")
lechoix.but.dbeOutput <- tclVar("")
lechoix.but.filename <- tclVar("")
# lechoix.but.strini <- tclVar("")
lechoix.p1 <- tclVar(0.025)
lechoix.p2 <- tclVar(0.975)
lechoix.p <- tclVar(10) # a voir si c 'est bien par defaut

tt <- tktoplevel(borderwidth=10)
tkwm.geometry(tt, "-100+50")
font2<-tkfont.create(family="Times",size=12,weight="bold")
font3<-tkfont.create(family="Times",size=12)
fontheading<-tkfont.create(family="Times",size=18,weight="bold")  
tkwm.title(tt,gettext(domain="R-COSTgui","COST - makeICdf"))
tkgrid(tklabel(tt, text=gettext(domain="R-COSTgui","Export to InterCatch"),font=fontheading))
tkgrid(tklabel(tt,text="")) # Ligne de blanc
frameTOP<- tkframe(tt, borderwidth=2, relief="groove")
frameBONUS<- tkframe(tt, borderwidth=2, relief="groove")
# frameA<- tkframe(tt, borderwidth=2, relief="groove")
# frameB<- tkframe(tt, borderwidth=2, relief="groove")
# frameC<- tkframe(tt, borderwidth=2, relief="groove")
frameD<- tkframe(tt, borderwidth=2, relief="groove")
# frameE<- tkframe(frameTOP, borderwidth=2, relief="groove")
frameF<- tkframe(tt, borderwidth=2, relief="groove")



# les entry
val.desc <- tclVar("")
entry.desc <-tkentry(frameTOP,width="20",textvariable=val.desc,state="disabled")
val.species <- tclVar("")
entry.species <-tkentry(frameTOP,width="20",textvariable=val.species,state="disabled")
val.out <- tclVar("aggreg")
entry.out <-tkentry(frameD,width="20",textvariable=val.out)
val.param <- tclVar("")
entry.param <-tkentry(frameTOP,width="20",textvariable=val.param,state="disabled")
val.methodDesc <- tclVar("")
entry.methodDesc <-tkentry(frameTOP,width="20",textvariable=val.methodDesc,state="disabled")
val.catchCat <- tclVar("")
entry.catchCat <-tkentry(frameTOP,width="20",textvariable=val.catchCat,state="disabled")
val.timeStrata <- tclVar("")
entry.timeStrata <-tkentry(frameTOP,width="20",textvariable=val.timeStrata,state="disabled")


val.techStrata <- tclVar("")
entry.techStrata <-tkentry(frameTOP,width="20",textvariable=val.techStrata,state="disabled")


val.spaceStrata <- tclVar("")
entry.spaceStrata <-tkentry(frameTOP,width="20",textvariable=val.spaceStrata,state="disabled")


val.wt <- tclVar("totalW")
entry.wt <-tkentry(frameF,width="8",textvariable=val.wt,state="normal")

chemin.filename <- tclVar("")
entry.filename <-tkentry(frameF,width="20",textvariable=chemin.filename,state="normal")


val.Country <- tclVar("UKE")
entry.Country <-tkentry(frameF,width="8",textvariable=val.Country,state="normal")


val.ReportingCategory <- tclVar("R")
entry.ReportingCategory <-tkentry(frameF,width="8",textvariable=val.ReportingCategory,state="normal")

val.SamplesOrigin <- tclVar("M")
entry.SamplesOrigin <-tkentry(frameF,width="8",textvariable=val.SamplesOrigin,state="normal")


# methodDesc	
	# eval(as.name(out))@catchCat
# # val.param <- tclVar("")
# entry.param <-tkentry(frameTOP,width="20",textvariable=val.param)

#les combo

listCANUMtype<-c("age", "lngt" , "length")
combo.CANUMtype <- tkwidget(frameF,"ComboBox",editable=FALSE,values=listCANUMtype,state="normal")


# listbootMethod<-c("samples","otoliths")
# combo.bootMethod <- tkwidget(frameF,"ComboBox",editable=FALSE,values=listbootMethod,state="normal")

#les spin
spin.p1<-tdspinner(frameF,from=0,to=1,inc=0.001,width=5,textvariable = lechoix.p1,state="normal",command=function(...)proba(1))
spin.p2<-tdspinner(frameF,from=0,to=1,inc=0.001,width=5,textvariable = lechoix.p2,state="disabled",command=function(...)proba(2))
spin.p<-tdspinner(frameF,from=1,to=100,inc=1,width=5,textvariable = lechoix.p,state="disabled")


# les radio
# tvarcatchCat <- tclVar("NULL")
# rbdis <- tkradiobutton(frameB,command=function()choixrb())
# rblan <- tkradiobutton(frameB,command=function()choixrb())
# tkconfigure(rbdis,variable=tvarcatchCat,value="DIS",state="disabled")
# tkconfigure(rblan,variable=tvarcatchCat,value="LAN",state="disabled")


# tvarmethodDesc <- tclVar("NULL")
# rbanal <- tkradiobutton(frameC,command=function()choixrb())
# rbboot <- tkradiobutton(frameC,command=function()choixrb())
# tkconfigure(rbanal,variable=tvarmethodDesc,value="analytic",state="disabled")
# tkconfigure(rbboot,variable=tvarmethodDesc,value="bootstrap",state="disabled")
#les bouton


# but.strini <- tkbutton(frameA,text=gettext(domain="R-COSTgui","<select>"),state="active",command=function(...) choixdataset.multi("strIni","but.strini"),foreground="red",relief="ridge")
# but.newstrini <- tkbutton(frameA,text=gettext(domain="R-COSTgui","New strIni type object"),state="active",command=function(...) .cost.strIni())#,foreground="red",relief="ridge")
# but.resetstrini <- tkbutton(frameA,text="reset",state="active",command=function(...) reset.strIni())#,foreground="red",relief="ridge")
valid.but <- tkbutton(frameD,text=gettext(domain="R-COSTgui","Validate"),state="disabled",command=function(...) valid())
but.filename <- tkbutton(frameF,text=gettext(domain="R-COSTgui","Browse"),state="normal",command=function(...) parcour("filename"))

# but.csObject <- tkbutton(frameBONUS,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset.multi(c("csDataCons"),"but.csObject"),foreground="red",relief="ridge")

but.dbeOutput <- tkbutton(frameBONUS,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset.multi(c("dbeOutput"),"but.dbeOutput"),foreground="red",relief="ridge")

# les check

cbValue.incl.precision<-tclVar("1")
cb.incl.precision <- tkcheckbutton(frameF,variable=cbValue.incl.precision,state="normal",command=function()checkind())

cbValue.timeStrata<-tclVar("0")
cb.timeStrata <- tkcheckbutton(frameF,variable=cbValue.timeStrata,state="normal")

cbValue.techStrata<-tclVar("0")
cb.techStrata <- tkcheckbutton(frameF,variable=cbValue.techStrata,state="normal")

cbValue.spaceStrata<-tclVar("0")
cb.spaceStrata <- tkcheckbutton(frameF,variable=cbValue.spaceStrata,state="normal")

# cbValue.fillGaps<-tclVar("0")
# cb.fillGaps <- tkcheckbutton(frameF,variable=cbValue.fillGaps,state="normal",command=function()checkfill())

cbValue.output.df<-tclVar("0")
cb.output.df <- tkcheckbutton(frameF,variable=cbValue.output.df,state="normal")


cbValue.append<-tclVar("0")
cb.append <- tkcheckbutton(frameF,variable=cbValue.append,state="normal")


#text

text.valid<-tklabel(frameD,text=gettext(domain="R-COSTgui","out :"))
#list

# list.species <-tklistbox(frameE,selectmode="unique",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.species.scroll,...))
# list.species.scroll<-tkscrollbar(frameE,repeatinterval=5,command=function(...)tkyview(list.species,...))

tkgrid(frameBONUS)
tkgrid(frameTOP)
# tkgrid(frameA)
# tkgrid(frameB)
# tkgrid(frameC)
tkgrid(frameF)

tkgrid(frameD)

# tkgrid(tklabel(frameBONUS,text="csDataCons:",font=font2),but.csObject)
tkgrid(tklabel(frameBONUS,text=gettext(domain="R-COSTgui","dbeOutput:"),font=font2),but.dbeOutput)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Description : ")),entry.desc)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Catch category : ")),entry.catchCat)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Method description : ")),entry.methodDesc)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Species : ")),entry.species)


tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Parameter : ")),entry.param)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Technical strata : ")),entry.techStrata)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Temporal strata : ")),entry.timeStrata)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Space strata : ")),entry.spaceStrata)
# tkgrid(tklabel(frameE,text=gettext(domain="R-COSTgui","Species"),font=font2))
# tkgrid(list.species,list.species.scroll,sticky="nswe")entry.methodDesc <-tkentry(frameTOP,width="20",textvariable=val.methodDesc,state="disabled")
# val.catchCat <- tclVar("")
# entry.catchCa



# tkgrid(frameE,columnspan=2)#,sticky="ew")

# tkgrid(tklabel(frameA,text=gettext(domain="R-COSTgui","StrIni : "),font=font2),but.strini,but.newstrini,but.resetstrini)
# tkgrid(tklabel(frameB,text="LAN : ",font=font2),rblan,tklabel(frameB,text="DIS : ",font=font2),rbdis)
# tkgrid(tklabel(frameC,text="analytic : ",font=font2),rbanal,tklabel(frameC,text="bootstrap : ",font=font2),rbboot)

tkgrid(frameF)

# tkgrid(tklabel(frameF,text="type : "),combo.type)
# tkgrid(tklabel(frameF,text="bootMethod : "),combo.bootMethod)
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","filename : ")),entry.filename,but.filename)
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","output.df : ")),cb.output.df)
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","append : ")),cb.append)
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","CANUMtype : ")),combo.CANUMtype)
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","Country : ")),entry.Country)
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","ReportingCategory : ")),entry.ReportingCategory)
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","SamplesOrigin : ")),entry.SamplesOrigin)
# tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui"," Usage : ")),tklabel(frameF,text=" ?? "))


# # tkgrid(tklabel(frameF,text="trace : "),cb.trace)
# tkgrid(tklabel(frameF,text="incl.precision : "),cb.incl.precision)
# tkgrid(tklabel(frameF,text="probas : "),spin.p1,spin.p2)
tkgrid(valid.but)


tkfocus(tt)

}
.cost.RaiseAge <-
function(){




try(require(tcltk))
try(require(tcltk2))
try(library(Rcmdr))
try(require(tkrplot))
try(tclRequire("BWidget"))
try(library(COSTcore))
try(library(COSTeda));try(library(COSTgui));try(library(COSTdbe));try(library(COSTdbe))



tdspinner <- function(parent, ...) {tkwidget(parent, "spinbox", ...)}


valid<-function(){


#boot

if ( tclvalue(tvarmethodDesc)=="bootstrap"){
cmd2<-paste(tclvalue(val.out),"<-RaiseAgeBoot(dbeOutput=",tclvalue(lechoix.but.dbeOutput),",csObject=",tclvalue(lechoix.but.csObject),sep="")


if (tclvalue(cbValue.fillGaps) =="1"){
cmd2<-paste(cmd2,",fillGaps=",as.logical(as.numeric(tclvalue(cbValue.fillGaps))),",p=",as.numeric(tclvalue(lechoix.p)),sep="")
}

}

if ( tclvalue(tvarmethodDesc)=="analytical"){
cmd2<-paste(tclvalue(val.out),"<-RaiseAgeBoot(dbeOutput=",tclvalue(lechoix.but.dbeOutput),",csObject=",tclvalue(lechoix.but.csObject),sep="")
}

if( tclvalue(tvartype)=="direct"){


cmd2<-paste(cmd2,",clObject='",tclvalue(lechoix.but.clObject),"'",sep="")

}


cmd2<-paste(cmd2,",type='",tclvalue(tvartype),"'",sep="")

# cmd2<-paste(cmd2,",bootMethod='samples'",sep="")





# cmd2<-paste(cmd2,",incl.precision=",as.logical(as.numeric(tclvalue(cbValue.incl.precision))),",trace=",as.logical(as.numeric(tclvalue(cbValue.trace))),sep="")




if (tclvalue(lechoix.p1)!="0.025" & tclvalue(cbValue.incl.precision) !="0"){
tclvalue(lechoix.p2)<- round(as.numeric(1-as.numeric(tclvalue(lechoix.p1))),3)
cmd2<-paste(cmd2,paste(",probs=c(",tclvalue(lechoix.p1),",",tclvalue(lechoix.p2),")",sep=""),sep="")
}

if ( tclvalue(val.sex) !=""){
cmd2<-paste(cmd2,",sex='",tclvalue(val.sex),"'",sep="")

}


cmd2<-paste(cmd2,")",sep="")

print(cmd2)
doItAndPrint(cmd2)



}















slotNamesdata<-function(data){
ploom<-slotNames(data)
out<-c()
for (p in ploom){
if(is(slot(data, p))[1] == "data.frame"){
out<-c(out,p)
}
}

return(out)
}



choixdataset.multi<-function(id,nom.but){
print(id)
try(tkdestroy(tt2),silent=T)
tt2<-tktoplevel()
tkwm.geometry(tt2, "-100+50")     
listdata <-NULL
for ( i in id){listdata <- c(listdata,malist(i))}
dataSetsBox <- variableListBox(tt2, listdata, title=gettext(domain="R-COSTgui","Select one dataset"))# voir initialselection
tkgrid(getFrame(dataSetsBox), sticky="nwes",column=1,row=1)
  tkgrid.rowconfigure(getFrame(dataSetsBox), 1, weight = 1)
  tkgrid.rowconfigure(tt2, 1, weight = 1)  
onOK <- function(){	
out<-getSelection(dataSetsBox)

		if ( length(out) != 0){
		eval(parse(text=paste("tclvalue(lechoix.",nom.but,")<-out",sep="")))
		tkconfigure(eval(as.name(nom.but)),text=out)
		# print(GetIt((eval(as.name(paste("lechoix.",nom.but,sep=""))))))
		# tkconfigure( names.list.quali, state="disabled" )	
		# tkconfigure( names.list.quanti, state="disabled" )
		
		

		# print("on met a jour la liste des variables")

		# a ce niveau on a le nom du cs objet, on va lister toute les variaible exploitable pour le spaceplot
	if (nom.but == "but.dbeOutput"){

	
	# on charge la description
	tclvalue(val.desc)<-eval(as.name(out))@desc
	tclvalue(val.param)<-eval(as.name(out))@param
	# tclvalue(val.param)<-
	tclvalue(val.methodDesc)<-eval(as.name(out))@methodDesc	
	tclvalue(val.catchCat)<-eval(as.name(out))@catchCat
	tclvalue(val.species)<-eval(as.name(out))@species
	tclvalue(val.timeStrata)<-eval(as.name(out))@strataDesc@timeStrata
	tclvalue(val.spaceStrata)<-eval(as.name(out))@strataDesc@spaceStrata
	tclvalue(val.techStrata)<-eval(as.name(out))@strataDesc@techStrata
	
	# on traite methodDesc
	# if ( tclvalue(val.methodDesc) != "NA"){
	tclvalue(tvarmethodDesc)<-tclvalue(val.methodDesc)# meme pas besoin de faire le IF ici en fait
		# }
	choixrb("meth")
	
	# [1] "quarter"

# Slot "spaceStrata":
# [1] NA

# Slot "techStrata":
	
	
  # listslot<-slotNamesdata(eval(as.name(out)))
# araj<-c()
# for ( i in 1:length(listslot)){	
# truc<-do.call("slot",args =list(as.name(out),listslot[i]))
# araj<-unique(c(araj,levels(as.factor(truc[["spp"]]))));araj
# }
# tkdelete(list.species,"0","end");
# for (var in araj) {tkinsert(list.species, "end", var)}

# on check les LAN et DIS

	# LD<-unique(substr(eval(as.name(out))@hl$sort,1,3))
	# print(LD)
# if(!is.element("DIS",LD)){
# print("pas de DIS")
# tkconfigure(rbdis,state="disabled")
# tclvalue(tvarcatchCat)<-"LAN"

# }
# if(!is.element("LAN",LD)){
# print("pas de LAN")
# tkconfigure(rblan,state="disabled")
# tclvalue(tvarcatchCat)<-"DIS"
# }

		}
	
		
		
		
if ( tclvalue(tvarmethodDesc) !="NA"	&	tclvalue(lechoix.but.csObject)!= "" & tclvalue(lechoix.but.dbeOutput)!= ""){

tkconfigure(valid.but,state="normal")

}
		
# aactiver<-list(entry.ylab,entry.xlab,list.time,list.tech,list.space,but.refr,entry.fonction,entry.main,cb.simu,cb.auto,list.time,list.tech,spin.x1,spin.x2,spin.y1,spin.y2,rbsel.image,rbsel.contour,rbsel.bubble,rbsel.value)
# # for ( i in 1:length(aactiver)){tkconfigure(aactiver[[i]],state="normal")}

# lapply(aactiver,tkconfigure,state="normal")

# choixtime<-c("year","semester","quarter","month")
# choixspace<-c("rect","subrect","area")
# choixtech<-c("commCat","commCatScl", "foCatEu5", "foCatEu6", "foCatNat","catchCat")
# tkdelete(list.time,"0","end");for (var in choixtime) {tkinsert(list.time, "end", var)}
# tkdelete(list.tech,"0","end");for (var in choixtech) {tkinsert(list.tech, "end", var)}
# tkdelete(list.space,"0","end");for (var in choixspace) {tkinsert(list.space, "end", var)}


	
		
}
tkdestroy(tt2)	
		
}
	
OK.but <-tkbutton(tt2,text=gettext(domain="R-COSTgui","   OK   "),command=onOK)
tkgrid(OK.but,column=1,row=2)
tkfocus(tt2)
}

#fonctions pratique pour tk
malist<-function (pat) {

# retrouve dans ls les element de type pat
    Vars <- ls(all.names = TRUE,env=.GlobalEnv)
	# print(Vars)
    if (length(Vars) == 0) {return(Vars)}
    names(which(sapply(Vars, function(.x) {	is(get(.x))[1]==pat	}	)	)	)
	}
	
getcontentlistbox<-function(listbox){
out<-c()
i<-0		
while (length(as.character(tkget(listbox,i)))!=0){
out<-c(out,as.character(tkget(listbox,i)))
i<-i+1
}
return(out)
}

GetIt<-function(objet){
if (attr(objet,"class")=="tkwin"){
		if (as.character(tkwinfo("class",objet))=="Listbox"){
		return(getcontentlistbox(objet))
		}
}
if (attr(objet,"class")=="tclVar"){
return(tclvalue(objet))

}
}

DoItmini<-function(comm,env=.GlobalEnv){
  result <- parse(text = paste(comm))
  exprs<-result
  i<-1
  #for (i in seq_along(exprs)) {
  ei <- exprs[i]
  print(ei)
  result <- eval(ei, envir = env)
  
}

reset.strIni<-function(){
tkconfigure(but.strini,text=gettext(domain="R-COSTgui","<select>"))
tclvalue(lechoix.but.strini)<-""
}

choixrb<-function(id){
# # if("NULL" != tclvalue(tvarmethodDesc) & "NULL" != tclvalue(tvarcatchCat)){

# # tkconfigure(valid.but,state="active")

# # }

# on gere le cas direct et cl
if ( id =="type"){
if (tclvalue(tvartype)=="direct"){
# cl et font
tkconfigure(but.clObject,state="normal")
tkconfigure(labelCL,font=font2)
}
if (tclvalue(tvartype)!="direct"){
# cl et font
tkconfigure(but.clObject,state="disabled")
tkconfigure(labelCL,font=font1)
}

}

# on gere le type
if ( id=="meth"){
tkconfigure(rbfixedK,state="normal")
tkconfigure(rbpropK,state="normal")
tkconfigure(rbagesK,state="normal")
tkconfigure(rbp,state="normal")
if ( tclvalue(tvarmethodDesc)=="analytical"){
tkconfigure(rbdirect,state="normal")
tkconfigure(cb.fillGaps,state="disabled")
tkconfigure(spin.p,state="disabled")



}
if ( tclvalue(tvarmethodDesc)=="bootstrap"){
tkconfigure(rbdirect,state="disabled")
tkconfigure(cb.fillGaps,state="normal")
tkconfigure(but.clObject,state="disabled")
tkconfigure(labelCL,font=font1)
checkfill()

if (tclvalue(tvartype)=="direct"){tclvalue(tvartype)<-"p"}
}

if ( tclvalue(tvarmethodDesc) !="NA"	&	tclvalue(lechoix.but.csObject)!= "" & tclvalue(lechoix.but.dbeOutput)!= ""){

tkconfigure(valid.but,state="normal")

}
}

#prevoir activer validate si c'est pas fait

}

proba<-function(val){
tclvalue(lechoix.p2)<- round(as.numeric(1-as.numeric(tclvalue(lechoix.p1))),3)
}

checkind<-function(){
tkconfigure(spin.p1,state=c("disabled","normal")[as.numeric(tclvalue(cbValue.incl.precision))+1])}

checkfill<-function(){

tkconfigure(spin.p,state=c("disabled","normal")[as.numeric(tclvalue(cbValue.fillGaps))+1])}


lechoix.but.csObject <- tclVar("")
lechoix.but.clObject <- tclVar("")
lechoix.but.dbeOutput <- tclVar("")
# lechoix.but.strini <- tclVar("")
lechoix.p1 <- tclVar(0.025)
lechoix.p2 <- tclVar(0.975)
lechoix.p <- tclVar(10) # a voir si c 'est bien par defaut

tt <- tktoplevel(borderwidth=10)
tkwm.geometry(tt, "-100+50")
font2<-tkfont.create(family="Times",size=12,weight="bold")
font1<-font3<-tkfont.create(family="Times",size=12)
fontheading<-tkfont.create(family="Times",size=18,weight="bold")  
tkwm.title(tt,gettext(domain="R-COSTgui","COST - Raise parameters at Age"))
tkgrid(tklabel(tt, text=gettext(domain="R-COSTgui","Raise parameters at Age"),font=fontheading),row=0,column=1,columnspan=4)


tkgrid(tklabel(tt,text="")) # Ligne de blanc
frameTOP<- tkframe(tt, borderwidth=2, relief="groove")
frameBONUS<- tkframe(tt, borderwidth=2, relief="groove")
# frameA<- tkframe(tt, borderwidth=2, relief="groove")
# frameB<- tkframe(tt, borderwidth=2, relief="groove")
# frameC<- tkframe(tt, borderwidth=2, relief="groove")
frameD<- tkframe(tt, borderwidth=2, relief="groove")
# frameE<- tkframe(frameTOP, borderwidth=2, relief="groove")
frameF<- tkframe(tt, borderwidth=2, relief="groove")
framemeth<- tkframe(frameF, borderwidth=2, relief="groove")
frametype<- tkframe(frameF, borderwidth=2, relief="groove")
framespin<- tkframe(frameF, borderwidth=0, relief="groove")



# les entry
val.desc <- tclVar("")
entry.desc <-tkentry(frameTOP,width="20",textvariable=val.desc,state="disabled")
val.species <- tclVar("")
entry.species <-tkentry(frameTOP,width="20",textvariable=val.species,state="disabled")
val.out <- tclVar("age_dbe.boot")
entry.out <-tkentry(frameD,width="20",textvariable=val.out)
val.param <- tclVar("")
entry.param <-tkentry(frameTOP,width="20",textvariable=val.param,state="disabled")
val.methodDesc <- tclVar("")
entry.methodDesc <-tkentry(frameTOP,width="20",textvariable=val.methodDesc,state="disabled")
val.catchCat <- tclVar("")
entry.catchCat <-tkentry(frameTOP,width="20",textvariable=val.catchCat,state="disabled")
val.timeStrata <- tclVar("")
entry.timeStrata <-tkentry(frameTOP,width="20",textvariable=val.timeStrata,state="disabled")


val.techStrata <- tclVar("")
entry.techStrata <-tkentry(frameTOP,width="20",textvariable=val.techStrata,state="disabled")


val.spaceStrata <- tclVar("")
entry.spaceStrata <-tkentry(frameTOP,width="20",textvariable=val.spaceStrata,state="disabled")


val.sex <- tclVar("")
entry.sex <-tkentry(frameF,width="6",textvariable=val.sex,state="normal")


# methodDesc	
	# eval(as.name(out))@catchCat
# # val.param <- tclVar("")
# entry.param <-tkentry(frameTOP,width="20",textvariable=val.param)

#les combo

# listtype<-c("p", "fixedK", "propK", "agesK","direct")
# combo.type <- tkwidget(frameF,"ComboBox",editable=FALSE,values=listtype,state="normal")
listbootMethod<-c("samples","otoliths")
combo.bootMethod <- tkwidget(frameF,"ComboBox",editable=FALSE,values=listbootMethod,state="normal")

#les spin
spin.p1<-tdspinner(framespin,from=0,to=1,inc=0.001,width=5,textvariable = lechoix.p1,state="normal",command=function(...)proba(1))
spin.p2<-tdspinner(framespin,from=0,to=1,inc=0.001,width=5,textvariable = lechoix.p2,state="disabled",command=function(...)proba(2))
spin.p<-tdspinner(frameF,from=1,to=100,inc=1,width=5,textvariable = lechoix.p,state="normal")


# les radio
# tvarcatchCat <- tclVar("NULL")
# rbdis <- tkradiobutton(frameB,command=function()choixrb())
# rblan <- tkradiobutton(frameB,command=function()choixrb())
# tkconfigure(rbdis,variable=tvarcatchCat,value="DIS",state="disabled")
# tkconfigure(rblan,variable=tvarcatchCat,value="LAN",state="disabled")



c("p", "fixedK", "propK", "agesK","direct")
tvartype <- tclVar("NULL")
rbp <-      tkradiobutton(frametype,command=function()choixrb("type"))
rbfixedK <- tkradiobutton(frametype,command=function()choixrb("type"))
rbpropK <-  tkradiobutton(frametype,command=function()choixrb("type"))
rbagesK <-  tkradiobutton(frametype,command=function()choixrb("type"))
rbdirect <- tkradiobutton(frametype,command=function()choixrb("type"))
tkconfigure(rbp,variable=tvartype,value="p",state="disabled")
tkconfigure(rbfixedK,variable=tvartype,value="fixedK",state="disabled")
tkconfigure(rbpropK,variable=tvartype,value="propK",state="disabled")
tkconfigure(rbagesK,variable=tvartype,value="agesK",state="disabled")
tkconfigure(rbdirect,variable=tvartype,value="direct",state="disabled")






tvarmethodDesc <- tclVar("NULL")
rbanal <- tkradiobutton(framemeth,command=function()choixrb("meth"))
rbboot <- tkradiobutton(framemeth,command=function()choixrb("meth"))
tkconfigure(rbanal,variable=tvarmethodDesc,value="analytical",state="normal")
tkconfigure(rbboot,variable=tvarmethodDesc,value="bootstrap",state="normal")
#les bouton


# but.strini <- tkbutton(frameA,text=gettext(domain="R-COSTgui","<select>"),state="active",command=function(...) choixdataset.multi("strIni","but.strini"),foreground="red",relief="ridge")
# but.newstrini <- tkbutton(frameA,text=gettext(domain="R-COSTgui","New strIni type object"),state="active",command=function(...) .cost.strIni())#,foreground="red",relief="ridge")
# but.resetstrini <- tkbutton(frameA,text="reset",state="active",command=function(...) reset.strIni())#,foreground="red",relief="ridge")
valid.but <- tkbutton(frameD,text=gettext(domain="R-COSTgui","Validate"),state="disabled",command=function(...) valid())

but.csObject <- tkbutton(frameBONUS,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset.multi(c("csDataCons"),"but.csObject"),foreground="red",relief="ridge")
but.clObject <- tkbutton(frameBONUS,text=gettext(domain="R-COSTgui","<dataset>"),state="disabled",command=function(...) choixdataset.multi(c("clDataCons"),"but.clObject"),foreground="red",relief="ridge")

but.dbeOutput <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset.multi(c("dbeOutput"),"but.dbeOutput"),foreground="red",relief="ridge")

# les check

cbValue.incl.precision<-tclVar("1")
cb.incl.precision <- tkcheckbutton(frameF,variable=cbValue.incl.precision,state="normal",command=function()checkind())

cbValue.fillGaps<-tclVar("1")
cb.fillGaps <- tkcheckbutton(frameF,variable=cbValue.fillGaps,state="normal",command=function()checkfill())

cbValue.trace<-tclVar("0")
cb.trace <- tkcheckbutton(frameF,variable=cbValue.trace,state="normal")


#text

text.valid<-tklabel(frameD,text=gettext(domain="R-COSTgui","out :"))
#list

# list.species <-tklistbox(frameE,selectmode="unique",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.species.scroll,...))
# list.species.scroll<-tkscrollbar(frameE,repeatinterval=5,command=function(...)tkyview(list.species,...))

tkgrid(frameBONUS,row=1,column=2,columnspan=2)
tkgrid(frameTOP,row=2,column=1,columnspan=2,sticky="ns")
tkgrid(frameF,row=2,column=3,columnspan=2,sticky="ns")



# tkgrid(frameA)
# tkgrid(frameB)
# tkgrid(frameC)
# tkgrid(frameF)

tkgrid(frameD,row=4,column=2,columnspan=2)

tkgrid(tklabel(frameBONUS,text=gettext(domain="R-COSTgui","csDataCons:"),font=font2),but.csObject)
labelCL<-tklabel(frameBONUS,text=gettext(domain="R-COSTgui","clDataCons:"),font=font1)
tkgrid(labelCL,but.clObject)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","dbeOutput:"),font=font2),but.dbeOutput)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Description : ")),entry.desc)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Catch category : ")),entry.catchCat)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Method description : ")),entry.methodDesc)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Species : ")),entry.species)


tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Parameter : ")),entry.param)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Technical strata : ")),entry.techStrata)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Temporal strata : ")),entry.timeStrata)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Space strata : ")),entry.spaceStrata)
# tkgrid(tklabel(frameE,text=gettext(domain="R-COSTgui","Species"),font=font2))
# tkgrid(list.species,list.species.scroll,sticky="nswe")entry.methodDesc <-tkentry(frameTOP,width="20",textvariable=val.methodDesc,state="disabled")
# val.catchCat <- tclVar("")
# entry.catchCa



# tkgrid(frameE,columnspan=2)#,sticky="ew")

# tkgrid(tklabel(frameA,text=gettext(domain="R-COSTgui","StrIni : "),font=font2),but.strini,but.newstrini,but.resetstrini)
# tkgrid(tklabel(frameB,text="LAN : ",font=font2),rblan,tklabel(frameB,text="DIS : ",font=font2),rbdis)
# tkgrid(tklabel(frameC,text="analytic : ",font=font2),rbanal,tklabel(frameC,text="bootstrap : ",font=font2),rbboot)

tkgrid(frameF)
tkgrid(framemeth,row=1,column=1,columnspan=3)
labelanal<-tklabel(framemeth,text=gettext(domain="R-COSTgui","Analytical : "))
labelboot<-tklabel(framemeth,text=gettext(domain="R-COSTgui","Bootstrap : "))
tkgrid(labelanal,rbanal,labelboot,rbboot)#mettre a row2



tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","Type : ")),row=2,column=1)
tkgrid(frametype,row=2,column=2,columnspan=2)

tkgrid(tklabel(frametype,text=""),tklabel(frametype,text=gettext(domain="R-COSTgui","p : ")),rbp,tklabel(frametype,text=""))
tkgrid(tklabel(frametype,text=gettext(domain="R-COSTgui","Fixed allocation : ")),rbfixedK,tklabel(frametype,text=gettext(domain="R-COSTgui","Proportional allocation : ")),rbpropK)
tkgrid(tklabel(frametype,text=gettext(domain="R-COSTgui","Age sampling : ")),rbagesK ,tklabel(frametype,text=gettext(domain="R-COSTgui","Age and length sampling : ")),rbdirect)



# tkgrid(tklabel(frameF,text="bootMethod : "),row=2,column=1)
# tkgrid(combo.bootMethod,row=2,column=2,columnspan=2)



tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","Fill gaps : ")),row=3,column=1)
tkgrid(cb.fillGaps,row=3,column=2,sticky="w")
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","p : ")),row=4,column=1)
tkgrid(spin.p,row=4,column=2,sticky="w")
# tkgrid(tklabel(frameF,text="trace : "),row=5,column=1)
# tkgrid(cb.trace,row=5,column=2)
# tkgrid(tklabel(frameF,text="incl.precision : "),row=6,column=1)
# tkgrid(cb.incl.precision,row=6,column=2)
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","Probabilities : ")),row=5,column=1)
tkgrid(framespin,row=5,column=2,sticky="w")
tkgrid(spin.p1,spin.p2)


tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","Sex : ")),row=6,column=1)
tkgrid(entry.sex,row=6,column=2,sticky="w")


tkgrid(text.valid,entry.out,valid.but)


tkfocus(tt)

}
.cost.RaiseLength <-
function(){




try(require(tcltk))
try(require(tcltk2))
try(library(Rcmdr))
try(require(tkrplot))
try(tclRequire("BWidget"))
try(library(COSTcore))
try(library(COSTeda));try(library(COSTgui));try(library(COSTdbe));try(library(COSTdbe))



tdspinner <- function(parent, ...) {tkwidget(parent, "spinbox", ...)}


valid<-function(){

flag<-0
# pour tous
if (length(as.character(tkcurselection(list.species)))==0){
Message(message = gettext(domain="R-COSTgui","You have to specify a species"), type = "error")
flag<-1
}
if (tclvalue(lechoix.but.strini)==""){
Message(message = gettext(domain="R-COSTgui","You have to specify a strini"), type = "error")
flag<-1
}


# si sex coché
if ( tclvalue(cbValue.sex)=="1"){
if (length(as.character(tkcurselection(list.sex)))==0){
Message(message = gettext(domain="R-COSTgui","You have to specify a sex"), type = "error")
flag<-1
}
}



# si CL donc si popu

if (tclvalue(tvarelev)=="popul"){
if (tclvalue(lechoix.but.clObject)==""){
Message(message = gettext(domain="R-COSTgui","You have to specify a clDataCons object"), type = "error")
flag<-1
}


if (length(as.character(tkcurselection(list.taxon)))==0){
Message(message = gettext(domain="R-COSTgui","You have to specify a taxon"), type = "error")
flag<-1
}

}

#si number faut tclvalue(tvarmethodDesc)

if ( tclvalue(tvarparam)=="number@length"){
if ( tclvalue(tvarmethodDesc)=="NULL"){

Message(message = gettext(domain="R-COSTgui","You have to specify bootstrap or analytical"), type = "error")

flag<-1
}


}

print('fin des test')
print(flag)
if (flag==0){



# si aux marée : on utilise pas le CL, et on laisse la possibilité de choisir DIS
# si a la popultion , on met le CL, et pas de DIS

# qd on fait weight, (totu sauf number en fait) on fait bpboot : et tout ca c 'est du boostrap:
# number analyticals -> raiselgth
# number boot -> raiselegthboot
# les autre -> bpboo


if ( tclvalue(tvarparam)=="number@length"){

if (tclvalue(tvarmethodDesc)=="bootstrap"){


cmd1<-paste(tclvalue(val.out),"<-dbeObject(desc='",tclvalue(val.desc),
"',species='",paste(as.character(tkget(list.species,as.character(tkcurselection(list.species)))),collapse=" "),"',strataDesc=",tclvalue(lechoix.but.strini),",param='",tclvalue(tvarparam),"',catchCat='",tclvalue(tvarcatchCat),"',methodDesc='",tclvalue(tvarmethodDesc),"')",sep="")
print(cmd1)
doItAndPrint(cmd1)



cmd2<-paste(tclvalue(val.out),"<-RaiseLgthBoot(
dbeOutput=",tclvalue(val.out),
",spp='",paste(as.character(tkget(list.species,as.character(tkcurselection(list.species)))),collapse=" "),
"',taxon='",paste(as.character(tkget(list.taxon,as.character(tkcurselection(list.taxon)))),collapse=" "),
"',csObject=",tclvalue(lechoix.but.csObject),
",B=",tclvalue(lechoix.B),
sep="")
if(tclvalue(lechoix.but.clObject)!="" & tclvalue(tvarelev)=="popul"){cmd2<-paste(cmd2,",clObject=",tclvalue(lechoix.but.clObject),sep="")}
if(tclvalue(cbValue.sex)=="1"){cmd2<-paste(cmd2,",sex=",paste(as.character(tkget(list.sex,as.character(tkcurselection(list.sex)))),collapse=" "),sep="")}



as.logical(as.numeric(tclvalue(cbValue.sampPar)))
as.logical(as.numeric(tclvalue(cbValue.incl.precision)))

cmd2<-paste(cmd2,",sampPar=",as.logical(as.numeric(tclvalue(cbValue.sampPar))),sep="")
# cmd2<-paste(cmd2,",incl.precision=",as.logical(as.numeric(tclvalue(cbValue.incl.precision))),sep="") # fait tout planter.. sans raison!

# ,",incl.precision=",as.logical(as.numeric(tclvalue(cbValue.incl.precision)))


if (tclvalue(lechoix.p1)!="0.025" & tclvalue(cbValue.incl.precision) !="0"){
tclvalue(lechoix.p2)<- round(as.numeric(1-as.numeric(tclvalue(lechoix.p1))),3)
cmd2<-paste(cmd2,paste(",probs=c(",tclvalue(lechoix.p1),",",tclvalue(lechoix.p2),")",sep=""),sep="")
}

cmd2<-paste(cmd2,")",sep="")
print(cmd2)
# print("la")
# print(is(cmd1))
# print(is(cmd2))
# save(cmd2,cmd1,file="azert.Rdata")
doItAndPrint(cmd2)

# print("fin")





}



if (tclvalue(tvarmethodDesc)=="analytical"){




cmd1<-paste(tclvalue(val.out),"<-dbeObject(desc='",tclvalue(val.desc),
"',species='",paste(as.character(tkget(list.species,as.character(tkcurselection(list.species)))),collapse=" "),"',strataDesc=",tclvalue(lechoix.but.strini),",param='",tclvalue(tvarparam),"',catchCat='",tclvalue(tvarcatchCat),"',methodDesc='",tclvalue(tvarmethodDesc),"')",sep="")
print(cmd1)
doItAndPrint(cmd1)



cmd2<-paste(tclvalue(val.out),"<-RaiseLgth(dbeOutput=",tclvalue(val.out),
",spp='",paste(as.character(tkget(list.species,as.character(tkcurselection(list.species)))),collapse=" "),"'",sep="")

if ( length(as.character(tkcurselection(list.taxon))) !=0){
cmd2<-paste(cmd2,",taxon='",paste(as.character(tkget(list.taxon,as.character(tkcurselection(list.taxon)))),collapse=" "),"'",sep="")
}

cmd2<-paste(cmd2,",csObject=",tclvalue(lechoix.but.csObject),sep="")

if(tclvalue(lechoix.but.clObject)!="" & tclvalue(tvarelev)=="popul"){

cmd2<-paste(cmd2,",clObject=",tclvalue(lechoix.but.clObject),sep="")

#ptet ici rajouter taxon... car ya que CL qui le contient isn't it?
}



if(tclvalue(cbValue.sex)=="1"){cmd2<-paste(cmd2,",sex=",paste(as.character(tkget(list.sex,as.character(tkcurselection(list.sex)))),collapse=" "),sep="")}



as.logical(as.numeric(tclvalue(cbValue.sampPar)))
as.logical(as.numeric(tclvalue(cbValue.incl.precision)))

cmd2<-paste(cmd2,",sampPar=",as.logical(as.numeric(tclvalue(cbValue.sampPar))),sep="")
# cmd2<-paste(cmd2,",incl.precision=",as.logical(as.numeric(tclvalue(cbValue.incl.precision))),sep="") # fait tout planter.. sans raison!

# ,",incl.precision=",as.logical(as.numeric(tclvalue(cbValue.incl.precision)))


if (tclvalue(lechoix.p1)!="0.025" & tclvalue(cbValue.incl.precision) !="0"){
tclvalue(lechoix.p2)<- round(as.numeric(1-as.numeric(tclvalue(lechoix.p1))),3)
cmd2<-paste(cmd2,paste(",probs=c(",tclvalue(lechoix.p1),",",tclvalue(lechoix.p2),")",sep=""),sep="")
}

cmd2<-paste(cmd2,")",sep="")
print(cmd2)
# print("la")
# print(is(cmd1))
# print(is(cmd2))
# save(cmd2,cmd1,file="azert.Rdata")
doItAndPrint(cmd2)

# print("fin")


















}


}


if ( tclvalue(tvarparam)!="number@length"){




cmd1<-paste(tclvalue(val.out),"<-dbeObject(desc='",tclvalue(val.desc),
",nboot=",tclvalue(lechoix.B),
"',species='",paste(as.character(tkget(list.species,as.character(tkcurselection(list.species)))),collapse=" "),"',strataDesc=",tclvalue(lechoix.but.strini),",param='",tclvalue(tvarparam),"',catchCat='",tclvalue(tvarcatchCat),"',methodDesc='",tclvalue(tvarmethodDesc),"')",sep="")
print(cmd1)
doItAndPrint(cmd1)


#pas de taxon pour bpboot ni de sex

#"',taxon='",paste(as.character(tkget(list.taxon,as.character(tkcurselection(list.taxon)))),collapse=" "),

cmd2<-paste(tclvalue(val.out),"<-bpBoot(dbeOutput=",tclvalue(val.out),
",spp='",paste(as.character(tkget(list.species,as.character(tkcurselection(list.species)))),collapse=" "),

"',object=",tclvalue(lechoix.but.csObject),
",nboot=",tclvalue(lechoix.B),
sep="")
# if(tclvalue(lechoix.but.clObject)!="" & tclvalue(tvarelev)=="popul"){cmd2<-paste(cmd2,",clObject=",tclvalue(lechoix.but.clObject),sep="")}
# if(tclvalue(cbValue.sex)=="1"){cmd2<-paste(cmd2,",sex=",paste(as.character(tkget(list.sex,as.character(tkcurselection(list.sex)))),collapse=" "),sep="")}


#si au maree adjust=T sitabCA False


cmd2<-paste(cmd2,",adjust=",(tclvalue(tvarelev)=="maree"),sep="")



cmd2<-paste(cmd2,",sample.boot=",as.logical(as.numeric(tclvalue(cbValue.sampPar))),sep="")

# cmd2<-paste(cmd2,",incl.precision=",as.logical(as.numeric(tclvalue(cbValue.incl.precision))),sep="") # fait tout planter.. sans raison!

# ,",incl.precision=",as.logical(as.numeric(tclvalue(cbValue.incl.precision)))



if (tclvalue(lechoix.p1)!="0.025" & tclvalue(cbValue.incl.precision) !="0"){
tclvalue(lechoix.p2)<- round(as.numeric(1-as.numeric(tclvalue(lechoix.p1))),3)
cmd2<-paste(cmd2,paste(",probs=c(",tclvalue(lechoix.p1),",",tclvalue(lechoix.p2),")",sep=""),sep="")
}

cmd2<-paste(cmd2,")",sep="")
print(cmd2)
# print("la")
# print(is(cmd1))
# print(is(cmd2))
# save(cmd2,cmd1,file="azert.Rdata")
doItAndPrint(cmd2)

# print("fin")









}


}


}


slotNamesdata<-function(data){
ploom<-slotNames(data)
out<-c()
for (p in ploom){
if(is(slot(data, p))[1] == "data.frame"){
out<-c(out,p)
}
}

return(out)
}



choixdataset.multi<-function(id,nom.but){
print(id)
try(tkdestroy(tt2),silent=T)
tt2<-tktoplevel()
tkwm.geometry(tt2, "-100+50")     
listdata <-NULL
for ( i in id){listdata <- c(listdata,malist(i))}
dataSetsBox <- variableListBox(tt2, listdata, title=gettext(domain="R-COSTgui","Select one dataset"))# voir initialselection
tkgrid(getFrame(dataSetsBox), sticky="nwes",column=1,row=1)
  tkgrid.rowconfigure(getFrame(dataSetsBox), 1, weight = 1)
  tkgrid.rowconfigure(tt2, 1, weight = 1)  
onOK <- function(){	
out<-getSelection(dataSetsBox)

		if ( length(out) != 0){
		eval(parse(text=paste("tclvalue(lechoix.",nom.but,")<-out",sep="")))
		tkconfigure(eval(as.name(nom.but)),text=out)
		# print(GetIt((eval(as.name(paste("lechoix.",nom.but,sep=""))))))
		# tkconfigure( names.list.quali, state="disabled" )	
		# tkconfigure( names.list.quanti, state="disabled" )
		
		

		# print("on met a jour la liste des variables")

		# a ce niveau on a le nom du cs objet, on va lister toute les variaible exploitable pour le spaceplot
	if (nom.but == "but.csObject"){
	tclvalue(val.desc)<-eval(as.name(out))@desc
  listslot<-slotNamesdata(eval(as.name(out)))
araj<-c()
for ( i in 1:length(listslot)){	
truc<-do.call("slot",args =list(as.name(out),listslot[i]))
araj<-unique(c(araj,levels(as.factor(truc[["spp"]]))));araj
}
tkdelete(list.species,"0","end");
for (var in araj) {tkinsert(list.species, "end", var)}


# on rajoute les sexes

araj<-c()
  truc<-do.call("slot",args =list(as.name(out),"sl"))
araj<-unique(c(araj,levels(as.factor(truc[["sex"]]))));araj
  tkconfigure(list.sex,state="normal")
tkdelete(list.sex,"0","end");
for (var in araj) {tkinsert(list.sex, "end", var)}
#si c'est pas coché on va redescactiver

tkconfigure(list.sex,state=c("disabled","normal")[as.numeric(tclvalue(cbValue.sex))+1])




# on check les LAN et DIS

	LD<-unique(substr(eval(as.name(out))@hl$sort,1,3))
	print(LD)
if(!is.element("DIS",LD)){
print("pas de DIS")
tkconfigure(rbdis,state="disabled")
tclvalue(tvarcatchCat)<-"LAN"

}
if(!is.element("LAN",LD)){
print("pas de LAN")
tkconfigure(rblan,state="disabled")
tclvalue(tvarcatchCat)<-"DIS"
}

		}
	
	
	
	if (nom.but == "but.clObject"){
	# tclvalue(val.desc)<-eval(as.name(out))@desc
  # listslot<-slotNamesdata(eval(as.name(out)))

  araj<-c()
  # cl$taxon
  
  truc<-do.call("slot",args =list(as.name(out),"cl"))
araj<-unique(c(araj,levels(as.factor(truc[["taxon"]]))));araj
  
tkdelete(list.taxon,"0","end");
for (var in araj) {tkinsert(list.taxon, "end", var)}

# on check les LAN et DIS

	# # LD<-unique(substr(eval(as.name(out))@hl$sort,1,3))
	# # print(LD)
# # if(!is.element("DIS",LD)){
# # print("pas de DIS")
# # tkconfigure(rbdis,state="disabled")
# # tclvalue(tvarcatchCat)<-"LAN"

# # }
# # if(!is.element("LAN",LD)){
# # print("pas de LAN")
# # tkconfigure(rblan,state="disabled")
# # tclvalue(tvarcatchCat)<-"DIS"
# # }

		}	
	
		
# aactiver<-list(entry.ylab,entry.xlab,list.time,list.tech,list.space,but.refr,entry.fonction,entry.main,cb.simu,cb.auto,list.time,list.tech,spin.x1,spin.x2,spin.y1,spin.y2,rbsel.image,rbsel.contour,rbsel.bubble,rbsel.value)
# # for ( i in 1:length(aactiver)){tkconfigure(aactiver[[i]],state="normal")}

# lapply(aactiver,tkconfigure,state="normal")

# choixtime<-c("year","semester","quarter","month")
# choixspace<-c("rect","subrect","area")
# choixtech<-c("commCat","commCatScl", "foCatEu5", "foCatEu6", "foCatNat","catchCat")
# tkdelete(list.time,"0","end");for (var in choixtime) {tkinsert(list.time, "end", var)}
# tkdelete(list.tech,"0","end");for (var in choixtech) {tkinsert(list.tech, "end", var)}
# tkdelete(list.space,"0","end");for (var in choixspace) {tkinsert(list.space, "end", var)}

# on regarde s'il faut activer le bouton valider
if ( tclvalue(tvarparam) !="t" & tclvalue(lechoix.but.csObject)!=""
& tclvalue(lechoix.but.strini)!=""){
tkconfigure(valid.but,state="normal")
}


	
		
}
tkdestroy(tt2)	
		
}
	
OK.but <-tkbutton(tt2,text=gettext(domain="R-COSTgui","   OK   "),command=onOK)
tkgrid(OK.but,column=1,row=2)
tkfocus(tt2)
}

#fonctions pratique pour tk
malist<-function (pat) {

# retrouve dans ls les element de type pat
    Vars <- ls(all.names = TRUE,env=.GlobalEnv)
	# print(Vars)
    if (length(Vars) == 0) {return(Vars)}
    names(which(sapply(Vars, function(.x) {	is(get(.x))[1]==pat	}	)	)	)
	}
	
getcontentlistbox<-function(listbox){
out<-c()
i<-0		
while (length(as.character(tkget(listbox,i)))!=0){
out<-c(out,as.character(tkget(listbox,i)))
i<-i+1
}
return(out)
}

GetIt<-function(objet){
if (attr(objet,"class")=="tkwin"){
		if (as.character(tkwinfo("class",objet))=="Listbox"){
		return(getcontentlistbox(objet))
		}
}
if (attr(objet,"class")=="tclVar"){
return(tclvalue(objet))

}
}

DoItmini<-function(comm,env=.GlobalEnv){
  result <- parse(text = paste(comm))
  exprs<-result
  i<-1
  #for (i in seq_along(exprs)) {
  ei <- exprs[i]
  print(ei)
  result <- eval(ei, envir = env)
  
}

# reset.strIni<-function(){
# tkconfigure(but.strini,text=gettext(domain="R-COSTgui","<select>"))
# tclvalue(lechoix.but.strini)<-""
# }

choixrb<-function(id){




if ( id == "elev"){

if (tclvalue(tvarelev)=="maree"){
tkconfigure(rbdis,state="normal")
tkconfigure(labelcl,font=font1)
tkconfigure(but.clObject,state="disabled")

}

if (tclvalue(tvarelev)=="popul"){
tkconfigure(rbdis,state="disabled")
tclvalue(tvarcatchCat)<-"LAN"
tkconfigure(labelcl,font=font2)
tkconfigure(but.clObject,state="normal")

}



}

if ( id =="param"){

if( tclvalue(tvarparam) == "number@length"){

tkconfigure(rbanal,state="normal")
tkconfigure(rbboot,state="normal")
tkconfigure(labelanal,font=font2)
tkconfigure(labelboot,font=font2)

tkconfigure(rbmar,state="normal")
tkconfigure(rbpop,state="normal")
tkconfigure(rbtabCA,state="disabled")
#gerer la position
if ( tclvalue(tvarelev)=="tabCA"){
tclvalue(tvarelev)<-"maree"
}

}

if( tclvalue(tvarparam) != "number@length"){
tkconfigure(rbanal,state="disabled")
tkconfigure(rbboot,state="disabled")
tclvalue(tvarmethodDesc)<-"bootstrap"
tkconfigure(labelanal,font=font1)
tkconfigure(labelboot,font=font1)

tkconfigure(rbmar,state="normal")
tkconfigure(rbpop,state="disabled")
tkconfigure(rbtabCA,state="normal")
if ( tclvalue(tvarelev)=="popul"){
tclvalue(tvarelev)<-"maree"
}

}

}


# if("NULL" != tclvalue(tvarmethodDesc) & "NULL" != tclvalue(tvarcatchCat)){

# tkconfigure(valid.but,state="active")

# }
if ( tclvalue(tvarparam) !="t" & tclvalue(lechoix.but.csObject)!=""
& tclvalue(lechoix.but.strini)!=""){
tkconfigure(valid.but,state="normal")
}



#o cntrole le fait que si number et boot alors il FAUT CL et donc popul
if (tclvalue(tvarparam)=="number@length" & tclvalue(tvarmethodDesc)=="bootstrap" ){
tclvalue(tvarelev)<-"popul"
tkconfigure(rbdis,state="disabled")
tclvalue(tvarcatchCat)<-"LAN"
tkconfigure(labelcl,font=font2)
tkconfigure(but.clObject,state="normal")
tkconfigure(rbmar,state="disabled")
}


if (!(tclvalue(tvarparam)=="number@length" & tclvalue(tvarmethodDesc)=="bootstrap") ){
# tclvalue(tvarelev)<-"popul"
tkconfigure(rbmar,state="normal")

}


# si pas number... on fait bpboot et donc pas de CL
if (tclvalue(tvarparam)!="number@length"){

# tkconfigure(rbdis,state="disabled")
# tclvalue(tvarcatchCat)<-"LAN"
tkconfigure(labelcl,font=font1)
tkconfigure(but.clObject,state="disabled")


}

}

proba<-function(val){
tclvalue(lechoix.p2)<- round(as.numeric(1-as.numeric(tclvalue(lechoix.p1))),3)
}

checkind<-function(){
tkconfigure(spin.p1,state=c("disabled","normal")[as.numeric(tclvalue(cbValue.incl.precision))+1])}

lechoix.but.csObject <- tclVar("")
lechoix.but.clObject <- tclVar("")
lechoix.but.strini <- tclVar("")
lechoix.p1 <- tclVar(0.025)
lechoix.p2 <- tclVar(0.975)
lechoix.B <- tclVar(3)

tt <- tktoplevel(borderwidth=10)
tkwm.geometry(tt, "-100+50")
font2<-tkfont.create(family="Times",size=12,weight="bold")
font1<-font3<-tkfont.create(family="Times",size=12)
fontheading<-tkfont.create(family="Times",size=18,weight="bold")  
tkwm.title(tt,gettext(domain="R-COSTgui","COST - Raise parameters at length"))
tkgrid(tklabel(tt, text=gettext(domain="R-COSTgui","Raise parameters at length"),font=fontheading))
tkgrid(tklabel(tt,text="")) # Ligne de blanc
frameTOP<- tkframe(tt, borderwidth=2, relief="groove")
frameBONUS<- tkframe(tt, borderwidth=2, relief="groove")
framecont2<- tkframe(tt, borderwidth=0, relief="groove")
framecont<- tkframe(framecont2, borderwidth=0, relief="groove")
# frameA<- tkframe(tt, borderwidth=2, relief="groove")
frameB<- tkframe(framecont, borderwidth=2, relief="groove")
frameB2<- tkframe(framecont2, borderwidth=2, relief="groove")

frameC<- tkframe(framecont, borderwidth=2, relief="groove")
frameC2<- tkframe(framecont, borderwidth=2, relief="groove")
frameD<- tkframe(tt, borderwidth=2, relief="groove")
frameE<- tkframe(frameTOP, borderwidth=2, relief="groove")
frameE2<- tkframe(frameTOP, borderwidth=2, relief="groove")
frameE3<- tkframe(frameTOP, borderwidth=2, relief="groove")
frameE32<- tkframe(frameE3, borderwidth=0, relief="groove")
frameF<- tkframe(tt, borderwidth=2, relief="groove")



# les entry
val.desc <- tclVar("")
entry.desc <-tkentry(frameTOP,width="20",textvariable=val.desc)
val.species <- tclVar("")
entry.species <-tkentry(frameTOP,width="20",textvariable=val.species)
val.out <- tclVar("out_dbe")
entry.out <-tkentry(frameD,width="20",textvariable=val.out)
# val.param <- tclVar("")
# entry.param <-tkentry(frameTOP,width="20",textvariable=val.param)

#les combo

# listparam<-c("number@length","weight@length", "maturity@length", "sexratio@length")
# combo.param <- tkwidget(frameTOP,"ComboBox",editable=FALSE,values=listparam,state="normal")

#les spin
spin.p1<-tdspinner(frameF,from=0,to=1,inc=0.001,width=5,textvariable = lechoix.p1,state="normal",command=function(...)proba(1))
spin.p2<-tdspinner(frameF,from=0,to=1,inc=0.001,width=5,textvariable = lechoix.p2,state="disabled",command=function(...)proba(2))
spin.B<-tdspinner(frameF,from=1,to=10000,inc=1,width=5,textvariable = lechoix.B,state="normal")

# les radio


tvarparam <- tclVar("t")
rbnumber <- tkradiobutton(frameB2,command=function()choixrb("param"))
rbweight <- tkradiobutton(frameB2,command=function()choixrb("param"))
rbmaturity <- tkradiobutton(frameB2,command=function()choixrb("param"))
rbsexratio <- tkradiobutton(frameB2,command=function()choixrb("param"))




tkconfigure(rbnumber,variable=tvarparam,value="number@length",state="normal")
tkconfigure(rbweight,variable=tvarparam,value="weight@length",state="normal")
tkconfigure(rbmaturity,variable=tvarparam,value="maturity@length",state="normal")
tkconfigure(rbsexratio,variable=tvarparam,value="sexratio@length",state="normal")


tvarcatchCat <- tclVar("LAN")
rbdis <- tkradiobutton(frameB,command=function()choixrb("catchCat"))
rblan <- tkradiobutton(frameB,command=function()choixrb("catchCat"))
tkconfigure(rbdis,variable=tvarcatchCat,value="DIS",state="disabled")
tkconfigure(rblan,variable=tvarcatchCat,value="LAN",state="normal")


tvarmethodDesc <- tclVar("NULL")
rbanal <- tkradiobutton(frameC,command=function()choixrb("methodDesc"))
rbboot <- tkradiobutton(frameC,command=function()choixrb("methodDesc"))
tkconfigure(rbanal,variable=tvarmethodDesc,value="analytical",state="disabled")
tkconfigure(rbboot,variable=tvarmethodDesc,value="bootstrap",state="disabled")

tvarelev <- tclVar("popul")
rbmar <- tkradiobutton(frameC2,command=function()choixrb("elev"))
rbpop <- tkradiobutton(frameC2,command=function()choixrb("elev"))
rbtabCA <- tkradiobutton(frameC2,command=function()choixrb("elev"))
tkconfigure(rbmar,variable=tvarelev,value="maree",state="disabled")
tkconfigure(rbpop,variable=tvarelev,value="popul",state="normal")
tkconfigure(rbtabCA,variable=tvarelev,value="tabCA",state="disabled")

#les bouton


but.strini <- tkbutton(frameBONUS,text=gettext(domain="R-COSTgui","<select>"),state="active",command=function(...) choixdataset.multi("strIni","but.strini"),foreground="red",relief="ridge")
# but.newstrini <- tkbutton(frameA,text=gettext(domain="R-COSTgui","New strIni type object"),state="active",command=function(...) .cost.strIni())#,foreground="red",relief="ridge")
# but.resetstrini <- tkbutton(frameA,text="reset",state="active",command=function(...) reset.strIni())#,foreground="red",relief="ridge")
valid.but <- tkbutton(frameD,text=gettext(domain="R-COSTgui","Validate"),state="disabled",command=function(...) valid())

but.csObject <- tkbutton(frameBONUS,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset.multi(c("csDataCons"),"but.csObject"),foreground="red",relief="ridge")

but.clObject <- tkbutton(frameBONUS,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset.multi(c("clDataCons"),"but.clObject"),foreground="red",relief="ridge")

# les check




cbValue.incl.precision<-tclVar("1")
# cb.incl.precision <- tkcheckbutton(frameF,variable=cbValue.incl.precision,state="normal",command=function()checkind())

cbValue.sampPar<-tclVar("1")
cb.sampPar <- tkcheckbutton(frameF,variable=cbValue.sampPar,state="normal")


# cbValue.sample.boot<-tclVar("1")
# cb.sample.boot <- tkcheckbutton(frameF,variable=cbValue.sample.boot,state="normal")


cbValue.sex<-tclVar("0")
cb.sex <- tkcheckbutton(frameE32,variable=cbValue.sex,state="normal")


#text

text.valid<-tklabel(frameD,text=gettext(domain="R-COSTgui","out :"))
#list

list.species <-tklistbox(frameE,selectmode="unique",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.species.scroll,...))
list.species.scroll<-tkscrollbar(frameE,repeatinterval=5,command=function(...)tkyview(list.species,...))

list.taxon <-tklistbox(frameE2,selectmode="unique",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.taxon.scroll,...))
list.taxon.scroll<-tkscrollbar(frameE2,repeatinterval=5,command=function(...)tkyview(list.taxon,...))

list.sex <-tklistbox(frameE3,selectmode="unique",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.sex.scroll,...),state="disabled")
list.sex.scroll<-tkscrollbar(frameE3,repeatinterval=5,command=function(...)tkyview(list.sex,...))

tkgrid(frameBONUS)
# tkgrid(frameA)
tkgrid(frameTOP)
tkgrid(framecont2)

tkgrid(frameB2,framecont)
tkgrid(frameB)
tkgrid(frameC)
tkgrid(frameC2)
tkgrid(frameF)

tkgrid(frameD)


tkgrid(tklabel(frameB2,text=gettext(domain="R-COSTgui","parameter"),font=font2),columnspan=2)
tkgrid(tklabel(frameB2,text=gettext(domain="R-COSTgui","number@length :")),rbnumber)
tkgrid(tklabel(frameB2,text=gettext(domain="R-COSTgui","weight@length :")),rbweight)
tkgrid(tklabel(frameB2,text=gettext(domain="R-COSTgui","maturity@length :")),rbmaturity)
tkgrid(tklabel(frameB2,text=gettext(domain="R-COSTgui","sexratio@length :")),rbsexratio)




tkgrid(tklabel(frameBONUS,text=gettext(domain="R-COSTgui","csDataCons:"),font=font2),but.csObject)
labelcl<-tklabel(frameBONUS,text=gettext(domain="R-COSTgui","clDataCons:"),font=font2)
tkgrid(labelcl,but.clObject)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Description : ")),entry.desc)
# tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","species : "),font=font2),entry.species)
tkgrid(tklabel(frameE,text=gettext(domain="R-COSTgui","Species"),font=font2))
tkgrid(tklabel(frameE2,text=gettext(domain="R-COSTgui","Taxon"),font=font2))
tkgrid(frameE32)
tkgrid(cb.sex,tklabel(frameE32,text=gettext(domain="R-COSTgui","Sex"),font=font2))
tkgrid(list.species,list.species.scroll,sticky="nswe")
tkgrid(list.taxon,list.taxon.scroll,sticky="nswe")
tkgrid(list.sex,list.sex.scroll,sticky="nswe")




# tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Parameter : ")),list.param)
tkgrid(frameE,frameE2,frameE3)#,sticky="ew")

tkgrid(tklabel(frameBONUS,text=gettext(domain="R-COSTgui","StrIni : "),font=font2),but.strini)
tkgrid(tklabel(frameB,text=gettext(domain="R-COSTgui","Landings : "),font=font2),rblan,tklabel(frameB,text=gettext(domain="R-COSTgui","Discards : "),font=font2),rbdis)
labelanal<-tklabel(frameC,text=gettext(domain="R-COSTgui","Analytical : "),font=font1)
labelboot<-tklabel(frameC,text=gettext(domain="R-COSTgui","Bootstrap : "),font=font1)
tkgrid(labelanal,rbanal,labelboot,rbboot)
tkgrid(tklabel(frameC2,text=gettext(domain="R-COSTgui","To the population : "),font=font2),rbpop)
tkgrid(tklabel(frameC2,text=gettext(domain="R-COSTgui","To the sampled trips : "),font=font2),rbmar)
tkgrid(tklabel(frameC2,text=gettext(domain="R-COSTgui","Empirical CA table : "),font=font2),rbtabCA)

tkgrid(frameF)
# tkgrid(tklabel(frameF,text="sample.boot : "),cb.sample.boot)
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","Including partial species sampling : ")),cb.sampPar)
# tkgrid(tklabel(frameF,text="incl.precision : "),cb.incl.precision)
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","Number of replicates : ")),spin.B)
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","Probabilities : ")),spin.p1,spin.p2)
tkgrid(text.valid,entry.out,valid.but)


tkfocus(tt)

}
.cost.regarderdbe <-
function(){




try(require(tcltk))
try(require(tcltk2))
try(library(Rcmdr))
try(require(tkrplot))
try(tclRequire("BWidget"))
try(library(COSTcore))
try(library(COSTeda));try(library(COSTgui));try(library(COSTdbe));try(library(COSTdbe))



tdspinner <- function(parent, ...) {tkwidget(parent, "spinbox", ...)}


valid<-function(){

yop<-tclvalue(tcl(treeWidget,"selection","get"))

# on vire les accolades
yop<-strsplit(yop,"\\{")
yop<-unlist(strsplit(unlist(yop),"}"))
yop<-strsplit(yop," ")
yop<-yop[yop!=""]

# poru chauqe element de yop , on va virer les ""
yop<-lapply(yop,function(ch){ch[ch!=""]})
yop<-unlist(yop)
print(paste(tclvalue(lechoix.but.dbeOutput),yop,sep=""))
for ( u in paste(tclvalue(lechoix.but.dbeOutput),yop,sep="")){
print(u)
eval(parse(text=paste("print(",u,")",sep="")))
# doItAndPrint(paste("print(",u,")",sep=""),log=F)
doItAndPrint(paste("print(",u,")",sep=""),log=T)



}


}


slotNamesdata<-function(data){
ploom<-slotNames(data)
out<-c()
for (p in ploom){
if(is(slot(data, p))[1] == "data.frame"){
out<-c(out,p)
}
}

return(out)
}



choixdataset.multi<-function(id,nom.but){
print(id)
try(tkdestroy(tt2),silent=T)
tt2<-tktoplevel()
tkwm.geometry(tt2, "-100+50")     
listdata <-NULL
for ( i in id){listdata <- c(listdata,malist(i))}
dataSetsBox <- variableListBox(tt2, listdata, title=gettext(domain="R-COSTgui","Select one dataset"))# voir initialselection
tkgrid(getFrame(dataSetsBox), sticky="nwes",column=1,row=1)
  tkgrid.rowconfigure(getFrame(dataSetsBox), 1, weight = 1)
  tkgrid.rowconfigure(tt2, 1, weight = 1)  
onOK <- function(){	
out<-getSelection(dataSetsBox)

		if ( length(out) != 0){
		eval(parse(text=paste("tclvalue(lechoix.",nom.but,")<-out",sep="")))
		tkconfigure(eval(as.name(nom.but)),text=out)
		# print(GetIt((eval(as.name(paste("lechoix.",nom.but,sep=""))))))
		# tkconfigure( names.list.quali, state="disabled" )	
		# tkconfigure( names.list.quanti, state="disabled" )
		
		

		# print("on met a jour la liste des variables")

		# a ce niveau on a le nom du cs objet, on va lister toute les variaible exploitable pour le spaceplot
	if (nom.but == "but.dbeOutput"){

	
	# on charge la description
	tclvalue(val.desc)<-eval(as.name(out))@desc
	tclvalue(val.param)<-eval(as.name(out))@param
	# tclvalue(val.param)<-
	tclvalue(val.methodDesc)<-eval(as.name(out))@methodDesc	
	tclvalue(val.catchCat)<-eval(as.name(out))@catchCat
	tclvalue(val.species)<-eval(as.name(out))@species
	tclvalue(val.timeStrata)<-eval(as.name(out))@strataDesc@timeStrata
	tclvalue(val.spaceStrata)<-eval(as.name(out))@strataDesc@spaceStrata
	tclvalue(val.techStrata)<-eval(as.name(out))@strataDesc@techStrata
	
	
	
	# " on chage l'arbre
	
	
remplitree(out)


	

	
	

		}
	
		
		
		
# if ( 		tclvalue(lechoix.but.csObject)!= "" & tclvalue(lechoix.but.dbeOutput)!= ""){

tkconfigure(valid.but,state="normal")

# }
		
# aactiver<-list(entry.ylab,entry.xlab,list.time,list.tech,list.space,but.refr,entry.fonction,entry.main,cb.simu,cb.auto,list.time,list.tech,spin.x1,spin.x2,spin.y1,spin.y2,rbsel.image,rbsel.contour,rbsel.bubble,rbsel.value)
# # for ( i in 1:length(aactiver)){tkconfigure(aactiver[[i]],state="normal")}

# lapply(aactiver,tkconfigure,state="normal")

# choixtime<-c("year","semester","quarter","month")
# choixspace<-c("rect","subrect","area")
# choixtech<-c("commCat","commCatScl", "foCatEu5", "foCatEu6", "foCatNat","catchCat")
# tkdelete(list.time,"0","end");for (var in choixtime) {tkinsert(list.time, "end", var)}
# tkdelete(list.tech,"0","end");for (var in choixtech) {tkinsert(list.tech, "end", var)}
# tkdelete(list.space,"0","end");for (var in choixspace) {tkinsert(list.space, "end", var)}


	
		
}
tkdestroy(tt2)	
		
}
	
OK.but <-tkbutton(tt2,text=gettext(domain="R-COSTgui","   OK   "),command=onOK)
tkgrid(OK.but,column=1,row=2)
tkfocus(tt2)
}

#fonctions pratique pour tk
malist<-function (pat) {

# retrouve dans ls les element de type pat
    Vars <- ls(all.names = TRUE,env=.GlobalEnv)
	# print(Vars)
    if (length(Vars) == 0) {return(Vars)}
    names(which(sapply(Vars, function(.x) {	is(get(.x))[1]==pat	}	)	)	)
	}
	
getcontentlistbox<-function(listbox){
out<-c()
i<-0		
while (length(as.character(tkget(listbox,i)))!=0){
out<-c(out,as.character(tkget(listbox,i)))
i<-i+1
}
return(out)
}

GetIt<-function(objet){
if (attr(objet,"class")=="tkwin"){
		if (as.character(tkwinfo("class",objet))=="Listbox"){
		return(getcontentlistbox(objet))
		}
}
if (attr(objet,"class")=="tclVar"){
return(tclvalue(objet))

}
}

DoItmini<-function(comm,env=.GlobalEnv){
  result <- parse(text = paste(comm))
  exprs<-result
  i<-1
  #for (i in seq_along(exprs)) {
  ei <- exprs[i]
  print(ei)
  result <- eval(ei, envir = env)
  
}

reset.strIni<-function(){
tkconfigure(but.strini,text=gettext(domain="R-COSTgui","<select>"))
tclvalue(lechoix.but.strini)<-""
}

choixrb<-function(){
if("NULL" != tclvalue(tvarmethodDesc) & "NULL" != tclvalue(tvarcatchCat)){

tkconfigure(valid.but,state="active")

}

}

proba<-function(val){
tclvalue(lechoix.p2)<- round(as.numeric(1-as.numeric(tclvalue(lechoix.p1))),3)
}

checkind<-function(){
tkconfigure(spin.p1,state=c("disabled","normal")[as.numeric(tclvalue(cbValue.incl.precision))+1])}

checkfill<-function(){

tkconfigure(spin.p,state=c("disabled","normal")[as.numeric(tclvalue(cbValue.fillGaps))+1])}


remplitree<-function(aze){

options(warn=-1)


cestvide<-function(truc){
# print("xxxx")
# print(truc)
# print(is(truc))
if(is(truc)[1]=="data.frame" & sum(!is.na(truc))==0){return(T)}
if(is(truc)[1]=="numeric" & is.na(truc)){return(T)}
if(is(truc)[1]=="character" & is.na(truc)){return(T)}
if(is(truc)[1]=="factor" & is.na(truc)){return(T)}
if(is(truc)[1]=="strIni"){return(T)}
if(is(truc)[1]=="list"){
if (sum(!unlist(lapply(truc,cestvide)))==0){return(T)}
}

# print("petit soucis")
# print(is(truc))
return(F)
}

# print("coucou")
dbeO<-eval(as.name(aze))


tclvalue(cbValue.masq)

for ( i in slotNames(dbeO)){
 # print("i")
 # print(i)
tkdelete(treeWidget,paste("@",i,sep=""))

#pou chaque i on veut savoir si TOUT est vide
# si c 'est un jeu de donne on dit si que des NA

# faut une fonction, qui prend un truc qui dit si vide.
# puis on fait pour tout




# cestvide(truc)


if ( 
!cestvide(
do.call("slot",args =list(dbeO,i))
)
 | tclvalue(cbValue.masq)=="0"
)
{
tkinsert(treeWidget,"end","root",paste("@",i,sep=""),text=i)



for ( j in names(do.call("slot",args =list(dbeO,i))) ){
 # print("j")
 # print(j)
 tkdelete(treeWidget,paste(paste("@",i,sep=""),j,sep="$"))
if ( !cestvide(do.call("slot",args =list(dbeO,i))[[j]]) | tclvalue(cbValue.masq)=="0"){
tkinsert(treeWidget,"end",paste("@",i,sep=""),paste(paste("@",i,sep=""),j,sep="$"),text=j)
}
}
}

}
options(warn=1)
}

# lechoix.but.csObject <- tclVar("")
lechoix.but.dbeOutput <- tclVar("")
# lechoix.but.strini <- tclVar("")
# lechoix.p1 <- tclVar(0.025)
# lechoix.p2 <- tclVar(0.975)
# lechoix.p <- tclVar(10) # a voir si c 'est bien par defaut

tt <- tktoplevel(borderwidth=10)
tkwm.geometry(tt, "-100+50")
font2<-tkfont.create(family="Times",size=12,weight="bold")
font3<-tkfont.create(family="Times",size=12)
fontheading<-tkfont.create(family="Times",size=18,weight="bold")  
tkwm.title(tt,gettext(domain="R-COSTgui","COST - View dbeObjetc"))
tkgrid(tklabel(tt, text=gettext(domain="R-COSTgui","View dbeObjetc"),font=fontheading))
tkgrid(tklabel(tt,text="")) # Ligne de blanc
frameTOP<- tkframe(tt, borderwidth=2, relief="groove")
frameBONUS<- tkframe(tt, borderwidth=2, relief="groove")
frameA<- tkframe(tt, borderwidth=2, relief="groove")
framemasq<- tkframe(tt, borderwidth=2, relief="groove")
# frameB<- tkframe(tt, borderwidth=2, relief="groove")
# frameC<- tkframe(tt, borderwidth=2, relief="groove")
# frameD<- tkframe(tt, borderwidth=2, relief="groove")
# frameE<- tkframe(frameTOP, borderwidth=2, relief="groove")
# frameF<- tkframe(tt, borderwidth=2, relief="groove")

#tree


xScr       <- tkscrollbar(frameA,command=function(...)tkxview(treeWidget,...),orient="horizontal")
yScr       <- tkscrollbar(frameA,command=function(...)tkyview(treeWidget,...))
treeWidget <- tkwidget(frameA,"Tree",xscrollcommand=function(...)tkset(xScr,...),
                                 yscrollcommand=function(...)tkset(yScr,...),width=30,height=15)

	

# les entry
val.desc <- tclVar("")
entry.desc <-tkentry(frameTOP,width="20",textvariable=val.desc,state="disabled")
val.species <- tclVar("")
entry.species <-tkentry(frameTOP,width="20",textvariable=val.species,state="disabled")
# val.out <- tclVar("age_dbe.boot")
# entry.out <-tkentry(frameD,width="20",textvariable=val.out)
val.param <- tclVar("")
entry.param <-tkentry(frameTOP,width="20",textvariable=val.param,state="disabled")
val.methodDesc <- tclVar("")
entry.methodDesc <-tkentry(frameTOP,width="20",textvariable=val.methodDesc,state="disabled")
val.catchCat <- tclVar("")
entry.catchCat <-tkentry(frameTOP,width="20",textvariable=val.catchCat,state="disabled")
val.timeStrata <- tclVar("")
entry.timeStrata <-tkentry(frameTOP,width="20",textvariable=val.timeStrata,state="disabled")


val.techStrata <- tclVar("")
entry.techStrata <-tkentry(frameTOP,width="20",textvariable=val.techStrata,state="disabled")


val.spaceStrata <- tclVar("")
entry.spaceStrata <-tkentry(frameTOP,width="20",textvariable=val.spaceStrata,state="disabled")


# methodDesc	
	# eval(as.name(out))@catchCat
# # val.param <- tclVar("")
# entry.param <-tkentry(frameTOP,width="20",textvariable=val.param)

#les combo

# listtype<-c("p", "fixedK", "propK", "agesK")
# combo.type <- tkwidget(frameF,"ComboBox",editable=FALSE,values=listtype,state="normal")
# listbootMethod<-c("samples","otoliths")
# combo.bootMethod <- tkwidget(frameF,"ComboBox",editable=FALSE,values=listbootMethod,state="normal")


# #les spin
# spin.p1<-tdspinner(frameF,from=0,to=1,inc=0.001,width=5,textvariable = lechoix.p1,state="normal",command=function(...)proba(1))
# spin.p2<-tdspinner(frameF,from=0,to=1,inc=0.001,width=5,textvariable = lechoix.p2,state="disabled",command=function(...)proba(2))
# spin.p<-tdspinner(frameF,from=1,to=100,inc=1,width=5,textvariable = lechoix.p,state="disabled")


# les radio
# tvarcatchCat <- tclVar("NULL")
# rbdis <- tkradiobutton(frameB,command=function()choixrb())
# rblan <- tkradiobutton(frameB,command=function()choixrb())
# tkconfigure(rbdis,variable=tvarcatchCat,value="DIS",state="disabled")
# tkconfigure(rblan,variable=tvarcatchCat,value="LAN",state="disabled")


# tvarmethodDesc <- tclVar("NULL")
# rbanal <- tkradiobutton(frameC,command=function()choixrb())
# rbboot <- tkradiobutton(frameC,command=function()choixrb())
# tkconfigure(rbanal,variable=tvarmethodDesc,value="analytic",state="disabled")
# tkconfigure(rbboot,variable=tvarmethodDesc,value="bootstrap",state="disabled")
#les bouton


# but.strini <- tkbutton(frameA,text=gettext(domain="R-COSTgui","<select>"),state="active",command=function(...) choixdataset.multi("strIni","but.strini"),foreground="red",relief="ridge")
# but.newstrini <- tkbutton(frameA,text=gettext(domain="R-COSTgui","New strIni type object"),state="active",command=function(...) .cost.strIni())#,foreground="red",relief="ridge")
# but.resetstrini <- tkbutton(frameA,text="reset",state="active",command=function(...) reset.strIni())#,foreground="red",relief="ridge")
valid.but <- tkbutton(tt,text=gettext(domain="R-COSTgui","Print"),state="disabled",command=function(...) valid())

# but.csObject <- tkbutton(frameBONUS,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset.multi(c("csDataCons"),"but.csObject"),foreground="red",relief="ridge")

but.dbeOutput <- tkbutton(frameBONUS,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset.multi(c("dbeOutput"),"but.dbeOutput"),foreground="red",relief="ridge")

# les check

cbValue.masq<-tclVar("1")
cb.masq <- tkcheckbutton(framemasq,variable=cbValue.masq,state="normal",command=function(...)remplitree(tclvalue(lechoix.but.dbeOutput)))

# cbValue.fillGaps<-tclVar("0")
# cb.fillGaps <- tkcheckbutton(frameF,variable=cbValue.fillGaps,state="normal",command=function()checkfill())

# cbValue.trace<-tclVar("0")
# cb.trace <- tkcheckbutton(frameF,variable=cbValue.trace,state="normal")


#text

# text.valid<-tklabel(frameD,text=gettext(domain="R-COSTgui","out :"))
#list

# list.species <-tklistbox(frameE,selectmode="unique",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.species.scroll,...))
# list.species.scroll<-tkscrollbar(frameE,repeatinterval=5,command=function(...)tkyview(list.species,...))

tkgrid(frameBONUS)
tkgrid(framemasq)
tkgrid(tklabel(framemasq,text=gettext(domain="R-COSTgui","Hide empty slot:"),font=font2),cb.masq)
tkgrid(frameTOP)
tkgrid(frameA)
# tkgrid(frameB)
# tkgrid(frameC)
# tkgrid(frameF)

# tkgrid(frameD)

# tkgrid(tklabel(frameBONUS,text="csDataCons:",font=font2),but.csObject)
tkgrid(tklabel(frameBONUS,text=gettext(domain="R-COSTgui","dbeOutput:"),font=font2),but.dbeOutput)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Description : ")),entry.desc)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Catch category : ")),entry.catchCat)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Method description : ")),entry.methodDesc)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Species : ")),entry.species)


tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Parameter : ")),entry.param)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Technical strata : ")),entry.techStrata)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Temporal strata : ")),entry.timeStrata)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Space strata : ")),entry.spaceStrata)
# tkgrid(tklabel(frameE,text=gettext(domain="R-COSTgui","Species"),font=font2))
# tkgrid(list.species,list.species.scroll,sticky="nswe")entry.methodDesc <-tkentry(frameTOP,width="20",textvariable=val.methodDesc,state="disabled")
# val.catchCat <- tclVar("")
# entry.catchCa



# tkgrid(frameE,columnspan=2)#,sticky="ew")

# tkgrid(tklabel(frameA,text=gettext(domain="R-COSTgui","StrIni : "),font=font2),but.strini,but.newstrini,but.resetstrini)
# tkgrid(tklabel(frameB,text="LAN : ",font=font2),rblan,tklabel(frameB,text="DIS : ",font=font2),rbdis)
# tkgrid(tklabel(frameC,text="analytic : ",font=font2),rbanal,tklabel(frameC,text="bootstrap : ",font=font2),rbboot)

# tkgrid(frameF)

# tkgrid(tklabel(frameF,text="type : "),combo.type)
# tkgrid(tklabel(frameF,text="bootMethod : "),combo.bootMethod)
# tkgrid(tklabel(frameF,text="fillGaps : "),cb.fillGaps)
# tkgrid(tklabel(frameF,text="p : "),spin.p)
# tkgrid(tklabel(frameF,text="trace : "),cb.trace)
# tkgrid(tklabel(frameF,text="incl.precision : "),cb.incl.precision)
# tkgrid(tklabel(frameF,text="probas : "),spin.p1,spin.p2)
tkgrid(valid.but)

tkgrid(treeWidget,yScr)
tkgrid.configure(yScr,stick="nsw")
tkgrid(xScr)
tkgrid.configure(xScr,stick="new")

tkfocus(tt)

}
.cost.relativeValue <-
function(){



try(require(tcltk))
try(require(tcltk2))
try(library(Rcmdr))
try(require(tkrplot))
try(tclRequire("BWidget"))
try(library(COSTcore))
try(library(COSTeda));try(library(COSTgui))


# slotNamesdata<-function(data){
# ploom<-slotNames(data)
# out<-c()
# for (p in ploom){
# if(is(slot(data, p))[1] == "data.frame"){
# out<-c(out,p)
# }
# }

# return(out)
# }

choixdataset.multi<-function(id,nom.but){
#verbose#print(id)
try(tkdestroy(tt2),silent=T)
tt2<-tktoplevel()
tkwm.geometry(tt2, "-100+50")     
listdata <-NULL
for ( i in id){listdata <- c(listdata,malist(i))}
dataSetsBox <- variableListBox(tt2, listdata, title=gettext(domain="R-COSTgui","Select one dataset"))# voir initialselection
tkgrid(getFrame(dataSetsBox), sticky="nwes",column=1,row=1)
  tkgrid.rowconfigure(getFrame(dataSetsBox), 1, weight = 1)
  tkgrid.rowconfigure(tt2, 1, weight = 1)  
onOK <- function(){	
out<-getSelection(dataSetsBox)

		if ( length(out) != 0){
		eval(parse(text=paste("tclvalue(lechoix.",nom.but,")<-out",sep="")))
		tkconfigure(eval(as.name(nom.but)),text=out)
		# print(GetIt((eval(as.name(paste("lechoix.",nom.but,sep=""))))))
		# tkconfigure( names.list.quali, state="disabled" )	
		# tkconfigure( names.list.quanti, state="disabled" )
		
		

		# print("on met a jour la liste des variables")

		# a ce niveau on a le nom du cs objet, on va lister toute les variaible exploitable pour le spaceplot
	if (nom.but == "but.costOBJECT1"){

if(substr(is(eval(as.name(out)))[1],1,2)=="cs"){araj<-c("lenNum", "wt", "subSampWt" , "nbSamp", "nbInd")}
if(substr(is(eval(as.name(out)))[1],1,2)=="ce"){araj<-c("trpNum")}
if(substr(is(eval(as.name(out)))[1],1,2)=="cl"){araj<-c("landWt")}

tkdelete(list.field1,"0","end");
for (var in araj) {tkinsert(list.field1, "end", var)}

		}

			if (nom.but == "but.costOBJECT2"){

if(substr(is(eval(as.name(out)))[1],1,2)=="cs"){araj<-c("lenNum", "wt", "subSampWt" , "nbSamp", "nbInd")}
if(substr(is(eval(as.name(out)))[1],1,2)=="ce"){araj<-c("trpNum")}
if(substr(is(eval(as.name(out)))[1],1,2)=="cl"){araj<-c("landWt")}

tkdelete(list.field2,"0","end");
for (var in araj) {tkinsert(list.field2, "end", var)}

		}
		
		
		
# aactiver<-list(entry.ylab,entry.xlab,list.time,list.tech,list.space,but.refr,entry.fonction,entry.main,cb.simu,cb.auto,list.time,list.tech,spin.x1,spin.x2,spin.y1,spin.y2,rbsel.image,rbsel.contour,rbsel.bubble,rbsel.value)
# # for ( i in 1:length(aactiver)){tkconfigure(aactiver[[i]],state="normal")}

# lapply(aactiver,tkconfigure,state="normal")

# choixtime<-c("year","semester","quarter","month")
# choixspace<-c("rect","subrect","area")
# choixtech<-c("commCat","commCatScl", "foCatEu5", "foCatEu6", "foCatNat","catchCat")
# tkdelete(list.time,"0","end");for (var in choixtime) {tkinsert(list.time, "end", var)}
# tkdelete(list.tech,"0","end");for (var in choixtech) {tkinsert(list.tech, "end", var)}
# tkdelete(list.space,"0","end");for (var in choixspace) {tkinsert(list.space, "end", var)}


	
		
}
tkdestroy(tt2)	
		
}
	
OK.but <-tkbutton(tt2,text="   OK   ",command=onOK)
tkgrid(OK.but,column=1,row=2)
tkfocus(tt2)
}

#fonctions pratique pour tk
malist<-function (pat) {

# retrouve dans ls les element de type pat
    Vars <- ls(all.names = TRUE,env=.GlobalEnv)
	# print(Vars)
    if (length(Vars) == 0) {return(Vars)}
    names(which(sapply(Vars, function(.x) {	is(get(.x))[1]==pat	}	)	)	)
	}

getcontentlistbox<-function(listbox){
out<-c()
i<-0		
while (length(as.character(tkget(listbox,i)))!=0){
out<-c(out,as.character(tkget(listbox,i)))
i<-i+1
}
return(out)
}
GetIt<-function(objet){
if (attr(objet,"class")=="tkwin"){
		if (as.character(tkwinfo("class",objet))=="Listbox"){
		return(getcontentlistbox(objet))
		}
}
if (attr(objet,"class")=="tclVar"){
return(tclvalue(objet))

}
}
DoItmini<-function(comm,env=.GlobalEnv){
  result <- parse(text = paste(comm))
  exprs<-result
  i<-1
  #for (i in seq_along(exprs)) {
  ei <- exprs[i]
  #verbose#print(ei)
  result <- eval(ei, envir = env)
  
}

reset.strIni<-function(){
tkconfigure(but.strini,text=gettext(domain="R-COSTgui","<select>"))
tclvalue(lechoix.but.strini)<-""
}

# plotnom<-function(auto=F){

# print("plotnom")
# print(auto)
# print(tclvalue(cbValue.auto))
# # print("ert")
# if ( auto==T & tclvalue(cbValue.auto)=="0"){return(NULL)}

# if(!exists("monnom",e2)){print("pas de monnomt dans e2")}

# if(exists("monnom",e2)){
# print("monnom existe dans e2")
# # # str1<-as.character(paste(as.character(tkget(list.strate1,as.character(tkcurselection(list.strate1)))),collapse=" "))
# # # str2<-as.character(paste(as.character(tkget(list.strate2,as.character(tkcurselection(list.strate2)))),collapse=" "))
# # # str1
# # # str2
# print('ok')
# largs <- as.list(quote(list()))
# largs <- c(largs,list(x=quote(get("monnom",e2)),
# strat1=quote(as.character(paste(as.character(tkget(list.strate1,as.character(tkcurselection(list.strate1)))),collapse=" "))), 
# strat2=quote(as.character(paste(as.character(tkget(list.strate2,as.character(tkcurselection(list.strate2)))),collapse=" "))),
# selection=(as.logical(as.numeric(tclvalue(tvar))) & !auto)
# ));largs
# tmp <- c(as.list(call("plot")), eval(as.call(largs))   );tmp
   # #verbose# cat(deparse(as.call(tmp)), "\n")
# # eval(as.call(tmp))
   
 
   
# if ( tclvalue(cbValue.simu) == "1" & auto==T){

# #on affiche rien mais on met quand meme a jour le graph
# eval(as.call(tmp))


# }  
   
   
# if ( tclvalue(cbValue.simu) == "1" & auto==F){

# # si tclvalue(val.nom) n 'xiste pas , il faut ptet forcer sa generation ici dans rcmdr

# as.name(tclvalue(val.nom))
# if(!exists(as.character(tclvalue(val.nom)))){lePlot(flag=T)}


# largs <- as.list(quote(list()))
# largs <- c(largs,list(x=quote(as.name(tclvalue(val.nom))),
# strat1=quote(as.character(paste(as.character(tkget(list.strate1,as.character(tkcurselection(list.strate1)))),collapse=" "))), 
# strat2=quote(as.character(paste(as.character(tkget(list.strate2,as.character(tkcurselection(list.strate2)))),collapse=" "))),
# selection=quote((as.logical(as.numeric(tclvalue(tvar))) & !auto))
# ));largs
# tmp <- c(as.list(call("plot")), eval(as.call(largs))   );tmp


# doItAndPrint(paste(sep="",tclvalue(val.nom.plot),"<-",paste(deparse(as.call(tmp)),collapse="")))
# if ((as.logical(as.numeric(tclvalue(tvar))) & !auto)){
# X11()
# doItAndPrint(paste("plot(",tclvalue(val.nom.plot),")",sep=""))

# }


 # # paste(sep="",tclvalue(val.nom),"<-",deparse(as.call(tmp)))

# }
# if ( tclvalue(cbValue.simu) == "0"){
# if (!(as.logical(as.numeric(tclvalue(tvar))) & !auto)){
# eval(as.call(tmp))
# }
# if ((as.logical(as.numeric(tclvalue(tvar))) & !auto)){
# # eval(as.call(tmp))
# # doItAndPrint(paste("plot(",tclvalue(val.nom.plot),")",sep=""))

# AEF<-eval(as.call(tmp))
# X11()
# plot(AEF)
# # justDoIt("plot(eval(as.call(tmp)))")



# }

# }
# }


# }


lePlot<-function(){
leCS1<-NULL
leCS2<-NULL
lefield1<-NULL
lefield2<-NULL
lestr<-NULL
out1<-NULL
out2<-NULL
try(leCS1<- as.name(as.character(GetIt(lechoix.but.costOBJECT1))),silent=T)
try(leCS2<- as.name(as.character(GetIt(lechoix.but.costOBJECT2))),silent=T)
try(lestr<-as.name(as.character(GetIt(lechoix.but.strini))),silent=T)
try(lefield1<-as.character(tkget(list.field1,as.character(tkcurselection(list.field1)))),silent=T)
try(lefield2<-as.character(tkget(list.field2,as.character(tkcurselection(list.field2)))),silent=T)
try(out1<-tclvalue(val.nom1),silent=T)
try(out2<-tclvalue(val.nom2),silent=T)

if(length(leCS1)==0 & length(leCS2)==0){Message(message = gettext(domain="R-COSTgui","You have to choose a CostObject"),  type = "error");return(NULL)}
if(length(leCS1)!=0 & out1==""){Message(message = gettext(domain="R-COSTgui","You have to specify an out name's for CostObject1"),  type = "error");return(NULL)}
if(length(leCS2)!=0 & out2==""){Message(message = gettext(domain="R-COSTgui","You have to specify an out name's for CostObject2"),  type = "error");return(NULL)}


# if(length(lessp)==0){Message(message = "You have to choose a species",  type = "error");return(NULL)}
# print(out)


tkconfigure(gen.but,text=gettext(domain="R-COSTgui","wait..."),state="disabled")

# on va controler que tout est pret

cmd1<-cmd2<-NULL
if(length(leCS1)!=0){
type1<-is(eval(as.name(as.character(GetIt(lechoix.but.costOBJECT1)))))
cmd1<-paste(out1,"<-relativeValue(",sep="")
cmd1<-paste(sep="",cmd1,"data=",as.character(GetIt(lechoix.but.costOBJECT1)))
if(length(lestr)!=0 & substr(type1,3,nchar(type1)) != "DataCons"){cmd1<-paste(sep="",cmd1,",","strDef=",as.character(GetIt(lechoix.but.strini)))}
if(length(lefield1)!=0){cmd1<-paste(sep="",cmd1,",","field='",lefield1,"'")}
cmd1<-paste(cmd1,")",sep="")
cmd1
doItAndPrint(cmd1)
}


if(length(leCS2)!=0){
type2<-is(eval(as.name(as.character(GetIt(lechoix.but.costOBJECT2)))))
cmd2<-paste(out2,"<-relativeValue(",sep="")
cmd2<-paste(sep="",cmd2,"data=",as.character(GetIt(lechoix.but.costOBJECT2)))
if(length(lestr)!=0 & substr(type2,3,nchar(type2)) != "DataCons"){cmd2<-paste(sep="",cmd2,",","strDef=",as.character(GetIt(lechoix.but.strini)))}
if(length(lefield2)!=0){cmd2<-paste(sep="",cmd2,",","field='",lefield2,"'")}
cmd2<-paste(cmd2,")",sep="")
cmd2
doItAndPrint(cmd2)
}

flag<-0
cmd3<-paste("plot(",sep="")
if(length(cmd1)!=0){
cmd3<-paste(cmd3,out1,sep="")
flag<-1
}
if(length(cmd2)!=0){
if (flag==1){cmd3<-paste(cmd3,",",sep="")}
cmd3<-paste(cmd3,out2,sep="")
flag<-1
}
cmd3<-paste(cmd3,",cex.lab=",as.numeric(tclvalue(lechoix.cex.lab))
,",rot=",as.numeric(tclvalue(lechoix.rot)),")",sep="")
 # cex.lab=0.7, rot = 90)

doItAndPrint(cmd3)
tkconfigure(gen.but,text=gettext(domain="R-COSTgui","Validate"),state="normal")
# doItAndPrint(cmd)
}

tdspinner <- function(parent, ...) {tkwidget(parent, "spinbox", ...)}





lechoix.but.costOBJECT1 <- tclVar("")
lechoix.but.costOBJECT2 <- tclVar("")
lechoix.but.strini <- tclVar("")
e2 <- new.env(parent = .GlobalEnv)
assign("lechoix.but.costOBJECT1",lechoix.but.costOBJECT1,envir=e2)
assign("lechoix.but.costOBJECT2",lechoix.but.costOBJECT2,envir=e2)

tt <- tktoplevel(borderwidth=10)
tkwm.geometry(tt, "-100+50")
font2<-tkfont.create(family="Times",size=12,weight="bold")
font3<-tkfont.create(family="Times",size=12)
fontheading<-tkfont.create(family="Times",size=18,weight="bold")  
tkwm.title(tt,gettext(domain="R-COSTgui","COST - Figure : Proportionality of sample"))
tkgrid(tklabel(tt, text=gettext(domain="R-COSTgui","Proportionality of sample"),font=fontheading),row=0,column=1,columnspan=2)
tkgrid(tklabel(tt,text="")) # Ligne de blanc  
tkgrid(tklabel(tt,text="")) # Ligne de blanc


frameTOP<- tkframe(tt, borderwidth=2, relief="groove")
frameG<- tkframe(tt, borderwidth=2, relief="groove")
frameGb<- tkframe(frameG, borderwidth=2, relief="groove")
frameL<- tkframe(tt, borderwidth=2, relief="groove")
frameLb<- tkframe(frameL, borderwidth=2, relief="groove")
# frameE<- tkframe(tt, borderwidth=2, relief="groove")
frameF<- tkframe(tt, borderwidth=2, relief="groove")


but.strini <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<select>"),state="active",command=function(...) choixdataset.multi("strIni","but.strini"),foreground="red",relief="ridge")
but.newstrini <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","New strIni type object"),state="active",command=function(...) .cost.strIni())#,foreground="red",relief="ridge")
but.resetstrini <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","reset"),state="active",command=function(...) reset.strIni())#,foreground="red",relief="ridge")
but.costOBJECT1 <- tkbutton(frameG,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset.multi(c("clDataVal","csDataVal","ceDataVal","clDataCons","csDataCons","ceDataCons"),"but.costOBJECT1"),foreground="red",relief="ridge")
but.costOBJECT2 <- tkbutton(frameL,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset.multi(c("clDataVal","csDataVal","ceDataVal","clDataCons","csDataCons","ceDataCons"),"but.costOBJECT2"),foreground="red",relief="ridge")
gen.but <- tkbutton(frameF,text=gettext(domain="R-COSTgui","Validate"),state="normal",command=function(...) lePlot())

lechoix.rot <- tclVar(90)
spin.rot<-tdspinner(frameF,from=0,to=360,width=3,inc=1,textvariable = lechoix.rot,state="normal")

lechoix.cex.lab <- tclVar(0.7)
spin.cex.lab<-tdspinner(frameF,from=0,to=10,width=3,inc=0.1,textvariable = lechoix.cex.lab,state="normal")


text.nom1<-tklabel(frameG,text=gettext(domain="R-COSTgui","out :"))
val.nom1 <- tclVar("out1")
entry.nom1 <-tkentry(frameG,width="20",textvariable=val.nom1,state="normal")

text.nom2<-tklabel(frameL,text=gettext(domain="R-COSTgui","out :"))
val.nom2 <- tclVar("out2")
entry.nom2 <-tkentry(frameL,width="20",textvariable=val.nom2,state="normal")


list.field1 <-tklistbox(frameGb,selectmode="unique",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.field1.scroll,...),state="normal")
list.field1.scroll<-tkscrollbar(frameGb,repeatinterval=5,command=function(...)tkyview(list.field1,...))


list.field2 <-tklistbox(frameLb,selectmode="unique",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.field2.scroll,...),state="normal")
list.field2.scroll<-tkscrollbar(frameLb,repeatinterval=5,command=function(...)tkyview(list.field2,...))





tkgrid(frameTOP,row=1,column=1,columnspan=2)
tkgrid(frameG,row=2,column=1)
tkgrid(frameL,row=2,column=2)
#kgrid(frameE)
 tkgrid(frameF,row=3,column=1,columnspan=2)

tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","StrIni : "),font=font2),but.strini,but.newstrini,but.resetstrini)

tkgrid(tklabel(frameG,text=gettext(domain="R-COSTgui","CostObject1 : "),font=font2),row=1,column=1,columnspan=2)
tkgrid(but.costOBJECT1,row=2,column=1,columnspan=2)
tkgrid(frameGb,row=3,column=1,columnspan=2)
tkgrid(list.field1,list.field1.scroll)
tkgrid(text.nom1,row=4,column=1,columnspan=1)
tkgrid(entry.nom1,row=4,column=2,columnspan=1)

tkgrid(tklabel(frameL,text=gettext(domain="R-COSTgui","CostObject2 : "),font=font2),row=1,column=1,columnspan=2)
tkgrid(but.costOBJECT2,row=2,column=1,columnspan=2)
 tkgrid(frameLb,row=3,column=1,columnspan=2)
tkgrid(list.field2,list.field2.scroll)
tkgrid(text.nom2,row=4,column=1,columnspan=1)
tkgrid(entry.nom2,row=4,column=2,columnspan=1)



tkgrid(gen.but,tklabel(frameF,text=gettext(domain="R-COSTgui","rot : ")),spin.rot
,tklabel(frameF,text=gettext(domain="R-COSTgui","cex.lab : ")),spin.cex.lab
)
# tkgrid(refr.but)
# tkgrid(frameA)
tkfocus(tt)
}
.cost.spacePlot <-
function(){

try(require(tcltk))
try(require(tcltk2))
try(library(Rcmdr))
try(require(tkrplot))
try(tclRequire("BWidget"))
try(library(COSTcore))
try(library(COSTeda));try(library(COSTgui))

#fonctions pratique pour tk
tdspinner <- function(parent, ...) {tkwidget(parent, "spinbox", ...)}
malist<-function (pat) {

# retrouve dans ls les element de type pat
    Vars <- ls(all.names = TRUE,env=.GlobalEnv)
	# print(Vars)
    if (length(Vars) == 0) {return(Vars)}
    names(which(sapply(Vars, function(.x) {	is(get(.x))[1]==pat	}	)	)	)
	}

getcontentlistbox<-function(listbox){
out<-c()
i<-0		
while (length(as.character(tkget(listbox,i)))!=0){
out<-c(out,as.character(tkget(listbox,i)))
i<-i+1
}
return(out)
}
GetIt<-function(objet){
if (attr(objet,"class")=="tkwin"){
		if (as.character(tkwinfo("class",objet))=="Listbox"){
		return(getcontentlistbox(objet))
		}
}
if (attr(objet,"class")=="tclVar"){
return(tclvalue(objet))

}
}
DoItmini<-function(comm,env=.GlobalEnv){
  result <- parse(text = paste(comm))
  exprs<-result
  i<-1
  #for (i in seq_along(exprs)) {
  ei <- exprs[i]
  #verbose#print(ei)
  result <- eval(ei, envir = env)
  
}

#fonction commune
		
typevar<-function(dataset,filtre=F){
	# prend un jeux de donn?e et ressort dans une list les nom de variable en focntion de leur type
#filtre permet de ne pas sortir les variables non exploitables (NA ou NULL)	
if(!filtre){
jegarde<-function(vec){return(TRUE)}
}

if (filtre){
jegarde<-function(vec){
out<-TRUE
if(sum(is.na(vec))==length(vec) ){out<-FALSE}
if(sum(vec=="",na.rm=T)==length(vec) ){out<-FALSE}
return(out)
}
}
	out.quanti<-c()
		out.quali<-c()
    
		for ( i in 1:dim(dataset)[2]){

		if (is(dataset[,i])[1]=="numeric" & jegarde(dataset[,i])){out.quanti<-c(out.quanti,names(dataset)[i])}
		if (is(dataset[,i])[1]=="integer" & jegarde(dataset[,i])){out.quanti<-c(out.quanti,names(dataset)[i])}
		if (is(dataset[,i])[1]=="factor" & jegarde(dataset[,i])){out.quali<-c(out.quali,names(dataset)[i])}
		if (is(dataset[,i])[1]=="character" & jegarde(dataset[,i])){out.quali<-c(out.quali,names(dataset)[i])}
				}
		return(list(quanti=out.quanti,quali=out.quali))
		}
comblist2<-function(test){
# on va la combinier et en faire une fonction
out<-list()
out$quanti<-unique(as.vector(unlist(test[names(test)=="quanti"])))
out$quali<-unique(as.vector(unlist(test[names(test)=="quali"])))
return(out)
}
comblist<-function(lalist){
return(comblist2(do.call("c",lalist)))
}	
lesvar<-function(cho,rewrite=F){
variableList<-list()
listslot<-slotNamesdata(eval(as.name(tclvalue(cho))))

if (rewrite==T){
#on met en as.character year month et quarter
for ( i in 1:length(listslot)){
pom<-do.call("slot",args =list(as.name(as.character(GetIt(cho))),listslot[i]))
names(pom)
for (p in c("year","month","quarter")){
if(is.element(p,names(pom))){pom[[p]]<-as.character(pom[[p]])}
}
# on met pom ou il faut
eval(parse(text=paste(as.name(as.character(GetIt(cho))),"@",listslot[i],"<-pom",sep="")))
}



}



for ( i in 1:length(listslot)){
variableList[[i]]<-
typevar(do.call("slot",args =list(as.name(as.character(GetIt(cho))),listslot[i])),filtre=T)
}
# typevar(hakeVal_cl@cl)
return(comblist(variableList))
}


slotNamesdata<-function(data){
ploom<-slotNames(data)
out<-c()
for (p in ploom){
if(is(slot(data, p))[1] == "data.frame"){
out<-c(out,p)
}
}
return(out)
}
choixdataset.multi<-function(id,nom.but){
#verbose#print(id)
try(tkdestroy(tt2),silent=T)
tt2<-tktoplevel()
tkwm.geometry(tt2, "-100+50")     
listdata <-NULL
for ( i in id){listdata <- c(listdata,malist(i))}
dataSetsBox <- variableListBox(tt2, listdata, title=gettext(domain="R-COSTgui","Select one dataset"))# voir initialselection
tkgrid(getFrame(dataSetsBox), sticky="nwes",column=1,row=1)
  tkgrid.rowconfigure(getFrame(dataSetsBox), 1, weight = 1)
  tkgrid.rowconfigure(tt2, 1, weight = 1)  
onOK <- function(){	
out<-getSelection(dataSetsBox)
		if ( length(out) != 0){
		eval(parse(text=paste("tclvalue(lechoix.",nom.but,")<-out",sep="")))
		tkconfigure(eval(as.name(nom.but)),text=out)
		
araj<-sort(lesvar(lechoix.but.csOBJECT)$quanti)
tkdelete(list.variables,"0","end");for (var in araj) {tkinsert(list.variables, "end", var)}
		# si ce cl ou cs on propose subset sinon non
		listok<-NULL
	for ( i in c("csData","clData","ceData")){listok <- c(listok,malist(i))}
		if(is.element(out,listok)){
		
		
		
tkconfigure(but.csData.subset,state="normal")
		
		}
		
			if(!is.element(out,listok)){
		tkconfigure(but.csData.subset,state="disabled")
		tkconfigure(cb.csData.subset,state="disabled")
tclvalue(cbValue.csData.subset)<-"0"		
		}	
		
}
tkdestroy(tt2)	
		
}
	
OK.but <-tkbutton(tt2,text="   OK   ",command=onOK)
tkgrid(OK.but,column=1,row=2)
tkfocus(tt2)
}

lesdepths<-function(auto=T){
# on va ecrire depths
# print("lesdepths")

if(as.numeric(tclvalue(cbValue.50))+as.numeric(tclvalue(cbValue.100))+as.numeric(tclvalue(cbValue.200))+as.numeric(tclvalue(cbValue.500))==0){
cmd<-""
tclvalue(val.depths)<-cmd;cmd
}

if(as.numeric(tclvalue(cbValue.50))+as.numeric(tclvalue(cbValue.100))+as.numeric(tclvalue(cbValue.200))+as.numeric(tclvalue(cbValue.500))!=0){
cmd<-"c("
prem<-0
if (tclvalue(cbValue.50)=="1"){
cmd<-paste(cmd,"50",sep="")
prem<-1
}

if (tclvalue(cbValue.100)=="1"){
if(prem==1){cmd<-paste(cmd,",",sep="")}
cmd<-paste(cmd,"100",sep="")
prem<-1
}

if (tclvalue(cbValue.200)=="1"){
if(prem==1){cmd<-paste(cmd,",",sep="")}
cmd<-paste(cmd,"200",sep="")
prem<-1
}
if (tclvalue(cbValue.500)=="1"){
if(prem==1){cmd<-paste(cmd,",",sep="")}
cmd<-paste(cmd,"500",sep="")
prem<-1
}
cmd<-paste(cmd,")",sep="")
tclvalue(val.depths)<-cmd;cmd


}



modd(auto)
}

act.breaks.levels<-function(){

eval(parse(text=paste(sep="","tkconfigure(entry.breaks,state='",c("disabled","normal")[as.numeric(tclvalue(eval(parse(text=paste("cbValue.breaks",sep="")))))+1],"')")))



eval(parse(text=paste(sep="","tkconfigure(spin.breaks,state='",
c("disabled","disabled","normal")[
as.numeric(tclvalue(eval(parse(text=paste("cbValue.breaks",sep="")))))+
as.numeric(tclvalue(eval(parse(text=paste("cbValue.zlim",sep="")))))
+1]
,"')")))



}

leslim<-function(id){
eval(parse(text=paste(sep="","tkconfigure(spin.",id,"1,state='",c("disabled","normal")[as.numeric(tclvalue(eval(parse(text=paste("cbValue.",id,"lim",sep="")))))+1],"')")))
eval(parse(text=paste(sep="","tkconfigure(spin.",id,"2,state='",c("disabled","normal")[as.numeric(tclvalue(eval(parse(text=paste("cbValue.",id,"lim",sep="")))))+1],"')")))

if ( id=="z"){

eval(parse(text=paste(sep="","tkconfigure(spin.breaks,state='",
c("disabled","disabled","normal")[
as.numeric(tclvalue(eval(parse(text=paste("cbValue.breaks",sep="")))))+
as.numeric(tclvalue(eval(parse(text=paste("cbValue.zlim",sep="")))))
+1]
,"')")))


}



}

modd<-function(auto=F){
#verbose#print("modd")
#verbose#print(auto)
#il faudra tout vectoriser ici pour gagner du temps, avec ifelse2 
if ( tclvalue(val.maptype)=="values"){
tkconfigure(spin.digits.text,state="normal")
tkconfigure(spin.cex.text,state="normal")
tkconfigure(spin.thre,state="normal")
}
if ( tclvalue(val.maptype)!="values"){
tkconfigure(spin.digits.text,state="disabled")
tkconfigure(spin.cex.text,state="disabled")
tkconfigure(spin.thre,state="disabled")
}


if ( tclvalue(val.maptype)=="bubble"){
tkconfigure(spin.pch,state="normal")
tkconfigure(spin.cex.max.bubble,state="normal")
}
if ( tclvalue(val.maptype)!="bubble"){
tkconfigure(spin.pch,state="disabled")
tkconfigure(spin.cex.max.bubble,state="disabled")
}
if ( tclvalue(cbValue.scale)=="1"){
tkconfigure(combo.scaleplace,state="normal")
tkconfigure(entry.scale.title,state="normal")
tkconfigure(cb.scale.box,state="normal")
tkconfigure(spin.scale.cex,state="normal")
}
if ( tclvalue(cbValue.scale)!="1"){
tkconfigure(combo.scaleplace,state="disabled")
tkconfigure(entry.scale.title,state="disabled")
tkconfigure(cb.scale.box,state="disabled")
tkconfigure(spin.scale.cex,state="disabled")
}

# # # # # if ( tclvalue(cbValue.plotmap)=="1"){
# # # # # tkconfigure(rbsel.image,state="normal")
# # # # # tkconfigure(rbsel.contour,state="normal")
# # # # # tkconfigure(rbsel.bubble,state="normal")
# # # # # tkconfigure(rbsel.value,state="normal")
# # # # # }


# # # # # if ( tclvalue(cbValue.plotmap)!="1"){
# # # # # tkconfigure(rbsel.image,state="disabled")
# # # # # tkconfigure(rbsel.contour,state="disabled")
# # # # # tkconfigure(rbsel.bubble,state="disabled")
# # # # # tkconfigure(rbsel.value,state="disabled")

# # # # # tkconfigure(spin.digits.text,state="disabled")
# # # # # tkconfigure(spin.cex.text,state="disabled")

# # # # # tkconfigure(spin.pch,state="disabled")
# # # # # tkconfigure(spin.cex.max.bubble,state="disabled")
# # # # # }





Onplot(auto=auto)
}


Onplot<-function(auto=F){
#verbose#print("Onplot")
#verbose#print(auto)
#verbose#print(tclvalue(cbValue.auto))
if ( auto==T & tclvalue(cbValue.auto)=="0"){return(NULL)}

largs <- as.list(quote(list()))
lesparam<-list()

if (tclvalue(val.type)=="csobj"){
#cs objet
if(tclvalue(cbValue.csData.subset)=="0" ){
if (tclvalue(lechoix.but.csOBJECT)!=""){
lesparam$costobj<-quote(as.name(tclvalue(lechoix.but.csOBJECT)))}
}
if(tclvalue(cbValue.csData.subset)=="1" ){

if (tclvalue(cbValue.simu)=="1"){
lesub<-get("formule",.envi.spacePlot)
for ( i in 1:length(lesub)){doItAndPrint(lesub[[i]])}
lCS<-strsplit(lesub[[1]],"<-")[[1]][1]
lesparam$costobj<-quote(as.name(lCS))
}
if (tclvalue(cbValue.simu)=="0"){
lesub<-get("formule",.envi.spacePlot)[[1]];lesub
#ptet faire un as.call ici
lCS<-paste("get('",strsplit(lesub,"<-")[[1]][1],"',.envi.spacePlot)",sep="")
lesub<-get("formule",.envi.spacePlot)[[1]];lesub
lCStmp<-as.list(quote(list()))
lCStmp <- c(lCStmp,list(x=quote(strsplit(lesub,"<-")[[1]][1]),envir=quote(as.name(".envi.spacePlot"))))
tmp2 <- c(as.list(call("get")), eval(as.call(lCStmp))   );tmp2
lCS<-as.call(tmp2);lCS
lesparam$costobj<-quote(lCS)
}



}


if ( length(as.character(tkcurselection(list.variables))) != 0){
lesparam$variable=quote(as.character(paste(as.character(tkget(list.variables,as.character(tkcurselection(list.variables)))),collapse=" ")))}
if ( length(as.character(tkcurselection(list.space))) != 0){

lesparam$SpaceStrata=quote(as.character(paste(as.character(tkget(list.space,as.character(tkcurselection(list.space)))),collapse=" ")))
if(paste(as.character(tkget(list.space,as.character(tkcurselection(list.space)))),collapse=" ")==gettext(domain="R-COSTgui","none")){
lesparam$SpaceStrata=quote(NULL)
}


}
if ( length(as.character(tkcurselection(list.time))) != 0){
lesparam$timeStrata=quote(as.character(paste(as.character(tkget(list.time,as.character(tkcurselection(list.time)))),collapse=" ")))

if(paste(as.character(tkget(list.time,as.character(tkcurselection(list.time)))),collapse=" ")==gettext(domain="R-COSTgui","none")){
lesparam$timeStrata=quote(NULL)
}
}
if ( length(as.character(tkcurselection(list.tech))) != 0){
lesparam$TechStrata=quote(as.character(paste(as.character(tkget(list.tech,as.character(tkcurselection(list.tech)))),collapse=" ")))
if(paste(as.character(tkget(list.tech,as.character(tkcurselection(list.tech)))),collapse=" ")==gettext(domain="R-COSTgui","none")){
lesparam$TechStrata=quote(NULL)
}
}
}



#value
if (tclvalue(val.type)=="value"){


if (tclvalue(val.value) !=""){
lesparam$costobj<-quote(eval(parse(text=tclvalue(val.value))))}
if (tclvalue(val.area) !=""){
lesparam$variable<-quote(eval(parse(text=tclvalue(val.area))))
}


}

if (tclvalue(val.fonction) !=""){lesparam$func=quote(as.name(tclvalue(val.fonction)))}
if (tclvalue(val.main) !=""){lesparam$main=quote(tclvalue(val.main))}
if (tclvalue(val.xlab) !=""){lesparam$xlab=quote(tclvalue(val.xlab))}
if (tclvalue(val.ylab) !=""){lesparam$ylab=quote(tclvalue(val.ylab))}
lesparam$maptype=quote(tclvalue(val.maptype))


if (tclvalue(cbValue.xlim)=="1"){lesparam$xlim=quote(as.vector(c(as.numeric(tclvalue(lechoix.x1)),as.numeric(tclvalue(lechoix.x2)))))}
if (tclvalue(cbValue.ylim)=="1"){lesparam$ylim=quote(as.vector(c(as.numeric(tclvalue(lechoix.y1)),as.numeric(tclvalue(lechoix.y2)))))}
if ( (as.numeric(tclvalue(lechoix.z2)) +as.numeric(tclvalue(lechoix.z2)) != 0) & tclvalue(cbValue.zlim)=="1"){
lesparam$zlim=quote(as.vector(c(as.numeric(tclvalue(lechoix.z1)),as.numeric(tclvalue(lechoix.z2)))))
}
if ( tclvalue(cbValue.breaks)=="1"){
if ( (as.numeric(tclvalue(lechoix.z2)) +as.numeric(tclvalue(lechoix.z2)) != 0) & tclvalue(cbValue.zlim)=="1"){

argseq <- as.list(quote(list()))
seqparam<-list()
seqparam$from=as.numeric(tclvalue(lechoix.z1))
seqparam$to=as.numeric(tclvalue(lechoix.z2))
seqparam$length.out=as.numeric(tclvalue(lechoix.breaks))
argseq <- c(argseq,seqparam);argseq
tmpseq <- c(as.list(call("seq")), eval(as.call(argseq)));tmpseq
lesparam$breaks=quote(as.call(tmpseq))
}

if (tclvalue(val.breaks) !=""){lesparam$breaks=quote(eval(parse(text=tclvalue(val.breaks))))}



}
if (as.logical(as.numeric(tclvalue(cbValue.overlay)))){
lesparam$overlay<-as.logical(as.numeric(tclvalue(cbValue.overlay)))
}
listcheckbutton<-c("area.lines","statrects","fcoast","landmass","colour","scale")
for ( p in listcheckbutton){
lesparam[[p]]<-as.logical(as.numeric(tclvalue(eval(as.name(paste("cbValue.",p,sep=""))))))
}
if (tclvalue(cbValue.scale)=="1"){
if( c("n","o")[as.numeric(tclvalue(cbValue.scale.box))+1] != "o"){lesparam$scale.box<-c("n","o")[as.numeric(tclvalue(cbValue.scale.box))+1]}
lesparam$scale.cex=quote(as.numeric(tclvalue(lechoix.scale.cex)))
if (length(as.character(operator.scaleplace[as.numeric(tclvalue(tcl(combo.scaleplace,"getvalue")))+1]))!=0){lesparam$scaleplace=quote(as.character(operator.scaleplace[as.numeric(tclvalue(tcl(combo.scaleplace,"getvalue")))+1]))}
if ( tclvalue(val.scale.title) !=""){lesparam$scale.title=quote(tclvalue(val.scale.title))}
}
if (tclvalue(val.depths) !=""){lesparam$depths=quote(eval(parse(text=tclvalue(val.depths))))}
if ( tclvalue(val.maptype)=="bubble"){
lesparam$pch=quote(as.numeric(tclvalue(lechoix.pch)))
lesparam$cex.max.bubble=quote(as.numeric(tclvalue(lechoix.cex.max.bubble)))
}
if (tclvalue(val.maptype)=="values"){
lesparam$cex.text=quote(as.numeric(tclvalue(lechoix.cex.text)))
lesparam$digits.text=quote(as.numeric(tclvalue(lechoix.digits.text)))
}
if (tclvalue(lechoix.nplots)!=1){
lesparam$nplots=quote(as.numeric(tclvalue(lechoix.nplots)))
}
if (tclvalue(val.col.coast)!="blue"){lesparam$col.coast<-tclvalue(val.col.coast)}
if (tclvalue(val.col.cont)!="grey"){lesparam$col.cont<-tclvalue(val.col.cont)}
if (tclvalue(val.col.pch)!="red"){lesparam$col.pch<-tclvalue(val.col.pch)}
if (tclvalue(val.col.rect)!="grey"){lesparam$col.rect<-tclvalue(val.col.rect)}
if (tclvalue(val.col.land)!="white"){lesparam$col.land<-tclvalue(val.col.land)}
if (tclvalue(val.col.depth)!="grey"){lesparam$col.depth<-tclvalue(val.col.depth)}
if (tclvalue(val.col.text)!="black"){lesparam$col.text<-tclvalue(val.col.text)}





largs <- c(largs,lesparam);largs




tmp <- c(as.list(call("spacePlot")), eval(as.call(largs))   );tmp
#verbose# cat(deparse(as.call(tmp)), "\n")

   
   
   
if ( tclvalue(cbValue.simu) == "1" & auto==T){
#on affiche rien mais on met quand meme a jour le graph
eval(as.call(tmp))
}  
   
   
if ( tclvalue(cbValue.simu) == "1" & auto==F){
doItAndPrint(paste(deparse(as.call(tmp)),collapse=""))
}

if ( tclvalue(cbValue.simu) == "0"){
eval(as.call(tmp))
}

# si xlim et ylim sont décoché, on va ajuster les valeur.. en pratique on ajuste quoi qui e passe car 
# si coché alors la valeur est certaine pusique c'est celle utilisé
# au mment ou lon charge un csobject
#.. a chauqe plot c'est cool, ca reinitialise le truc


# si cost obj
if (tclvalue(val.type)=="csobj"){
truc<-eval(eval(lesparam$costobj))
if (length(truc)!=0){
# truc<-eval(eval(lesparam$costobj2))
truc2<-slot(truc,"hh")
if (tclvalue(cbValue.ylim)=="0"){
tclvalue(lechoix.y1)<-round(min(c(truc2$latIni,truc2$latFin),na.rm=T))
tclvalue(lechoix.y2)<-round(max(c(truc2$latIni,truc2$latFin),na.rm=T))
}
if (tclvalue(cbValue.xlim)=="0"){
tclvalue(lechoix.x1)<-round(min(c(truc2$lonIni,truc2$lonFin),na.rm=T))
tclvalue(lechoix.x2)<-round(max(c(truc2$lonIni,truc2$lonFin),na.rm=T))
}
}
}
}

reset.but.csOBJECT<-function(){
# on efface le bouton csobjetc et la liste des variables

		eval(parse(text=paste("tclvalue(lechoix.but.csOBJECT)<-''",sep="")))	
		tkconfigure(but.csOBJECT,text=gettext(domain="R-COSTgui","<dataset>"))
tkdelete(list.variables,"0","end")
}
mescouleurs<-function(auto=F){
tt.color <- tktoplevel()
tkwm.title(tt.color,"Color Selection")
ChangeColor <- function(lavar,flag=1)
{
#verbose#print(lavar)
#verbose#print(flag)
#verbose#print(eval(parse(text=paste("tclvalue(val.",lavar,")",sep=""))))
if ( flag==1){
   color <- tclvalue(tcl("tk_chooseColor",initialcolor=tclvalue(eval(as.name(paste("val.",lavar,sep="")))),title="Choose a color"))
  #verbose#print(color)
  
  if (nchar(color)>0){
    tkconfigure(eval(parse(text=paste("canvas.",lavar,sep=""))),bg=color)
	# tclvalue(eval(as.name(paste("val.",lavar,sep=""))))<-color
	eval(parse(text=paste("tclvalue(val.",lavar,")<-color",sep="")))
	
	}
	
	}
	
if (flag==2){
  tkconfigure(eval(parse(text=paste("canvas.",lavar,sep=""))),bg=eval(parse(text=paste("tclvalue(val.",lavar,")",sep=""))))

  
  	
  
}	
	
}

for (p in c("col.coast","col.cont","col.pch","col.rect","col.land","col.depth","col.text")){
eval(parse(text=paste("canvas.",p,"<- tkcanvas(tt.color,width='80',height='25',bg='",tclvalue(eval(as.name(paste("val.",p,sep="")))),"')",sep="")))
eval(parse(text=paste("ChangeColor.button.",p,"<-tkbutton(tt.color,text='nuancier',command=function(...) ChangeColor('",p,"',flag=1))",sep="")))


# entry.delta <-tkentry(frameF,width="8",textvariable=val.delta,state="normal")
eval(parse(text=paste("entry.",p,"<-tkentry(tt.color,width='8',textvariable=val.",p,",state='normal')",sep="")))


# on rajoute un tkgrid



tkgrid(tklabel(tt.color,text=p),eval(parse(text=paste("entry.",p,sep=""))),eval(parse(text=paste("canvas.",p,sep=""))),
eval(parse(text=paste("ChangeColor.button.",p,sep=""))))
eval(parse(text=paste("tkbind(entry.",p,",'<Return>',function(...)ChangeColor('",p,"',2))",sep="")))

}

# Onplot(auto=auto)
 }

 


faitsubset<-function(envi){
#verbose#print("faitsubset")

# on envoi différenement en fonction de ce que l'on plot



res<-.cost.subset.virtual(virtual=T,sour=as.numeric(is(eval(as.name(tclvalue(lechoix.but.csOBJECT))))==c("csData","clData","ceData")),dataname=tclvalue(lechoix.but.csOBJECT),LENV=envi,parr=tt,label="(mapping)")

tclvalue(cbValue.csData.subset)<-"1"
tkconfigure(cb.csData.subset,state="normal")

}

 
 
#les variables

lechoix.but.csOBJECT <- tclVar("")
lechoix.x1 <- tclVar(-20)
lechoix.x2 <- tclVar(20)
lechoix.y1 <- tclVar(44)
lechoix.y2 <- tclVar(60)

lechoix.x1 <- tclVar(0)
lechoix.x2 <- tclVar(1)
lechoix.y1 <- tclVar(1)
lechoix.y2 <- tclVar(0)




lechoix.z1 <- tclVar(0)
lechoix.z2 <- tclVar(0)
lechoix.pch <- tclVar(1)
lechoix.scale.cex <- tclVar(1)
lechoix.cex.max.bubble <- tclVar(1)
lechoix.digits.text <- tclVar(0)
lechoix.cex.text <- tclVar(1)
lechoix.nplots <- tclVar(1)
lechoix.breaks <- tclVar(8)
lechoix.thre <- tclVar(0)

# les couleurs
val.col.coast<-tclVar("blue")
val.col.cont<-tclVar("grey")
val.col.pch<-tclVar("red")
val.col.rect<-tclVar("grey")
val.col.land<-tclVar("white")
val.col.depth<-tclVar("grey")
val.col.text<-tclVar("black")

#le GUI


tt <- tktoplevel(borderwidth=10)
tkwm.geometry(tt, "-100+50")
.envi.spacePlot <- new.env(parent = .GlobalEnv)
assign(".envi.spacePlot",.envi.spacePlot,pos=1)
font2<-tkfont.create(family="Times",size=12,weight="bold")
font3<-tkfont.create(family="Times",size=12)
fontheading<-tkfont.create(family="Times",size=18,weight="bold")  
tkwm.title(tt,gettext(domain="R-COSTgui","COST - mapping"))
# # tkgrid(tklabel(tt,text="")) # Ligne de blanc  
# # tkgrid(tklabel(tt,text="")) # Ligne de blanc


#les frames

frameTOP<- tkframe(tt, borderwidth=2, relief="groove")
frameTOP2<- tkframe(tt, borderwidth=2, relief="groove")
frameTOP3<- tkframe(tt, borderwidth=2, relief="groove")
frameE<- tkframe(frameTOP, borderwidth=2, relief="groove")
frameF<- tkframe(tt, borderwidth=2, relief="groove")
frameG<- tkframe(tt, borderwidth=2, relief="groove")
frameA<- tkframe(tt, borderwidth=2, relief="groove")
frameB<- tkframe(tt, borderwidth=2, relief="groove")
framemap<- tkframe(tt, borderwidth=2, relief="groove")
framelabel<- tkframe(frameB, borderwidth=2, relief="groove")
framesize<- tkframe(tt, borderwidth=2, relief="groove")
frameA2<- tkframe(tt, borderwidth=2, relief="groove")
framedetail<- tkframe(tt, borderwidth=2, relief="groove")
framevalue<- tkframe(framemap, borderwidth=2, relief="groove")
framebubble<- tkframe(framemap, borderwidth=2, relief="groove")
framescale<- tkframe(tt, borderwidth=2, relief="groove")
framedepths<- tkframe(tt, borderwidth=2, relief="groove")
framebreaks<- tkframe(tt, borderwidth=2, relief="groove")




# les listbox

list.variables <-tklistbox(frameE,selectmode="unique",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.variables.scroll,...))
list.variables.scroll<-tkscrollbar(frameE,repeatinterval=5,command=function(...)tkyview(list.variables,...))
tkgrid(list.variables,list.variables.scroll,sticky="ns")
list.tech <-tklistbox(frameG,selectmode="unique",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.tech.scroll,...),state="disabled",width=12)
list.tech.scroll<-tkscrollbar(frameG,repeatinterval=5,command=function(...)tkyview(list.tech,...))
list.time <-tklistbox(frameG,selectmode="unique",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.time.scroll,...),state="disabled",width=12)
list.time.scroll<-tkscrollbar(frameG,repeatinterval=5,command=function(...)tkyview(list.time,...))
list.space <-tklistbox(frameG,selectmode="unique",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.space.scroll,...),state="disabled",width=12)
list.space.scroll<-tkscrollbar(frameG,repeatinterval=5,command=function(...)tkyview(list.space,...))



#les boutons

but.csOBJECT <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset.multi(c("csData","clData","ceData","clDataVal","ceDataVal","csDataVal"),"but.csOBJECT"),foreground="red",relief="ridge")
but.reset.csOBJECT <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","Reset"),state="active",command=function(...) reset.but.csOBJECT())


but.csData.subset <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","subset"),state="disabled",command=function(...) faitsubset(.envi.spacePlot))
cbValue.csData.subset<- tclVar("0")
cb.csData.subset<-tkcheckbutton(frameTOP,variable=cbValue.csData.subset,state="disabled")



# gen.but <- tkbutton(frameF,text="create",state="normal",command=function(...) gendelta())
but.refr <- tkbutton(frameA,text=gettext(domain="R-COSTgui","plot"),state="disabled",font=font2,foreground="darkblue",command=function(...) Onplot(auto=F))
but.X11 <- tkbutton(frameA,text=gettext(domain="R-COSTgui","New window"),state="disabled",font=font2,foreground="black",command=function(...) X11())
but.color <- tkbutton(framemap,text=gettext(domain="R-COSTgui","Color"),state="disabled",foreground="darkgreen",command=function(...) mescouleurs(auto=F))

# les combo box

# scaleplace

operator.scaleplace<-c("bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right","center")
combo.scaleplace <- tkwidget(framescale,"ComboBox",editable=FALSE,values=operator.scaleplace,state="disabled")





# les boutons radio
val.maptype <- tclVar("image")
rbsel.image <- tkradiobutton(framemap,state="disabled",command=function()modd(auto=T),variable=val.maptype,value="image",state="disabled")
rbsel.contour <- tkradiobutton(framemap,state="disabled",command=function()modd(auto=T),variable=val.maptype,value="contour",state="disabled")
rbsel.bubble <- tkradiobutton(framemap,state="disabled",command=function()modd(auto=T),variable=val.maptype,value="bubble",state="disabled")
rbsel.value <- tkradiobutton(framemap,state="disabled",command=function()modd(auto=T),variable=val.maptype,value="values",state="disabled")

val.type <- tclVar("csobj")
rbsel.typecsobj <-tkradiobutton(frameTOP3,state="disabled",command=function()ls(),variable=val.type,value="csobj",state="disabled")
rbsel.typevalue <-tkradiobutton(frameTOP3,state="disabled",command=function()ls(),variable=val.type,value="value",state="disabled")


# les tkentry

text.delta<-tklabel(frameF,text=gettext(domain="R-COSTgui","delta :"))
val.delta <- tclVar("deltas")
entry.delta <-tkentry(frameF,width="20",textvariable=val.delta,state="normal")
val.fonction <- tclVar("sum")
entry.fonction <-tkentry(frameA2,width="20",textvariable=val.fonction,state="disabled")
val.main <- tclVar("")
entry.main <-tkentry(frameA2,width="20",textvariable=val.main,state="disabled")

val.scale.title <- tclVar("")
entry.scale.title <-tkentry(framescale,width="20",textvariable=val.scale.title,state="disabled")
val.xlab <- tclVar("")
entry.xlab <-tkentry(frameA2,width="20",textvariable=val.xlab,state="disabled")
val.ylab <- tclVar("")
entry.ylab <-tkentry(frameA2,width="20",textvariable=val.ylab,state="disabled")

val.depths <- tclVar("")
entry.depths <-tkentry(framedepths,width="20",textvariable=val.depths,state="disabled")

val.breaks <- tclVar("")
entry.breaks <-tkentry(framebreaks,width="20",textvariable=val.breaks,state="disabled")


val.value <- tclVar("")
entry.value <-tkentry(frameTOP2,width="20",textvariable=val.value,state="disabled")


val.area <- tclVar("")
entry.area <-tkentry(frameTOP2,width="20",textvariable=val.area,state="disabled")


# les checkbutton
cbValue.simu<-tclVar("0")
cb.simu <- tkcheckbutton(frameA,variable=cbValue.simu,state="normal")
cbValue.auto<-tclVar("1")
cb.auto <- tkcheckbutton(frameA,variable=cbValue.auto,state="disabled")

cbValue.scale.box<-tclVar("1")
cb.scale.box<- tkcheckbutton(framescale,variable=cbValue.scale.box,state="disabled")

cbValue.breaks<-tclVar("0")
cb.breaks<- tkcheckbutton(framebreaks,variable=cbValue.breaks,state="normal",command=function()act.breaks.levels())


# cbValue.plotmap<-tclVar("1")
# cb.plotmap <- tkcheckbutton(framedetail,variable=cbValue.plotmap,state="disabled",command=function(...)modd(auto=T))

cbValue.overlay<-tclVar("0")
cb.overlay <- tkcheckbutton(framedetail,variable=cbValue.overlay,state="disabled",command=function()modd(auto=T))

# # # cbValue.squarmap<-tclVar("0")
# # # cb.squarmap <- tkcheckbutton(framedetail,variable=cbValue.squarmap,state="disabled")


cbValue.area.lines<-tclVar("0")
cb.area.lines <- tkcheckbutton(framedetail,variable=cbValue.area.lines,state="disabled",command=function()modd(auto=T))


cbValue.statrects<-tclVar("0")
cb.statrects <- tkcheckbutton(framedetail,variable=cbValue.statrects,state="disabled",command=function()modd(auto=T))

cbValue.fcoast<-tclVar("0")
cb.fcoast <- tkcheckbutton(framedetail,variable=cbValue.fcoast,state="disabled",command=function()modd(auto=T))

cbValue.landmass<-tclVar("1")
cb.landmass <- tkcheckbutton(framedetail,variable=cbValue.landmass,state="disabled",command=function()modd(auto=T))

cbValue.colour<-tclVar("1")
cb.colour <- tkcheckbutton(framedetail,variable=cbValue.colour,state="disabled",command=function()modd(auto=T))

cbValue.scale<-tclVar("0")
cb.scale <- tkcheckbutton(framescale,variable=cbValue.scale,state="disabled",command=function()modd(auto=T))

cbValue.xlim<-tclVar("0")
cb.xlim <- tkcheckbutton(framesize,variable=cbValue.xlim,state="normal",command=function()leslim(id="x"))

cbValue.ylim<-tclVar("0")
cb.ylim <- tkcheckbutton(framesize,variable=cbValue.ylim,state="normal",command=function()leslim(id="y"))

cbValue.zlim<-tclVar("0")
cb.zlim <- tkcheckbutton(framesize,variable=cbValue.zlim,state="normal",command=function()leslim(id="z"))


cbValue.50<-tclVar("0")
cb.50 <- tkcheckbutton(framedepths,variable=cbValue.50,state="normal",command=function()lesdepths(auto=T))

cbValue.100<-tclVar("0")
cb.100 <- tkcheckbutton(framedepths,variable=cbValue.100,state="normal",command=function()lesdepths(auto=T))

cbValue.200<-tclVar("0")
cb.200 <- tkcheckbutton(framedepths,variable=cbValue.200,state="normal",command=function()lesdepths(auto=T))

cbValue.500<-tclVar("0")
cb.500 <- tkcheckbutton(framedepths,variable=cbValue.500,state="normal",command=function()lesdepths(auto=T))


# les spinbox

spin.x1<-tdspinner(framesize,from=-1000,to=1000,width=5,command=function(...)Onplot(auto=T),textvariable = lechoix.x1,state="disabled")
spin.x2<-tdspinner(framesize,from=-1000,to=1000,width=5,command=function(...)Onplot(auto=T),textvariable = lechoix.x2,state="disabled")
spin.y1<-tdspinner(framesize,from=-1000,to=1000,width=5,command=function(...)Onplot(auto=T),textvariable = lechoix.y1,state="disabled")
spin.y2<-tdspinner(framesize,from=-1000,to=1000,width=5,command=function(...)Onplot(auto=T),textvariable = lechoix.y2,state="disabled")
spin.z1<-tdspinner(framesize,from=-1000,to=1000,width=5,command=function(...)Onplot(auto=T),textvariable = lechoix.z1,state="disabled")
spin.z2<-tdspinner(framesize,from=-1000,to=1000,width=5,command=function(...)Onplot(auto=T),textvariable = lechoix.z2,state="disabled")

spin.pch<-tdspinner(framebubble,from=1,to=15,width=4,command=function(...)Onplot(auto=T),textvariable = lechoix.pch,state="disabled")
spin.scale.cex<-tdspinner(framescale,from=1,to=15,inc=0.1,width=4,command=function(...)Onplot(auto=T),textvariable = lechoix.scale.cex,state="disabled")
spin.cex.max.bubble<-tdspinner(framebubble,from=1,to=15,inc=0.1,width=4,command=function(...)Onplot(auto=T),textvariable = lechoix.cex.max.bubble,state="disabled")
spin.digits.text<-tdspinner(framevalue,from=0,to=10,width=4,command=function(...)Onplot(auto=T),textvariable = lechoix.digits.text,state="disabled")
spin.cex.text<-tdspinner(framevalue,from=0,to=15,inc=0.1,width=4,command=function(...)Onplot(auto=T),textvariable = lechoix.cex.text,state="disabled")
spin.nplots<-tdspinner(frameA,from=1,to=9,width=4,command=function(...)Onplot(auto=T),textvariable = lechoix.nplots,state="disabled")
spin.thre<-tdspinner(framevalue,from=0,to=150,inc=0.1,width=4,command=function(...)Onplot(auto=T),textvariable = lechoix.thre,state="disabled")
spin.breaks<-tdspinner(framebreaks,from=1,to=45,inc=1,width=4,command=function(...)Onplot(auto=T),textvariable = lechoix.breaks,state="disabled")




#les BIND

tkbind(list.tech, "<ButtonRelease-1>", function(...)Onplot(auto=T))
tkbind(list.time, "<ButtonRelease-1>", function(...)Onplot(auto=T))
tkbind(list.space, "<ButtonRelease-1>", function(...)Onplot(auto=T))

#les grid
tkgrid(tklabel(tt, text=gettext(domain="R-COSTgui","mapping"),font=fontheading),row=0,column=4,sticky="w")
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Costobjet :"),font=font2),but.csOBJECT,but.reset.csOBJECT)

tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","use subset :")),cb.csData.subset,but.csData.subset)



tkgrid(frameTOP,row=2,column=1,columnspan=2,sticky="w",rowspan=2)#,frameD)
tkgrid(frameTOP2,row=2,column=3,columnspan=1,sticky="w",rowspan=2)#,frameD)
tkgrid(frameTOP3,row=1,column=1,columnspan=1,sticky="we")
tkgrid(tklabel(frameTOP3,text=gettext(domain="R-COSTgui","Value :")),rbsel.typevalue,tklabel(frameTOP3,text=gettext(domain="R-COSTgui","Csobjet :")),rbsel.typecsobj)
tkgrid(tklabel(frameTOP2,text=gettext(domain="R-COSTgui","Value :")))
tkgrid(entry.value)
tkgrid(tklabel(frameTOP2,text=gettext(domain="R-COSTgui","Area :")))
tkgrid(entry.area)


tkgrid(frameE,columnspan=3,sticky="we")#,row=3,column=1,columnspan=2)
# tkgrid(tklabel(tt,text=" "))
tkgrid(frameG,row=4,column=1,columnspan=3)
tkgrid(frameA,row=5,column=1,columnspan=3,sticky="we",rowspan=2)
tkgrid(framedetail,column=4,row=3,rowspan=2)
# # # tkgrid(frameB)
# # # tkgrid(framelabel)
tkgrid(frameA2,row=5,column=4,columnspan=3,sticky="we",rowspan=2)
tkgrid(framesize,row=4,column=5,columnspan=2,rowspan=1,sticky="wn")
tkgrid(framemap,row=1,column=5,columnspan=3,rowspan=3,sticky="we")
tkgrid(framescale,row=4,column=7,columnspan=2,rowspan=1)
# tkgrid()


tkgrid(tklabel(frameG,text=gettext(domain="R-COSTgui","Technical strata")),tklabel(frameG,text=" "),tklabel(frameG,text=gettext(domain="R-COSTgui","Temporal strata")),tklabel(frameG,text="  "),tklabel(frameG,text=gettext(domain="R-COSTgui","Space strata")),tklabel(frameG,text="  "))
tkgrid(list.tech,list.tech.scroll,list.time,list.time.scroll,list.space,list.space.scroll,sticky="ns")


tkgrid(tklabel(frameA,text=gettext(domain="R-COSTgui","save the script :")),row=1,column=1,sticky="w")
tkgrid(cb.simu,row=1,column=2,sticky="w")
tkgrid(but.refr,column=3,row=1,rowspan=2,sticky="wnse")
tkgrid(but.X11,column=3,row=3,rowspan=2,sticky="wnse")
tkgrid(tklabel(frameA,text=gettext(domain="R-COSTgui","automatic :")),row=2,column=1,sticky="w")
tkgrid(cb.auto,row=2,column=2,sticky="w")
 tkgrid(tklabel(frameA,text=gettext(domain="R-COSTgui","nplots :")),row=3,column=1,sticky="w")
tkgrid(spin.nplots,row=3,column=2,sticky="w")

 tkgrid.columnconfigure(frameA, 1, weight = 1)
 tkgrid.columnconfigure(frameA, 2, weight = 1)
 tkgrid.columnconfigure(frameA, 3, weight = 4)

tkgrid.columnconfigure(tt, 1, weight = 1)
tkgrid.columnconfigure(tt, 2, weight = 1)
tkgrid.columnconfigure(tt, 3, weight = 1)
tkgrid.columnconfigure(tt, 4, weight = 1)
tkgrid.columnconfigure(tt, 5, weight = 1)
tkgrid.columnconfigure(tt, 6, weight = 1)
tkgrid.columnconfigure(tt, 7, weight = 1)
tkgrid.columnconfigure(tt, 8, weight = 1)
 

 tkgrid.rowconfigure(tt, 1, weight = 1)
tkgrid.rowconfigure(tt, 2, weight = 1)
tkgrid.rowconfigure(tt, 3, weight = 1)
tkgrid.rowconfigure(tt, 4, weight = 1)
tkgrid.rowconfigure(tt, 5, weight = 1)

 

tkgrid(tklabel(frameA2,text=gettext(domain="R-COSTgui","Title :")),entry.main,sticky="e")
tkgrid(tklabel(frameA2,text=gettext(domain="R-COSTgui","xlab :")),entry.xlab,sticky="e")	
tkgrid(tklabel(frameA2,text=gettext(domain="R-COSTgui","Y label")),entry.ylab,sticky="e")
tkgrid(tklabel(frameA2,text=gettext(domain="R-COSTgui","function :")),entry.fonction,sticky="e")






tkgrid(cb.xlim,tklabel(framesize,text=gettext(domain="R-COSTgui","xlim :")),spin.x1,spin.x2)
tkgrid(cb.ylim,tklabel(framesize,text=gettext(domain="R-COSTgui","ylim :")),spin.y1,spin.y2)
tkgrid(cb.zlim,tklabel(framesize,text=gettext(domain="R-COSTgui","zlim :")),spin.z1,spin.z2)



# tkgrid(tklabel(framedetail,text="plotmap :"),cb.plotmap,sticky="w")
tkgrid(tklabel(framedetail,text=gettext(domain="R-COSTgui","overlay :")),cb.overlay,sticky="w")
# # tkgrid(tklabel(framedetail,text="squarmap :"),cb.squarmap)
tkgrid(tklabel(framedetail,text=gettext(domain="R-COSTgui","area.lines :")),cb.area.lines,sticky="w")
tkgrid(tklabel(framedetail,text=gettext(domain="R-COSTgui","statrects :")),cb.statrects,sticky="w")
tkgrid(tklabel(framedetail,text=gettext(domain="R-COSTgui","fcoast :")),cb.fcoast,sticky="w")
tkgrid(tklabel(framedetail,text=gettext(domain="R-COSTgui","landmass :")),cb.landmass,sticky="w")
tkgrid(tklabel(framedetail,text=gettext(domain="R-COSTgui","colour :")),cb.colour,sticky="w")

tkgrid(framedepths,row=5,column=7,sticky="en")
tkgrid(tklabel(framedepths,text=gettext(domain="R-COSTgui","depths :")),row=1,column=1,sticky="en")
tkgrid(entry.depths,sticky="e",row=1,column=2,sticky="ew",columnspan=7)
tkgrid(tklabel(framedepths,text="50 :"),row=2,column=1)
tkgrid(cb.50,row=2,column=2)
tkgrid(tklabel(framedepths,text="100 :"),row=2,column=3)
tkgrid(cb.100,row=2,column=4)
tkgrid(tklabel(framedepths,text="200 :"),row=2,column=5)
tkgrid(cb.200,row=2,column=6)
tkgrid(tklabel(framedepths,text="500 :"),row=2,column=7)
tkgrid(cb.500,row=2,column=8)

tkgrid(framebreaks,row=6,column=7,sticky="enws")
tkgrid(cb.breaks,row=1,column=1,sticky="ensw",rowspan=2)
tkgrid(tklabel(framebreaks,text=gettext(domain="R-COSTgui","breaks :")),row=1,column=2,sticky="en")
tkgrid(entry.breaks,sticky="e",row=1,column=3,sticky="ew",columnspan=7)
tkgrid(tklabel(framebreaks,text=gettext(domain="R-COSTgui","levels :")),row=2,column=2,sticky="en")
tkgrid(spin.breaks,row=2,column=3,sticky="ew",columnspan=2)

# tkgrid(tklabel(framebubble,text="Bubble"),columnspan=2)
tkgrid(tklabel(framebubble,text=gettext(domain="R-COSTgui","pch :")),spin.pch,sticky="e")
tkgrid(tklabel(framebubble,text=gettext(domain="R-COSTgui","cex.max :")),spin.cex.max.bubble,sticky="e")

# tkgrid(tklabel(framevalue,text="Values"),columnspan=2)
tkgrid(tklabel(framevalue,text=gettext(domain="R-COSTgui","digits :")),spin.digits.text)
tkgrid(tklabel(framevalue,text=gettext(domain="R-COSTgui","threshold :")),spin.thre)
tkgrid(tklabel(framevalue,text=gettext(domain="R-COSTgui","cex :")),spin.cex.text)

# # # tkgrid(tklabel(framescale,text="scale :"),cb.scale,sticky="w",columnspan=4)
# # # tkgrid(tklabel(framescale,text="scaleplace :"),combo.scaleplace,sticky="w",columnspan=4)
# # # tkgrid(tklabel(framescale,text="titre scale :"),entry.scale.title,sticky="w",columnspan=4)
# # # tkgrid(tklabel(framescale,text="scale.box :"),cb.scale.box,tklabel(framescale,text="scale.cex :"),spin.scale.cex,sticky="w")
# # # # tkgrid()

tkgrid(tklabel(framescale,text=gettext(domain="R-COSTgui","scale :")),row=1,column=1,columnspan=2,sticky="w")
tkgrid(cb.scale,row=1,column=3,columnspan=2)
tkgrid(tklabel(framescale,text=gettext(domain="R-COSTgui","scaleplace :")),row=2,column=1,columnspan=2,sticky="w")
tkgrid(combo.scaleplace,row=2,column=3,columnspan=2,sticky="we")
tkgrid(tklabel(framescale,text=gettext(domain="R-COSTgui","titre scale :")),row=3,column=1,columnspan=2,sticky="w")
tkgrid(entry.scale.title,row=3,column=3,columnspan=2,sticky="we")
tkgrid(tklabel(framescale,text=gettext(domain="R-COSTgui","scale.box :")),row=4,column=1,sticky="w")
tkgrid(cb.scale.box,row=4,column=2)
tkgrid(ttklabel(framescale,text=gettext(domain="R-COSTgui","scale.cex :")),row=4,column=3,sticky="w")
tkgrid(spin.scale.cex,row=4,column=4)






# tkgrid(tklabel(framemap,text=""),tklabel(framemap,text="maptype "))
# tkgrid(tklabel(framemap,text="image :"),rbsel.image)
# tkgrid(tklabel(framemap,text="contour :"),rbsel.contour)
# tkgrid(tklabel(framemap,text="bubble :"),rbsel.bubble)
# tkgrid(tklabel(framemap,text="value :"),rbsel.value)



tkgrid(tklabel(framemap,text=gettext(domain="R-COSTgui","maptype ")),row=1,column=1,columnspan=4)
tkgrid(tklabel(framemap,text=gettext(domain="R-COSTgui","image")),row=2,column=1)
tkgrid(tklabel(framemap,text=gettext(domain="R-COSTgui","contour")),row=2,column=2)
tkgrid(tklabel(framemap,text=gettext(domain="R-COSTgui","bubble")),row=2,column=3)
tkgrid(tklabel(framemap,text=gettext(domain="R-COSTgui","value")),row=2,column=4)
tkgrid(rbsel.image,row=3,column=1)
tkgrid(rbsel.contour,row=3,column=2)
tkgrid(rbsel.bubble,row=3,column=3)
tkgrid(rbsel.value,row=3,column=4)
tkgrid(framebubble,row=4,column=3,sticky="ns")
tkgrid(framevalue,row=4,column=4)
tkgrid(but.color,row=4,column=1,columnspan=2,sticky="we")




# on active tout .
#,cb.squarmap
aactiver<-list(rbsel.typecsobj,rbsel.typevalue,entry.area,entry.value,spin.nplots,but.color,entry.depths,cb.overlay,cb.area.lines,cb.statrects,cb.fcoast,cb.landmass,cb.colour,cb.scale,entry.ylab,entry.xlab,list.time,list.tech,list.space,but.X11,but.refr,entry.fonction,entry.main,cb.simu,cb.auto,list.time,list.tech,rbsel.image,rbsel.contour,rbsel.bubble,rbsel.value)
lapply(aactiver,tkconfigure,state="normal")
choixtime<-c(gettext(domain="R-COSTgui","none"),"year","semester","quarter","month")
choixspace<-c(gettext(domain="R-COSTgui","none"),"rect","subrect","area")
choixtech<-c(gettext(domain="R-COSTgui","none"),"commCat","commCatScl", "foCatEu5", "foCatEu6", "foCatNat","catchCat")
tkdelete(list.time,"0","end");for (var in choixtime) {tkinsert(list.time, "end", var)}
tkdelete(list.tech,"0","end");for (var in choixtech) {tkinsert(list.tech, "end", var)}
tkdelete(list.space,"0","end");for (var in choixspace) {tkinsert(list.space, "end", var)}

tkfocus(tt)
}
.cost.stratAggreg <-
function(){




try(require(tcltk))
try(require(tcltk2))
try(library(Rcmdr))
try(require(tkrplot))
try(tclRequire("BWidget"))
try(library(COSTcore))
try(library(COSTeda));try(library(COSTgui));try(library(COSTdbe));try(library(COSTdbe))



tdspinner <- function(parent, ...) {tkwidget(parent, "spinbox", ...)}


valid<-function(){



# puis on fait stratAggreg

cmd2<-paste(tclvalue(val.out),"<-stratAggreg(object=",tclvalue(lechoix.but.dbeOutput),sep="")


cmd2<-paste(cmd2,",timeStrata=",as.logical(as.numeric(tclvalue(cbValue.timeStrata))),
",techStrata=",as.logical(as.numeric(tclvalue(cbValue.techStrata))),
",spaceStrata=",as.logical(as.numeric(tclvalue(cbValue.spaceStrata))),
",incl.precision=",as.logical(as.numeric(tclvalue(cbValue.incl.precision))),
sep="")




if (tclvalue(lechoix.p1)!="0.025" & tclvalue(cbValue.incl.precision) !="0"){
tclvalue(lechoix.p2)<- round(as.numeric(1-as.numeric(tclvalue(lechoix.p1))),3)
cmd2<-paste(cmd2,paste(",probs=c(",tclvalue(lechoix.p1),",",tclvalue(lechoix.p2),")",sep=""),sep="")
}


if (tclvalue(tvarwt) != "NULL"){
cmd2<-paste(cmd2,",wt='",tclvalue(tvarwt),"'",sep="")
}


cmd2<-paste(cmd2,")",sep="")
print(cmd2)
doItAndPrint(cmd2)



}


slotNamesdata<-function(data){
ploom<-slotNames(data)
out<-c()
for (p in ploom){
if(is(slot(data, p))[1] == "data.frame"){
out<-c(out,p)
}
}

return(out)
}



choixdataset.multi<-function(id,nom.but){
print(id)
try(tkdestroy(tt2),silent=T)
tt2<-tktoplevel()
tkwm.geometry(tt2, "-100+50")     
listdata <-NULL
for ( i in id){listdata <- c(listdata,malist(i))}
dataSetsBox <- variableListBox(tt2, listdata, title=gettext(domain="R-COSTgui","Select one dataset"))# voir initialselection
tkgrid(getFrame(dataSetsBox), sticky="nwes",column=1,row=1)
  tkgrid.rowconfigure(getFrame(dataSetsBox), 1, weight = 1)
  tkgrid.rowconfigure(tt2, 1, weight = 1)  
onOK <- function(){	
out<-getSelection(dataSetsBox)

		if ( length(out) != 0){
		eval(parse(text=paste("tclvalue(lechoix.",nom.but,")<-out",sep="")))
		tkconfigure(eval(as.name(nom.but)),text=out)
		# print(GetIt((eval(as.name(paste("lechoix.",nom.but,sep=""))))))
		# tkconfigure( names.list.quali, state="disabled" )	
		# tkconfigure( names.list.quanti, state="disabled" )
		
		

		# print("on met a jour la liste des variables")

		# a ce niveau on a le nom du cs objet, on va lister toute les variaible exploitable pour le spaceplot
	if (nom.but == "but.dbeOutput"){

	
	# on charge la description
	tclvalue(val.desc)<-eval(as.name(out))@desc
	tclvalue(val.param)<-eval(as.name(out))@param
	# tclvalue(val.param)<-
	tclvalue(val.methodDesc)<-eval(as.name(out))@methodDesc	
	tclvalue(val.catchCat)<-eval(as.name(out))@catchCat
	tclvalue(val.species)<-eval(as.name(out))@species
	tclvalue(val.timeStrata)<-eval(as.name(out))@strataDesc@timeStrata
	tclvalue(val.spaceStrata)<-eval(as.name(out))@strataDesc@spaceStrata
	tclvalue(val.techStrata)<-eval(as.name(out))@strataDesc@techStrata
	
	
	# on active si pas number
	if (tclvalue(val.param) !="param"){
	
	tkconfigure(rbtotalW,state="normal")
	tkconfigure(rbtotalN,state="normal")
	}
	
	
	if (tclvalue(val.param) =="param"){
	tclvalue(tvarwt)<-"NULL"
	tkconfigure(rbtotalW,state="disabled")
	tkconfigure(rbtotalN,state="disabled")
	
	}
	
	# [1] "quarter"

# Slot "spaceStrata":
# [1] NA

# Slot "techStrata":
	
	
  # listslot<-slotNamesdata(eval(as.name(out)))
# araj<-c()
# for ( i in 1:length(listslot)){	
# truc<-do.call("slot",args =list(as.name(out),listslot[i]))
# araj<-unique(c(araj,levels(as.factor(truc[["spp"]]))));araj
# }
# tkdelete(list.species,"0","end");
# for (var in araj) {tkinsert(list.species, "end", var)}

# on check les LAN et DIS

	# LD<-unique(substr(eval(as.name(out))@hl$sort,1,3))
	# print(LD)
# if(!is.element("DIS",LD)){
# print("pas de DIS")
# tkconfigure(rbdis,state="disabled")
# tclvalue(tvarcatchCat)<-"LAN"

# }
# if(!is.element("LAN",LD)){
# print("pas de LAN")
# tkconfigure(rblan,state="disabled")
# tclvalue(tvarcatchCat)<-"DIS"
# }

		}
	
		
		
		
# if ( 		tclvalue(lechoix.but.csObject)!= "" & tclvalue(lechoix.but.dbeOutput)!= ""){

tkconfigure(valid.but,state="normal")

# }
		
# aactiver<-list(entry.ylab,entry.xlab,list.time,list.tech,list.space,but.refr,entry.fonction,entry.main,cb.simu,cb.auto,list.time,list.tech,spin.x1,spin.x2,spin.y1,spin.y2,rbsel.image,rbsel.contour,rbsel.bubble,rbsel.value)
# # for ( i in 1:length(aactiver)){tkconfigure(aactiver[[i]],state="normal")}

# lapply(aactiver,tkconfigure,state="normal")

# choixtime<-c("year","semester","quarter","month")
# choixspace<-c("rect","subrect","area")
# choixtech<-c("commCat","commCatScl", "foCatEu5", "foCatEu6", "foCatNat","catchCat")
# tkdelete(list.time,"0","end");for (var in choixtime) {tkinsert(list.time, "end", var)}
# tkdelete(list.tech,"0","end");for (var in choixtech) {tkinsert(list.tech, "end", var)}
# tkdelete(list.space,"0","end");for (var in choixspace) {tkinsert(list.space, "end", var)}


	
		
}
tkdestroy(tt2)	
		
}
	
OK.but <-tkbutton(tt2,text=gettext(domain="R-COSTgui","   OK   "),command=onOK)
tkgrid(OK.but,column=1,row=2)
tkfocus(tt2)
}

#fonctions pratique pour tk
malist<-function (pat) {

# retrouve dans ls les element de type pat
    Vars <- ls(all.names = TRUE,env=.GlobalEnv)
	# print(Vars)
    if (length(Vars) == 0) {return(Vars)}
    names(which(sapply(Vars, function(.x) {	is(get(.x))[1]==pat	}	)	)	)
	}
	
getcontentlistbox<-function(listbox){
out<-c()
i<-0		
while (length(as.character(tkget(listbox,i)))!=0){
out<-c(out,as.character(tkget(listbox,i)))
i<-i+1
}
return(out)
}

GetIt<-function(objet){
if (attr(objet,"class")=="tkwin"){
		if (as.character(tkwinfo("class",objet))=="Listbox"){
		return(getcontentlistbox(objet))
		}
}
if (attr(objet,"class")=="tclVar"){
return(tclvalue(objet))

}
}

DoItmini<-function(comm,env=.GlobalEnv){
  result <- parse(text = paste(comm))
  exprs<-result
  i<-1
  #for (i in seq_along(exprs)) {
  ei <- exprs[i]
  print(ei)
  result <- eval(ei, envir = env)
  
}

reset.strIni<-function(){
tkconfigure(but.strini,text=gettext(domain="R-COSTgui","<select>"))
tclvalue(lechoix.but.strini)<-""
}

# choixrb<-function(){
# if("NULL" != tclvalue(tvarmethodDesc) & "NULL" != tclvalue(tvarcatchCat)){

# tkconfigure(valid.but,state="active")

# }

# }

proba<-function(val){
tclvalue(lechoix.p2)<- round(as.numeric(1-as.numeric(tclvalue(lechoix.p1))),3)
}

checkind<-function(){
tkconfigure(spin.p1,state=c("disabled","normal")[as.numeric(tclvalue(cbValue.incl.precision))+1])}

checkfill<-function(){

tkconfigure(spin.p,state=c("disabled","normal")[as.numeric(tclvalue(cbValue.fillGaps))+1])}


# lechoix.but.csObject <- tclVar("")
lechoix.but.dbeOutput <- tclVar("")
# lechoix.but.strini <- tclVar("")
lechoix.p1 <- tclVar(0.025)
lechoix.p2 <- tclVar(0.975)
lechoix.p <- tclVar(10) # a voir si c 'est bien par defaut

tt <- tktoplevel(borderwidth=10)
tkwm.geometry(tt, "-100+50")
font2<-tkfont.create(family="Times",size=12,weight="bold")
font3<-tkfont.create(family="Times",size=12)
fontheading<-tkfont.create(family="Times",size=18,weight="bold")  
tkwm.title(tt,gettext(domain="R-COSTgui","COST - Agregate strata"))
tkgrid(tklabel(tt, text=gettext(domain="R-COSTgui","Agregate strata"),font=fontheading))
tkgrid(tklabel(tt,text="")) # Ligne de blanc
frameTOP<- tkframe(tt, borderwidth=2, relief="groove")
frameBONUS<- tkframe(tt, borderwidth=2, relief="groove")
# frameA<- tkframe(tt, borderwidth=2, relief="groove")
# frameB<- tkframe(tt, borderwidth=2, relief="groove")
# frameC<- tkframe(tt, borderwidth=2, relief="groove")
frameD<- tkframe(tt, borderwidth=2, relief="groove")
# frameE<- tkframe(frameTOP, borderwidth=2, relief="groove")
frameF<- tkframe(tt, borderwidth=2, relief="groove")
framewt<- tkframe(frameF, borderwidth=2, relief="groove")



# les entry
val.desc <- tclVar("")
entry.desc <-tkentry(frameTOP,width="20",textvariable=val.desc,state="disabled")
val.species <- tclVar("")
entry.species <-tkentry(frameTOP,width="20",textvariable=val.species,state="disabled")
val.out <- tclVar("aggreg")
entry.out <-tkentry(frameD,width="20",textvariable=val.out)
val.param <- tclVar("")
entry.param <-tkentry(frameTOP,width="20",textvariable=val.param,state="disabled")
val.methodDesc <- tclVar("")
entry.methodDesc <-tkentry(frameTOP,width="20",textvariable=val.methodDesc,state="disabled")
val.catchCat <- tclVar("")
entry.catchCat <-tkentry(frameTOP,width="20",textvariable=val.catchCat,state="disabled")
val.timeStrata <- tclVar("")
entry.timeStrata <-tkentry(frameTOP,width="20",textvariable=val.timeStrata,state="disabled")


val.techStrata <- tclVar("")
entry.techStrata <-tkentry(frameTOP,width="20",textvariable=val.techStrata,state="disabled")


val.spaceStrata <- tclVar("")
entry.spaceStrata <-tkentry(frameTOP,width="20",textvariable=val.spaceStrata,state="disabled")


val.wt <- tclVar("totalW")
entry.wt <-tkentry(frameF,width="8",textvariable=val.wt,state="normal")


# methodDesc	
	# eval(as.name(out))@catchCat
# # val.param <- tclVar("")
# entry.param <-tkentry(frameTOP,width="20",textvariable=val.param)

#les combo

# listtype<-c("p", "fixedK", "propK", "agesK")
# combo.type <- tkwidget(frameF,"ComboBox",editable=FALSE,values=listtype,state="normal")
# listbootMethod<-c("samples","otoliths")
# combo.bootMethod <- tkwidget(frameF,"ComboBox",editable=FALSE,values=listbootMethod,state="normal")

#les spin
spin.p1<-tdspinner(frameF,from=0,to=1,inc=0.001,width=5,textvariable = lechoix.p1,state="normal",command=function(...)proba(1))
spin.p2<-tdspinner(frameF,from=0,to=1,inc=0.001,width=5,textvariable = lechoix.p2,state="disabled",command=function(...)proba(2))
spin.p<-tdspinner(frameF,from=1,to=100,inc=1,width=5,textvariable = lechoix.p,state="disabled")


# les radio
tvarwt <- tclVar("totalW")
# tvarwt <- "NULL"
rbtotalW <- tkradiobutton(frameF)
rbtotalN <- tkradiobutton(frameF)
tkconfigure(rbtotalW,variable=tvarwt,value="totalW",state="disabled")
tkconfigure(rbtotalN,variable=tvarwt,value="totalN",state="disabled")


# tvarmethodDesc <- tclVar("NULL")
# rbanal <- tkradiobutton(frameC,command=function()choixrb())
# rbboot <- tkradiobutton(frameC,command=function()choixrb())
# tkconfigure(rbanal,variable=tvarmethodDesc,value="analytic",state="disabled")
# tkconfigure(rbboot,variable=tvarmethodDesc,value="bootstrap",state="disabled")
#les bouton


# but.strini <- tkbutton(frameA,text=gettext(domain="R-COSTgui","<select>"),state="active",command=function(...) choixdataset.multi("strIni","but.strini"),foreground="red",relief="ridge")
# but.newstrini <- tkbutton(frameA,text=gettext(domain="R-COSTgui","New strIni type object"),state="active",command=function(...) .cost.strIni())#,foreground="red",relief="ridge")
# but.resetstrini <- tkbutton(frameA,text="reset",state="active",command=function(...) reset.strIni())#,foreground="red",relief="ridge")
valid.but <- tkbutton(frameD,text=gettext(domain="R-COSTgui","Validate"),state="disabled",command=function(...) valid())

# but.csObject <- tkbutton(frameBONUS,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset.multi(c("csDataCons"),"but.csObject"),foreground="red",relief="ridge")

but.dbeOutput <- tkbutton(frameBONUS,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset.multi(c("dbeOutput"),"but.dbeOutput"),foreground="red",relief="ridge")

# les check

cbValue.incl.precision<-tclVar("1")
cb.incl.precision <- tkcheckbutton(frameF,variable=cbValue.incl.precision,state="normal",command=function()checkind())

cbValue.timeStrata<-tclVar("0")
cb.timeStrata <- tkcheckbutton(frameF,variable=cbValue.timeStrata,state="normal")

cbValue.techStrata<-tclVar("0")
cb.techStrata <- tkcheckbutton(frameF,variable=cbValue.techStrata,state="normal")

cbValue.spaceStrata<-tclVar("0")
cb.spaceStrata <- tkcheckbutton(frameF,variable=cbValue.spaceStrata,state="normal")

# cbValue.fillGaps<-tclVar("0")
# cb.fillGaps <- tkcheckbutton(frameF,variable=cbValue.fillGaps,state="normal",command=function()checkfill())

# cbValue.trace<-tclVar("0")
# cb.trace <- tkcheckbutton(frameF,variable=cbValue.trace,state="normal")


#text

text.valid<-tklabel(frameD,text=gettext(domain="R-COSTgui","out :"))
#list

# list.species <-tklistbox(frameE,selectmode="unique",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.species.scroll,...))
# list.species.scroll<-tkscrollbar(frameE,repeatinterval=5,command=function(...)tkyview(list.species,...))

tkgrid(frameBONUS)
tkgrid(frameTOP)
# tkgrid(frameA)
# tkgrid(frameB)
# tkgrid(frameC)
tkgrid(frameF)

tkgrid(frameD)

# tkgrid(tklabel(frameBONUS,text="csDataCons:",font=font2),but.csObject)
tkgrid(tklabel(frameBONUS,text=gettext(domain="R-COSTgui","dbeOutput:"),font=font2),but.dbeOutput)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Description : ")),entry.desc)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Catch component : ")),entry.catchCat)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Method description : ")),entry.methodDesc)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Species : ")),entry.species)


tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Parameter : ")),entry.param)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Technical strata : ")),entry.techStrata)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Temporal strata : ")),entry.timeStrata)
tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Space strata : ")),entry.spaceStrata)
# tkgrid(tklabel(frameE,text=gettext(domain="R-COSTgui","Species"),font=font2))
# tkgrid(list.species,list.species.scroll,sticky="nswe")entry.methodDesc <-tkentry(frameTOP,width="20",textvariable=val.methodDesc,state="disabled")
# val.catchCat <- tclVar("")
# entry.catchCa



# tkgrid(frameE,columnspan=2)#,sticky="ew")

# tkgrid(tklabel(frameA,text=gettext(domain="R-COSTgui","StrIni : "),font=font2),but.strini,but.newstrini,but.resetstrini)
# tkgrid(tklabel(frameB,text="LAN : ",font=font2),rblan,tklabel(frameB,text="DIS : ",font=font2),rbdis)
# tkgrid(tklabel(frameC,text="analytic : ",font=font2),rbanal,tklabel(frameC,text="bootstrap : ",font=font2),rbboot)

tkgrid(frameF)

# tkgrid(tklabel(frameF,text="type : "),combo.type)
# tkgrid(tklabel(frameF,text="bootMethod : "),combo.bootMethod)
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","Temporal strata : ")),cb.timeStrata)
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","Technical strata : ")),cb.techStrata)
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","Space strata : ")),cb.spaceStrata)

tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","Weighting : ")),framewt)

tkgrid(tklabel(framewt,text=gettext(domain="R-COSTgui","Total weight : ")),rbtotalW)
tkgrid(tklabel(framewt,text=gettext(domain="R-COSTgui","Total number : ")),rbtotalN)

# tkgrid(tklabel(frameF,text="trace : "),cb.trace)
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","Including precision : ")),cb.incl.precision)
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","Probabilities : ")),spin.p1,spin.p2)
tkgrid(text.valid,entry.out,valid.but)


tkfocus(tt)

}
.cost.strIni <-
function(){

try(require(tcltk))
try(require(tcltk2))
try(library(Rcmdr))
try(require(tkrplot))
try(tclRequire("BWidget"))
try(library(COSTcore))
try(library(COSTeda));try(library(COSTgui))

#fonctions pratique pour tk

getcontentlistbox<-function(listbox){
out<-c()
i<-0		
while (length(as.character(tkget(listbox,i)))!=0){
out<-c(out,as.character(tkget(listbox,i)))
i<-i+1
}
return(out)
}

GetIt<-function(objet){
if (attr(objet,"class")=="tkwin"){
		if (as.character(tkwinfo("class",objet))=="Listbox"){
		return(getcontentlistbox(objet))
		}
}
if (attr(objet,"class")=="tclVar"){
return(tclvalue(objet))

}
}

DoItmini<-function(comm,env=.GlobalEnv){
  result <- parse(text = paste(comm))
  exprs<-result
  i<-1
  #for (i in seq_along(exprs)) {
  ei <- exprs[i]
  #verbose#print(ei)
  result <- eval(ei, envir = env)
  
}

valid<-function(){
# yo<-strIni(timeStrata="month", techStrata="foCatNat",spaceStrata="NA")

# t1<-as.character(choixtime[as.numeric(tclvalue(tcl(combo.timestrata,"getvalue")))+1])
# t2<-as.character(choixtech[as.numeric(tclvalue(tcl(combo.techstrata,"getvalue")))+1])
# t3<-as.character(choixspace[as.numeric(tclvalue(tcl(combo.spacestrata,"getvalue")))+1])
t1<-NULL
t2<-NULL
t3<-NULL

try(silent=T,t1<-tclvalue(tkget(list.timestrata,as.character(tkcurselection(list.timestrata)))) )
try(silent=T,t2<-tclvalue(tkget(list.techstrata,as.character(tkcurselection(list.techstrata)))) )
try(silent=T,t3<-tclvalue(tkget(list.spacestrata,as.character(tkcurselection(list.spacestrata))))) 
t1;t2;t3

if(length(t1)==0){t1<-"NA"}
if(length(t2)==0){t2<-"NA"}
if(length(t3)==0){t3<-"NA"}
if(t1=="tout") {t1<-"NA"}
if(t2=="tout"){t2<-"NA"}
if(t3=="tout"){t3<-"NA"}
t1;t2;t3



cmd1<-paste(tclvalue(val.valid),"<-strIni(",sep="");i<-0

# pour résoudre le bug.
if ( t1 == "NA"){
#verbose#print("on impose timestrata = year")
t1<-"year"}


if(t1!="NA"){cmd1<-paste(cmd1,"timeStrata='",t1,"'",sep="");i<-i+1}
if(t2!="NA"){if (i==1){cmd1<-paste(cmd1,",",sep="")};cmd1<-paste(cmd1,"techStrata='",t2,"'",sep="");i<-i+1}
if(t3!="NA"){if (i>=1){
cmd1<-paste(cmd1,",",sep="")};
cmd1<-paste(cmd1,"spaceStrata='",t3,"'",sep="")
;i<-i+1}


if (tclvalue(from.tc)!="" & tclvalue(to.tc)!=""){
cmd1<-paste(cmd1,",tcRec= list(from=",tclvalue(from.tc),",to=",tclvalue(to.tc),")",sep="")
}


if (tclvalue(from.tp)!="" & tclvalue(to.tp)!=""){
cmd1<-paste(cmd1,",tpRec= list(from=",tclvalue(from.tp),",to=",tclvalue(to.tp),")",sep="")
}

if (tclvalue(from.sp)!="" & tclvalue(to.sp)!=""){
cmd1<-paste(cmd1,",spRec= list(from=",tclvalue(from.sp),",to=",tclvalue(to.sp),")",sep="")
}


cmd1<-paste(cmd1,");",tclvalue(val.valid),sep="");cmd1
doItAndPrint(cmd1)

   # tcRec = list(from=c("2","3"),
                # to = c("2+3","2+3")))


}


choixrecode<-function(id){

#verbose#print(id)
tt2<-tktoplevel()
tkwm.geometry(tt2, "-100+50")
tkwm.title(tt2,paste(sep="",id,"Rec"))
tkgrid(tklabel(tt2, text=gettext(paste(sep="",id,"Rec")),font=fontheading),column=2,row=1)

onOK<-function(){
#verbose#print("onOK")	
}
populate<-function(id){
#verbose#print(id)


col1<-get("col1",1)
col2<-get("col2",1)
# # print(col1)
# # print(col2)
#on va modifier les 2 entry
# as.character(col1[as.numeric(tclvalue(tcl(combo.col1,"getvalue")))+1])
# tclvalue(from.tc)<-paste(tclvalue(label.tc),"$",
# as.character(col1[as.numeric(tclvalue(tcl(combo.col1,"getvalue")))+1])
# ,sep="")


if (length(as.character(col1[as.numeric(tclvalue(tcl(combo.col1,"getvalue")))+1]))!=0 &
length(as.character(col2[as.numeric(tclvalue(tcl(combo.col2,"getvalue")))+1]))!=0 ){


eval(parse(text=paste("tclvalue(from.",id,")<-paste(sep='',tclvalue(label.",id,"),'$','",as.character(col1[as.numeric(tclvalue(tcl(combo.col1,"getvalue")))+1]),"')",sep="")))
eval(parse(text=paste(  "tclvalue(to.",id,")<-paste(sep='',tclvalue(label.",id,"),'$','",as.character(col2[as.numeric(tclvalue(tcl(combo.col2,"getvalue")))+1]),"')",sep="")))

}

# tclvalue(to.tc)<-  paste(tclvalue(label.tc),"$",as.character(col2[as.numeric(tclvalue(tcl(combo.col2,"getvalue")))+1]),sep="")



}

loaddataset <-function(id){
#verbose#print("load")

# on ouvre une tit fenetre avec lesep, le nom et parcourir

tt4<-tktoplevel()
tkwm.geometry(tt4, "-500+50")   
lesep <- tclVar(",")# séparateur par defaut
entry.sep<-tkentry(tt4,width="2",textvariable=lesep)
chemin.nomdata <- tclVar("")
entry.chemin <-tkentry(tt4,width="20",textvariable=chemin.nomdata)
val.nomdata <- tclVar("NewData")
entry.nomdata <-tkentry(tt4,width="20",textvariable=val.nomdata)
but.parcour <- tkbutton(tt4,text=gettext(domain="R-COSTgui","Browse"),state="active",command=function(...) parcour())
valid.but <- tkbutton(tt4,text=gettext(domain="R-COSTgui","Validate"),state="active",command=function(...) valid())

tkgrid(tklabel(tt4,text=gettext(domain="R-COSTgui","out objetc :")),row=1,column=1)
tkgrid(entry.nomdata,row=1,column=2,columnspan=2)
tkgrid(tklabel(tt4,text=gettext(domain="R-COSTgui","path :")),row=2,column=1)
tkgrid(entry.chemin,row=2,column=2,columnspan=2)
tkgrid(but.parcour,row=2,column=4)
tkgrid(valid.but,row=3,column=4)
tkgrid(tklabel(tt4,text=gettext(domain="R-COSTgui","sep :")),row=3,column=2)
tkgrid(entry.sep,row=3,column=3)

valid<-function(){
 #verbose#print("ici valid")
 # on verifie que tout est pas vide.
 
if (tclvalue(chemin.nomdata) != "" & tclvalue(val.nomdata) != "" & tclvalue(lesep) !=""){
cmd<- paste(tclvalue(val.nomdata),"<-read.csv2('",tclvalue(chemin.nomdata),"',sep='",tclvalue(lesep),"')",sep="");cmd

eval(parse(text=cmd))
# faut mettre ce nouveau jeu de donnée dans le truc principale

# assign(tclvalue(val.nomdata),as.name(tclvalue(val.nomdata)))
 assign(tclvalue(val.nomdata),eval(as.name(tclvalue(val.nomdata))),pos=1)
# assign("col1",names(eval(as.name(out))),pos=1)


# " on charge le jeu dans l'interface principale

	# tkdestroy(tt3)	

	
# on valide le jdd
out<-tclvalue(val.nomdata)
#verbose#print("voila2:")
#verbose#print(out)
#verbose#print(head(eval(as.name(out))))
		# #verbose#print(GetIt((eval(as.name(paste("label.tc"))))))
		
		#verbose#print("id")
		#verbose#print(id)
		# if ( length(out) != 0){
		
		
		# DoItmini(paste("tclvalue(choix",id,")<-out",sep=""),env=e1)
		
		
		eval(parse(text=paste("tclvalue(label.",id,")<-out",sep="")))
		#verbose#print("bon")
		

		# # DoItmini(paste("tkconfigure(choice.",id,"Data.but, text=out)",sep=""))
		tkconfigure(eval(as.name("but.selecttab")),text=out)
		
		# print(GetIt((eval(as.name(paste("choix",id,sep=""))))))
		# on peuple les combo
		
	#verbose#print("bon2")
		eval(parse(text=paste("col1<-names(eval(as.name(out)))",sep="")))
		eval(parse(text=paste("col2<-names(eval(as.name(out)))",sep="")))
	#verbose#print("bon3")
	#verbose#print(col1)
	#verbose#print(col2)
		tkconfigure( combo.col1, state="normal" ,values=col1)	
		tkconfigure( combo.col2, state="normal" ,values=col2)
			#verbose#print("bon4")
		 assign("col1",names(eval(as.name(out))),pos=1)
		 assign("col2",names(eval(as.name(out))),pos=1)
			#verbose#print("bon5")
			#verbose#print("bon5")
		# "on active le bouton edit
		tkconfigure(but.edittab,state="normal")
		tkconfigure(but.savetab,state="normal")
	#verbose#print("bon6")

	tkdestroy(tt4)
}
 
 }

 parcour<-function(){
  chemin<-"NULL"
  # chemin<-file.choose()
chemin<-tclvalue(tkgetOpenFile(filetypes = "{{csv} {.csv}}"))
  
  
  
  
  if (chemin!="NULL"){
  tclvalue(chemin.nomdata)<-paste(strsplit(chemin,"\\\\")[[1]],collapse="/")
  
}


}



}

savedataset <-function(id){
#verbose#print("save")
# paste("tclvalue(label.",id,")",sep="")

chemin<-"NULL"
# chemin<-file.choose()
chemin<-tclvalue(tkgetSaveFile())
  
  
  
  
  if (chemin!="NULL"){
  #verbose#print(paste(sep="","write.csv2(",eval(parse(text=paste("tclvalue(label.",id,")",sep=""))),",file='",paste(strsplit(chemin,"\\\\")[[1]],collapse="/"),"')"))
  eval(parse(text=(paste(sep="","write.csv2(row.names = FALSE,",eval(parse(text=paste("tclvalue(label.",id,")",sep=""))),",file='",paste(strsplit(chemin,"\\\\")[[1]],collapse="/"),"')"))))
  }







}

editdataset <-function(id){
# #verbose#print(paste("tclvalue(label.",id,")<-edit(eval(as.name(tclvalue(label.",id,"))))",sep=""))

# eval(parse(text=paste("tclvalue(label.",id,")",sep="")))

eval(parse(text=paste("tclvalue(label.",id,")<-edit(eval(as.name(tclvalue(label.",id,"))))",sep="")))

#verbose#print("ok")


 assign(tclvalue(label.tc),eval(as.name(tclvalue(label.tc))),pos=1)
 # #verbose#print(names(eval(as.name(tclvalue(label.tc)))))
		eval(parse(text=paste("col1<-names(eval(as.name(tclvalue(label.tc))))",sep="")))
		eval(parse(text=paste("col2<-names(eval(as.name(tclvalue(label.tc))))",sep="")))

		tkconfigure( combo.col1, state="disabled" ,values=c(""))	
		tkconfigure( combo.col2, state="disabled" ,values=c(""))	
		# # tkconfigure( combo.col1, state="normal" ,values=c())	
		# # #verbose#print(col1)
		tkconfigure( combo.col1, state="normal" ,values=col1)	
		tkconfigure( combo.col2, state="normal" ,values=col2)
 assign("col1",names(eval(as.name(tclvalue(label.tc)))),pos=1)
		 assign("col2",names(eval(as.name(tclvalue(label.tc)))),pos=1)
}


creattab <-function(id){
malist<-function (pat) {

# retrouve dans ls les element de type pat
    Vars <- ls(all.names = TRUE,env=.GlobalEnv)
	# #verbose#print(Vars)
    if (length(Vars) == 0) {return(Vars)}
    names(which(sapply(Vars, function(.x) {	is(get(.x))[1]==pat	}	)	)	)
	}

choixdataset.multi<-function(ido,nom.but){


#verbose#print(ido)
try(tkdestroy(tt8),silent=T)
tt8<-tktoplevel()
tkwm.geometry(tt8, "-100+50")     
listdata <-NULL
for ( i in ido){listdata <- c(listdata,malist(i))}
dataSetsBox <- variableListBox(tt8, listdata, title=gettext(domain="R-COSTgui","Select one dataset"))# voir initialselection
tkgrid(getFrame(dataSetsBox), sticky="nwes",column=1,row=1)
  tkgrid.rowconfigure(getFrame(dataSetsBox), 1, weight = 1)
  tkgrid.rowconfigure(tt8, 1, weight = 1)  
onOK <- function(){	
out<-getSelection(dataSetsBox)
		if ( length(out) != 0){
		eval(parse(text=paste("tclvalue(lechoix.",nom.but,")<-out",sep="")))
		tkconfigure(eval(as.name(nom.but)),text=out)
		# araj<-sort(lesvar(lechoix.but.csOBJECT)$quanti)
		# tkdelete(list.variables,"0","end");for (var in araj) {tkinsert(list.variables, "end", var)}
	# a ce niveau on sait quelle strate nous interesse
		
		# si sp on charge tous les facteur de area, rect, subrect
		# si tp on charge year semester quarter month
		# si tc CommCat
		# choixtime<-c("tout","year","semester","quarter","month")
# choixspace<-c("tout","rect","subrect","area")
# choixtech<-c("tout","commCat","commCatScl", "foCatEu5", "foCatEu6", "foCatNat","catchCat")
	
		# on récupere
		# eval(paste(text=(paste(tclvalue(newtab),"$old<-lesfact",sep=""))))
		
}
tkdestroy(tt8)	
		
}
	
OK.but <-tkbutton(tt8,text="   OK   ",command=onOK)
tkgrid(OK.but,column=1,row=2)
tkfocus(tt8)
}

actpreload<-function(){
if (tclvalue(cbValue.preload)=="1"){tkconfigure(but.csOBJECT,state="normal")}
if (tclvalue(cbValue.preload)=="0"){tkconfigure(but.csOBJECT,state="disabled")}

}
#verbose#print("creatab")
try(tkdestroy(tt4),silent=T)
# try(tkdestroy(tt2),silent=T) # c ne fonctionne pas vraiment, je voudrais que ca ferme la fenetre, ou vrifie avant d'en ouvrir une novuelle
tt4<-tktoplevel()
tkwm.geometry(tt4, "-500+50")     

newtab <- tclVar("newTAB")
entry.newtab <-tkentry(tt4,width="20",textvariable=newtab)

cbValue.preload <- tclVar("0")
cb.preload <- tkcheckbutton(tt4,variable=cbValue.preload,command=function(...)actpreload())

lechoix.but.csOBJECT <- tclVar("")
but.csOBJECT <- tkbutton(tt4,text=gettext(domain="R-COSTgui","<dataset>"),state="disabled",command=function(...) choixdataset.multi(c("csData","clData","ceData","clDataVal","ceDataVal","csDataVal"),"but.csOBJECT"),foreground="red",relief="ridge")


OK4.but <-tkbutton(tt4,text="   OK   ",command=function() onOK4())

tkgrid(tklabel(tt4,text=gettext(domain="R-COSTgui","name : "),font=font2),row=1,column=1)
tkgrid(entry.newtab,row=1,column=2,columnspan=2)
tkgrid(tklabel(tt4,text=gettext(domain="R-COSTgui","preload : ")),row=2,column=1)
tkgrid(cb.preload,row=2,column=2)
tkgrid(but.csOBJECT,row=2,column=3)
tkgrid(OK4.but,row=3,column=2)



onOK4<-function(){

slotNamesdata<-function(data){
ploom<-slotNames(data)
out<-c()
for (p in ploom){
if(is(slot(data, p))[1] == "data.frame"){
out<-c(out,p)
}
}

return(out)
}
#verbose#print("on valide")
# tclvalue(newtab)
eval(parse(text=paste(tclvalue(newtab),"<-data.frame(matrix(0,1,2))",sep="")))
eval(parse(text=paste("names(",tclvalue(newtab),")<-c('old','new')",sep="")))
eval(parse(text=paste(tclvalue(newtab),"$old<-as.character(",tclvalue(newtab),"$old)",sep="")))
eval(parse(text=paste(tclvalue(newtab),"$new<-as.character(",tclvalue(newtab),"$new)",sep="")))


# sil ya eu preload on fait les modif ici
tiyop<-tclvalue(lechoix.but.csOBJECT)
if (tclvalue(cbValue.preload)=="1" & tiyop!=""){
# #verbose#print("on est dans preload")
	# #verbose#print(id)
		# # if ( id=="sp"){lall<-c("rect","subrect","area")}
		# # if ( id=="tc"){lall<-c("commCat","commCatScl", "foCatEu5", "foCatEu6", "foCatNat","catchCat")}
		# # if ( id=="tp"){lall<-c("year","semester","quarter","month")}
		
		# en fait non on va charger que la strate selectionne
		list.ok<-eval(as.name(paste("list.",c("space","tech","time")[which(id==c("sp","tc","tp"))],"strata",sep="")))
		lall<-tclvalue(tkget(list.ok,as.character(tkcurselection(list.ok))))
		
		#verbose#print(lall)
		lesfact<-c()
		# #verbose#print(nom.but)
		# paste(sep="","tclvalue(lechoix.",nom.but,")")
		# #verbose#print(eval(parse(text=paste(sep="","tclvalue(lechoix.",nom.but,")"))))
		
		#verbose#print(tiyop)
		
		yop<-eval(as.name(tiyop))
		# #verbose#print(head(yop))
		for ( slo in slotNamesdata(yop) ){
		for ( i in 1:length(lall)){
		truc<-slot(yop,slo)
		# #verbose#print(names(truc))
		if (is.element(lall[i],names(truc))){lesfact<-unique(c(lesfact,levels(as.factor(truc[[lall[i]]]))))}
		}}
		#verbose#print(lesfact)


#verbose#print(head(eval(as.name(tclvalue(newtab)))))
# #verbose#print(paste(tclvalue(newtab),"$old<-lesfact",sep=""))
# eval(paste(text=(paste(tclvalue(newtab),"$old<-lesfact",sep=""))))
  # eval(parse(text=paste(tclvalue(newtab),"$old<-lesfact",sep="")))

  
  
eval(parse(text=paste(tclvalue(newtab),"<-data.frame(
cbind(
lesfact
,
rep('',length(lesfact))
)
)",sep="")))
eval(parse(text=paste("names(",tclvalue(newtab),")<-c('old','new')",sep="")))
# eval(parse(text=paste(tclvalue(newtab),"$old<-as.character(",tclvalue(newtab),"$old)",sep="")))
# eval(parse(text=paste(tclvalue(newtab),"$new<-as.character(",tclvalue(newtab),"$new)",sep="")))
  
  
  
  
  
  
  
  
  
  
  
  
#verbose#print(head(eval(as.name(tclvalue(newtab)))))

}




eval(parse(text=paste(tclvalue(newtab),"<-edit(",tclvalue(newtab),")",sep="")))

# assign(tclvalue(newtab),as.name(tclvalue(newtab)),pos=1)


 assign(tclvalue(newtab),eval(as.name(tclvalue(newtab))),pos=1)
try(tkdestroy(tt4),silent=T)
}



}


choixdataset<-function(id){
malist<-function (pat) {

# retrouve dans ls les element de type pat
    Vars <- ls(all.names = TRUE,env=.GlobalEnv)
	# print(Vars)
    if (length(Vars) == 0) {return(Vars)}
    names(which(sapply(Vars, function(.x) {	is(get(.x))[1]==pat	}	)	)	)
	}
	



#verbose#print(id)
try(tkdestroy(tt3),silent=T)
# try(tkdestroy(tt2),silent=T) # c ne fonctionne pas vraiment, je voudrais que ca ferme la fenetre, ou vrifie avant d'en ouvrir une novuelle
tt3<-tktoplevel()
tkwm.geometry(tt3, "-500+50")     
# listdata <- listDataSets()
# listdata <- c(malist("csData"),malist("ceData"),malist("clData"))
listdata <- malist("data.frame")
dataSetsBox <- variableListBox(tt3, listdata, title=gettext(domain="R-COSTgui","Select one dataset"))# voir initialselection
tkgrid(getFrame(dataSetsBox), sticky="nwes",column=1,row=1)
tkgrid.rowconfigure(getFrame(dataSetsBox), 1, weight = 1)
tkgrid.rowconfigure(tt3, 1, weight = 1)  
  					
		
	

onOK2 <- function(){	

out<-getSelection(dataSetsBox)
	
		#verbose#print("voila2:")
		#verbose#print(GetIt((eval(as.name(paste("label.tc"))))))
		
		#verbose#print("id")
		#verbose#print(id)
		if ( length(out) != 0){
		
		
		# DoItmini(paste("tclvalue(choix",id,")<-out",sep=""),env=e1)
		
		
		eval(parse(text=paste("tclvalue(label.",id,")<-out",sep="")))
		#verbose#print("bon")
		

		# # DoItmini(paste("tkconfigure(choice.",id,"Data.but, text=out)",sep=""))
		tkconfigure(eval(as.name("but.selecttab")),text=out)
		
		# print(GetIt((eval(as.name(paste("choix",id,sep=""))))))
		# on peuple les combo
		

		eval(parse(text=paste("col1<-names(eval(as.name(out)))",sep="")))
		eval(parse(text=paste("col2<-names(eval(as.name(out)))",sep="")))

		tkconfigure( combo.col1, state="normal" ,values=col1)	
		tkconfigure( combo.col2, state="normal" ,values=col2)
		
		 assign("col1",names(eval(as.name(out))),pos=1)
		 assign("col2",names(eval(as.name(out))),pos=1)
		
		# "on active le bouton edit
		tkconfigure(but.edittab,state="normal")
		tkconfigure(but.savetab,state="normal")
		
		# # for ( i in -1:3){
		# # # print(i)
		 # # try( print(sort(ls(env=as.environment(i)))))
		# # repo<-0
		# # try( repo<-sum(sort(ls(env=as.environment(i)))=="col1"))
		# # if( repo !=0){print(i)}
		# # }
		
		
		
		# on va assigner
		
		
		}
			tkdestroy(tt3)	
		
}
	
OK2.but <-tkbutton(tt3,text="   OK   ",command=onOK2)
tkgrid(OK2.but,column=1,row=2)
tkfocus(tt3)
}

fin<-function(){
tkdestroy(tt2)

}

entry.in  <-tkentry(tt2,width="20",textvariable=eval(as.name(paste(sep="","from.",id))))
entry.out <-tkentry(tt2,width="20",textvariable=eval(as.name(paste(sep="","to.",id))))

but.creattab <-tkbutton(tt2,text=gettext(domain="R-COSTgui","create"),command=function(...) creattab(id))
but.selecttab  <-tkbutton(tt2,text=tclvalue(eval(as.name(paste("label.",id,sep="")))),command=function(...) choixdataset(id),foreground="red",relief="ridge")
but.edittab  <-tkbutton(tt2,text=gettext(domain="R-COSTgui","edit"),command=function(...) editdataset(id),state="disabled")
but.savetab  <-tkbutton(tt2,text=gettext(domain="R-COSTgui","save as CSV"),command=function(...) savedataset(id),state="disabled")
but.loadtab  <-tkbutton(tt2,text=gettext(domain="R-COSTgui","load a CSV"),command=function(...) loaddataset(id),state="normal")
but.populate  <-tkbutton(tt2,text=gettext(domain="R-COSTgui","^ apply ^"),command=function(...) populate(id))
but.fin  <-tkbutton(tt2,text="   OK   ",command=function(...) fin())


col1<-c("")
col2<-c("")
combo.col1 <- tkwidget(tt2,"ComboBox",editable=FALSE,values=col1,state="disabled",command=function(...)print("zer"))
combo.col2 <- tkwidget(tt2,"ComboBox",editable=FALSE,values=col2,state="disabled",command=function(...)print("zer2"))

tkgrid(tklabel(tt2,text=gettext(domain="R-COSTgui","from : "),font=font2),row=2,column=0)
tkgrid(entry.in,row=2,column=1,columnspan=4,sticky="we")
tkgrid(tklabel(tt2,text=gettext(domain="R-COSTgui","to : "),font=font2),row=3,column=0)
tkgrid(entry.out,row=3,column=1,columnspan=4,sticky="we")

tkgrid(tklabel(tt2,text=gettext(domain="R-COSTgui","former :"),font=font2),row=5,column=0)
tkgrid(tklabel(tt2,text=gettext(domain="R-COSTgui","new :"),font=font2),row=6,column=0)

tkgrid(combo.col1,row=5,column=1,columnspan=2)
tkgrid(but.selecttab,row=5,column=3,sticky="we")
tkgrid(but.loadtab,row=5,column=4,sticky="we")
tkgrid(but.savetab,row=6,column=4,sticky="we")
tkgrid(combo.col2,row=6,column=1,columnspan=2)
tkgrid(but.creattab,row=7,column=3,sticky="we",columnspan=2)
tkgrid(but.edittab,row=6,column=3,sticky="we")

tkgrid(but.populate,row=4,column=2,columnspan=2,sticky="we")
tkgrid(but.fin,row=8,column=3,columnspan=1,sticky="we")

# OK.but <-tkbutton(tt2,text="   OK   ",command=onOK)
# tkgrid(OK.but)#,column=1,row=2)
tkfocus(tt2)






}

#le GUI
col1<-c("")
col2<-c("")
from.tc <- tclVar("")
to.tc   <- tclVar("")
label.tc   <- tclVar(gettext(domain="R-COSTgui","<select>"))

from.tp <- tclVar("")
to.tp   <- tclVar("")
label.tp   <- tclVar(gettext(domain="R-COSTgui","<select>"))

from.sp <- tclVar("")
to.sp   <- tclVar("")
label.sp   <- tclVar(gettext(domain="R-COSTgui","<select>"))

# # # # lechoixcl <- tclVar("")
# # # # lechoixce <- tclVar("")
# # e2 <- new.env(parent = .GlobalEnv)
# # assign("lechoixcs",lechoixcs,envir=e2)
# # assign("lechoixcl",lechoixcl,envir=e2)
tt <- tktoplevel(borderwidth=10)
tkwm.geometry(tt, "-100+50")
font2<-tkfont.create(family="Times",size=12,weight="bold")
font3<-tkfont.create(family="Times",size=12)
fontheading<-tkfont.create(family="Times",size=18,weight="bold")  
tkwm.title(tt,gettext(domain="R-COSTgui","COST - Stratification"))
tkgrid(tklabel(tt, text=gettext(domain="R-COSTgui","Stratification"),font=fontheading))
tkgrid(tklabel(tt,text="")) # Ligne de blanc
# frameTOP<- tkframe(tt, borderwidth=2, relief="groove")
frameD<- tkframe(tt, borderwidth=2, relief="groove")
frameE<- tkframe(tt, borderwidth=2, relief="groove")
frameF<- tkframe(tt, borderwidth=2, relief="groove")
frameB<- tkframe(tt, borderwidth=2, relief="groove")

but.recodetp <- tkbutton(frameE,text=gettext(domain="R-COSTgui","tpRec"),state="disabled",command=function(...) choixrecode("tp"))
but.recodetc <- tkbutton(frameD,text=gettext(domain="R-COSTgui","tcRec"),state="disabled",command=function(...) choixrecode("tc"))
but.recodesp <- tkbutton(frameF,text=gettext(domain="R-COSTgui","spRec"),state="disabled",command=function(...) choixrecode("sp"))
# but.csData <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset("cs"),foreground="red",relief="ridge")
# # but.ceData <- tkbutton(frameTOP,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset("ce"),foreground="red",relief="ridge")
# tkgrid(tklabel(frameTOP,text=gettext"CsData : ",font=font2),but.csData)
# # tkgrid(tklabel(frameTOP,text="ClData : ",font=font2),but.clData)
# # tkgrid(tklabel(frameTOP,text="CeData : ",font=font2),but.ceData)
# tkgrid(frameTOP)
tkgrid(frameD,sticky="we")
tkgrid(frameE,sticky="we")
tkgrid(frameF,sticky="we")
tvar <- tclVar(TRUE)
# rbseloui <- tkradiobutton(frameD,command=function()choixfraction())
# rbselnon <- tkradiobutton(frameD,command=function()choixfraction())
# tkconfigure(rbseloui,variable=tvar,value=TRUE,state="disabled")
# tkconfigure(rbselnon,variable=tvar,value=FALSE,state="disabled")
# tkgrid(tklabel(frameD,text="Selection - "),tklabel(frameD,text="Non : "),rbselnon,tklabel(frameD,text="oui : "),rbseloui)
# liste.spp <-tklistbox(frameE,selectmode="multiple",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(liste.spp.scroll,...))
# liste.spp.scroll<-tkscrollbar(frameE,repeatinterval=5,command=function(...)tkyview(liste.spp,...))
# tkgrid(liste.spp,liste.spp.scroll)
# on met un menu déroulant

# choixtype<-c("sl","ml","wl")
choixtime<-c("tout","year","semester","quarter","month")
choixspace<-c("tout","rect","subrect","area")
choixtech<-c("tout","commCat","commCatScl", "foCatEu5", "foCatEu6", "foCatNat","catchCat")


# timestrata : c 'est un parametre : month year quarter et semester
# space strata : parametre fixe ; rect, subrect, area


# # combo.timestrata <- tkwidget(frameE,"ComboBox",editable=FALSE,values=choixtime,state="normal",command=function() print("coucou"))
# # combo.techstrata <- tkwidget(frameD,"ComboBox",editable=FALSE,values=choixtech,state="normal",command=function() print("coucou"))
# # combo.spacestrata <- tkwidget(frameF,"ComboBox",editable=FALSE,values=choixspace,state="normal",command=function() print("coucou"))

list.timestrata <-tklistbox(frameE,selectmode="unique",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.timestrata.scroll,...))
list.timestrata.scroll<-tkscrollbar(frameE,repeatinterval=5,command=function(...)tkyview(list.timestrata,...))

list.techstrata <-tklistbox(frameD,selectmode="unique",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.techstrata.scroll,...))
list.techstrata.scroll<-tkscrollbar(frameD,repeatinterval=5,command=function(...)tkyview(list.techstrata,...))

list.spacestrata <-tklistbox(frameF,selectmode="unique",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.spacestrata.scroll,...))
list.spacestrata.scroll<-tkscrollbar(frameF,repeatinterval=5,command=function(...)tkyview(list.spacestrata,...))

# on charge
tkdelete(list.timestrata,"0","end")
for (var in choixtime) {tkinsert(list.timestrata, "end", var)}

tkdelete(list.techstrata,"0","end")
for (var in choixtech) {tkinsert(list.techstrata, "end", var)}

tkdelete(list.spacestrata,"0","end")
for (var in choixspace) {tkinsert(list.spacestrata, "end", var)}


tkgrid(tklabel(frameE,text=gettext(domain="R-COSTgui","Temporal strata :")),list.timestrata,list.timestrata.scroll,but.recodetp,sticky="ns")
tkgrid(tklabel(frameD,text=gettext(domain="R-COSTgui","Technical strata :")),list.techstrata,list.techstrata.scroll,but.recodetc,sticky="ns")
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","Space strata :")),list.spacestrata,list.spacestrata.scroll,but.recodesp,sticky="ns")

# tkconfigure(list.timestrata.scroll,sticky="ns")

frameA<- tkframe(tt, borderwidth=2, relief="groove")
valid.but <- tkbutton(frameA,text=gettext(domain="R-COSTgui","Validate"),state="normal",command=function(...) valid())

tkgrid(frameB)
tkgrid(frameA)
text.valid<-tklabel(frameA,text=gettext(domain="R-COSTgui","out :"))
val.valid <- tclVar("myStrIni")
entry.valid<-entry.chemin.rdata <-tkentry(frameA,width="20",textvariable=val.valid)
tkgrid(text.valid,entry.valid,valid.but)

actt<-function(truc){
list.ok<-eval(as.name(paste("list.",truc,"strata",sep="")))
but.ok<-eval(as.name(paste("but.recode",c("sp","tc","tp")[which(truc==c("space","tech","time"))],sep="")))
tkconfigure(but.ok,state=c("normal","disabled")[as.numeric(tclvalue(tkget(list.ok,as.character(tkcurselection(list.ok))))=="tout")+1])
}

# # tkbind(combo.timestrata, "<<ComboboxSelected>>",function(...)varvalid("Time"))
# # tkbind(combo.techstrata, "<<ComboboxSelected>>",function(...)varvalid("tech"))
# # tkbind(combo.spacestrata, "<<ComboboxSelected>>",function(...)varvalid("space"))
 tkbind (list.timestrata, "<ButtonRelease-1>", function(...) actt("time"))
 tkbind (list.techstrata, "<ButtonRelease-1>", function(...) actt("tech"))
 tkbind (list.spacestrata, "<ButtonRelease-1>", function(...) actt("space"))
# # tkbind(names.list.quali, "<ButtonRelease-1>", function(...)varvalid("quali"))
# # tkbind(names.list.quanti, "<ButtonRelease-1>", function(...)varvalid("quanti"))
tkfocus(tt)
}
.cost.subset <-
function(){
.cost.subset.virtual(virtual=F,sour=c(1,1,1),dataname=NULL,LENV=NULL,parr=NULL,label="")
}
.cost.subset.virtual <-
function(virtual=F,sour=c(1,1,1),dataname=NULL,LENV=NULL,parr=NULL,label=""){
#on doit choisiréééééééééé le type d'objet activable. suffit de bloquer les check bouton en fonction du truc



try(require(tcltk))
try(require(tcltk2))
try(library(Rcmdr))
try(require(tkrplot))
try(tclRequire("BWidget"))
try(library(COSTcore))
try(library(COSTeda));try(library(COSTgui))

ifelse2<-function(test,yes,no){
if (test ==T) {return(yes)}
if (test ==F) {return(no)}
}

malist<-function (pat) {


    Vars <- ls(all.names = TRUE,env=.GlobalEnv)
	# #verbose#print(Vars)
    if (length(Vars) == 0) {return(Vars)}
    names(which(sapply(Vars, function(.x) {	is(get(.x))[1]==pat	}	)	)	)
	}
	
DoItmini<-function(comm,env=.GlobalEnv){
  result <- parse(text = paste(comm))
  exprs<-result
  i<-1
  #for (i in seq_along(exprs)) {
  ei <- exprs[i]
  # # #verbose#print(ei)
  result <- eval(ei, envir = env)
  
}

DoIt<-function (command) 
{
  # # #verbose#print(command)
  result <- try(parse(text = paste(command)), silent = TRUE)
  if (class(result)[1] == "try-error") {
    try(Message(message = paste(strsplit(result, ":")[[1]][2]), 
                type = "error"))
    print("error")
    return(result)
  }
  else {
    exprs <- result
    result <- NULL
  }
  for (i in seq_along(exprs)) {
    ei <- exprs[i]
    result <- try(withVisible(eval(ei, envir = .GlobalEnv)), 
                  silent = TRUE)
    if (class(result)[1] == "try-error") {
      try(Message(message = paste(strsplit(result, ":")[[1]][2]), 
                  type = "error"))
      print("error")
      return(result)
    }
    result <- if (result$visible == FALSE) 
      NULL
    else result$value
    #print(result)
  }
  return(result)
}

GetIt<-function(objet){


if (attr(objet,"class")=="tkwin"){
		if (as.character(tkwinfo("class",objet))=="Listbox"){
		out<-getcontentlistbox(objet)
		
		
		
		
		return(out)
		}
}

if (attr(objet,"class")=="tclVar"){
return(tclvalue(objet))

}
}


getcontentlistbox<-function(listbox){
out<-c()
i<-0		
while (length(as.character(tkget(listbox,i)))!=0){

out<-c(out,paste(collapse=" ",as.character(tkget(listbox,i))))
i<-i+1
if (i == 100){break}
}
return(out)
}

conddoubl<-function(){

lesregles<-GetIt(liste.regles)


tkdelete(liste.regles,"0","end")
for (hop in sort(unique(lesregles))){
tkinsert(liste.regles, "end", hop)

}}

summar <-function(eldata){

command<-paste("summary(",eldata,")",sep="")
doItAndPrint(command)
}

view.fun <-function(eldata){
  command<-paste("View(",eldata,")",sep="")
  doItAndPrint(command)
}

plot.fun <-function(eldata){
  command<-paste("plot(",eldata,")",sep="")
  doItAndPrint(command)
}

condsuppr<-function(){


if(length(as.character(tkcurselection(liste.regles)))==0){return(gettext(domain="R-COSTgui","no selected rules"))}


lesregles<-GetIt(liste.regles)

avirer<-as.numeric(as.character(tkcurselection(liste.regles)))+1
tkdelete(liste.regles,"0","end")
for (hop in sort(lesregles[-avirer])){
tkinsert(liste.regles, "end", hop)
}

}

subvalid<-function(simu=F,virtual=F){

if (virtual==T){simu<-T}


if ( simu==F){


tkconfigure(subvalid.but,state="disabled",text = gettext(domain="R-COSTgui","wait..."))
tkconfigure(savevalid.but,state="disabled",text = gettext(domain="R-COSTgui","wait..."))
}
if ( simu==T){


tkconfigure(preview.but,state="disabled",text = gettext(domain="R-COSTgui","wait..."))}

messim<-list()
#verbose# if ( simu == T ){print(gettext(domain="R-COSTgui","just a simulation"))}
lesregles<-GetIt(liste.regles)
if (length(lesregles)==0){print(gettext(domain="R-COSTgui","no rules"));return(NULL)}

lesfact<-NULL
abid<-lesregles;
if ( length(grep("is.na",abid))!=0){
abid<-abid[-grep("is.na",abid)]}

if (length(abid)!=0){
for ( j in c(" ! %in% "," %in% "," %>% "," %<% "," %>=% "," %<=% ")){
abid<-unlist(strsplit(abid,j))}
abid
length(abid)
lesfact<-abid[seq(1,length(abid),2)]
}

if ( length(grep("is.na",lesregles))!=0){
bidd<-lesregles[grep("is.na",lesregles)]
yout<-unlist(strsplit(bidd,"is.na\\("))
bonusNA<-unique(unlist(strsplit(yout[seq(2,length(yout),2)],"\\)")))

lesfact<-unique(c(lesfact,bonusNA))
}

lesfact<-unique(cbind(lesfact,rep("*#*",length(lesfact)),rep("*#*",length(lesfact)),rep("*#*",length(lesfact))))
lesfact

jegarde<-function(vec){
out<-TRUE
if(sum(is.na(vec))==length(vec) ){out<-FALSE}
if(sum(vec=="",na.rm=T)==length(vec) ){out<-FALSE}
return(out)
}

if ( GetIt(choixcs) !="" & tclvalue(cbValue.csData)=="1"){
lesslot<-slotNamesdata(eval(as.name(GetIt(choixcs))))
for ( i in 1:length(lesslot)){

truc<-do.call("slot",args =list(as.name(as.character(GetIt(choixcs))),lesslot[i]))
truc<-names(truc)[apply(truc,MARGIN=2,FUN=jegarde)]




if(sum(is.element(lesfact[,1],truc))>0){


if(is(lesfact[is.element(lesfact[,1],truc),])[1]=="matrix"){
lesfact[is.element(lesfact[,1],truc),][,2]<-paste(lesfact[is.element(lesfact[,1],truc),][,2],lesslot[i],sep="*#*")
}


if(is(lesfact[is.element(lesfact[,1],truc),])[1]=="character"){
lesfact[is.element(lesfact[,1],truc),][2]<-paste(lesfact[is.element(lesfact[,1],truc),][2],lesslot[i],sep="*#*")
}
}



}
lesfact}

if ( GetIt(choixcl) !="" & tclvalue(cbValue.clData)=="1"){
lesslot<-slotNamesdata(eval(as.name(GetIt(choixcl))))
for ( i in 1:length(lesslot)){
truc<-do.call("slot",args =list(as.name(as.character(GetIt(choixcl))),lesslot[i]))
truc<-names(truc)[apply(truc,MARGIN=2,FUN=jegarde)]

if(sum(is.element(lesfact[,1],truc))>0){
if(is(lesfact[is.element(lesfact[,1],truc),])[1]=="matrix"){
lesfact[is.element(lesfact[,1],truc),][,3]<-paste(lesfact[is.element(lesfact[,1],truc),][,3],lesslot[i],sep="*#*")
}


if(is(lesfact[is.element(lesfact[,1],truc),])[1]=="character"){
lesfact[is.element(lesfact[,1],truc),][3]<-paste(lesfact[is.element(lesfact[,1],truc),][3],lesslot[i],sep="*#*")
}
}











}
lesfact}

if ( GetIt(choixce) !="" & tclvalue(cbValue.ceData)=="1"){
lesslot<-slotNamesdata(eval(as.name(GetIt(choixce))))
for ( i in 1:length(lesslot)){

truc<-do.call("slot",args =list(as.name(as.character(GetIt(choixce))),lesslot[i]))
truc<-names(truc)[apply(truc,MARGIN=2,FUN=jegarde)]





if(sum(is.element(lesfact[,1],truc))>0){
if(is(lesfact[is.element(lesfact[,1],truc),])[1]=="matrix"){
lesfact[is.element(lesfact[,1],truc),][,4]<-paste(lesfact[is.element(lesfact[,1],truc),][,4],lesslot[i],sep="*#*")
}


if(is(lesfact[is.element(lesfact[,1],truc),])[1]=="character"){
lesfact[is.element(lesfact[,1],truc),][4]<-paste(lesfact[is.element(lesfact[,1],truc),][4],lesslot[i],sep="*#*")
}
}









}
lesfact}

matodo<-list()

lesfactSAVE<-lesfact
if ( GetIt(choixcs) !="" & tclvalue(cbValue.csData)=="1"){


for ( p in c("sl","hh","ca","tr","hl")){

if(is(lesfact)[1]=="matrix"){


matodo[[p]]<-lesfact[grep(p,lesfact[,2]),]



if(length(grep(p,lesfact[,2]))!=0){
lesfact<-lesfact[-grep(p,lesfact[,2]),]
}
}



if(is(lesfact)[1]=="character"){
if(length(grep(p,lesfact[2]))!=0)
{
matodo[[p]]<-lesfact


lesfact<-NULL
}




}



}




}

lesfact<-lesfactSAVE
if ( GetIt(choixcl) !="" & tclvalue(cbValue.clData)=="1"){
for ( p in c("cl")){
matodo[[p]]<-lesfact[grep(p,lesfact[,3]),]

if(length(grep(p,lesfact[,3]))!=0){
lesfact<-lesfact[-grep(p,lesfact[,3]),]
}

}
}

lesfact<-lesfactSAVE
if ( GetIt(choixce) !="" & tclvalue(cbValue.ceData)=="1"){
for ( p in c("ce")){
matodo[[p]]<-lesfact[grep(p,lesfact[,4]),]


if(length(grep(p,lesfact[,4]))!=0){
lesfact<-lesfact[-grep(p,lesfact[,4]),]
}

}
}


genregles<-function(plop){

if (length(plop)==0){
return("")
}


if ( is(plop)[1]=="character"){

plop<-plop[1]
}
if ( is(plop)[1]!="character"){

plop<-plop[,1]
}


afa<-c()
araj<-list()
for ( i in plop){

if(length(grep(paste("^",i," ! %in%",sep=""),lesregles))!=0){
ag<-unlist(strsplit(lesregles[grep(paste("^",i," ! %in% ",sep=""),lesregles)],paste(i," ! %in% ",sep="")))
ag<-ag[ag!=""];
if (length(ag)>1){
alv<-"c(";flg<-0
for ( g in ag){if (flg==1){alv<-paste(alv,",",g,sep="")};if (flg==0){alv<-paste(alv,g,sep="")};flg<-1}
alv<-paste(alv,")",sep="");alv}

if (length(ag)==1){
alv<-ag}
araj[[length(araj)+1]]<-paste("!(",i," %in% ",alv,")",sep="")
}



for (X in c(" %in% "," %>% "," %<% "," %>=% "," %<=% ")){
if(length(grep(paste("^",i,X,sep=""),lesregles))!=0){
ag<-unlist(strsplit(lesregles[grep(paste("^",i,X,sep=""),lesregles)],paste(i,X,sep="")))
ag<-ag[ag!=""];

if (length(ag)>1){
alv<-"c(";flg<-0
for ( g in ag){if (flg==1){alv<-paste(alv,",",g,sep="")};if (flg==0){alv<-paste(alv,g,sep="")};flg<-1}
alv<-paste(alv,")",sep="");alv
}

if (length(ag) == 1){alv<-ag}
araj[[length(araj)+1]]<-paste(i,X,alv,sep="")
}
}


if (i == "BIDOUILLEquarter"){
# c 'st ici que l'on rajoute la regle a la main qui va bien pour hh en fonction de .. ca
# # print("YOUPI")
	# out_cs<-subset(SOLcs,quarters(as.Date(date)) %in% c("Q1","Q2"),table="hh")	
	info<-genregles(matodo$ca);info
	# il faut virer ce qui ne fait pas référence a quarter
# info<- "!(fishId %in% 0)  &  quarter %in% c('1','2') & !(fishId %in% 0) & quarter %in% c('1','2')"	
	ert<-strsplit(info," & ")[[1]];ert
	aze<-ert[grep("quarter",ert)];aze
info<-paste(aze,collapse=" & ")
	# info<-"!(quarter %in% '4')  &  quarter %in% c('2','3')"
	# info<-"!(quarter %in% '4')"
	# info<-"quarter %in% c('2','3')"
	
	
buene<-paste(strsplit(info,"quarter")[[1]],collapse="quarters(as.Date(date))")
buene<-paste(strsplit(buene," '")[[1]],collapse=" 'Q")
buene<-paste(strsplit(buene,"\\('")[[1]],collapse="('Q")
buene<-paste(strsplit(buene,",'")[[1]],collapse=",'Q")
buene
araj[[length(araj)+1]]<-buene

}


}




if (sum(lesregles==paste("is.na(",i,")",sep=""))!=0){
araj[[length(araj)+1]]<-paste("is.na(",i,")",sep="")
}

if (sum(lesregles==paste("!is.na(",i,")",sep=""))!=0){
araj[[length(araj)+1]]<-paste("!is.na(",i,")",sep="")
}



afa<-paste(araj,collapse="  &  ")
return(afa)
}


# s'il ya quarter dans CA, il faut bidouiller aussi hh

if (sum(matodo$ca=="quarter")!=0){
if ( sum(matodo$hh=="quarter")!=0){ print("SOUCIS ICI VOIR AVEC VINCENT!!")}
if ( sum(matodo$hh=="quarter")==0){ 
matodo$hh<-rbind(matodo$hh,"BIDOUILLEquarter")
}
}



genregles(matodo$sl)
genregles(matodo$hh)
genregles(matodo$ca)
genregles(matodo$tr)
genregles(matodo$hl)
genregles(matodo$ce)
genregles(matodo$cl)


peutlink<-function(todo){
yop<-list()
for (n in names(todo)){
if (  is(todo[[n]])[1] != "character"){yop[[n]]<-length(grep("ca",todo[[n]]))==dim(todo[[n]])[1] & length(grep("ca",todo[[n]]))!=0}
if (  is(todo[[n]])[1] == "character"){yop[[n]]<-length(grep("ca",todo[[n]]))==1}
}
return(yop)
}

leslink<-peutlink(matodo)

elflag<-0
flagspp<-0
if (length(grep("^spp.=",lesregles))!=0 | length(grep("^spp %in",lesregles))!=0){

flagspp<-1
}
if (length(lesregles) !=0){


if(tclvalue(cbValue.csData)	=="1"){



if (genregles(matodo$sl)!=""){


elsourcecs<-as.character(GetIt(choixcs))

if (flagspp==1){commandsl<-paste(GetIt(sortiecs.lab),"<-subsetSpp(",as.character(GetIt(choixcs)),",",genregles(matodo$sl),",table='sl',link=", as.logical(c(FALSE,TRUE)[1+as.numeric(tclvalue(cbValue.link))] * leslink$sl),")",sep="")}
if (flagspp==0){commandsl<-paste(GetIt(sortiecs.lab),"<-subset(",as.character(GetIt(choixcs)),",",genregles(matodo$sl),",table='sl',link=", as.logical(c(FALSE,TRUE)[1+as.numeric(tclvalue(cbValue.link))] * leslink$sl),")",sep="")}


if(simu==F){doItAndPrint(commandsl)}
if(simu==T){messim[[length(messim)+1]]<-commandsl}
elflag<-1
}



if(genregles(matodo$hh)!=""){

if ( elflag == 1){
commandhh<-paste(GetIt(sortiecs.lab),"<-subset(",as.character(GetIt(sortiecs.lab)),",",genregles(matodo$hh),",table='hh',link=", as.logical(c(FALSE,TRUE)[1+as.numeric(tclvalue(cbValue.link))] * leslink$hh),")",sep="")
}

if ( elflag == 0){
commandhh<-paste(GetIt(sortiecs.lab),"<-subset(",as.character(GetIt(choixcs)),",",genregles(matodo$hh),",table='hh',link=", as.logical(c(FALSE,TRUE)[1+as.numeric(tclvalue(cbValue.link))] * leslink$hh),")",sep="")
elflag<-1
}


if(simu==F){doItAndPrint(commandhh)}
if(simu==T){messim[[length(messim)+1]]<-commandhh}
}

if(genregles(matodo$ca)!=""){


if ( elflag == 1){




commandca<-paste(GetIt(sortiecs.lab),"<-subset(",as.character(GetIt(sortiecs.lab)),",",genregles(matodo$ca),",table='ca',link=", as.logical(c(FALSE,TRUE)[1+as.numeric(tclvalue(cbValue.link))] * leslink$ca),")",sep="")
}


if ( elflag == 0){





commandca<-paste(GetIt(sortiecs.lab),"<-subset(",as.character(GetIt(choixcs)),",",genregles(matodo$ca),",table='ca',link=", as.logical(c(FALSE,TRUE)[1+as.numeric(tclvalue(cbValue.link))] * leslink$ca),")",sep="")
elflag<-1
}


if(simu==F){doItAndPrint(commandca)}
if(simu==T){messim[[length(messim)+1]]<-commandca}

}





if(genregles(matodo$tr)!=""){

if ( elflag == 1){
commandtr<-paste(GetIt(sortiecs.lab),"<-subset(",as.character(GetIt(sortiecs.lab)),",",genregles(matodo$tr),",table='tr',link=", as.logical(c(FALSE,TRUE)[1+as.numeric(tclvalue(cbValue.link))] * leslink$tr),")",sep="")
}

if ( elflag == 0){
commandtr<-paste(GetIt(sortiecs.lab),"<-subset(",as.character(GetIt(choixcs)),",",genregles(matodo$tr),",table='tr',link=", as.logical(c(FALSE,TRUE)[1+as.numeric(tclvalue(cbValue.link))] * leslink$tr),")",sep="")
elflag<-1
}



if(simu==F){doItAndPrint(commandtr)}
if(simu==T){messim[[length(messim)+1]]<-commandtr}
}

if(genregles(matodo$hl)!=""){

if ( elflag == 1){
commandhl<-paste(GetIt(sortiecs.lab),"<-subset(",as.character(GetIt(sortiecs.lab)),",",genregles(matodo$hl),",table='hl',link=", as.logical(c(FALSE,TRUE)[1+as.numeric(tclvalue(cbValue.link))] * leslink$hl),")",sep="")
}

if ( elflag == 0){
commandhl<-paste(GetIt(sortiecs.lab),"<-subset(",as.character(GetIt(choixcs)),",",genregles(matodo$hl),",table='hl',link=", as.logical(c(FALSE,TRUE)[1+as.numeric(tclvalue(cbValue.link))] * leslink$hl),")",sep="")
elflag<-1
}

if(simu==F){doItAndPrint(commandhl)}
if(simu==T){messim[[length(messim)+1]]<-commandhl}
}


}
if(tclvalue(cbValue.clData)	=="1"){

elsourcecl<-as.character(GetIt(choixcl))







if(genregles(matodo$cl)!=""){
commandcl<-paste(GetIt(sortiecl.lab),"<-subset(",as.character(GetIt(choixcl)),",",genregles(matodo$cl),")",sep="")


if(simu==F){doItAndPrint(commandcl)}
if(simu==T){messim[[length(messim)+1]]<-commandcl}


}



}
if(tclvalue(cbValue.ceData)	=="1"){

elsourcece<-as.character(GetIt(choixce))








if(genregles(matodo$ce)!=""){
commandce<-paste(GetIt(sortiece.lab),"<-subset(",as.character(GetIt(choixce)),",",genregles(matodo$ce),")",sep="")

if(simu==F){doItAndPrint(commandce)}
if(simu==T){messim[[length(messim)+1]]<-commandce}
}


}


}



if (simu==T & length(messim) !=0){

if (virtual==F){etemp <- new.env(parent = .GlobalEnv)}
if (virtual==T){etemp <- LENV}


#verbose#print(messim)
for ( i in 1:length(messim)){DoItmini(messim[[i]],etemp)}
ls(env=etemp)
assign("formule",messim,etemp)

try(.OUTCE <- get(GetIt(sortiece.lab), envir = etemp),silent=T)
try(.OUTCS <- get(GetIt(sortiecs.lab), envir = etemp),silent=T)
try(.OUTCL <- get(GetIt(sortiecl.lab), envir = etemp),silent=T)

  tkconfigure(label.cs,text="")
  tkconfigure(label.ce,text="")
  tkconfigure(label.cl,text="")
   
  if(exists(".OUTCS")){
  
 # dim()
  
  azer<-sapply(dim(eval(as.name(as.character(GetIt(choixcs))))),paste,sep=" x ",collapse=" x ")
    azer2<-sapply(dim(.OUTCS),paste,sep=" x ",collapse=" x ")
    tkconfigure(label.cs,text=paste(  as.character(GetIt(choixcs))," ", paste(names(azer),paste("[",as.vector(azer),"]",sep=""),collapse=" ",sep=":"), "\n", " -> ",  GetIt(sortiecs.lab)," ", paste(names(azer2),paste("[",as.vector(azer2),"]",sep=""),collapse=" ",sep=":"),sep=""))
# on redonne le focus
tkfocus(parr)
	try(rm(.OUTCS),silent=T)
	}   

  if(exists(".OUTCE")){
   tkconfigure(label.ce,text=paste(  as.character(GetIt(choixce)), " [", paste(dim(eval(as.name(as.character(GetIt(choixce))))),collapse=" x "),"] ",  " -> ",  GetIt(sortiece.lab), " [",paste(dim(.OUTCE),sep=" x ",collapse=" x "),"] ",sep=""))

     try(rm(.OUTCE),silent=T)
   }    
	
  if(exists(".OUTCL")){	
	tkconfigure(label.cl,text=paste(  as.character(GetIt(choixcl)), " [", paste(dim(eval(as.name(as.character(GetIt(choixcl))))),collapse=" x "),"] ",  " -> ",  GetIt(sortiecl.lab), " [",paste(dim(.OUTCL),sep=" x ",collapse=" x "),"] ",sep=""))
try(rm(.OUTCL),silent=T) 
 }
  



  gc()#poru vraimen vider la memoire
  
 }
 
 
 
 
if ( simu==F){tkconfigure(subvalid.but,state="normal",text = gettext(domain="R-COSTgui","generate the subset"))}
if ( simu==F){tkconfigure(savevalid.but,state="normal",text = gettext(domain="R-COSTgui","save the subset"))}
if ( simu==T){tkconfigure(preview.but,state="normal",text = gettext(domain="R-COSTgui","Preview"))}
 
 
 if ( virtual==F & exists("etemp")){
 
 try(silent=T,rm(etemp))
 
 
 }
 }

condvalid<-function(){




if (GetIt(tvar) == "quanti"){
elvar<-as.name(as.character(tkget(names.list.quanti,tkcurselection(names.list.quanti))))

valseuil<-GetIt(var.quanti)



loper<-as.character(operator.quanti[as.numeric(tclvalue(tcl(combo.oper.quanti,"getvalue")))+1])

if(length(loper)!=0){



lacond<-paste(elvar,loper,valseuil,sep="")
tkinsert(liste.regles, "end", lacond)}
}


if (GetIt(tvar) == "quali"){
elvar<-as.name(as.character(tkget(names.list.quali,tkcurselection(names.list.quali))))

for (rg in as.character(tkcurselection(levels.list))){







# tcltk::tclvalue(tkget(levels.list,"0"))
# tcltk::tclvalue(tkget(levels.list,"1"))
# tcltk::tclvalue(tkget(levels.list,"2"))




if ( tcltk::tclvalue(tkget(levels.list,rg))==""){
ellev<-''
}
if ( tcltk::tclvalue(tkget(levels.list,rg))!=""){

ellev<-as.name(paste(
as.character(tcltk::tclvalue(
tkget(levels.list,rg)
)
)
,collapse=" "))
}
















loper<-as.character(operator.quali[as.numeric(tclvalue(tcl(combo.oper.quali,"getvalue")))+1])

if(loper == "=="){loper<-" %in% "}
if (ellev == '%NA%'){
# print("NA detecte")
if(length(loper)!=0){
if (loper==" %in% "){lacond<-paste("is.na(",elvar,")",sep="")}

if (loper==" ! %in% "){lacond<-paste("!is.na(",elvar,")",sep="")}
		
		}
}


if (ellev != '%NA%'){
if(length(loper)!=0){
lacond<-paste(elvar,loper,"\'",ellev,"\'",sep="")
}
}
tkinsert(liste.regles, "end", lacond)


}
}

		tkconfigure( subvalid.but, state="normal" )
		tkconfigure( savevalid.but, state="normal" )
		tkconfigure( preview.but, state="normal" )
		tkconfigure( condsuppr.but, state="normal" )
		tkconfigure( conddoubl.but, state="normal" )



}

varvalid<-function(id){
#verbose#print('varvalid')
    # # tkconfigure( rb9, state="active" )	
		# # tkconfigure( rb10, state="active" )	
		
		
		
		tkconfigure( combo.oper.quanti, state="disabled" ) #raj
tkconfigure( combo.oper.quali, state="disabled" )#raj
tkconfigure( names.list.quanti, state="disabled" )	#raj
		
		
		
		
		
		
		
if (id == "quanti"){
tclvalue(tvar)<-"quanti"
		 #verbose#print("quanti ici")
		tkconfigure( names.list.quali, state="disabled" )#raj
tkconfigure( levels.list, state="disabled" )#raj
tkconfigure( names.list.quanti, state="normal" )#raj	
tkconfigure(label.quali, font=font3)#raj
tkconfigure(label.quanti, font=font2)#raj
tkconfigure(label.seuil.quanti, font=font2)#raj
tkconfigure(label.levels.quali, font=font3)#raj
tkconfigure(frameSUBR,borderwidth=2)#raj
tkconfigure(frameSUBL,borderwidth=4)#raj
try(elvec<-as.name(as.character(tkget(names.list.quanti,tkcurselection(names.list.quanti)))),silent=T)

tkconfigure( condvalid.but, state="normal" ,text =gettext(domain="R-COSTgui","Validate the rules"))
		tkconfigure( combo.oper.quanti, state="normal" )
		tkconfigure( combo.oper.quali, state="disabled" )
		}
		
		
		
if (id == "quali"){
  # # #verbose#print("quali ici")
  tclvalue(tvar)<-"quali"
  tkconfigure( names.list.quanti, state="disabled" )	#raj
tkconfigure( names.list.quali, state="normal" )	#raj
tkconfigure(label.quali, font=font2)#raj
tkconfigure(label.quanti, font=font3)#raj
tkconfigure(label.seuil.quanti, font=font3)#raj
tkconfigure(label.levels.quali, font=font2)#raj
tkconfigure(frameSUBR,borderwidth=4)#raj
tkconfigure(frameSUBL,borderwidth=2)#raj
  tkconfigure( levels.list, state="normal" )
  # GetIt(choix)
elvec<-NULL
 try(silent=T, elvec<-as.name(as.character(tkget(names.list.quali,tkcurselection(names.list.quali)))))
  # # # 
		# a ce niveau il faut lister TOUS les facteur de TOUS les dataset possible qui ont la vaialbe selectionnée
		
		# # #verbose#print(elvec)
 
 touslesfactor<-function(var){
 
 lesfact<-function(cho,var){
 
 # on va parcourir tous les dataset de cho et sortir tous les faceur s'ils ont var en variable
 # slotNamesdata(cho)
  listslot<-slotNamesdata(eval(as.name(tclvalue(cho))))
mf<-c()
for ( i in 1:length(listslot)){	
truc<-do.call("slot",args =list(as.name(as.character(GetIt(cho))),listslot[i]))
if ( sum(names(truc)==var) ==0 ){
#verbose# cat("pas de ")
#verbose# cat(var)
#verbose# cat("pour ")
#verbose# cat(listslot[i],"\n")
}

if ( sum(names(truc)==var) !=0 ){
#verbose# cat("ok pour ")
#verbose# cat(listslot[i],"\n")














leslevs<-levels(as.factor(truc[[as.character(var)]]))

if (sum(is.na(as.factor(truc[[as.character(var)]])))!=0){
leslevs<-c(leslevs,'%NA%')
}

mf<-c(mf,leslevs)


}

}
 return(mf)
 }
 
base<-c()		
if(tclvalue(cbValue.ceData)	=="1"){

base<-c(base,lesfact(choixce,var=var))
}
if(tclvalue(cbValue.csData)	=="1"){

base<-c(base,lesfact(choixcs,var=var))	
}
if(tclvalue(cbValue.clData)	=="1"){

base<-c(base,lesfact(choixcl,var=var))	
}


base<-unique(base)



return(base)
 
 
 
 
 
 }
 
 
 
 
 
 
  
  # # # eval(  parse(text=paste("leslevels<-",paste("levels(factor(",paste(eldata,elvec, sep="$"),"))",sep=""),sep=""))  )
if (length(elvec)!=0){leslevels<-touslesfactor(elvec)
   
   # leslevels[3]<-"   "
   # leslevels[4]<-"    "
   # leslevels
   
   
  tkdelete(levels.list,"0","end")
  for (var in sort(leslevels)) {tkinsert(levels.list, "end", var)}













  tkconfigure( condvalid.but, state="normal" ,text =gettext(domain="R-COSTgui","Validate the rules"))
  tkconfigure( combo.oper.quali, state="normal" )
  tkconfigure( combo.oper.quanti, state="disabled" )}
}



}

choixtypevar<-function(...){

tkconfigure( combo.oper.quanti, state="disabled" )
tkconfigure( combo.oper.quali, state="disabled" )

tkconfigure( names.list.quanti, state="disabled" )	



if (GetIt(tvar)=="quali"){

tkconfigure( names.list.quanti, state="disabled" )	
tkconfigure( names.list.quali, state="normal" )	




tkconfigure(label.quali, font=font2)
tkconfigure(label.quanti, font=font3)


tkconfigure(label.seuil.quanti, font=font3)
tkconfigure(label.levels.quali, font=font2)

tkconfigure(frameSUBR,borderwidth=4)
tkconfigure(frameSUBL,borderwidth=2)
}

if (GetIt(tvar)=="quanti"){
tkconfigure( names.list.quali, state="disabled" )
tkconfigure( levels.list, state="disabled" )
tkconfigure( names.list.quanti, state="normal" )	







tkconfigure(label.quali, font=font3)
tkconfigure(label.quanti, font=font2)
tkconfigure(label.seuil.quanti, font=font2)
tkconfigure(label.levels.quali, font=font3)
tkconfigure(frameSUBR,borderwidth=2)
tkconfigure(frameSUBL,borderwidth=4)
}

}

none<-function(){
NULL}

data.valid<-function(){
#verbose#print("data.valid")
typevar<-function(dataset,filtre=F){
	# prend un jeux de donn?e et ressort dans une list les nom de variable en focntion de leur type

if(!filtre){
jegarde<-function(vec){return(TRUE)}
}

if (filtre){
jegarde<-function(vec){
out<-TRUE
if(sum(is.na(vec))==length(vec) ){out<-FALSE}
if(sum(vec=="",na.rm=T)==length(vec) ){out<-FALSE}
return(out)
}
}
	out.quanti<-c()
		out.quali<-c()
    
		for ( i in 1:dim(dataset)[2]){

		if (is(dataset[,i])[1]=="numeric" & jegarde(dataset[,i])){out.quanti<-c(out.quanti,names(dataset)[i])}
		if (is(dataset[,i])[1]=="integer" & jegarde(dataset[,i])){out.quanti<-c(out.quanti,names(dataset)[i])}
		if (is(dataset[,i])[1]=="factor" & jegarde(dataset[,i])){out.quali<-c(out.quali,names(dataset)[i])}
		if (is(dataset[,i])[1]=="character" & jegarde(dataset[,i])){out.quali<-c(out.quali,names(dataset)[i])}
				}
		return(list(quanti=out.quanti,quali=out.quali))
		}
comblist2<-function(test){

out<-list()
out$quanti<-unique(as.vector(unlist(test[names(test)=="quanti"])))
out$quali<-unique(as.vector(unlist(test[names(test)=="quali"])))
return(out)
}
comblist<-function(lalist){
return(comblist2(do.call("c",lalist)))
}	
lesvar<-function(cho,rewrite=F){
variableList<-list()

listslot<-slotNamesdata(eval(as.name(tclvalue(cho))))

if (rewrite==T){

for ( i in 1:length(listslot)){
pom<-do.call("slot",args =list(as.name(as.character(GetIt(cho))),listslot[i]))
names(pom)
for (p in c("year","month","quarter")){
if(is.element(p,names(pom))){pom[[p]]<-as.character(pom[[p]])}
}

eval(parse(text=paste(as.name(as.character(GetIt(cho))),"@",listslot[i],"<-pom",sep="")))
}



}



for ( i in 1:length(listslot)){
variableList[[i]]<-
typevar(do.call("slot",args =list(as.name(as.character(GetIt(cho))),listslot[i])),filtre=T)
}




return(comblist(variableList))
}

if (tclvalue(choixcs)!="0"){
if ( dim(eval(as.name(tclvalue(choixcs)))@ca)[1] !=1){tkconfigure(cb.link,state="normal")}
if ( dim(eval(as.name(tclvalue(choixcs)))@ca)[1] ==1){tclvalue(cbValue.link)<-"0";tkconfigure(cb.link,state="disabled")}
		}
		
		
		tkconfigure( names.list.quali, state="normal" )	
		tkconfigure( names.list.quanti, state="normal" )	
		tkdelete(names.list.quanti,"0","end")
		tkdelete(names.list.quali,"0","end")
		tkdelete(levels.list,"0","end")
		
		# lesvar(choixce,rewrite=T)
		
		
		base<-list(quanti=c(),quali=c())		
if(tclvalue(cbValue.ceData)	=="1"){base<-comblist2(c(base,lesvar(choixce,rewrite=T)))}
if(tclvalue(cbValue.csData)	=="1"){
base<-
comblist2(c(base,
lesvar(choixcs,rewrite=T)
))


}
if(tclvalue(cbValue.clData)	=="1"){base<-comblist2(c(base,lesvar(choixcl,rewrite=T)))	}


base$quali<-sort(base$quali)
base$quanti<-sort(base$quanti)
enpremier<-c("area","rect", "year", "quarter", "month", "foCatNat", "foCatEu5", "foCatEu6","commCat","spp")
base$quali<-c(enpremier[is.element(enpremier,base$quali)],base$quali[!is.element(base$quali,enpremier)])		
base$quanti<-c(enpremier[is.element(enpremier,base$quanti)],base$quanti[!is.element(base$quanti,enpremier)])		
		
		
		for (var in base$quanti) {tkinsert(names.list.quanti, "end", var)}
		for (var in base$quali) {tkinsert(names.list.quali, "end", var)}
		tkconfigure( names.list.quanti, state="disabled" )
		tkconfigure( names.list.quali, state="disabled" )
		tkconfigure( combo.oper.quali, state="disabled" )
		tkconfigure( combo.oper.quanti, state="disabled" )    
		
		# on vide la liste des regles
		tkdelete(liste.regles,"0","end")
    
}

desact<-function(id){
montkconf<-function(lebut,lecb){
etat<-c("disabled","normal")

tkconfigure(lebut,state=etat[1+as.numeric(tclvalue(lecb))])
}

etat<-c("disabled","normal")
as.name(paste("choice.",id,"Data.but",sep=""))
as.name(paste("cbValue.",id,"Data",sep=""))
as.name(paste("sortie",id,sep=""))
do.call("montkconf",list(lebut=as.name(paste("choice.",id,"Data.but",sep="")),lecb=as.name(paste("cbValue.",id,"Data",sep=""))))
do.call("montkconf",list(lebut=as.name(paste("sortie",id,sep="")),lecb=as.name(paste("cbValue.",id,"Data",sep=""))))




length(as.character(tkcurselection(names.list.quali)))# donne 0 si pas chargé ou vide.. 
length(as.character(tkcurselection(names.list.quanti)))
}

slotNamesdata<-function(data){
ploom<-slotNames(data)
out<-c()
for (p in ploom){
if(is(slot(data, p))[1] == "data.frame"){
out<-c(out,p)
}
}

return(out)
}

choixdataset<-function(id,forc=NULL){

if (length(forc)==0){

try(tkdestroy(tt2),silent=T)
tt2<-tktoplevel()
tkwm.geometry(tt2, "-100+50")     
listdata <- malist(paste(id,"Data",sep=""))
dataSetsBox <- variableListBox(tt2, listdata, title=gettext(domain="R-COSTgui","Select one dataset"))# voir initialselection
tkgrid(getFrame(dataSetsBox), sticky="nwes",column=1,row=1)
tkgrid.rowconfigure(getFrame(dataSetsBox), 1, weight = 1)
tkgrid.rowconfigure(tt2, 1, weight = 1)  
}  					
						

onOK <- function(forc=NULL){	

if (length(forc)==0){
out<-getSelection(dataSetsBox)
	}
if (length(forc)!=0){
out<-forc
#verbose#print("on impose out")
	}
	
		# # #verbose#print("voila2:")
		# # #verbose#print(GetIt((eval(as.name(paste("choix",id,sep=""))))))
		
		# # #verbose#print("id")
		# # #verbose#print(id)
if (length(out) != 0){
		
		
		# DoItmini(paste("tclvalue(choix",id,")<-out",sep=""),env=e1)
		
		
		eval(parse(text=paste("tclvalue(choix",id,")<-out",sep="")))
		# # #verbose#print("bon")
		

		# # DoItmini(paste("tkconfigure(choice.",id,"Data.but, text=out)",sep=""))
		tkconfigure(eval(as.name(paste("choice.",id,"Data.but",sep=""))),text=out)
		
		# #verbose#print(GetIt((eval(as.name(paste("choix",id,sep=""))))))
		tkconfigure( names.list.quali, state="disabled" )	
		tkconfigure( names.list.quanti, state="disabled" )
		
		}
try(tkdestroy(tt2)	,silent=T)
}
	
	
	
	
	
if (length(forc)==0){
OK.but <-tkbutton(tt2,text="   OK   ",command= function(...) onOK())
tkgrid(OK.but,column=1,row=2)
tkfocus(tt2)}

if (length(forc)!=0){
onOK(forc)
}

}



require(tcltk)


ttSUB <- tktoplevel(borderwidth=10)


tkwm.geometry(ttSUB, "-100+50")                                                   
font2<-tkfont.create(family="Times",size=12,weight="bold")
font3<-tkfont.create(family="Times",size=11)
fontheading<-tkfont.create(family="Times",size=18,weight="bold")  
tkwm.title(ttSUB,paste(gettext(domain="R-COSTgui","COST - subset"),label))
tkgrid(tklabel(ttSUB, text=paste("Subset",label),font=fontheading), columnspan=4,sticky="w",column=4)
tkgrid(tklabel(ttSUB,text="")) # Ligne de blanc  
tkgrid(tklabel(ttSUB,text="")) # Ligne de blanc  




frameSUBTOP<- tkframe(ttSUB, borderwidth=2, relief="groove")
frameSUBTOPright<- tkframe(frameSUBTOP, borderwidth=2, relief="groove")
tiframeSUB<- tkframe(ttSUB, borderwidth=2, relief="groove")
frameSUBR<- tkframe(ttSUB, borderwidth=2, relief="groove")
frameSUBL<- tkframe(ttSUB, borderwidth=2, relief="groove")
frameSUBG<- tkframe(ttSUB, borderwidth=2, relief="groove")
tiframeSUB2<- tkframe(frameSUBG, borderwidth=2, relief="groove")
tiframeSUB3<- tkframe(frameSUBG, borderwidth=2, relief="groove")


labelText <- tclVar(gettext(domain="R-COSTgui","Validate"))
labelText.cs <- tclVar("<dataset>")
labelText.cl <- tclVar("<dataset>")
labelText.ce <- tclVar("<dataset>")



choixcs <- tclVar(0)
choixce <- tclVar(0)
choixcl <- tclVar(0)
choixtable <- tclVar(0)

tvar <- tclVar("NULL")

var.quanti <- tclVar(0)
if (virtual==F){
sortiecs.lab <- tclVar("out_cs")
sortiece.lab <- tclVar("out_ce")
sortiecl.lab <- tclVar("out_cl")
}

if (virtual==T){
sortiecs.lab <- tclVar("mysubset_cs")
sortiece.lab <- tclVar("mysubset_ce")
sortiecl.lab <- tclVar("mysubset_cl")
}

choice.but <- tkbutton(frameSUBTOP,text=GetIt(labelText),command=function()data.valid())       #choixdataset#,foreground="red"
choice.csData.but <- tkbutton(frameSUBTOP,text=GetIt(labelText.cs),command=function(...)choixdataset("cs"),foreground="red",relief="ridge",state="disabled")       #choixdataset#,foreground="red"
choice.clData.but <- tkbutton(frameSUBTOP,text=GetIt(labelText.cl),command=function(...)choixdataset("cl"),foreground="red",relief="ridge",state="disabled")       #choixdataset#,foreground="red"
choice.ceData.but <- tkbutton(frameSUBTOP,text=GetIt(labelText.ce),command=function(...)choixdataset("ce"),foreground="red",relief="ridge",state="disabled")       #choixdataset#,foreground="red"





     




                        



subvalid.but <- tkbutton(frameSUBG,text=gettext(domain="R-COSTgui","generate the subset"),state="disabled",command=function()subvalid(virtual=virtual),foreground="darkblue")
savevalid.but <- tkbutton(frameSUBG,text=gettext(domain="R-COSTgui","save the subset"),state="disabled",command=function()subvalid(virtual=F),foreground="darkgreen")
preview.but <- tkbutton(frameSUBG,text=gettext(domain="R-COSTgui","Preview"),state="disabled",command=function(...)subvalid(T),foreground="darkblue")


condvalid.but <- tkbutton(ttSUB,text=gettext(domain="R-COSTgui","<no rules yet>"),state="disabled",command=function()condvalid())
condsuppr.but <- tkbutton(frameSUBG,text=gettext(domain="R-COSTgui","delete the rule"),state="disabled",command=function()condsuppr())
conddoubl.but <- tkbutton(frameSUBG,text=gettext(domain="R-COSTgui","reorganize the rules"),state="disabled",command=function()conddoubl())















names.list.quanti <-tklistbox(frameSUBL,selectmode="unique",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(names.list.quanti.scroll,...))
names.list.quanti.scroll<-tkscrollbar(frameSUBL,repeatinterval=5,command=function(...)tkyview(names.list.quanti,...))
names.list.quali <-tklistbox(frameSUBR,selectmode="unique",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(names.list.quali.scroll,...))
names.list.quali.scroll<-tkscrollbar(frameSUBR,repeatinterval=5,command=function(...)tkyview(names.list.quali,...))
liste.regles <-tklistbox(frameSUBG,selectmode="multiple",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(liste.regles.scroll,...))
liste.regles.scroll<-tkscrollbar(frameSUBG,repeatinterval=5,command=function(...)tkyview(liste.regles,...))
levels.list <-tklistbox(frameSUBR,selectmode="multiple",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(levels.list.scroll,...))
levels.list.scroll<-tkscrollbar(frameSUBR,repeatinterval=5,command=function(...)tkyview(levels.list,...))  


entry.quanti <-tkentry(frameSUBL,width="10",textvariable=var.quanti)
sortiecs <-tkentry(frameSUBG,width="20",textvariable=sortiecs.lab,state="disabled")
sortiece <-tkentry(frameSUBG,width="20",textvariable=sortiece.lab,state="disabled")
sortiecl <-tkentry(frameSUBG,width="20",textvariable=sortiecl.lab,state="disabled")



operator.quanti<-c(" %in% "," ! %in% "," %>% "," %<% "," %>=% "," %<=% ")
combo.oper.quanti <- tkwidget(frameSUBL,"ComboBox",editable=FALSE,values=operator.quanti,state="disabled")
operator.quali<-c(" %in% "," ! %in% ")
combo.oper.quali <- tkwidget(frameSUBR,"ComboBox",editable=FALSE,values=operator.quali,state="disabled")









cbValue.ceData <- tclVar("0")
cbValue.csData <- tclVar("0")
cbValue.clData <- tclVar("0")
cbValue.link <- tclVar("1")
cb.ceData <- tkcheckbutton(frameSUBTOP,variable=cbValue.ceData,command=function(...)desact("ce"))
cb.csData <- tkcheckbutton(frameSUBTOP,variable=cbValue.csData,command=function(...)desact("cs"))
cb.clData <- tkcheckbutton(frameSUBTOP,variable=cbValue.clData,command=function(...)desact("cl"))
cb.link <- tkcheckbutton(frameSUBTOP,variable=cbValue.link)







frameSUBBONUS<- tkframe(ttSUB, borderwidth=2, relief="groove") #mettre un padding

label.cs<-tklabel(frameSUBBONUS,text="")
label.ce<-tklabel(frameSUBBONUS,text="")
label.cl<-tklabel(frameSUBBONUS,text="")
tkgrid(label.cs)
tkgrid(tklabel(frameSUBBONUS,text=" "))
tkgrid(label.cl)
tkgrid(tklabel(frameSUBBONUS,text=" "))
tkgrid(label.ce)







	










tkgrid(frameSUBTOP,columnspan=4,column=0,row=1,sticky="we")
tkgrid(tklabel(frameSUBTOP,text=gettext(domain="R-COSTgui","Dataset"),font=font2),columnspan=3,column=0,row=0)

textCS<-tklabel(frameSUBTOP,text=gettext(domain="R-COSTgui","CsData : "),font=font2)
textCE<-tklabel(frameSUBTOP,text=gettext(domain="R-COSTgui","CeData : "),font=font2)
textCL<-tklabel(frameSUBTOP,text=gettext(domain="R-COSTgui","ClData : "),font=font2)
tkgrid(cb.csData,textCS,choice.csData.but)
tkgrid(cb.clData,textCL,choice.clData.but)
tkgrid(cb.ceData,textCE,choice.ceData.but)

if(virtual==F){tkgrid(choice.but,tklabel(frameSUBTOP,text=gettext(domain="R-COSTgui","CA link :")),cb.link)}
if(virtual==T){tkgrid(tklabel(frameSUBTOP,text=gettext(domain="R-COSTgui","CA link :")),cb.link)}













tkgrid(tiframeSUB,columnspan=2,column=1,row=2,sticky="")# les column sont etrange ici.. ptet du sticky

tkgrid(frameSUBL,row=3,column=0,columnspan=2,rowspan=2,sticky="nsew")
tkgrid(frameSUBR,row=3,column=2,columnspan=2,rowspan=2,sticky="nsew")
label.quanti<-tklabel(frameSUBL,text=gettext(domain="R-COSTgui","quantitative variables"),font=font3)
tkgrid(label.quanti,columnspan=4)
label.quali<-tklabel(frameSUBR,text=gettext(domain="R-COSTgui","qualitative variables"),font=font3)
tkgrid(label.quali,columnspan=4)
tkgrid(names.list.quanti,names.list.quanti.scroll,sticky="nswe")
tkgrid(names.list.quali,names.list.quali.scroll,sticky="nswe")
tkgrid(tklabel(frameSUBR,text="",font=font2),columnspan=4)
tkgrid(tklabel(frameSUBL,text="",font=font2),columnspan=4)


tkgrid(combo.oper.quanti)
tkgrid(combo.oper.quali)

label.levels.quali<-tklabel(frameSUBR,text=gettext(domain="R-COSTgui","qualitative levels"),font=font3)
tkgrid(label.levels.quali,columnspan=4)
tkgrid(levels.list,levels.list.scroll,sticky="ns")
tkgrid(tklabel(frameSUBR,text="",font=font2),columnspan=4)
tkgrid(tklabel(frameSUBL,text="",font=font2),columnspan=4)
label.seuil.quanti <-tklabel(frameSUBL,text=gettext(domain="R-COSTgui","quantitative threshold"),font=font3)
tkgrid(label.seuil.quanti,columnspan=4)
tkgrid(entry.quanti)
tkgrid(tklabel(frameSUBL,text=""),columnspan=4)
tkgrid(tklabel(frameSUBL,text=""),columnspan=4)

tkgrid(condvalid.but,columnspan=2,column=1,row=6)

tkgrid.columnconfigure(ttSUB,0, weight = 1)
tkgrid.columnconfigure(ttSUB,1, weight = 1)
tkgrid.columnconfigure(ttSUB,2, weight = 1)
tkgrid.columnconfigure(ttSUB,3, weight = 1)
tkgrid.columnconfigure(tiframeSUB,0, weight = 1)
tkgrid.columnconfigure(tiframeSUB,1, weight = 1)
tkgrid.columnconfigure(frameSUBTOP,0, weight = 1)
tkgrid.columnconfigure(frameSUBTOP,1, weight = 1)
tkgrid(tklabel(ttSUB,text="  "),sticky="nsew",column=4,row=3)
tkgrid(frameSUBG,sticky="nsew",column=5,row=3)
tkgrid(tklabel(frameSUBG,text=gettext(domain="R-COSTgui","list of rules"),font=font2),column=0,row=0,columnspan=2)
tkgrid(liste.regles,liste.regles.scroll)
tkgrid.configure(liste.regles,column=0,row=1,columnspan=2,sticky="nsew")
tkgrid.configure(liste.regles.scroll,column=2,row=1,columnspan=1,sticky="ns")
tkgrid(tklabel(frameSUBG,text=""),column=3,row=1,columnspan=1,sticky="nsew")
tkgrid(condsuppr.but,column=4,row=1,columnspan=2,sticky="ew")
tkgrid(conddoubl.but,column=4,row=2,columnspan=2,sticky="ew")

tkgrid.columnconfigure(ttSUB, 0, weight = 1)# a ajuster.
tkgrid.rowconfigure(ttSUB, 0, weight = 1) # aajuster
tkgrid.columnconfigure(frameSUBG, 0, weight = 4)
tkgrid.columnconfigure(frameSUBG, 1, weight = 4)
tkgrid.columnconfigure(frameSUBG, 2, weight = 0)
tkgrid.columnconfigure(frameSUBG, 3, weight = 0)
tkgrid.columnconfigure(frameSUBG, 4, weight = 0)
tkgrid.rowconfigure(frameSUBG, 0, weight = 0)
tkgrid.rowconfigure(frameSUBG, 1, weight = 0)
tkgrid(tiframeSUB2)
tkgrid(tiframeSUB3)


tkgrid(tklabel(frameSUBG,text=""))
label.sortie.cs<-tklabel(frameSUBG,text=gettext(domain="R-COSTgui","out CS:"))
label.sortie.cl<-tklabel(frameSUBG,text=gettext(domain="R-COSTgui","out CL:"))
label.sortie.ce<-tklabel(frameSUBG,text=gettext(domain="R-COSTgui","out CE:"))

tkgrid(label.sortie.cs,sortiecs)
tkgrid(label.sortie.cl,sortiecl)
tkgrid(label.sortie.ce,sortiece)
tkgrid(tklabel(frameSUBG,text=""))
ifelse2(virtual==F,tkgrid(subvalid.but,preview.but,columnspan=2),tkgrid(subvalid.but,savevalid.but,columnspan=2))
tkgrid(frameSUBBONUS,sticky="nsew",column=5,row=1)







tkbind(names.list.quali, "<ButtonRelease-1>", function(...)varvalid("quali"))
tkbind(names.list.quanti, "<ButtonRelease-1>", function(...)varvalid("quanti"))




# on gere sour.

# if (virtual==T){
# #on coche tout

# }


if (sour[1]==0 & virtual ==T){
tkconfigure(cb.csData,state="disabled")
tkconfigure(textCS,font=font3)
}


if (sour[1]==1 & virtual ==T){
tkconfigure(cb.csData,state="disabled")
tkconfigure(choice.csData.but,state="disabled")
tclvalue(cbValue.csData)<-"1"
tkconfigure(textCS,font=font2)
choixdataset("cs",forc=dataname)
data.valid()
tkconfigure(sortiecs,state="normal")
}



if (sour[2]==0 & virtual ==T){
tkconfigure(cb.clData,state="disabled")
tkconfigure(textCL,font=font3)
}

if (sour[2]==1 & virtual ==T){
tkconfigure(cb.clData,state="disabled")
tkconfigure(choice.clData.but,state="disabled")
tclvalue(cbValue.clData)<-"1"
tkconfigure(textCL,font=font2)
choixdataset("cl",forc=dataname)
data.valid()
tkconfigure(sortiecl,state="normal")
}



if (sour[3]==0 & virtual ==T){
tkconfigure(cb.ceData,state="disabled")
tkconfigure(textCE,font=font3)
}

if (sour[3]==1 & virtual ==T){
tkconfigure(cb.ceData,state="disabled")
tkconfigure(choice.ceData.but,state="disabled")
tclvalue(cbValue.ceData)<-"1"
tkconfigure(textCE,font=font2)
choixdataset("ce",forc=dataname)
data.valid()
tkconfigure(sortiece,state="normal")
}




tkfocus(ttSUB)
}
.cost.totVolume <-
function(){




try(require(tcltk))
try(require(tcltk2))
try(library(Rcmdr))
try(require(tkrplot))
try(tclRequire("BWidget"))
try(library(COSTcore))
try(library(COSTeda));try(library(COSTgui));try(library(COSTdbe));try(library(COSTdbe))



tdspinner <- function(parent, ...) {tkwidget(parent, "spinbox", ...)}


valid<-function(){



# on fait des test
flag<-0
# pour tous
if (length(as.character(tkcurselection(list.species)))==0){
Message(message = gettext(domain="R-COSTgui","You have to specify a species"), type = "error")
flag<-1
}
if (tclvalue(lechoix.but.strini)==""){
Message(message = gettext(domain="R-COSTgui","You have to specify a strini"), type = "error")
flag<-1
}

if (tclvalue(tvartype)=="NULL"){
Message(message = gettext(domain="R-COSTgui","You have to specify a type"), type = "error")
flag<-1
}


if ( flag==0){


cmd1<-paste(tclvalue(val.out),"<-dbeObject(desc='",tclvalue(val.desc),
"',species='",paste(as.character(tkget(list.species,as.character(tkcurselection(list.species)))),collapse=" "),"',strataDesc=",tclvalue(lechoix.but.strini),",param='",tclvalue(tvarparam),"',catchCat='",tclvalue(tvarcatchCat),"',methodDesc='",tclvalue(tvarmethodDesc),"')",sep="")
print(cmd1)
doItAndPrint(cmd1)

# puis on fait totVolume





cmd2<-paste(tclvalue(val.out),"<-totVolume(dbeOutput=",tclvalue(val.out),sep="")

cmd2<-paste(cmd2,",csObject=",tclvalue(lechoix.but.csObject),sep="")


if (tclvalue(tvartype)!="Landings"){
cmd2<-paste(cmd2,",ceObject=",tclvalue(lechoix.but.ceObject),sep="")
}


if (tclvalue(tvartype)=="Landings"){
cmd2<-paste(cmd2,",clObject=",tclvalue(lechoix.but.clObject),sep="")
}

cmd2<-paste(cmd2,",type='",tclvalue(tvartype),"'",sep="")


# a voir
# # # # if ( length(as.character(tkcurselection(list.landSpp)))!=0){
# # # # leslandSpp<-NULL 
# # # # for (rg in as.character(tkcurselection(list.landSpp))){
# # # # leslandSpp<-c(leslandSpp,as.name(paste(as.character(tkget(list.landSpp,rg)),collapse=" ")))
# # # # }  
# # # # XX2<-"c("
# # # # for ( i in 1:length(leslandSpp)){XX2<-paste(XX2,"'",as.character(leslandSpp[[i]]),"'",sep="")
# # # # if ( i !=length(leslandSpp)){XX2<-paste(XX2,",",sep="")}}
# # # # XX2<-paste(XX2,")",sep="")

# # # # cmd2<-paste(cmd2,

# # # # ",landSpp=",XX2,

# # # # sep="")
# # # # }


# if (length(as.character(listtype[as.numeric(tclvalue(tcl(combo.type,"getvalue")))+1]))!=0){
# cmd2<-paste(cmd2,",type='",as.character(listtype[as.numeric(tclvalue(tcl(combo.type,"getvalue")))+1]),"'",sep="")
# }




cmd2<-paste(cmd2,",sampPar=",as.logical(as.numeric(tclvalue(cbValue.sampPar))),
",incl.precision=",as.logical(as.numeric(tclvalue(cbValue.incl.precision))),
sep="")




if (tclvalue(lechoix.p1)!="0.025" & tclvalue(cbValue.incl.precision) !="0"){
tclvalue(lechoix.p2)<- round(as.numeric(1-as.numeric(tclvalue(lechoix.p1))),3)
cmd2<-paste(cmd2,paste(",probs=c(",tclvalue(lechoix.p1),",",tclvalue(lechoix.p2),")",sep=""),sep="")
}


cmd2<-paste(cmd2,")",sep="")
print(cmd2)
doItAndPrint(cmd2)

}

}


slotNamesdata<-function(data){
ploom<-slotNames(data)
out<-c()
for (p in ploom){
if(is(slot(data, p))[1] == "data.frame"){
out<-c(out,p)
}
}

return(out)
}



choixdataset.multi<-function(id,nom.but){
print(id)
try(tkdestroy(tt2),silent=T)
tt2<-tktoplevel()
tkwm.geometry(tt2, "-100+50")     
listdata <-NULL
for ( i in id){listdata <- c(listdata,malist(i))}
dataSetsBox <- variableListBox(tt2, listdata, title=gettext(domain="R-COSTgui","Select one dataset"))# voir initialselection
tkgrid(getFrame(dataSetsBox), sticky="nwes",column=1,row=1)
  tkgrid.rowconfigure(getFrame(dataSetsBox), 1, weight = 1)
  tkgrid.rowconfigure(tt2, 1, weight = 1)  
onOK <- function(){	
out<-getSelection(dataSetsBox)

		if ( length(out) != 0){
		eval(parse(text=paste("tclvalue(lechoix.",nom.but,")<-out",sep="")))
		tkconfigure(eval(as.name(nom.but)),text=out)
		# print(GetIt((eval(as.name(paste("lechoix.",nom.but,sep=""))))))
		# tkconfigure( names.list.quali, state="disabled" )	
		# tkconfigure( names.list.quanti, state="disabled" )
		
		

		# print("on met a jour la liste des variables")

		
	if ( nom.but == "but.csObject"){

	#on charge les species
	  listslot<-slotNamesdata(eval(as.name(out)))
araj<-c()
for ( i in 1:length(listslot)){	
truc<-do.call("slot",args =list(as.name(out),listslot[i]))
araj<-unique(c(araj,levels(as.factor(truc[["spp"]]))));araj
}
tkdelete(list.species,"0","end");
for (var in araj) {tkinsert(list.species, "end", var)}
	

}	
		
		# a ce niveau on a le nom du cs objet, on va lister toute les variaible exploitable pour le spaceplot
	if (nom.but == "but.dbeOutput"){

	
	# on charge la description
	tclvalue(val.desc)<-eval(as.name(out))@desc
	tclvalue(val.param)<-eval(as.name(out))@param
	# tclvalue(val.param)<-
	tclvalue(val.methodDesc)<-eval(as.name(out))@methodDesc	
	tclvalue(val.catchCat)<-eval(as.name(out))@catchCat
	tclvalue(val.species)<-eval(as.name(out))@species
	tclvalue(val.timeStrata)<-eval(as.name(out))@strataDesc@timeStrata
	tclvalue(val.spaceStrata)<-eval(as.name(out))@strataDesc@spaceStrata
	tclvalue(val.techStrata)<-eval(as.name(out))@strataDesc@techStrata
	
	
	
	# [1] "quarter"

# Slot "spaceStrata":
# [1] NA

# Slot "techStrata":
	
	
  # listslot<-slotNamesdata(eval(as.name(out)))
# araj<-c()
# for ( i in 1:length(listslot)){	
# truc<-do.call("slot",args =list(as.name(out),listslot[i]))
# araj<-unique(c(araj,levels(as.factor(truc[["spp"]]))));araj
# }
# tkdelete(list.species,"0","end");
# for (var in araj) {tkinsert(list.species, "end", var)}

# on check les LAN et DIS

	# LD<-unique(substr(eval(as.name(out))@hl$sort,1,3))
	# print(LD)
# if(!is.element("DIS",LD)){
# print("pas de DIS")
# tkconfigure(rbdis,state="disabled")
# tclvalue(tvarcatchCat)<-"LAN"

# }
# if(!is.element("LAN",LD)){
# print("pas de LAN")
# tkconfigure(rblan,state="disabled")
# tclvalue(tvarcatchCat)<-"DIS"
# }

		}
	
		
		
choixrb("ert")		
 # if ( 		tclvalue(lechoix.but.ceObject)!= "" & tclvalue(lechoix.but.csObject)!= "" & tclvalue(lechoix.but.dbeOutput)!= ""){

# tkconfigure(valid.but,state="normal")

 # }
		
# aactiver<-list(entry.ylab,entry.xlab,list.time,list.tech,list.space,but.refr,entry.fonction,entry.main,cb.simu,cb.auto,list.time,list.tech,spin.x1,spin.x2,spin.y1,spin.y2,rbsel.image,rbsel.contour,rbsel.bubble,rbsel.value)
# # for ( i in 1:length(aactiver)){tkconfigure(aactiver[[i]],state="normal")}

# lapply(aactiver,tkconfigure,state="normal")

# choixtime<-c("year","semester","quarter","month")
# choixspace<-c("rect","subrect","area")
# choixtech<-c("commCat","commCatScl", "foCatEu5", "foCatEu6", "foCatNat","catchCat")
# tkdelete(list.time,"0","end");for (var in choixtime) {tkinsert(list.time, "end", var)}
# tkdelete(list.tech,"0","end");for (var in choixtech) {tkinsert(list.tech, "end", var)}
# tkdelete(list.space,"0","end");for (var in choixspace) {tkinsert(list.space, "end", var)}


	
		
}
tkdestroy(tt2)	
		
}
	
OK.but <-tkbutton(tt2,text=gettext(domain="R-COSTgui","   OK   "),command=onOK)
tkgrid(OK.but,column=1,row=2)
tkfocus(tt2)
}

#fonctions pratique pour tk
malist<-function (pat) {

# retrouve dans ls les element de type pat
    Vars <- ls(all.names = TRUE,env=.GlobalEnv)
	# print(Vars)
    if (length(Vars) == 0) {return(Vars)}
    names(which(sapply(Vars, function(.x) {	is(get(.x))[1]==pat	}	)	)	)
	}
	
getcontentlistbox<-function(listbox){
out<-c()
i<-0		
while (length(as.character(tkget(listbox,i)))!=0){
out<-c(out,as.character(tkget(listbox,i)))
i<-i+1
}
return(out)
}

GetIt<-function(objet){
if (attr(objet,"class")=="tkwin"){
		if (as.character(tkwinfo("class",objet))=="Listbox"){
		return(getcontentlistbox(objet))
		}
}
if (attr(objet,"class")=="tclVar"){
return(tclvalue(objet))

}
}

DoItmini<-function(comm,env=.GlobalEnv){
  result <- parse(text = paste(comm))
  exprs<-result
  i<-1
  #for (i in seq_along(exprs)) {
  ei <- exprs[i]
  print(ei)
  result <- eval(ei, envir = env)
  
}

reset.strIni<-function(){
tkconfigure(but.strini,text=gettext(domain="R-COSTgui","<select>"))
tclvalue(lechoix.but.strini)<-""
}

choixrb<-function(id){


if ( id=="type"){

if(tclvalue(tvartype)=="Landings"){
 # landing a besoin de cl mais pas de ce
 # trip ,foNum,foDur t time ont besoind ce mais pas de cl

 tkconfigure(labCE,font=font1)
 tkconfigure(labCL,font=font2)
 tkconfigure(but.clObject,state="normal")
 tkconfigure(but.ceObject,state="disabled")
  
 
}

if(tclvalue(tvartype)!="Landings"){
 # landing a besoin de cl mais pas de ce
 # trip ,foNum,foDur t time ont besoind ce mais pas de cl

 tkconfigure(labCL,font=font1)
 tkconfigure(labCE,font=font2)
 tkconfigure(but.ceObject,state="normal")
 tkconfigure(but.clObject,state="disabled")
  
 
}


}








if("NULL" != tclvalue(tvarmethodDesc) & "NULL" != tclvalue(tvarcatchCat)
& tclvalue(lechoix.but.csObject) != ""
& tclvalue(lechoix.but.strini) != ""
& tclvalue(tvarparam) != "t"

){

tkconfigure(valid.but,state="active")

}

}

proba<-function(val){
tclvalue(lechoix.p2)<- round(as.numeric(1-as.numeric(tclvalue(lechoix.p1))),3)
}

checkind<-function(){
tkconfigure(spin.p1,state=c("disabled","normal")[as.numeric(tclvalue(cbValue.incl.precision))+1])}

checkfill<-function(){

tkconfigure(spin.p,state=c("disabled","normal")[as.numeric(tclvalue(cbValue.fillGaps))+1])}


lechoix.but.csObject <- tclVar("")
lechoix.but.ceObject <- tclVar("")
lechoix.but.clObject <- tclVar("")
# # lechoix.but.dbeOutput <- tclVar("")
lechoix.but.strini <- tclVar("")
lechoix.p1 <- tclVar(0.025)
lechoix.p2 <- tclVar(0.975)
lechoix.p <- tclVar(10) # a voir si c 'est bien par defaut

tt <- tktoplevel(borderwidth=10)
tkwm.geometry(tt, "-100+50")
font2<-tkfont.create(family="Times",size=12,weight="bold")
font1<-font3<-tkfont.create(family="Times",size=12)
fontheading<-tkfont.create(family="Times",size=18,weight="bold")  
tkwm.title(tt,gettext(domain="R-COSTgui","COST - totVolume"))
tkgrid(tklabel(tt, text=gettext(domain="R-COSTgui","Estimate total volume"),font=fontheading))
tkgrid(tklabel(tt,text="")) # Ligne de blanc
frameTOP<- tkframe(tt, borderwidth=2, relief="groove")
frameeng<- tkframe(tt, borderwidth=0, relief="groove")
frameBONUS<- tkframe(frameeng, borderwidth=2, relief="groove")
framecont2<- tkframe(tt, borderwidth=0, relief="groove")
framecont<- tkframe(framecont2, borderwidth=0, relief="groove")
# frameA<- tkframe(tt, borderwidth=2, relief="groove")
frameB<- tkframe(framecont, borderwidth=2, relief="groove")
framebanane<- tkframe(framecont, borderwidth=2, relief="groove")
frameB2<- tkframe(framecont2, borderwidth=2, relief="groove")
frameC<- tkframe(framecont, borderwidth=2, relief="groove")
frameC2<- tkframe(framecont, borderwidth=2, relief="groove")
# frameA<- tkframe(tt, borderwidth=2, relief="groove")
# frameB<- tkframe(tt, borderwidth=2, relief="groove")
# frameC<- tkframe(tt, borderwidth=2, relief="groove")
frameD<- tkframe(tt, borderwidth=2, relief="groove")
# frameE<- tkframe(frameTOP, borderwidth=2, relief="groove")
frameE<- tkframe(frameeng, borderwidth=2, relief="groove")
frameE2<- tkframe(frameTOP, borderwidth=2, relief="groove")
frameE3<- tkframe(frameTOP, borderwidth=2, relief="groove")
frameF<- tkframe(framecont, borderwidth=2, relief="groove")
frametype<- tkframe(framecont2, borderwidth=2, relief="groove")



# # les entry

val.desc <- tclVar("")
entry.desc <-tkentry(framebanane,width="20",textvariable=val.desc)
val.species <- tclVar("")
entry.species <-tkentry(frameTOP,width="20",textvariable=val.species)
val.out <- tclVar("out_dbe")
entry.out <-tkentry(frameD,width="20",textvariable=val.out)

# val.desc <- tclVar("")
# entry.desc <-tkentry(frameTOP,width="20",textvariable=val.desc,state="disabled")
# val.species <- tclVar("")
# entry.species <-tkentry(frameTOP,width="20",textvariable=val.species,state="disabled")
val.out <- tclVar("totV")
entry.out <-tkentry(frameD,width="20",textvariable=val.out)
# val.param <- tclVar("")
# entry.param <-tkentry(frameTOP,width="20",textvariable=val.param,state="disabled")
# val.methodDesc <- tclVar("")
# entry.methodDesc <-tkentry(frameTOP,width="20",textvariable=val.methodDesc,state="disabled")
# val.catchCat <- tclVar("")
# entry.catchCat <-tkentry(frameTOP,width="20",textvariable=val.catchCat,state="disabled")
# val.timeStrata <- tclVar("")
# entry.timeStrata <-tkentry(frameTOP,width="20",textvariable=val.timeStrata,state="disabled")
# val.techStrata <- tclVar("")
# entry.techStrata <-tkentry(frameTOP,width="20",textvariable=val.techStrata,state="disabled")
# val.spaceStrata <- tclVar("")
# entry.spaceStrata <-tkentry(frameTOP,width="20",textvariable=val.spaceStrata,state="disabled")
# val.wt <- tclVar("totalW")
# entry.wt <-tkentry(frameF,width="8",textvariable=val.wt,state="normal")
# methodDesc	
# eval(as.name(out))@catchCat
# # val.param <- tclVar("")
# entry.param <-tkentry(frameTOP,width="20",textvariable=val.param)

#les combo



listtype<-c("trip" , "fo", "fd" ,"Landings" , "Time")
combo.type <- tkwidget(frameF,"ComboBox",editable=FALSE,values=listtype,state="normal")


# listbootMethod<-c("samples","otoliths")
# combo.bootMethod <- tkwidget(frameF,"ComboBox",editable=FALSE,values=listbootMethod,state="normal")

#les spin
spin.p1<-tdspinner(frameF,from=0,to=1,inc=0.001,width=5,textvariable = lechoix.p1,state="normal",command=function(...)proba(1))
spin.p2<-tdspinner(frameF,from=0,to=1,inc=0.001,width=5,textvariable = lechoix.p2,state="disabled",command=function(...)proba(2))
spin.p<-tdspinner(frameF,from=1,to=100,inc=1,width=5,textvariable = lechoix.p,state="disabled")


# les radio


tvarcatchCat <- tclVar("LAN")
rbdis <- tkradiobutton(frameB,command=function()choixrb("catchCat"))
rblan <- tkradiobutton(frameB,command=function()choixrb("catchCat"))
tkconfigure(rbdis,variable=tvarcatchCat,value="DIS",state="normal")
tkconfigure(rblan,variable=tvarcatchCat,value="LAN",state="normal")


tvarmethodDesc <- tclVar("NULL")
rbanal <- tkradiobutton(frameC,command=function()choixrb("methodDesc"))
rbboot <- tkradiobutton(frameC,command=function()choixrb("methodDesc"))
tkconfigure(rbanal,variable=tvarmethodDesc,value="analytical",state="normal")
tkconfigure(rbboot,variable=tvarmethodDesc,value="bootstrap",state="normal")


tvarparam <- tclVar("t")
rbnumber <- tkradiobutton(frameB2,command=function()choixrb("param"))
rbweight <- tkradiobutton(frameB2,command=function()choixrb("param"))
rbmaturity <- tkradiobutton(frameB2,command=function()choixrb("param"))
rbsexratio <- tkradiobutton(frameB2,command=function()choixrb("param"))

tkconfigure(rbnumber,variable=tvarparam,value="number@length",state="normal")
tkconfigure(rbweight,variable=tvarparam,value="weight@length",state="normal")
tkconfigure(rbmaturity,variable=tvarparam,value="maturity@length",state="normal")
tkconfigure(rbsexratio,variable=tvarparam,value="sexratio@length",state="normal")




tvartype <- tclVar("NULL")
rbtrip <-      tkradiobutton(frametype,command=function()choixrb("type"))
rbfo <- tkradiobutton(frametype,command=function()choixrb("type"))
rbfd <-  tkradiobutton(frametype,command=function()choixrb("type"))
rblandings <-  tkradiobutton(frametype,command=function()choixrb("type"))
rbtime <- tkradiobutton(frametype,command=function()choixrb("type"))
tkconfigure(rbtrip,variable=tvartype,value="trip",state="normal")
tkconfigure(rbfo,variable=tvartype,value="fo",state="normal")
tkconfigure(rbfd,variable=tvartype,value="fd",state="normal")
tkconfigure(rblandings,variable=tvartype,value="Landings",state="normal")
tkconfigure(rbtime,variable=tvartype,value="Time",state="normal")


# tvarmethodDesc <- tclVar("NULL")
# rbanal <- tkradiobutton(frameC,command=function()choixrb())
# rbboot <- tkradiobutton(frameC,command=function()choixrb())
# tkconfigure(rbanal,variable=tvarmethodDesc,value="analytic",state="disabled")
# tkconfigure(rbboot,variable=tvarmethodDesc,value="bootstrap",state="disabled")
#les bouton


but.strini <- tkbutton(frameBONUS,text=gettext(domain="R-COSTgui","<select>"),state="active",command=function(...) choixdataset.multi("strIni","but.strini"),foreground="red",relief="ridge")
# but.newstrini <- tkbutton(frameA,text=gettext(domain="R-COSTgui","New strIni type object"),state="active",command=function(...) .cost.strIni())#,foreground="red",relief="ridge")
# but.resetstrini <- tkbutton(frameA,text="reset",state="active",command=function(...) reset.strIni())#,foreground="red",relief="ridge")
valid.but <- tkbutton(frameD,text=gettext(domain="R-COSTgui","Validate"),state="disabled",command=function(...) valid())

but.csObject <- tkbutton(frameBONUS,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset.multi(c("csDataCons"),"but.csObject"),foreground="red",relief="ridge")
but.ceObject <- tkbutton(frameBONUS,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset.multi(c("ceDataCons"),"but.ceObject"),foreground="red",relief="ridge")
but.clObject <- tkbutton(frameBONUS,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset.multi(c("clDataCons"),"but.clObject"),foreground="red",relief="ridge")

# but.dbeOutput <- tkbutton(frameBONUS,text=gettext(domain="R-COSTgui","<dataset>"),state="active",command=function(...) choixdataset.multi(c("dbeOutput"),"but.dbeOutput"),foreground="red",relief="ridge")

# les check

cbValue.incl.precision<-tclVar("1")
cb.incl.precision <- tkcheckbutton(frameF,variable=cbValue.incl.precision,state="normal",command=function()checkind())

# cbValue.timeStrata<-tclVar("0")
# cb.timeStrata <- tkcheckbutton(frameF,variable=cbValue.timeStrata,state="normal")

# cbValue.techStrata<-tclVar("0")
# cb.techStrata <- tkcheckbutton(frameF,variable=cbValue.techStrata,state="normal")

# cbValue.spaceStrata<-tclVar("0")
# cb.spaceStrata <- tkcheckbutton(frameF,variable=cbValue.spaceStrata,state="normal")

# cbValue.fillGaps<-tclVar("0")
# cb.fillGaps <- tkcheckbutton(frameF,variable=cbValue.fillGaps,state="normal",command=function()checkfill())

cbValue.sampPar<-tclVar("1")
cb.sampPar <- tkcheckbutton(frameF,variable=cbValue.sampPar,state="normal")


#text

text.valid<-tklabel(frameD,text=gettext(domain="R-COSTgui","out :"))
#list

list.species <-tklistbox(frameE,selectmode="unique",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(list.species.scroll,...))
list.species.scroll<-tkscrollbar(frameE,repeatinterval=5,command=function(...)tkyview(list.species,...))

tkgrid(frameeng)
tkgrid(frameBONUS,frameE,sticky="ns")
# tkgrid(frameE,columnspan=2,sticky="ew")


tkgrid(frameTOP)
tkgrid(framecont2)
# tkgrid(framecont3)

tkgrid(frameB2,frametype,framecont,sticky="ns")
tkgrid(framebanane)
tkgrid(frameB)
tkgrid(frameC)
tkgrid(frameC2)
tkgrid(frameF)
tkgrid(frameD)




tkgrid(tklabel(frameB,text=gettext(domain="R-COSTgui","Landings : "),font=font2),rblan,tklabel(frameB,text=gettext(domain="R-COSTgui","Discards : "),font=font2),rbdis)
labelanal<-tklabel(frameC,text=gettext(domain="R-COSTgui","Analytical : "),font=font1)
labelboot<-tklabel(frameC,text=gettext(domain="R-COSTgui","Bootstrap : "),font=font1)
tkgrid(labelanal,rbanal,labelboot,rbboot)
tkgrid(tklabel(frameB2,text=gettext(domain="R-COSTgui","Parameter :"),font=font2),columnspan=2)
tkgrid(tklabel(frameB2,text=gettext(domain="R-COSTgui","number@length :")),rbnumber)
tkgrid(tklabel(frameB2,text=gettext(domain="R-COSTgui","weight@length :")),rbweight)
tkgrid(tklabel(frameB2,text=gettext(domain="R-COSTgui","maturity@length :")),rbmaturity)
tkgrid(tklabel(frameB2,text=gettext(domain="R-COSTgui","sexratio@length :")),rbsexratio)

# tkgrid(frameTOP)
# tkgrid(frameA)
# tkgrid(frameB)
# tkgrid(frameC)


# tkgrid(tklabel(frameBONUS,text="csDataCons:",font=font2),but.csObject)
# tkgrid(tklabel(frameBONUS,text="dbeOutput:",font=font2),but.dbeOutput)
tkgrid(tklabel(frameBONUS,text=gettext(domain="R-COSTgui","csDataCons:"),font=font2),but.csObject)
labCE<-tklabel(frameBONUS,text=gettext(domain="R-COSTgui","ceDataCons:"),font=font2)
tkgrid(labCE,but.ceObject)
labCL<-tklabel(frameBONUS,text=gettext(domain="R-COSTgui","clDataCons:"))
tkgrid(labCL,but.clObject)
tkgrid(tklabel(frameBONUS,text=gettext(domain="R-COSTgui","StrIni : "),font=font2),but.strini)
# tkgrid(tklabel(frameBONUS,text=gettext(domain="R-COSTgui","StrIni : "),font=font2),but.strini)

# tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Description : ")),entry.desc)
# tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Catch category : ")),entry.catchCat)
# tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Method description : ")),entry.methodDesc)
# tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Species : ")),entry.species)
tkgrid(tklabel(framebanane,text=gettext(domain="R-COSTgui","Description : ")),entry.desc)
tkgrid(tklabel(frameE,text=gettext(domain="R-COSTgui","Species"),font=font2))


tkgrid(list.species,list.species.scroll,sticky="nswe")
# tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Parameter : ")),entry.param)
# tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Technical strata : ")),entry.techStrata)
# tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Temporal strata : ")),entry.timeStrata)
# tkgrid(tklabel(frameTOP,text=gettext(domain="R-COSTgui","Space strata : ")),entry.spaceStrata)
# tkgrid(tklabel(frameE,text=gettext(domain="R-COSTgui","Species"),font=font2))
# tkgrid(list.species,list.species.scroll,sticky="nswe")entry.methodDesc <-tkentry(frameTOP,width="20",textvariable=val.methodDesc,state="disabled")
# val.catchCat <- tclVar("")
# entry.catchCa



# tkgrid(frameE,columnspan=2)#,sticky="ew")

# tkgrid(tklabel(frameA,text=gettext(domain="R-COSTgui","StrIni : "),font=font2),but.strini,but.newstrini,but.resetstrini)

# tkgrid(tklabel(frameC,text="analytic : ",font=font2),rbanal,tklabel(frameC,text="bootstrap : ",font=font2),rbboot)


# tkgrid(tklabel(frameF,text="type : "),combo.type)
# tkgrid(tklabel(frameF,text="bootMethod : "),combo.bootMethod)
tkgrid(tklabel(frametype,text=gettext(domain="R-COSTgui","Type"),font=font2),columnspan=2)#,row=1,column=1)
# tkgrid(frametype,row=1,column=2,columnspan=2)

tkgrid(tklabel(frametype,text=gettext(domain="R-COSTgui","Trip : ")),rbtrip)
tkgrid(tklabel(frametype,text=gettext(domain="R-COSTgui","Fishing operations : ")),rbfo)
tkgrid(tklabel(frametype,text=gettext(domain="R-COSTgui","Fishing duration: ")),rbfd)
tkgrid(tklabel(frametype,text=gettext(domain="R-COSTgui","Landings : ")),rblandings)
tkgrid(tklabel(frametype,text=gettext(domain="R-COSTgui","Time : ")),rbtime)


# tkgrid(tklabel(frameF,text="techStrata : "),cb.techStrata)
# tkgrid(tklabel(frameF,text="spaceStrata : "),cb.spaceStrata)

# tkgrid(tklabel(frameF,text="wt : "),entry.wt)
# tkgrid(tklabel(frameF,text="trace : "),cb.trace)
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","Partial sampling : ")),row=2,column=1)
tkgrid(cb.sampPar,row=2,column=2,columnspan=2)
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","Including precision : ")),row=3,column=1)
tkgrid(cb.incl.precision,row=3,column=2,columnspan=2)
tkgrid(tklabel(frameF,text=gettext(domain="R-COSTgui","Probabilities : ")),row=4,column=1)
tkgrid(spin.p1,row=4,column=2,sticky="e")
tkgrid(spin.p2,row=4,column=3,sticky="w")

tkgrid(text.valid,entry.out,valid.but)


tkfocus(tt)

}
