check.fields <-function(costobj,logfile=FALSE){
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# check.fileds
# function that checks the fields of csData clData and ceData
# objects against the expected values 
# in code.list for characters, and numeric list for reals and integers
# works on the variables in (data.frame!) variable.list
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	data(code.list)
	data(variable.list)
	data(numeric.list)
	if(logfile){
		objname <-paste(substitute(costobj))
		cat(paste("printing to: ",objname,".logfile.txt\n",sep=""))
		sink(paste(objname,"logfile.txt",sep="."))
		cat(paste(objname," logfile created ",date(),"\n\n"))
	}

	objclass <-class(costobj)[1]

	if(objclass %in% c("clData", "ceData", "csData", "ceDataVal", "csDataVal", "clDataVal")!=TRUE)
		stop("costobj not one of csDate clData, or ceData")

	if(class(costobj)[1]=="clData"|class(costobj)[1]=="clDataVal"){
		cat("\tThis is a cl Data object\n")
		dframe1 <-list(cl_table=costobj@cl)
	}

	if(class(costobj)[1]=="ceData"|class(costobj)[1]=="ceDataVal"){
		cat("\tThis is a ce Data object\n")
		dframe1 <-list(ce_table=costobj@ce)
	}

	if(class(costobj)[1]=="csData"|class(costobj)[1]=="csDataVal"){
		cat("\tThis is a cs Data object\n")
		dframe1 <-list(tr_table=costobj@tr, hh_table=costobj@hh, sl_table=costobj@sl, hl_table=costobj@hl, ca_table=costobj@ca)
	}
	cat("\n")
	for(k in 1:length(dframe1)){
		fnames <-names(dframe1[[k]])
		cat(paste(names(dframe1[k]),"\n"))
		for(i in 1:length(fnames)){
			hasnas <-"F"
			eval(parse(text=paste("field <-dframe1[[",k,"]][",i,"]",sep="")))
			ftype <-as.character(variable.list$type[match(fnames[i], as.character(variable.list$variable))])
			if(is.na(ftype))
				stop(paste(fnames[i],"is not a recognised variable name",sep=" "))
			if(ftype=="s"){
				eval(parse(text=paste("fieldcode <-code.list$",fnames[i],"[1]",sep="")))
				fieldcode <-as.vector(unlist(fieldcode))
				field <-as.character(unlist(field))
					if(length(which(is.na(field)))==length(field)){
						cat("\t",paste("$",fnames[i]," is entirely NA",sep=""),"\n")
					} else {
						naindex <-which(is.na(match(field,fieldcode)))
						if(length(naindex)>0){
							ufvals <-unique(field[naindex])
							ufstring <-ifelse(length(ufvals[!is.na(ufvals)])>1," unrecognised entries of values:"," unrecognised entries of value:") 
							cat("\t",paste("$",fnames[i]," contains ",length(naindex),ufstring,"\n",sep=""))
							cat( "\t\t",ufvals,"\n")
						} else {
							cat("\t",paste("$",fnames[i]," is OK\n",sep=""))
						}
					}
				}
			if(ftype=="i"|ftype=="r"){
				cstrings <-as.character(unlist(field))
				suppressWarnings(num <-as.numeric(as.character(unlist(field))))
				n1 <-length(cstrings[!is.na(cstrings)])
				n2 <-length(num[!is.na(num)])
				if(n2<n1){
					ufvals <-unique(cstrings[which(!is.na(cstrings))])
					cstrings <-as.character(unlist(field))
					ufstring <-ifelse(length(ufvals[!is.na(ufvals)])>1," unrecognised entries of character strings:"," unrecognised entries of character string:") 
					cat("\t",paste("$",fnames[i]," is a numeric variable and contains ", length(cstrings[!is.na(cstrings)]),ufstring,"\n",sep=""))
					cat( "\t\t",ufvals,"\n")
				} else {
					field <-as.numeric(as.character(unlist(field)))
					eval(parse(text=paste("numericcode <-numeric.list$",fnames[i],sep="")))
					if(length(which(is.na(field)))==length(field)){
						cat("\t",paste("$",fnames[i]," is entirely NA\n",sep=""))
					} else {
						int <-ifelse(integertest(field,na.rm=T),1,2)
						outcome <-ifelse(int==2&ftype=="i",2,1)
						outcome2 <-ifelse(min(field,na.rm=T)>=numericcode[1]&&max(field,na.rm=T)<=numericcode[2],1,2)
						if(length(which(is.na(field)))>0&length(which(is.na(field)))<length(field)){
							cat("\t",paste("$",fnames[i]," contains ",length(which(is.na(field)))," NA values ",sep=""))
							hasnas <-"T"
						}
						if(outcome==1&outcome2==1&hasnas=="F"){
							cat("\t",paste("$",fnames[i]," is OK\n",sep=""))
						}
						if(outcome==1&outcome2==1&hasnas=="T"){
							cat("but is otherwise OK\n")
						}
						if(outcome==1&outcome2==2&hasnas=="F"){
							cat("\t",paste("$",fnames[i]," contains values out of specified range ",numericcode[1]," - ",numericcode[2],"\n",sep=""))
						}
						if(outcome==1&outcome2==2&hasnas=="T"){
							cat(paste("and contains values out of specified range ",numericcode[1]," - ",numericcode[2],"\n",sep=""))
						}
						if(outcome==2&outcome2==1&hasnas=="F"){
							cat("\t",paste("$",fnames[i]," contains non integer values\n",sep=""))
						}
						if(outcome==2&outcome2==1&hasnas=="T"){
							cat("and contains non integer values\n")
						}	
						if(outcome==2&outcome2==2&hasnas=="F"){
							cat("\t",paste("$",fnames[i]," contains non integer values and values out of specified range ",numericcode[1]," - ",numericcode[2],"\n",sep=""))
						}
						if(outcome==2&outcome2==2&hasnas=="T"){
							cat(paste(", contains non integer values and values out of specified range ",numericcode[1]," - ",numericcode[2],"\n",sep=""))
						}
					}
				}
			}
		}
	}
	if(logfile){
		sink()
		cat("finished\n")
	}
}
#------------------end of check.fields function------------------------

#setGeneric("check.fields")
#--------------------end of methods--------------
integertest <- function(x,na.rm=F){
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# integertest
# function that tests for an integer
# (is.integer does not discriminate real from integer) 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	out <-FALSE
	if(na.rm){
		x <-x[!is.na(x)]
	}
	if(length(x)>0&length(which(!is.na(x)))==length(x)){
		xout <-rep(NA,2*(length(x)))
		index1 <-seq(1,length(xout)-1,2)
		index2 <-index1+1
		xout[index1] <-x
		xout[index2] <-round(x,0)
		if(sum(diff(xout,lag=1)[index1])==0){
			out <-TRUE
		}
	}
	return(out)
}

