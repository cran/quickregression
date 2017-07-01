#' @title Quick Linear Regression
#' @description
#' \code{} Reduces the independent variables based on specified P value and Variance Inflation Factor (VIF) level, and reduces following manual efforts.\cr
#' 1. Checking VIF first then removing number of independent variables based on the VIF level.\cr
#' 2. Then Checking p-value of remaining independent variables and removing them.\cr
#'\cr
#' User can select significance level and VIF level as argument.\cr
#'\cr
#' Please note: Function reduces above manual efforts, hence I called it as quick regression.\cr
#' Also, function uses existing lm() function as is, so it will not improve core lm() function execution.\cr
#' User can provide existing arguments of lm functions.\cr
#'\cr
#' Especially with small data set it would be very handy tool for Linear Model preparation.\cr
#' @keywords qlm
#' @param  data is data set name (e.g. bank)
#' @param  V_dependent is dependent variable name. No need of double quotes.
#' @param  signifi is significant level in lm model.(e.g. 0.05,0.01) (default to 0.05)
#' @param  vifl is variance-inflation level. (default to 5)
#' @param  subset	Existing lm() function argument, an optional vector specifying a subset of observations to be used in the fitting process.
#' @param  weights	Existing lm() function argument, an optional vector of weights to be used in the fitting process. Should be NULL or a numeric vector. If non-NULL, weighted least squares is used with weights weights (that is, minimizing sum(w*e^2)); otherwise ordinary least squares is used.
#' @param  na.action Existing lm() function argument, a function which indicates what should happen when the data contain NAs. The default is set by the na.action setting of options, and is na.fail if that is unset. The ‘factory-fresh’ default is na.omit. Another possible value is NULL, no action. Value na.exclude can be useful.
#' @param  method	Existing lm() function argument, the method to be used; for fitting, currently only method = "qr" is supported; method = "model.frame" returns the model frame (the same as with model = TRUE).
#' @param  model,x,y,qr	Existing lm() function argument, logicals. If TRUE the corresponding components of the fit (the model frame, the model matrix, the response, the QR decomposition) are returned.
#' @param  singular.ok	Existing lm() function argument, logical. If FALSE (the default in S but not in R) a singular fit is an error.
#' @param  contrasts Existing lm() function argument, an optional list.
#' @param  offset Existing lm() function argument, this can be used to specify an a priori known component to be included in the linear predictor during fitting. This should be NULL or a numeric vector of length equal to the number of cases. One or more offset terms can be included in the formula instead or as well, and if more than one are specified their sum is used.
#' @import car
#' @import stats
#' @export
#' @examples
#'   a<-mtcars[,c(1,3,4,5,6,7)]
#'   b<-qlm(a,mpg)
#'   summary(b)
#'   b<-qlm(a,mpg,signifi =0.20)
#'   summary(b)
#'   b<-qlm(a,mpg,signifi =0.20,vifl=20)
#'   summary(b)


qlm =function(data,V_dependent,signifi=0.05,vifl=5,subset= NULL, weights= NULL, na.action= NULL,
              method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
              singular.ok = TRUE, contrasts = NULL, offset= NULL){

  V_dependent=as.character(substitute(V_dependent))          # To get dependent variable without double quote
  weights=eval(substitute(weights),envir = data)                # To get dependent variable without double quote
  subset=eval(substitute(subset),envir = data)                  # To get dependent variable without double quote
  method=eval(substitute(method),envir = data)                  # To get dependent variable without double quote
  method1=method
  a7=names(data)															               # Start of VIF check formula creation
  a4=as.numeric(which(a7 == V_dependent))
  a5=paste0("+",a7[-a4],collapse = "")
  a5=substr(a5,2,nchar(a5))
  a5=paste(V_dependent,a5,sep="~")                           # End of VIF check formula creation
  p1=c()
  p2=c()

  print("Variable removed due to high VIF")

  for (i in 1 : ncol(data)){                                 ## VIF check start

    a1= lm(as.formula(a5),data,subset, weights, na.action,
           method1,model,x,y,qr,singular.ok,contrasts,offset)

    if(method=="model.frame"){
      print(a1)
      return(a1)
    }
    if(ncol(data)<3){return(a1)}else{}

    a8=sort(vif(a1),decreasing = T)[1]

    if(a8 > vifl){

      a3=names(a8)[1]										                     # Start of VIF check formula creation
      a4=c(a4,as.numeric(which(a7 == a3)))
      a5=paste0("+",a7[-c(a4)],collapse = "")
      a5=substr(a5,2,nchar(a5))
      a5=paste(V_dependent,a5,sep="~")                       # End of VIF check formula creation

      p1=c(p1,a3)                                            # for printing varibale name
    }

    else{

      print(p1)
      break

    }
  }                                                           ## VIF check end


  ## P value check start
  a6=summary(a1)$coefficients[,4]                             # Get P value column
  a6=a6[-1]                                                   # Remove intercept

  cat("\n")
  print("Variable removed due to high P value")

  for (i in 1 : ncol(data)){

    a9=sort(a6,decreasing = T)[1]

    if(a9 > signifi){

      a3=names(a9)[1]											                     # Start of P value check formula creation
      a4=c(a4,as.numeric(which(a7 == a3)))
      a5=paste0("+",a7[-c(a4)],collapse = "")                  # creating formula
      a5=substr(a5,2,nchar(a5))                                # creating formula
      a5=paste(V_dependent,a5,sep="~")                         # End of P value check formula creation

      a1= lm(as.formula(a5),data,subset, weights, na.action,
             method,model,x,y,qr,singular.ok,contrasts,offset)

      a6=summary(a1)$coefficients[,4]
      a6=a6[-1]

      p2=c(p2,a3)												                       # for printing varibale name
    }

    else{

      print(p2)
      break

    }
  }                                                             ## End of P value check

  return(a1)

}
