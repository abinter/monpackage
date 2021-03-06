% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/appariement_cond.R
\name{appariement_cond}
\alias{appariement_cond}
\title{Appairing cases and controls}
\usage{
appariement_cond(data, case_cond, cond, valeur_cond, nb_temoins, id)
}
\arguments{
\item{data}{Dataframe with variables for cases and controls}

\item{case_cond}{Variable distinging cases from controls}

\item{cond}{Appairing variable}

\item{valeur_cond}{Value of the apparing variable}

\item{nb_temoins}{Number of controls for one case}

\item{id}{Identifier columns ( in character)}
}
\description{
We registered case and control population ids. Then we sample without replacement controls.
}
\examples{
data<-as.data.frame(c(1,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,1,0,1))
colnames(data)<-"hyperactivity"
data$sex<-c(1,1,2,1,1,2,1,1,1,1,1,2,2,2,1,1,1,1,2,1)
data$id<-seq(1,length(data$hyperactivity))
appariement_cond(data,data$hyperactivity,data$sex,1,2,"id")

}
