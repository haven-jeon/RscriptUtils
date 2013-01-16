#Copyright 2013 Heewon Jeon(madjakarta@gmail.com)
#
#This file is part of RscriptUtils.
#
#RscriptUtils. is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.

#RscriptUtils. is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with RscriptUtils.  If not, see <http://www.gnu.org/licenses/>



#' get_relative_rscript_path
#' 
#' function to get relative rscript path 
#' 
#' @return relative path
#'
#' @export
get_relative_rscript_path <- function(){
  args <- commandArgs(trailingOnly = F)
  if(!any(grepl("--file",args))){
    stop("must run this function with Rscript mode!")
  } 
  scriptPath <- dirname(sub("--file=","",args[grep("--file",args)]))
  return(scriptPath)
}

#' get_absolute_rscript_path
#' 
#' function to get absolute rscript path 
#' 
#' @return absolute path
#'
#' @export
get_absolute_rscript_path <- function(){
  scriptRelPath <- get_relative_rscript_path()
  return(file.path(getwd(), scriptRelPath))
}

#' source_on_rscript
#' 
#' function to source based on rscript path 
#' 
#' @param file file to source
#' @param ... arguments passed to \code{\link{source}()}
#' @export
source_on_rscript <- function(file,...){
  source(file=file.path(get_relative_rscript_path(), file), ...)
}


#' stderr_to_file
#' 
#' Set stderr messages to specific file 
#' 
#' @param filename output file
#' @param open file open mode; see \code{\link{file}}
#' @param ... arguments passed to \code{\link{file}()}
#'
#' @export
stderr_to_file <- function(filename, open="at",...){
  msg <- file(file.path(get_relative_rscript_path(), filename), open=open,...)
  sink(msg, type="message")
}


#' stdout_to_file
#' 
#' Set stdout messages to specific file 
#' 
#' @param filename output file
#' @param open file open mode; see \code{\link{file}}
#' @param ... arguments passed to \code{\link{file}()}
#'
#' @export
stdout_to_file <- function(filename, open="at",...){
  out <- file(file.path(get_relative_rscript_path(), filename), open=open,...)
  sink(out, type="output")
}


#' write_stderr_msg
#' 
#' Write stderr messages with timed informations 
#' 
#' @param msg messages
#'
#' @export
write_stderr_msg <- function(msg){
  new_msg <- paste(strftime(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), msg)
  write(new_msg, stderr())
}



#' write_stdout_msg
#' 
#' Write stdout messages with timed informations 
#' 
#' @param msg messages
#'
#' @export
write_stdout_msg <- function(msg){
  new_msg <- paste(strftime(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), msg)
  write(new_msg, stdout())
}



