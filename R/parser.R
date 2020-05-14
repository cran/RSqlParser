
# cleaning the SQL
.clean <- function(raw)
{
  # make it upper case
  raw <- toupper(raw)
  raw <- stringr::str_replace(raw, ";","")
  raw <- gsub("[\n\r\t]"," ",raw)

  # put space between braces and alphabets
  raw <- gsub("([A-Z])(\\))", "\\1 \\2", raw)
  raw <- gsub("(\\))([A-Z])", "\\1 \\2", raw)
  raw <- gsub("(\\()([A-Z])", "\\1 \\2", raw)
  raw <- gsub("([A-Z])(\\()", "\\1 \\2", raw)

  raw <- gsub("([0-9])(\\))", "\\1 \\2", raw)
  raw <- gsub("(\\()([0-9])", "\\1 \\2", raw)

  raw <- gsub("(['])(\\))", "\\1 \\2", raw)
  raw <- gsub("(\\()(['])", "\\1 \\2", raw)

  raw <- gsub("(\\()(\\()", "\\1 \\2", raw)
  raw <- gsub("(\\))(\\))", "\\1 \\2", raw)

  return(raw)
  #raw <- gsub("(\\))(\\;)", "\\1 \\2", raw)
  #raw <- gsub("([A-Z])(\\;)", "\\1 \\2", raw)
  #raw <- gsub("([0-9])(\\;)", "\\1 \\2", raw)
}

#' Get the names of the selected columns in the sql statement
#'
#' This function takes sql statement and returns all the columns selected in the sql
#' statement.
#'
#' @param sql statement
#' @return names of columns selected in sql statement
#' @examples
#' get_all_select_cols_with_alias("Select p from abc where xyz = 5")
#' @export
get_all_select_cols_with_alias <- function(sql)
{
  # select columns
  sql <- .clean(sql)
  selects <- stringr::str_match_all(sql, "SELECT (.*?) FROM")
  # select<-c()
  column <- c()
  columns <- c()
  alias <- c()
  k <- 1

  for(i in 1 : nrow(selects[[1]]))
  {

    cols <-strsplit(selects[[1]][i,2], ",")[[1]]
    for(j in 1 : length(cols))
    {
      cols[j]<- stringr::str_trim(cols[j])
      columns <-strsplit(cols[j], "\\s+")[[1]]
      column[k] <- columns[1]

      if(length(columns) == 1)
        alias[k] <- ""
      else if(length(columns) == 2)
        alias[k] <- columns[2]
      else if(length(columns) == 3 && columns[2]=="AS")
      {
        alias[k]<- columns[3]
      }
      else
      {
        index <- stringi::stri_locate_last_fixed(cols[j],")")

        if( index[1,1] == nchar(cols[j]))
        {
          alias[k] <- ""
          column[k] <- cols[j]
        }
        else
        {
          column[k]<- substr(cols[j],1,index)
          alias[k] <- substr(cols[j], index, nchar(cols[j]))
        }

      }
      k <- k+1
    }
  }
  columns_alias <- data.frame(column,alias)
  return(columns_alias)
}


#' Get the names of the tables with alias present in the sql
#'
#' This function takes sql statement and returns the list of names
#' of tables and alias for that table, if any present in the sql statement.
#' The first column represent table name and second column represents alias.
#'
#' @param sql statement
#' @return names of tables with alias present in sql statement
#' @examples
#' get_all_tables_with_alias("Select p from abc where xyz = 5")
#' @export
get_all_tables_with_alias<- function(sql)
{
  sql <- .clean(sql)
  list<-strsplit(sql, "\\s+")[[1]]
  v <- c()
  k <- 1
  found <- FALSE
  table <- c()
  alias<- c()

  for (i in 1:length(list)) {

    if( list[i] == "FROM"  || list[i] ==  "JOIN" )
    {
      start <- i
      while(i <= length(list) && list[i] != "WHERE" && list[i] != "GROUP" && list[i] != "ORDER" &&
            list[i] != ";" && list[i]!= "(" && list[i]!=")" && list[i]!="ON"
            && list[i]!="INNER" && list[i]!="OUTER" && list[i]!="LEFT"
            && list[i]!="RIGHT" && list[i]!="FULL" ){
        i <- i+1
      }

      if((i-1)-(start+1)+1 == 3)
      {
        table[k]<- list[start+1]
        alias[k]<- list[start+3]
        k<-k+1
      }
      else if((i-1)-(start+1)+1 == 2)
      {
        table[k]<- list[start+1]
        alias[k]<- list[start+2]
        k<-k+1
      }
      else if((i-1)-(start+1)+1== 1)
      {
        table[k]<- list[start+1]
        alias[k]<- ""
        k<-k+1
      }
      else {
        cross_join <- paste0(list[(start+1):(i-1)], collapse=" ")
        cross_join_tbls <- strsplit(cross_join, ",")[[1]]


        for(j in 1: length(cross_join_tbls))
        {
          each_tbl <- strsplit(stringr::str_trim(cross_join_tbls[j]),"\\s+")[[1]]


            table[k] <- each_tbl[1]
            if(length(each_tbl) == 3)
              alias[k] <- each_tbl[3]
            else if (length(each_tbl) == 2)
                  alias[k] <- each_tbl[2]
            else
                alias[k]<-" "
            k <- k+1

        } # j loop
      } #else
    }# if
  } # for
  tables_alias <- data.frame(table,alias)
  return(tables_alias)
}
#' Get the subqueries in sql statement
#'
#' This function takes sql statement and returns the list of subqueries.It does not
#' return nested sub queries. For nested subqueries, each subquery needs to be fed
#' into the method again.
#' @param sql statement
#' @return all subqueries for the sql statement
#' @examples
#' get_all_subqueries("select * from users where appid in
#'   (select appid from applications)")
#' @export
get_all_subqueries <-function(sql) {
  sql <- .clean(sql)

  list_of_tokens<-strsplit(sql, "\\s+")[[1]]

  count <- 0
  found <- FALSE
  brackets <- 0
  start <- 1
  selects <- c()
  k <- 1
  for(i in 1 : length(list_of_tokens))
  {
    if(list_of_tokens[i] == "SELECT")
    {
      count <- count+1
      if(count > 1 && found == FALSE)
      {
        brackets <- 1
        found <- TRUE
        start <- i
      }
    }
    if(count > 1)
    {
      if(found)
      {
        if(list_of_tokens[i] == "(")
          brackets <- brackets + 1
        if(list_of_tokens[i] == ")")
          brackets <- brackets - 1
        if(brackets == 0)
        {
          selects[k] <- paste0(list_of_tokens[start:(i-1)], collapse=" ")
          k <- k+1
          found <- FALSE
          brackets <- 1
        }
      }
    }
  }
  return (selects)
}

#' Get the bind variables in sql statement
#'
#' This function takes sql statement and returns all the bind variables in the sql statement.
#'
#' @param sql statement
#' @return all bind variables in the sql statement
#' @examples
#' get_all_bind_variables("select * from users where userid = :bind_userid")
#' @export
get_all_bind_variables <- function(sql)
{
  sql <- .clean(sql)
  list_of_tokens<-strsplit(sql, "\\s+")[[1]]
  bind_variables <- c()
  k <- 1
  for(i in 1 : length(list_of_tokens))
  {
    if(startsWith(list_of_tokens[i],":"))
    {
      bind_variables[k] <- list_of_tokens[i]
      k <- k+1
    }
  }
  return(bind_variables)
}

