
library(RSqlParser)

expect_list_equal<-function(t,s)
{
  if(length(setdiff(t,s))>0)
    return(1)
  else
    return(0)
}

test_that("get_all_tables_with_alias gets name of the tables with alias present in sql statements", {

  df<-data.frame(table=c("USERS"),alias=c("U"))
  expect_equal(expect_list_equal(get_all_tables_with_alias(c("select * from users u")), df),0)

})
test_that("get_all_select_cols_with_alias	 gets the selected columns with alias present in sql staments", {

  df<-data.frame(table=c("NAME"),alias=c("N"))
  expect_equal(expect_list_equal(get_all_select_cols_with_alias(c("select name n from users u")), df),0)

})
test_that("get_all_bind_variables gets bind varables present in the sql statement", {
  expect_equal(get_all_bind_variables("select * from users where userid = :bind_userid"), c(":BIND_USERID"),0)

})
test_that("get_all_subqueries gets the sub queries (not the nested one) present in the sql statement", {
  expect_equal(get_all_subqueries("select * from applications where userid in (select user_id from users)"), c("SELECT USER_ID FROM USERS"),0)

})

