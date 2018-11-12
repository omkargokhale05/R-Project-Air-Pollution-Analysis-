#sqlite database connection
setwd("D:\")
library("RSQLite")
db=dbConnect(SQLite(),dbname="ruia.db")
dbListTables(db) #it will show all tables
dbListFields(db,"summer") #it will show all fields of summer table
summer_data= dbGetQuery(db,"select * from summer")
winter_data= dbGetQuery(db,"select * from winter")
attach(summer_data)
attach(winter_data)
dbDiscoonect(db) #it will disconnect the database