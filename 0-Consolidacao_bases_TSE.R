# Brazilian Elections
#
rm(list=ls())

Sys.setlocale("LC_CTYPE")
# [1] "en_US.UTF-8"

# Packages:
library("SciencesPo", quietly = T, warn.conflicts = F)
library(dplyr, quietly = T, warn.conflicts = F)
options(dplyr.print_max=30)
setwd("~/Dropbox")

## connecting
db <- src_sqlite("Eleicoes.sqlite", create = T)

Candidatos <- tbl(db, "Candidatos")

# Must tryCatch(detach(package:plyr)) before.
#
## How many elections we have so far:
Candidatos %>% group_by(ANO_ELEICAO) %>% summarize(n = n()) %>% head(12)

## How many candidates are in the master file by office:
Candidatos %>% filter(DESCRICAO_CARGO=="DEPUTADO FEDERAL") %>% group_by(ANO_ELEICAO) %>% summarize(n = n()) %>% head(50)

Candidatos %>% filter(DESCRICAO_CARGO=="DEPUTADO ESTADUAL"|DESCRICAO_CARGO=="DEPUTADO DISTRITAL") %>% group_by(ANO_ELEICAO) %>% summarize(n = n()) %>% head(50)

## Chacking and Sanitizing files: ####
filter(Candidatos, ANO_ELEICAO==1994) %>% with(., freq(SIGLA_UF))
# TODO: missing few states, need to few in the blanks
filter(Candidatos, ANO_ELEICAO==1996) %>% with(., freq(SIGLA_UF))
# TODO: missing few states, need to few in the blanks
filter(Candidatos, ANO_ELEICAO==1998) %>% with(., freq(SIGLA_UF))
# TODO: missing few states, need to few in the blanks
filter(Candidatos, ANO_ELEICAO==2000) %>% with(., freq(SIGLA_UF))
# Apparently fine
filter(Candidatos, ANO_ELEICAO==2002) %>% with(., freq(SIGLA_UF))
# Apparently fine
filter(Candidatos, ANO_ELEICAO==2004) %>% with(., freq(SIGLA_UF))
# Apparently fine
filter(Candidatos, ANO_ELEICAO==2006) %>% with(., freq(SIGLA_UF))
# Apparently fine
filter(Candidatos, ANO_ELEICAO==2008) %>% with(., freq(SIGLA_UF))
# Apparently fine
filter(Candidatos, ANO_ELEICAO==2010) %>% with(., freq(SIGLA_UF))
# TODO: Include
filter(Candidatos, ANO_ELEICAO==2012) %>% with(., freq(SIGLA_UF))
# Apparently fine
filter(Candidatos, ANO_ELEICAO==2010) %>% with(., freq(SIGLA_UF))
# TODO: Include
filter(Candidatos, ANO_ELEICAO==2014) %>% with(., freq(SIGLA_UF))
# TODO: Include

# bringing the 2014 data:
require(readxl)
cand_2014 <- as.data.frame(read_excel(file.choose())) # reading 2014_em_setembro
tryCatch(detach(package:readxl)) # freeing everything that I can.
cand_2014

names(Candidatos) %in% names(cand_2014)
names1 <- sort(names(Candidatos)); names2 <- sort(names(cand_2014))
cbind(names1,names2)
d = rbind.fill(Candidatos, cand_2014)

# Here is an approach to figure out how many candidates were runnig in 1994:
# I filter those candidates receiving at least 1 vote, so I know the candidacy was valid.
votacacao90 %>% filter(`DESCRICAO_CARGO`=="DEPUTADO FEDERAL", `TOTAL_VOTOS ` >0) %>% summarize(n = n()) %>% head(50)

votacacao90 %>% filter(`DESCRICAO_CARGO`=="DEPUTADO ESTADUAL"|`DESCRICAO_CARGO`=="DEPUTADO DISTRITAL", `TOTAL_VOTOS ` >0) %>% summarize(n = n()) %>% head(50)



library(RSQLite, quietly = T)
setwd("~/Dropbox")

## connecting and writing
# system("ls *.db", show=TRUE)
db <- dbConnect(SQLite(), dbname = "Eleicoes.sqlite")

  dbWriteTable(conn = db,
             name = "Votacacao1990",
             value = votacacao90,
             row.names = FALSE)
# For tables
dbListTables(db)

dbListFields(db, "Candidatos")
dbListFields(db, "Vagas")
dbListFields(db, "Votacao_1986")
dbListFields(db, "Votacao_1989")
dbListFields(db, "Votacao_1990")



## There are only information for two presidential elections. So, some imputation will be needed.
vagas %>% arrange(ANO_ELEICAO) %>%
  filter(`DESCRICAO_CARGO`=="PRESIDENTE") %>%
  head(50)

# dat23[52555,] <- c(2014, "ELEIÇÕES 2014", "BR", "BR", "BRASIL", 1, "PRESIDENTE", 1)

#dbGetQuery(db, "EXPLAIN QUERY PLAN SELECT * FROM Candidatos")
#dbGetQuery(db, "EXPLAIN QUERY PLAN SELECT * FROM Votacacao1990")
# There are white spaces in the field names;
#
query = "UPDATE Votacacao1990 SET TOTAL_VOTOS = TRIM(Replace(Replace(Replace(TOTAL_VOTOS ,'\t',''),'\n',''),'\r',''))"
dbGetQuery(db, )

query = "UPDATE Votacacao1990 SET TOTAL_VOTOS = TRIM(LTRIN(TOTAL_VOTOS))"
dbGetQuery(db, query)


## Analysing Vagas:
library(RSQLite, quietly = T)
setwd("~/Dropbox")

db <- dbConnect(SQLite(), dbname = "Eleicoes.sqlite")

query =  "SELECT * FROM Vagas"
vagas = dbGetQuery(db, query)

names(vagas)

# Seats to return for feds
fed_vagas = vagas %>%
  filter(ano > 1985, desc_cargo =='DEPUTADO FEDERAL') %>%
  group_by(ano, uf, desc_cargo) %>%
  summarize(vagas) %>%
  ungroup()

fed_vagas %>% tidyr::spread(ano, vagas)

# Seats to return for ass
ass_vagas = vagas %>%
  filter(ano > 1985, desc_cargo =='DEPUTADO ESTADUAL'|desc_cargo =='DEPUTADO DISTRITAL') %>%
  group_by(ano, uf, desc_cargo) %>%
  summarize(vagas) %>%
  ungroup()

ass_vagas %>% tidyr::spread(ano, vagas)



#renomeia base de dados candidatos:
names(candidatos) <- c('ano_eleicao','num_turno', "uf", "unid_elei",'desc_unid_elei', 'cargo', 'desc_cargo', 'nome_cand','seq_cand','num_cand','nome_urna_cand','cod_sit_cand','desc_sit_cand','num_part','sigla_part', 'nome_part', 'cod_leg','sigla_leg','comp_leg', 'nome_leg', 'cod_ocup','desc_ocup', 'data_nasc','tit_cand','idade_elei','sexo', 'desc_sexo','cod_grau_instr','desc_grau_instr','cod_est_civil','desc_est_civil','cod_nac','desc_nac','uf_nasc','cod_mun_nasc','nome_mun_nasc', 'desp_max_camp','cod_sit_tot_turno','desc_sit_tot_turno')





# Using now my new wrapper function database:
library(SciencesPo)
setwd("~/Dropbox")
db <- database("Eleicoes")

head(db, "Candidatos")

# Get number of candidates for each UF by year:
query =  "SELECT * FROM Candidatos"
candidatos = dbGetQuery(db, query)


ass_can candidatos %>%
  filter(ano_eleicao > 1985, desc_cargo == 'DEPUTADO ESTADUAL'| desc_cargo =='DEPUTADO DISTRITAL') %>%
  group_by(ano_eleicao, uf, desc_cargo) %>%
  summarize(n=n()) %>%
  ungroup()

ass_can %>% tidyr::spread(ano, vagas)

fed_can candidatos %>%
  filter(ano_eleicao > 1985, desc_cargo == 'DEPUTADO FEDERAL') %>%
  group_by(ano_eleicao, uf, desc_cargo) %>%
  summarize(n=n()) %>%
  ungroup()

fed_can %>% tidyr::spread(ano, vagas)



query = "INSERT INTO Vagas VALUES(1994, 'ELEICOES 1994', 'RN', 'RN', 'RIO GRANDE DO NORTE', 'DEPUTADO ESTADUAL', 24);"
dbGetQuery(db, query)

query = "INSERT INTO Vagas VALUES(1994, 'ELEICOES 1994', 'RN', 'RN', 'RIO GRANDE DO NORTE', 'DEPUTADO FEDERAL', 8);"
dbGetQuery(db, query)

query = "UPDATE Vagas SET desc_unid_elei = 'GOIÁS' WHERE desc_unid_elei = 'GOIAS';"
dbGetQuery(db, query)


query = "DELETE FROM Vagas WHERE ano = 1994 AND uf = 'MG' AND  desc_cargo = 'DEPUTADO DISTRITAL';"
dbGetQuery(db, query)

query =  "DROP TABLE Vagas"
dbGetQuery(db, query)

query = "SELECT COUNT(*) FROM Vagas;"
dbGetQuery(db, query)


query = "UPDATE Vagas SET desc_cargo = 'DEPUTADO DISTRITAL' WHERE desc_cargo = 'Deputado Distrital';"
dbGetQuery(db, query)

query = "UPDATE Vagas SET desc_cargo = 'DEPUTADO ESTADUAL' WHERE desc_cargo = 'Deputado Estadual';"
dbGetQuery(db, query)

query = "UPDATE Vagas SET desc_cargo = 'DEPUTADO FEDERAL' WHERE desc_cargo = 'Deputado Federal';"
dbGetQuery(db, query)

query = "UPDATE Vagas SET desc_cargo = 'VICE-GOVERNADOR' WHERE desc_cargo = 'Vice-governador';"
dbGetQuery(db, query)

query = "UPDATE Vagas SET desc_cargo = 'GOVERNADOR' WHERE desc_cargo = 'Governador';"
dbGetQuery(db, query)

query = "UPDATE Vagas SET desc_cargo = 'PRESIDENTE' WHERE desc_cargo = 'Presidente';"
dbGetQuery(db, query)

query = "UPDATE Vagas SET desc_cargo = 'SENADOR' WHERE desc_cargo = 'Senador';"
dbGetQuery(db, query)

query = "UPDATE Vagas SET desc_cargo = 'VICE-PRESIDENTE' WHERE desc_cargo = 'Vice-presidente';"
dbGetQuery(db, query)

query = "UPDATE Vagas SET desc_cargo = '1º SUPLENTE' WHERE desc_cargo = '1º Suplente';"
dbGetQuery(db, query)

query = "UPDATE Vagas SET desc_cargo = '2º SUPLENTE' WHERE desc_cargo = '2º Suplente';"
dbGetQuery(db, query)

query = "DELETE FROM Vagas WHERE DESCRICAO_CARGO = 'VOCÊ É A FAVOR | DA CRIAÇÃO DO MUNICÍPIO DE | EXTREMA DE RONDÔNIA?';"
dbGetQuery(db, query)

query = "DELETE FROM Vagas WHERE DESCRICAO_CARGO = 'VOCÊ É A FAVOR DA | ALTERAÇÃO DO NOME | DA CIDADE DE EMBU | PARA EMBU DAS ARTES?';"
dbGetQuery(db, query)


query =  "SELECT COUNT(*) FROM Vagas"
dbGetQuery(db, query)

query = "INSERT INTO Vagas VALUES(1946, '', 'BR', 'BR', 'BRASIL',2, 'VICE-PRESIDENTE', 1);"
dbGetQuery(db, query)

## Doesn't work for SQLITE
query = "ALTER TABLE Votacacao1990 DROP COLUMN DATA_GERACAO"
dbGetQuery(db, query)

SQLite has limited ALTER TABLE support that you can use to add a column to the end of a table or to change the name of a table. If you want to make more complex changes in the structure of a table, you will have to recreate the table. You can save existing data to a temporary table, drop the old table, create the new table, then copy the data back in from the temporary table.

For example, suppose you have a table named "t1" with columns names "a", "b", and "c" and that you want to delete column "c" from this table. The following steps illustrate how this could be done:

BEGIN TRANSACTION;
CREATE TEMPORARY TABLE t1_backup(a,b);
INSERT INTO t1_backup SELECT a,b FROM t1;
DROP TABLE t1;
CREATE TABLE t1(a,b);
INSERT INTO t1 SELECT a,b FROM t1_backup;
DROP TABLE t1_backup;
COMMIT;


query = "SELECT sum(TOTAL_VOTOS) FROM Votacacao1990"
dbGetQuery(db, query)


dbReadTable(db, "Votacacao1990")

query = "SELECT * FROM Vagas';"

query = "SELECT * FROM Candidatos WHERE ANO_ELEICAO = 1998"
all =dbSendQuery(db, query)
dados = fetch(all,n=-1) # n=-1 means "get all"
dbHasCompleted(all) # Are we done?



#Whether it is ‘Dan’ OR ‘Den’. In this case we can use ‘LIKE’ operator provided by SQL.
"SELECT * FROM student WHERE name LIKE 'd%n';"

"SELECT * FROM student WHERE name = 'dan';"

"SELECT * FROM student WHERE name = 'dan' AND age = 24;"

"SELECT * FROM student WHERE name = 'dan' OR age > 25"

#SELECT ProductName, Price, FORMAT(NOW(),'YYYY-MM-DD') AS PerDate FROM Products;

# LTRIM - removes any leading spaces from left side of string
# RTRIM - removes any spaces from right
# UPDATE TABLE set CompanyName = LTRIM(RTRIM(CompanyName))

DELETE FROM MY_TABLE
WHERE MY_COLUMN IS NULL
OR TRIM(MY_COLUMN) = ''

# One row at a time
query="select * from testing"
rs=dbSendQuery(con,query)
while (!dbHasCompleted(rs))
{
  d=fetch(rs,n=1)
  print(d$grp)
}

# Close the connection to the database
dbDisconnect(db)

dbSendQuery(conn = db,
            "INSERT INTO School
         VALUES (1, 'urban', 'state', 'medium')")



Candidatos %>% group_by(DESCRICAO_SEXO,DESCRICAO_CARGO) %>% summarize(n = n()) %>% head(25)

# Detecting extraneous infos on gender:
Candidatos %>% filter(DESCRICAO_SEXO=="#NE#") %>% View()

Candidatos$DESCRICAO_SEXO <- ifelse(Candidatos$DESCRICAO_SEXO=="#NE#", NA, Candidatos$DESCRICAO_SEXO)

mutate(Candidatos, ifelse(DESCRICAO_SEXO=="#NE#", NA, DESCRICAO_SEXO))

pull <- function(x,y) {x[,if(is.name(substitute(y))) deparse(substitute(y)) else y, drop = FALSE][[1]]}

dados <- Candidatos %>%
  select(everything()) %>%
  collect()


Candidatos %>%
  group_by(ANO_ELEICAO, SIGLA_UF, DESCRICAO_CARGO) %>%
  mutate(profit= ifelse(is.na(profit), mean(profit, na.rm=TRUE), profit))





setwd("~/Downloads/CONSULTA_VAGAS_1945")
(files <- list.files(pattern= '\\.txt$'))
(info = file.info(files) )
empty = rownames(info[info$size == 0, ])
vagas1945 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))


setwd("~/Downloads/CONSULTA_VAGAS_1947")
(files <- list.files(pattern= '\\.txt$'))
(info = file.info(files))
empty = rownames(info[info$size == 0, ])
vagas1947 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))


setwd("~/Downloads/CONSULTA_VAGAS_1950")
(files <- list.files(pattern= '\\.txt$') )
(info = file.info(files) )
empty = rownames(info[info$size == 0, ])
vagas1950 <-do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))


setwd("~/Downloads/CONSULTA_VAGAS_1954")
(files <- list.files(pattern= '\\.txt$') )
(info = file.info(files) )
empty = rownames(info[info$size == 0, ])
vagas1954 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))


setwd("~/Downloads/CONSULTA_VAGAS_1955")
(files <- list.files(pattern= '\\.txt$') )
(info = file.info(files) )
empty = rownames(info[info$size == 0, ])
vagas1955 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))


setwd("~/Downloads/CONSULTA_VAGAS_1958")
files <- list.files(pattern= '\\.txt$')
info = file.info(files)
empty = rownames(info[info$size == 0, ])
vagas1958 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))



setwd("~/Downloads/CONSULTA_VAGAS_1960")
files <- list.files(pattern= '\\.txt$')
info = file.info(files)
empty = rownames(info[info$size == 0, ])
vagas1960 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))


setwd("~/Downloads/CONSULTA_VAGAS_1962")
files <- list.files(pattern= '\\.txt$')
info = file.info(files)
empty = rownames(info[info$size == 0, ])
vagas1962 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))


setwd("~/Downloads/CONSULTA_VAGAS_1965")
files <- list.files(pattern= '\\.txt$')
info = file.info(files)
empty = rownames(info[info$size == 0, ])
vagas1965 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))


setwd("~/Downloads/CONSULTA_VAGAS_1966")
(files <- list.files(pattern= '\\.txt$'))
(info = file.info(files))
empty = rownames(info[info$size == 0, ])
vagas1966 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))

setwd("~/Downloads/CONSULTA_VAGAS_1970")
files <- list.files(pattern= '\\.txt$')
info = file.info(files)
empty = rownames(info[info$size == 0, ])
vagas1970 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))


setwd("~/Downloads/CONSULTA_VAGAS_1974")
files <- list.files(pattern= '\\.txt$')
info = file.info(files)
empty = rownames(info[info$size == 0, ])
vagas1974 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))


setwd("~/Downloads/CONSULTA_VAGAS_1978")
files <- list.files(pattern= '\\.txt$')
info = file.info(files)
empty = rownames(info[info$size == 0, ])
vagas1978 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))

setwd("~/Downloads/CONSULTA_VAGAS_1986")
files <- list.files(pattern= '\\.txt$')
info = file.info(files)
empty = rownames(info[info$size == 0, ])
vagas1986 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))


setwd("~/Downloads/CONSULTA_VAGAS_1990")
(files <- list.files(pattern= '\\.txt$'))
(info = file.info(files))
empty = rownames(info[info$size == 0, ])
vagas1990 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))


setwd("~/Downloads/consulta_vagas_1994")
(files <- list.files(pattern= '\\.txt$'))
(info = file.info(files))
empty = rownames(info[info$size == 0, ])
vagas1994 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))


setwd("~/Downloads/consulta_vagas_1996")
files <- list.files(pattern= '\\.txt$')
info = file.info(files)
empty = rownames(info[info$size == 0, ])
vagas1996 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))


setwd("~/Downloads/consulta_vagas_1998")
files <- list.files(pattern= '\\.txt$')
info = file.info(files)
empty = rownames(info[info$size == 0, ])
vagas1998 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))



setwd("~/Downloads/consulta_vagas_2000")
files <- list.files(pattern= '\\.txt$')
info = file.info(files)
empty = rownames(info[info$size == 0, ])
vagas2000 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))


setwd("~/Downloads/consulta_vagas_2002")
files <- list.files(pattern= '\\.txt$')
info = file.info(files)
empty = rownames(info[info$size == 0, ])
vagas2002 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))



setwd("~/Downloads/consulta_vagas_2004")
files <- list.files(pattern= '\\.txt$')
info = file.info(files)
empty = rownames(info[info$size == 0, ])
vagas2004 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))



setwd("~/Downloads/consulta_vagas_2006")
files <- list.files(pattern= '\\.txt$')
info = file.info(files)
empty = rownames(info[info$size == 0, ])
vagas2006 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))



setwd("~/Downloads/consulta_vagas_2008")
(files <- list.files(pattern= '\\.txt$'))
(info = file.info(files))
empty = rownames(info[info$size == 0, ])
vagas2008 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))




setwd("~/Downloads/consulta_vagas_2010")
(files <- list.files(pattern= '\\.txt$'))
(info = file.info(files))
empty = rownames(info[info$size == 0, ])
vagas2010 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))


setwd("~/Downloads/consulta_vagas_2012")
(files <- list.files(pattern= '\\.txt$'))
(info = file.info(files))
empty = rownames(info[info$size == 0, ])
vagas2012 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))


setwd("~/Downloads/consulta_vagas_2014")
(files <- list.files(pattern= '\\.txt$'))
(info = file.info(files) )
empty = rownames(info[info$size == 0, ])
vagas2014 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))


library(data.table)
#Set fill to true to take care of the missing values, if any.
vagas1 = rbindlist(mget(ls(pattern="vagas")), fill=TRUE)

library(plyr)
df.fill <- lapply(ls(pattern = "vagas"), get)
vagas <- do.call("rbind.fill", df.fill)

# little check freq(vagas$V5)

colnames(vagas)<- c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO", "DESCRICAO_ELEICAO", "SIGLA_UF", "SIGLA_UE", "NOME_UE", "CODIGO_CARGO",
                 "DESCRICAO_CARGO", "QTDE_VAGAS")


vagas[52563,] <- c(1985, "ELEIÇÕES 1985", "BR", "BR", "BRASIL", 1, "PRESIDENTE", 1)
vagas[52562,] <- c(1978, "ELEIÇÕES 1978", "BR", "BR", "BRASIL", 1, "PRESIDENTE", 1)
vagas[52561,] <- c(1974, "ELEIÇÕES 1974", "BR", "BR", "BRASIL", 1, "PRESIDENTE", 1)
vagas[52560,] <- c(1969, "ELEIÇÕES 1969", "BR", "BR", "BRASIL", 1, "PRESIDENTE", 1)
vagas[52559,] <- c(1966, "ELEIÇÕES 1966", "BR", "BR", "BRASIL", 1, "PRESIDENTE", 1)
vagas[52558,] <- c(1964, "ELEIÇÕES 1964", "BR", "BR", "BRASIL", 1, "PRESIDENTE", 1)
vagas[52557,] <- c(1955, "ELEIÇÕES 1955", "BR", "BR", "BRASIL", 1, "PRESIDENTE", 1)
vagas[52556,] <- c(1950, "ELEIÇÕES 1950", "BR", "BR", "BRASIL", 1, "PRESIDENTE", 1)
vagas[52555,] <- c(2014, "ELEIÇÕES 2014", "BR", "BR", "BRASIL", 1, "PRESIDENTE", 1)
vagas[52554,] <- c(2010, "ELEIÇÕES 2010", "BR", "BR", "BRASIL", 1, "PRESIDENTE", 1)
vagas[52553,] <- c(2006, "ELEIÇÕES 2006", "BR", "BR", "BRASIL", 1, "PRESIDENTE", 1)
vagas[52552,] <- c(2002, "ELEIÇÕES 2002", "BR", "BR", "BRASIL", 1, "PRESIDENTE", 1)
vagas[52551,] <- c(1998, "ELEIÇÕES 1998", "BR", "BR", "BRASIL", 1, "PRESIDENTE", 1)
vagas[52550,] <- c(1994, "ELEIÇÕES 1994", "BR", "BR", "BRASIL", 1, "PRESIDENTE", 1)
vagas[52549,] <- c(1989, "ELEIÇÕES 1989", "BR", "BR", "BRASIL", 1, "PRESIDENTE", 1)

##Get all different first five letter strings for all cvs files in directory "."
file.prefixes <- unique(sapply(list.files(path=".", pattern="*.csv"), substr, 1,5))
##Group all matching file names according to file.prefixes into a list
file.list <- lapply(file.prefixes, function(x)list.files(pattern=paste("^",x,".*.csv",sep=""), path="."))
names(file.list) <- file.prefixes ##just for convenience

##parse all csv files in file.list, create a list of lists containing all tables for each prefix                                                    tables <- lapply(file.list, function(filenames)lapply(filenames, function(file)read.table(file, header=TRUE)))

##for each prefix, rbind the tables. Result is a list of length being length(file.prefixes)
##  each containing a matrix with the combined data parsed from the files that match the prefix
joined.tables <- lapply(tables, function(t)do.call(rbind, t))

##Save tables to files

for(prefix in names(joined.tables))write.table(joined.tables[[prefix]], paste(prefix, ".csv", sep=""))

# Load required packages
library(plyr)

# Generate 100 example data frames
for(i in 1:100){
  assign(paste0('df', i),
         data.frame(x = rep(1:100),
         y = seq(from = 1,
                 to = 1000,
                 length = 100))) }


setwd("~/Downloads/bem_candidato_2008")
(files <- list.files(pattern= '\\.txt$'))
(info = file.info(files))
empty = rownames(info[info$size == 0, ])
bem2008 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))


setwd("~/Downloads/bem_candidato_2010")
(files <- list.files(pattern= '\\.txt$'))
(info = file.info(files))
empty = rownames(info[info$size == 0, ])
bem2010 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))


setwd("~/Downloads/bem_candidato_2012")
(files <- list.files(pattern= '\\.txt$'))
(info = file.info(files))
empty = rownames(info[info$size == 0, ])
bem2012 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))


setwd("~/Downloads/bem_candidato_2014")
(files <- list.files(pattern= '\\.txt$'))
(info = file.info(files))
empty = rownames(info[info$size == 0, ])
bem2014 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))

library(plyr)
df.fill <- lapply(ls(pattern = "bem"), get)
bens_declarados <- do.call("rbind.fill", df.fill)

colnames(bens_declarados) <- c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO", "DESCRICAO_ELEICAO", "SIGLA_UF", "SQ_CANDIDATO", "CD_TIPO_BEM_CANDIDATO","DS_TIPO_BEM_CANDIDATO", "DETALHE_BEM", "VALOR_BEM", "DATA_ULTIMA_ATUALIZACAO", "HORA_ULTIMA_ATUALIZACAO")



## Campaing Spending ###

paths <- dir("data", pattern = "\\.csv$", full.names = TRUE)
names(paths) <- basename(paths)
plyr::ldply(paths, read.csv, stringsAsFactors = FALSE)

setwd("~/Downloads/prestacao_final_2014/untitled")
(files <- list.files(pattern= '\\.txt$'))
(info = file.info(files))
empty = rownames(info[info$size == 0, ])
despesas2014 <- do.call(rbind, lapply(list.files(path=".", pattern="despesas_partidos_2014"), read.table, sep=";",header=TRUE, stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))

# check if we're missing some unit:
length(table(despesas2014$UF))
table(despesas2014$UF)
head(despesas2014)

despesas2014$Cód..Eleição <-NULL
despesas2014$Desc..Eleição <- NULL
despesas2014$Data.e.hora <- NULL

# cleaning data stuff
despesas2014$Data.da.despesa1 = dmy_hms(despesas2014$Data.da.despesa)
despesas2014$Data.da.despesa = despesas2014$Data.da.despesa1


library(RSQLite, quietly = T)
setwd("~/Dropbox")

## connecting and writing
# system("ls *.db", show=TRUE)
db <- dbConnect(SQLite(), dbname = "Eleicoes.sqlite")

dbWriteTable(conn = db,
             name = "Despesas_can_2010",
             value = despesas2010,
             row.names = FALSE)

# update TABLE_NAME set FIELD_NAME = replace(replace(substr(FIELD_NAME, 1,length(FIELD_NAME)-3),',',''),'.','') || '.' || substr(FIELD_NAME,-2)

# For tables
dbListTables(db)
dbDisconnect(db)


names(votamunzona1994) <- c('ANO_ELEICAO', 'NUM_TURNO', 'DESCRICAO_ELEICAO', 'SIGLA_UF','SIGLA_UE', 'CODIGO_MUNICIPIO','NOME_MUNICIPIO', 'NUMERO_ZONA', 'CODIGO_CARGO', 'NUMERO_CAND', 'SQ_CANDIDATO', 'NOME_CANDIDATO', 'NOME_URNA_CANDIDATO', 'DESCRICAO_CARGO', 'COD_SIT_CAND_SUPERIOR', 'DESC_SIT_CAND_SUPERIOR','CODIGO_SIT_CANDIDATO', 'DESC_SIT_CANDIDATO', 'CODIGO_SIT_CAND_TOT', 'DESC_SIT_CAND_TOT', 'NUMERO_PARTIDO', 'SIGLA_PARTIDO', 'NOME_PARTIDO', 'SEQUENCIAL_LEGENDA', 'NOME_COLIGACAO', 'COMPOSICAO_LEGENDA', 'TOTAL_VOTOS')



rename_map <- c('ANO_ELEICAO'='ano_eleicao',
                            'NUM_TURNO'='num_turno',
                            'DESCRICAO_ELEICAO'='desc_elei',
                            'SIGLA_UF'='uf',
                            'SIGLA_UE'='unid_elei',
                            'DESCRICAO_UE'='desc_unid_elei',
                            'CODIGO_MUNICIPIO'='cod_unid_elei',
                            'NOME_MUNICIPIO'='desc_unid_elei',
                            'NUMERO_ZONA'='num_zona',
                            'CODIGO_CARGO'='cargo',
                            'DESCRICAO_CARGO'='desc_cargo',
                            'NUMERO_CAND'='num_cand',
                             'NUMERO_CANDIDATO'='num_cand',
                            'SQ_CANDIDATO'='seq_cand',
                            'SEQUENCIAL_CANDIDATO'='seq_cand',
                            'NOME_CANDIDATO'='nome_cand',
                            'NOME_URNA_CANDIDATO'='nome_urna_cand',
                            'COD_SIT_CAND_SUPERIOR'='cod_sit_cand',
                            'DESC_SIT_CAND_SUPERIOR'='desc_sit_cand_sup',
                            'CODIGO_SIT_CANDIDATO'= 'cod_sit_cand_sup',
                            'DESC_SIT_CANDIDATO'= 'desc_sit_cand',
                            'DES_SITUACAO_CANDIDATURA'= 'desc_sit_cand',
                            'CODIGO_SIT_CAND_TOT'='cod_sit_tot_turno',
                            'DESC_SIT_CAND_TOT'='desc_sit_tot_turno',
                            'DESC_SIT_TOT_TURNO'='desc_sit_tot_turno',
                            'NUMERO_PARTIDO'='num_part',
                            'CODIGO_LEGENDA'='num_part',
                            'SIGLA_PARTIDO'='sigla_part',
                            'NOME_PARTIDO'='nome_part',
                            'SEQUENCIAL_LEGENDA'='seq_leg',
                            'NOME_LEGENDA'='nome_leg',
                            'NOME_COLIGACAO'='nome_leg',
                            'COMPOSICAO_LEGENDA'='comp_leg',
                            'TOTAL_VOTOS'='total_votos'
                              )

query =  "SELECT * FROM Votacao_1982"
data = dbGetQuery(db, query)


## Replace observations with caharacters "NE" , "NULO", "-1", and "-3" by "NA"
data[data == "#NE#"] = 'NA'
data[data == "#NULO#"] = 'NA'
data[data == "-1"] = 'NA'
data[data == "-3"] = 'NA'


names(data) <- rename_map[names(data)]
data[1:2,]

# query =  "DROP TABLE Votacao_1982"
# dbGetQuery(db, query)

dbWriteTable(conn = db,
             name = "Votacao_1974",
             value = data,
             row.names = FALSE)

dbListTables(db)

  names(data) <- c('ANO_ELEICAO', 'NUM_TURNO', 'DESCRICAO_ELEICAO', 'SIGLA_UF','SIGLA_UE', 'CODIGO_CARGO', 'NUMERO_CAND', 'SQ_CANDIDATO', 'NOME_CANDIDATO', 'NOME_URNA_CANDIDATO', 'DESCRICAO_CARGO', 'COD_SIT_CAND_SUPERIOR', 'DESC_SIT_CAND_SUPERIOR','CODIGO_SIT_CANDIDATO', 'DESC_SIT_CANDIDATO', 'CODIGO_SIT_CAND_TOT', 'DESC_SIT_CAND_TOT', 'NUMERO_PARTIDO', 'SIGLA_PARTIDO', 'NOME_PARTIDO', 'SEQUENCIAL_LEGENDA', 'NOME_COLIGACAO', 'COMPOSICAO_LEGENDA', 'TOTAL_VOTOS')

# Query to bind all tables together
dbGetQuery(db, 'CREATE TABLE Votacao AS
SELECT * FROM Votacao_1945
UNION ALL
SELECT * FROM Votacao_1947
UNION ALL
SELECT * FROM Votacao_1950
UNION ALL
SELECT * FROM Votacao_1954
UNION ALL
SELECT * FROM Votacao_1955
UNION ALL
SELECT * FROM Votacao_1958
UNION ALL
SELECT * FROM Votacao_1960
UNION ALL
SELECT * FROM Votacao_1962
UNION ALL
SELECT * FROM Votacao_1965
UNION ALL
SELECT * FROM Votacao_1966
UNION ALL
SELECT * FROM Votacao_1970
UNION ALL
SELECT * FROM Votacao_1974
UNION ALL
SELECT * FROM Votacao_1978
UNION ALL
SELECT * FROM Votacao_1982
UNION ALL
SELECT * FROM Votacao_1986
UNION ALL
SELECT * FROM Votacao_1989
UNION ALL
SELECT * FROM Votacao_1990')

query =  "SELECT * FROM Votacao"
data = dbGetQuery(db, query)

names(candidatos) <- c(  'cod_leg','sigla_leg', , 'cod_ocup','desc_ocup', 'data_nasc','tit_cand','idade_elei','sexo', 'desc_sexo','cod_grau_instr','desc_grau_instr','cod_est_civil','desc_est_civil','cod_nac','desc_nac','uf_nasc','cod_mun_nasc','nome_mun_nasc', 'desp_max_camp','cod_sit_tot_turno','desc_sit_tot_turno')

names(data2) <- c('nome_cand', 'desc_cargo', 'cargo', 'num_cand', 'uf', 'desc_unid_elei', 'unid_elei', 'num_part','sigla_part', 'receita', 'data_rece', 'desc_rece', 'desc_recurso', 'nome_doador','doc_doador')

# Renaming  2010 campaign finance data:
names(data2) <- c('uf', 'sigla_part','num_cand', 'desc_cargo', 'nome_cand', 'cpf_cand', 'doc_doador','nome_doador', 'data_rece', 'receita', 'tipo_rece' , 'fonte_recurso', 'desc_recurso', 'desc_rece')

# Fixing dates
data2$data_rece1 = data2$data_rece
data2$data_rece1 = as.Date(data2$data_rece1, format="%d/%m/%Y")

year = as.numeric(format(data2$data_rece1, format = "%Y"));
month = as.numeric(format(data2$data_rece1, format = "%m"));
day = as.numeric(format(data2$data_rece1, format = "%d"));
year=ifelse(year <2010, 2010,year); year=ifelse(year >2010, 2010,year);
data2$data_rece = as.character(as.Date(paste(day,month, year, sep="/"),format="%d/%m/%Y"))

data2$data_rece1 <- NULL
rm(year);rm(month);rm(day);
data2$receita= as.numeric(gsub(data2$receita, patt=",", replace="."))


# Renaming 2014 campaign finance data:
names(data2) <- c('cnpj_cand', 'seq_cand', 'uf', 'sigla_part','num_cand', 'desc_cargo', 'nome_cand', 'cpf_cand', 'doc_doador','nome_doador', 'uf_doador', 'num_part_doador', 'num_cand_doador', 'setor_econ_doador', 'desc_setor_econ_doador', 'data_rece', 'receita', 'tipo_rece', 'fonte_recurso', 'desc_recurso', 'desc_rece', 'doc_doador_ori', 'nome_doador_ori', 'tipo_doador_ori', 'desc_setor_econ_doador_ori', 'nome_doador_ori_RF')

data2$data_rece1 = data2$data_rece
data2$data_rece1 = as.Date(data2$data_rece1, format="%d/%m/%Y")

year = as.numeric(format(data2$data_rece1, format = "%Y"));
month = as.numeric(format(data2$data_rece1, format = "%m"));
day = as.numeric(format(data2$data_rece1, format = "%d"));
year=ifelse(year <2014, 2014,year); year=ifelse(year >2014, 2014,year);
data2$data_rece = as.character(as.Date(paste(day,month, year, sep="/"),format="%d/%m/%Y"))

data2$data_rece1 <- NULL
rm(year);rm(month);rm(day);
data2$receita= as.numeric(gsub(data2$receita, patt=",", replace="."))



names(rec_can_2008) = c('nome_cand', 'desc_sexo', 'desc_cargo', 'cargo', 'num_cand', 'uf', 'desc_unid_elei', 'unid_elei', 'tit_cand', 'cpf_cand', 'cnpj_cand', 'num_part', 'sigla_part', 'receita', 'data_rece', 'desc_rece', 'desc_recurso', 'nome_doador', 'doc_doador', 'uf_doador', 'desc_unid_doador','desc_sit_cadas', 'adm_fin', 'doc_adm_fin')

year = as.numeric(format(desp_can_2008$data_desp, format = "%Y"));
month = as.numeric(format(desp_can_2008$data_desp, format = "%m"));
day = as.numeric(format(desp_can_2008$data_desp, format = "%d"));



desp_can_2008$data_desp_ori = desp_can_2008$data_desp
desp_can_2008$data_desp = as.Date(paste(day,month, year, sep="/"),format="%d/%m/%Y")



desp_can_2008$despesa= as.numeric(gsub(desp_can_2008$despesa, patt=",", replace="."))






setwd("~/Desktop/data")
(files <- list.files(pattern= '\\.txt$'))
(info = file.info(files))
empty = rownames(info[info$size == 0, ])
votamunzona1994 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))


setwd("~/Desktop/data")
(files <- list.files(pattern= '\\.txt$'))
(info = file.info(files))
empty = rownames(info[info$size == 0, ])
votamunzona1998 <- do.call(rbind, lapply(list.files(path=".", pattern="\\.txt$"), read.table, sep=";",stringsAsFactors=FALSE,fileEncoding="LATIN1", fill=TRUE,blank.lines.skip=TRUE))


# From now reading files in parallel:
setwd('~/Desktop/data/')
(files <- list.files(pattern= '\\.txt$'))
(info = file.info(files))
empty = rownames(info[info$size == 0, ])

library(doParallel); registerDoParallel(cores = 4); library(foreach)
data2 <- foreach(i = files, .combine = rbind) %dopar% read.table(i, header = FALSE, sep=";", dec=",", stringsAsFactors=FALSE, fileEncoding="LATIN1", fill=TRUE, blank.lines.skip=TRUE)


# Reading in old file <1986:
setwd('~/Downloads/VOTACAO_CANDIDATO_UF_1970/')
(files <- list.files(pattern='\\.txt$'))
(info = file.info(files))
(empty = rownames(info[info$size == 0, ]))
FOREACH = {library(doParallel);registerDoParallel(cores = 4);library(foreach);
  data2 <- foreach(i = files, .combine = rbind) %dopar% read.table(i, header = FALSE, sep=";", stringsAsFactors=FALSE, fileEncoding="LATIN1", fill=TRUE, blank.lines.skip=TRUE) }

data2$V1 <- NULL; data2$V2 <- NULL;
data2[1:2,]

names(data2) <- c('ANO_ELEICAO', 'NUM_TURNO', 'DESCRICAO_ELEICAO', 'SIGLA_UF','SIGLA_UE', 'CODIGO_CARGO', 'NUMERO_CAND', 'SQ_CANDIDATO', 'NOME_CANDIDATO', 'NOME_URNA_CANDIDATO', 'DESCRICAO_CARGO', 'COD_SIT_CAND_SUPERIOR', 'DESC_SIT_CAND_SUPERIOR','CODIGO_SIT_CANDIDATO', 'DESC_SIT_CANDIDATO', 'CODIGO_SIT_CAND_TOT', 'DESC_SIT_CAND_TOT', 'NUMERO_PARTIDO', 'SIGLA_PARTIDO', 'NOME_PARTIDO', 'SEQUENCIAL_LEGENDA', 'NOME_COLIGACAO', 'COMPOSICAO_LEGENDA', 'TOTAL_VOTOS')


