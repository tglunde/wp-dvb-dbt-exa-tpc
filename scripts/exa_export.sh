#!/bin/bash


EXA_PORT=$(docker port exa_exasol_1 | grep 'tcp' | grep '8888' | grep -oP '(?<=:)\w+')
DSN=localhost:$EXA_PORT
USER=sys
PWD=exasol
EXAPLUS=exaplus

$EXAPLUS -c $DSN -u $USER -p $PWD -sql 'create schema if not exists EXA_TOOLBOX;'
$EXAPLUS -c $DSN -u $USER -p $PWD -f `dirname $0`/ddl/CREATE_*DDL*.sql
$EXAPLUS -c $DSN -u $USER -p $PWD -sql 'drop table if exists db_history.database_ddl;' 
$EXAPLUS -c $DSN -u $USER -p $PWD -sql 'execute script EXA_TOOLBOX.create_ddl(false,false,true);'
$EXAPLUS -c $DSN -u $USER -p $PWD -sql "export (select ddl from db_history.database_ddl) into local csv file '$1' DELIMIT=NEVER;"
$EXAPLUS -c $DSN -u $USER -p $PWD -f `dirname $0`/ddl/dvb_cfg_export.sql
