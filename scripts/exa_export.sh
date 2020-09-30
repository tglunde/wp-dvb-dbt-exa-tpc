#!/bin/bash

DSN=localhost:8888
USER=sys
PWD=exasol
EXAPLUS=~/project/EXAplus-7.0.0/exaplus

$EXAPLUS -c $DSN -u $USER -p $PWD -sql 'create schema if not exists tool;'
$EXAPLUS -c $DSN -u $USER -p $PWD -f `dirname $0`/CREATE_*DDL*.sql
$EXAPLUS -c $DSN -u $USER -p $PWD -sql 'drop table if exists db_history.database_ddl;' 
$EXAPLUS -c $DSN -u $USER -p $PWD -sql 'execute script tool.create_ddl(true,true,true);'
$EXAPLUS -c $DSN -u $USER -p $PWD -sql "export (select ddl from db_history.database_ddl) into local csv file '$1' DELIMIT=NEVER;"
