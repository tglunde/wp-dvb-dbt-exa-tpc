#!/bin/bash

#EXA_PORT=$(docker port exa_exasol_1 | grep 'tcp' | grep '8888' | grep -oP '(?<=:)\w+')
#DSN=localhost:$EXA_PORT

DSN=52.143.0.192:8563
USER=sys
PWD=Start123
EXAPLUS=exaplus
IMPORT_FILE=./import_sf500.sql

#Create schema and tables
$EXAPLUS -c $DSN -u $USER -p $PWD -f ./sql_dump.sql

#Import data 
ls | parallel -j 8 -a $IMPORT_FILE $EXAPLUS - c $DSN -u $USER -p $PWD -s dbt -sql
