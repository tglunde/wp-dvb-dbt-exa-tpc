#!/bin/bash

#start exasol
docker-compose -f `dirname $0`/../iac/exa/docker-compose.yml up -d exasol

#wait for db

`dirname $0`/dvb_dbsetup.sh 

cd `dirname $0`/../iac/dvb
docker-compose up -d
cd -

