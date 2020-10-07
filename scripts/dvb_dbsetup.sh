#!/bin/bash

EXA_PORT=$(docker port exa_exasol_1 | grep 'tcp' | grep '8888' | grep -oP '(?<=:)\w+')
EXA_DSN=exasol:$EXA_PORT
EXA_USER=sys
EXA_PWD=exasol
AUTH_PWD=$(cat `dirname $0`/../iac/dvb/secrets/authenticator_password.txt)
DCOFILE=`dirname $0`/../iac/exa/docker-compose.yml


docker-compose -f $DCOFILE run --rm -e AUTHENTICATOR_PASSWORD=$AUTH_PWD \
    --entrypoint '/dvb_sql/create_db.sh' dvb_client \
    $EXA_DSN $EXA_USER $EXA_PWD
