#!/bin/bash

EXA_PORT=$(docker port exa_exasol_1 | grep 'tcp' | grep '8888' | grep -oP '(?<=:)\w+')
EXA_DSN=52.143.0.192:8563
EXA_USER=sys
EXA_PWD=Start123
AUTH_PWD=$(cat `dirname $0`/../iac/dvb/secrets/authenticator_password.txt)
DCOFILE=`dirname $0`/../iac/exa/docker-compose.yml


docker-compose -f $DCOFILE run --rm -e AUTHENTICATOR_PASSWORD=$AUTH_PWD \
    --entrypoint '/dvb_sql/create_db.sh' dvb_client \
    $EXA_DSN $EXA_USER $EXA_PWD
