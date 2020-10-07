#!/bin/bash

docker-compose -f `dirname $0`/../iac/dvb/docker-compose.yml down

docker-compose -f `dirname $0`/../iac/exa/docker-compose.yml down

docker volume rm exa_exasol