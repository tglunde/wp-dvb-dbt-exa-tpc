#!/bin/bash

DSN=localhost:8888
USER=sys
PWD=exasol
EXAPLUS=~/project/EXAplus-7.0.0/exaplus

$EXAPLUS -c $DSN -u $USER -p $PWD -f $1 
