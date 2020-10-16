# Setting up and running a Datavault Builder (DVB) / Exasol (EXA) tandem

## Update configuration

Change connection data in:

- scripts/dvb_dbsetup.sh
- scripts/exa_import.sh
- iac/dvb/docker-compose.yml

Obviously the data should match accordingly.

## Provision runtime images

Exasol images can be simply pulled.

For the Datavault Build you need to login at the public docker repository hub.docker.com.

## running

run ```scripts/startup.sh``` 

This will setup you can create all the structures via database to run DVB successfully, 


# Next steps

Under the DVB folder is an exported DVB deployment. Import it with DVB into DVB.
