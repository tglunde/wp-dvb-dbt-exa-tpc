echo $'\nStopping DB1...'
docker exec exa_exasol_1 dwad_client stop-wait DB1
echo
docker exec exa_exasol_1 dwad_client setup DB1 db1.cfg
echo $'\nStarting DB1...'
docker exec exa_exasol_1 dwad_client start-wait DB1
