#! /bin/bash
# Usage: ./access2csv.sh filename.accdb
# convert .accdb file to CSV 
# produces CSV tables in it
# the schema of the data base as filename.accdb.schema.txt

java -jar access2csv.jar $1

java -jar access2csv.jar $1 --schema > $1.schema.txt



