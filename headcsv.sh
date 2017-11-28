#!/bin/bash
# Usage: make short head files of all *.csv files in directory
for f in *.csv
do
	echo "Create *.head.csv file - $f"
        head "$f" > "$f.head.csv" 
done