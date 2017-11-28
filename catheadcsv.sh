#!/bin/bash
# Usage: concatenate the *.csv.header files onto the *.csv files in directory
for f in *.csv
do
	echo "$f"
	echo "$f.header"
	echo "$f.2"
        cat "$f.header" "$f" > "$f.2"
	head "$f.2"
done

# then rename from *.csv.2 to *.h.csv
rename -v 's/\.csv.2$/\.h.csv/'  *.csv.2
