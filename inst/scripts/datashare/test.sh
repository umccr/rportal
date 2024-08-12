#!/usr/bin/env bash

set -euo pipefail

./datashare.R --subject_id SBJ03144 --library_id_tumor L2301290 --csv_output urls.csv
./datashare.R --subject_id SBJ04397 --library_id_tumor L2301291 --csv_output urls.csv --append
./datashare.R --wts --subject_id SBJ05560 --library_id_tumor L2401254 --csv_output urls.csv --append
