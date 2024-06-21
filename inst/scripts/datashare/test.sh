#!/usr/bin/env bash

set -euo pipefail

./datashare.R --subject_id SBJ03144 --library_id_tumor L2301290 --csv_output urls.csv
./datashare.R --subject_id SBJ04397 --library_id_tumor L2301291 --csv_output urls.csv --append
./datashare.R --subject_id SBJ04398 --library_id_tumor L2301292 --csv_output urls.csv --append
./datashare.R --subject_id SBJ04399 --library_id_tumor L2301293 --csv_output urls.csv --append
./datashare.R --subject_id SBJ04400 --library_id_tumor L2301294 --csv_output urls.csv --append
./datashare.R --subject_id SBJ04400 --library_id_tumor L2301295 --csv_output urls.csv --append
