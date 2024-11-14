#!/usr/bin/env bash

set -euo pipefail

#./datashare.R --subject_id SBJ03144 --library_id_tumor L2301290 --csv_output urls.csv
#./datashare.R --subject_id SBJ04397 --library_id_tumor L2301291 --csv_output urls.csv --append
#./datashare.R --subject_id SBJ05378 --wts --library_id_tumor L2401074 --csv_output urls.csv --append
#./datashare.R --wts --subject_id SBJ05560 --library_id_tumor L2401254 --csv_output urls.csv --append
#./datashare.R --wts --subject_id SBJ05424 --library_id_tumor L2401135 --wts_wfrn_prefix umccr__atlas__wts_tumor_only --csv_output urls.csv --append
./datashare.R --wts --s3 --library_id_tumor L2401578 --csv_output urls.csv
./datashare.R --s3 --library_id_tumor L2401595 --csv_output urls.csv --append
