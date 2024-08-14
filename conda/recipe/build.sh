#!/bin/bash

export DISABLE_AUTOBREW=1
${R} CMD INSTALL --build . ${R_ARGS}

# Copy CLI to conda bin
mkdir -p ${PREFIX}/bin
cp ${SRC_DIR}/inst/scripts/datashare/datashare.R ${PREFIX}/bin
chmod +x ${PREFIX}/bin/datashare.R
