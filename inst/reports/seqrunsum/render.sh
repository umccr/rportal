date_start="2024-08-03"
date_end="2024-08-05"
out="seqrunsum_${date_start}_${date_end}.html"

quarto render report.qmd \
    -P date_start:${date_start} \
    -P date_end:${date_end} \
    -o ${out} \
    --output-dir nogit
