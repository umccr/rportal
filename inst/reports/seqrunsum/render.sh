start="2024-05-18"
end="2024-05-20"
out="seqrunsum_${start}_${end}.html"

quarto render report.qmd \
    -P date_start:${start} \
    -P date_end:${end} \
    -o ${out} \
    --output-dir nogit
