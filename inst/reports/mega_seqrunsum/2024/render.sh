out="mega_seqrunsum.html"

quarto render report.qmd \
    -o ${out} \
    --output-dir nogit
