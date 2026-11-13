#!/bin/bash
# SBW genotype subset: {{n_rows}} sample(s) selected on the James Lab data portal.
# Save this as e.g. {{out}}.sh on the server. Step 1 can take hours, so run it overnight:
#   nohup bash {{out}}.sh > {{out}}.log 2>&1 &
set -euo pipefail

OUT="{{out}}"

if [ {{min_alleles}} -gt {{max_alleles}} ]; then
  echo "Minimum alleles ({{min_alleles}}) is larger than maximum ({{max_alleles}})" >&2
  exit 1
fi

# Samples to keep: Fastq_ID, one per line, no header
cat > "${OUT}_samples.txt" <<'EOF'
{{#rows}}
{{datapoint_name}}
{{/rows}}
EOF

{{#run_step1}}
# Step 1: filter SNPs by read depth, genotype quality and minor allele count (slow)
vcftools --vcf "{{input_vcf}}" \
  --minDP {{min_dp}} --minGQ {{min_gq}} --mac {{mac}} \
  --recode --recode-INFO-all --out "${OUT}_step1"
STEP1="${OUT}_step1.recode.vcf"
{{/run_step1}}
{{^run_step1}}
# Step 1 skipped: reusing an existing step-1 VCF
STEP1="{{step1_vcf}}"
{{/run_step1}}

# Step 2: keep only the selected samples
vcftools --vcf "$STEP1" --keep "${OUT}_samples.txt" \
  --recode --recode-INFO-all --out "${OUT}_step2"

# Step 3: drop SNPs with too much missing data
vcftools --vcf "${OUT}_step2.recode.vcf" --max-missing {{max_missing}} \
  --recode --recode-INFO-all --out "${OUT}_step3"

# Step 4: keep SNPs with the chosen number of alleles
vcftools --vcf "${OUT}_step3.recode.vcf" \
  --min-alleles {{min_alleles}} --max-alleles {{max_alleles}} \
  --recode --recode-INFO-all --out "${OUT}"

echo "Done: ${OUT}.recode.vcf"