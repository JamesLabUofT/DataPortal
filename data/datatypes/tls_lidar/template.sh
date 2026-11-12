# template.sh
Rscript /lab/portal/pull_data.R \
  --type {{datatype_id}} \
  --product {{product}} \
  --dest {{dest_dir}} \
  --ids {{#rows}}{{datapoint_name}}{{^last}},{{/last}}{{/rows}}