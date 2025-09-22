###########################################################################################################

update(
  filepath = data_path,
  input_file = updated_data,
  output_file = updated_data
)

###########################################################################################################

filter(
  filepath = data_path,
  input_file = updated_data,
  output_file = filtered_data
)

###########################################################################################################

stitch(
  filepath = data_path,
  input_file = filtered_data,
  output_file = stitched_data
)

###########################################################################################################

nt_merge(
  filepath = data_path,
  stitch_input_file = stitched_data,
  orig_input_file = updated_data,
  output_file = merged_data
)
