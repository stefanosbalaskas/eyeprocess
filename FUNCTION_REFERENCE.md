# eyeprocess public function reference

This development source exports **223 functions**. Functions in the experimental model layer require independent methodological and numerical validation before confirmatory use.

## Canonical dataset and provenance

- `add_provenance()`
- `append_eye_table()`
- `as_eye_dataset()`
- `compact_eye_dataset()`
- `get_eye_table()`
- `is_eye_dataset()`
- `new_eye_dataset()`
- `provenance_manifest()`
- `set_eye_table()`
- `validate_eye_dataset()`

## Trials, responses, and AOIs

- `add_responses()`
- `assign_aois()`
- `assign_trials()`
- `build_aoi_visits()`
- `build_item_responses()`
- `build_stimulus_intervals()`
- `build_trials()`
- `new_aoi()`
- `register_aois()`

## Timebase and coordinate operations

- `align_clock()`
- `apply_clock_transform()`
- `audit_clock_sync()`
- `audit_coordinate_spaces()`
- `audit_timebase()`
- `convert_coordinates()`
- `convert_xy()`
- `coordinate_space()`
- `estimate_clock_transform()`
- `estimate_sampling_rate()`
- `normalize_timebase()`
- `register_coordinate_space()`
- `synchronize_eye_biometrics()`

## Psychometric and statistical models

- `align_response_matrices()`
- `check_local_dependence()`
- `fit_accuracy_rt()`
- `fit_dif()`
- `fit_dynamic_aoi_model()`
- `fit_explanatory_irt()`
- `fit_irt()`
- `fit_joint_process_model()`
- `fit_shared_process_factor()`
- `item_parameters()`
- `model_data()`
- `model_fit_statistics()`
- `person_scores()`
- `response_matrix()`
- `response_time_matrix()`

## Quality and governance

- `analysis_readiness()`
- `audit_aois()`
- `audit_episodes()`
- `audit_event_order()`
- `audit_missingness()`
- `audit_pupil_quality()`
- `audit_sampling_rate()`
- `audit_signal_quality()`
- `audit_trial_coverage()`
- `check_feature_level()`
- `check_process_leakage()`
- `compare_aoi_definitions()`
- `compare_preprocessing()`
- `interpretive_warnings()`
- `sensitivity_process()`
- `store_quality()`

## Empirical export validation

- `anonymize_eye_dataset()`
- `compare_eye_datasets()`
- `create_validation_bundle()`
- `discover_validation_cases()`
- `eye_format_profiles()`
- `fingerprint_eye_dataset()`
- `format_compatibility_matrix()`
- `format_validation_spec()`
- `init_validation_corpus()`
- `inspect_eye_source()`
- `read_validation_manifest()`
- `roundtrip_eye_dataset()`
- `schema_coverage()`
- `schema_coverage_summary()`
- `source_preservation_audit()`
- `validate_eye_corpus()`
- `validate_eye_source()`
- `validate_eyelink_export()`
- `validate_generic_export()`
- `validate_pupillabs_export()`
- `validate_smi_export()`
- `validate_tobii_export()`
- `validation_manifest()`
- `write_format_validation_report()`
- `write_validation_manifest()`

## Export, reports, and bridges

- `as_eye_biometrics()`
- `export_canonical()`
- `import_canonical()`
- `read_eye_dataset()`
- `report_eye_dataset()`
- `report_processirt()`
- `write_eye_dataset()`
- `write_provenance()`

## Preprocessing and ocular events

- `baseline_pupil()`
- `detect_blinks()`
- `detect_fixations_idt()`
- `detect_fixations_ivt()`
- `detect_saccades()`
- `filter_gaze()`
- `filter_pupil()`
- `flag_gaze_outliers()`
- `gaze_velocity()`
- `interpolate_pupil()`
- `preprocess_eye()`
- `preprocess_spec()`
- `pupil_deconvolve()`
- `rolling_apply()`

## Schema and coordinate registry

- `canonical_table_names()`
- `empty_eye_table()`
- `eye_schema()`
- `new_coordinate_space()`
- `schema_table()`
- `standardize_eye_table()`
- `validate_eye_table()`

## Adapter registry and detection

- `combine_eye_datasets()`
- `detect_eye_format()`
- `eye_mapping()`
- `infer_eye_mapping()`
- `read_eye_export()`
- `read_eye_folder()`
- `register_eye_adapter()`
- `remap_recording_ids()`
- `supported_eye_formats()`
- `unregister_eye_adapter()`
- `validate_eye_mapping()`

## Feature extraction and scanpaths

- `derive_all_features()`
- `derive_biometric_features()`
- `derive_gaze_features()`
- `derive_pupil_features()`
- `derive_rt_features()`
- `feature_dictionary()`
- `feature_spec()`
- `features_wide()`
- `gaze_entropy()`
- `scanpath_sequence()`
- `summarize_fixations()`
- `transition_entropy()`
- `transition_matrix()`
- `trial_table()`

## Experimental models

- `estimate_ez_diffusion()`
- `fit_gaze_informed_irt()`
- `fit_gaze_weighted_choice()`
- `fit_multimodal_irt()`
- `fit_process_irt()`
- `fit_pupil_informed_irt()`
- `fit_strategy_mixture()`
- `functional_pupil_features()`
- `model_missing_process()`
- `process_irt_diagnostics()`
- `process_irt_spec()`
- `sensitivity_missing_process()`

## Gazepoint import

- `gp_align_media_ids()`
- `gp_audit_file_pairs()`
- `gp_check_biometrics_sync()`
- `gp_check_fixation_ids()`
- `gp_check_media_timing()`
- `gp_check_pupil_channels()`
- `gp_check_sampling_rate()`
- `gp_check_validity_fields()`
- `gp_identify_export_type()`
- `gp_list_export_fields()`
- `gp_match_biometrics()`
- `gp_match_recordings()`
- `gp_pair_exports()`
- `gp_parse_markers()`
- `gp_parse_media_events()`
- `gp_parse_user_events()`
- `gp_profile_export()`
- `gp_reconstruct_stimuli()`
- `gp_reconstruct_trials()`
- `gp_validate_export()`
- `is_gazepoint_export()`
- `read_gazepoint()`
- `read_gazepoint_aoi_statistics()`
- `read_gazepoint_biometrics()`
- `read_gazepoint_combined()`
- `read_gazepoint_events()`
- `read_gazepoint_fixations()`
- `read_gazepoint_folder()`
- `read_gazepoint_gaze()`

## Other vendor imports

- `is_eyelink_export()`
- `is_pupil_labs_export()`
- `is_smi_export()`
- `is_tobii_export()`
- `pupil_labs_format()`
- `read_eyelink_asc()`
- `read_eyelink_edf()`
- `read_eyelink_report()`
- `read_pupil_core()`
- `read_pupil_neon()`
- `read_pupillabs()`
- `read_smi()`
- `read_smi_aoi_export()`
- `read_smi_event_export()`
- `read_smi_raw_export()`
- `read_tobii()`

## Simulation and validation

- `parameter_recovery()`
- `power_process_simulation()`
- `simulate_eye_dataset()`
- `simulate_process_irt()`

## Visualisation

- `plot_aoi_dwell()`
- `plot_biometrics()`
- `plot_clock_alignment()`
- `plot_coordinate_spaces()`
- `plot_eye_overview()`
- `plot_eye_trace()`
- `plot_feature_correlation()`
- `plot_feature_distribution()`
- `plot_fixations()`
- `plot_gaze_heatmap()`
- `plot_item_difficulty()`
- `plot_missingness()`
- `plot_model_diagnostics()`
- `plot_pupil_timeseries()`
- `plot_sampling_rate()`
- `plot_scanpath()`
- `plot_signal_quality()`
- `plot_transition_matrix()`
- `plot_trial_timeline()`

## Generic import and mapping

- `read_eye_generic()`
