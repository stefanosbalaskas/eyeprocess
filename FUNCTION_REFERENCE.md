# eyeprocess function reference

Current package version: **0.8.0**

- Functions defined: 1436
- Exported functions: 812
- Registered S3 methods: 254

This file is generated from the current `NAMESPACE` and top-level `R/` source definitions. `NAMESPACE` is authoritative for the public export and registered-S3 surface.

## Exported functions

- `add_provenance()`
  Source: `R/002-class.R:296`
- `add_responses()`
  Source: `R/008-trials-aoi.R:168`
- `addm_glam_proxy_features()`
  Source: `R/066-process-decision-features-0-8.R:70`
- `adjust_pupil_confounds()`
  Source: `R/061-pupil-advanced-representations-0-8.R:403`
- `advanced_model_evidence_spec()`
  Source: `R/021-validation-program.R:401`
- `advanced_validation_grid()`
  Source: `R/022-advanced-models-v2.R:397`
- `algorithm_facet_effects()`
  Source: `R/055-requested-api-completion-0-7.R:170`
- `align_clock()`
  Source: `R/007-coordinates-time.R:207`
- `align_response_matrices()`
  Source: `R/013-models.R:43`
- `analysis_readiness()`
  Source: `R/011-quality-governance.R:242`
- `anonymize_eye_dataset()`
  Source: `R/017-format-validation.R:1167`
- `aoi_balance_coordinates()`
  Source: `R/032-compositional-aoi.R:242`
- `aoi_trajectory_features()`
  Source: `R/060-process-window-representations-0-8.R:340`
- `append_eye_table()`
  Source: `R/002-class.R:291`
- `apply_clock_transform()`
  Source: `R/007-coordinates-time.R:241`
- `apply_device_linking()`
  Source: `R/036-device-linking.R:79`
- `apply_offline_recalibration()`
  Source: `R/034-calibration-recalibration.R:149`
- `apply_preflight_decision()`
  Source: `R/058-process-preflight-governance-0-8.R:330`
- `as_eye_biometrics()`
  Source: `R/015-export-report-bridges.R:249`
- `as_eye_dataset()`
  Source: `R/002-class.R:62`
- `as_eyeprocess_eyeris()`
  Source: `R/020-interoperability-storage.R:583`
- `as_eyeprocess_eyetools()`
  Source: `R/020-interoperability-storage.R:574`
- `as_eyeprocess_eyetrackingr()`
  Source: `R/020-interoperability-storage.R:577`
- `as_eyeprocess_gazer()`
  Source: `R/020-interoperability-storage.R:580`
- `as_eyeprocess_pupillometryr()`
  Source: `R/020-interoperability-storage.R:586`
- `as_irt_recovery_results()`
  Source: `R/052-irt-validation-0-7.R:111`
- `as_procdata_sequence()`
  Source: `R/020-interoperability-storage.R:601`
- `as_seqhmm_data()`
  Source: `R/020-interoperability-storage.R:634`
- `as_traminer_sequence()`
  Source: `R/020-interoperability-storage.R:618`
- `assign_aois()`
  Source: `R/008-trials-aoi.R:303`
- `assign_aois_probabilistic()`
  Source: `R/031-probabilistic-aoi.R:35`
- `assign_process_feature_family()`
  Source: `R/066-process-decision-features-0-8.R:138`
- `assign_trials()`
  Source: `R/008-trials-aoi.R:147`
- `audit_3pl_process_signatures()`
  Source: `R/068-bayesian-3pl-process-diagnostics-0-8.R:268`
- `audit_advanced_model_evidence()`
  Source: `R/021-validation-program.R:499`
- `audit_aoi_separation()`
  Source: `R/031-probabilistic-aoi.R:140`
- `audit_aois()`
  Source: `R/011-quality-governance.R:156`
- `audit_bank_decision_stability()`
  Source: `R/043-item-bank-optimization.R:152`
- `audit_benchmark_release()`
  Source: `R/029-benchmark-reproducibility.R:315`
- `audit_bias()`
  Source: `R/052-irt-validation-0-7.R:179`
- `audit_biometric_imputation()`
  Source: `R/067-advanced-sensitivity-diagnostics-0-8.R:207`
- `audit_biometric_preflight()`
  Source: `R/058-process-preflight-governance-0-8.R:193`
- `audit_candidate_item_bank()`
  Source: `R/062-context-process-structure-0-8.R:530`
- `audit_channel_incremental_information()`
  Source: `R/052-irt-validation-0-7.R:849`
- `audit_clock_sync()`
  Source: `R/007-coordinates-time.R:282`
- `audit_convergence()`
  Source: `R/052-irt-validation-0-7.R:235`
- `audit_coordinate_spaces()`
  Source: `R/007-coordinates-time.R:119`
- `audit_coverage()`
  Source: `R/052-irt-validation-0-7.R:206`
- `audit_device_equivalence()`
  Source: `R/036-device-linking.R:108`
- `audit_distractor_attention()`
  Source: `R/050-process-irt-models-0-7.R:342`
- `audit_episodes()`
  Source: `R/011-quality-governance.R:108`
- `audit_event_order()`
  Source: `R/011-quality-governance.R:124`
- `audit_evidence_dependencies()`
  Source: `R/046-evidence-provenance-graph.R:121`
- `audit_fairness_transportability()`
  Source: `R/044-process-dif-fairness.R:130`
- `audit_frontier_model_contract()`
  Source: `R/065-frontier-gates-0-8.R:184`
- `audit_identifiability()`
  Source: `R/052-irt-validation-0-7.R:285`
- `audit_interval_width()`
  Source: `R/052-irt-validation-0-7.R:221`
- `audit_irf_shape()`
  Source: `R/051-advanced-process-irt-0-7.R:681`
- `audit_item_reduction_sensitivity()`
  Source: `R/067-advanced-sensitivity-diagnostics-0-8.R:124`
- `audit_latent_distribution()`
  Source: `R/055-requested-api-completion-0-7.R:310`
- `audit_measurement_transportability()`
  Source: `R/052-irt-validation-0-7.R:796`
- `audit_missingness()`
  Source: `R/011-quality-governance.R:170`
- `audit_model_promotion()`
  Source: `R/023-validation-orchestration.R:1471`
- `audit_multivariate_process_quality()`
  Source: `R/058-process-preflight-governance-0-8.R:422`
- `audit_nonparametric_rasch()`
  Source: `R/067-advanced-sensitivity-diagnostics-0-8.R:85`
- `audit_norm_transportability()`
  Source: `R/045-process-norms.R:89`
- `audit_presentation_accessibility()`
  Source: `R/058-process-preflight-governance-0-8.R:443`
- `audit_process_adjusted_dif()`
  Source: `R/051-advanced-process-irt-0-7.R:505`
- `audit_process_anomalies()`
  Source: `R/058-process-preflight-governance-0-8.R:370`
- `audit_process_drift()`
  Source: `R/059-deployment-drift-0-8.R:88`
- `audit_process_external_validity()`
  Source: `R/062-context-process-structure-0-8.R:397`
- `audit_process_local_dependence()`
  Source: `R/057-emerging-process-irt-0-7.R:50`
- `audit_process_measurement_invariance()`
  Source: `R/050-process-irt-models-0-7.R:556`
- `audit_process_reliability()`
  Source: `R/035-process-reliability.R:122`
- `audit_process_window_sensitivity()`
  Source: `R/060-process-window-representations-0-8.R:284`
- `audit_pupil_fatigue_drift()`
  Source: `R/061-pupil-advanced-representations-0-8.R:424`
- `audit_pupil_frequency_stability()`
  Source: `R/061-pupil-advanced-representations-0-8.R:160`
- `audit_pupil_quality()`
  Source: `R/011-quality-governance.R:80`
- `audit_pupil_registration()`
  Source: `R/037-pupil-registration.R:151`
- `audit_recalibration()`
  Source: `R/034-calibration-recalibration.R:177`
- `audit_rmse()`
  Source: `R/052-irt-validation-0-7.R:192`
- `audit_roundtrip_loss()`
  Source: `R/025-vendor-corpus.R:530`
- `audit_sampling_rate()`
  Source: `R/011-quality-governance.R:20`
- `audit_sbc()`
  Source: `R/052-irt-validation-0-7.R:499`
- `audit_signal_filter()`
  Source: `R/061-pupil-advanced-representations-0-8.R:528`
- `audit_signal_quality()`
  Source: `R/011-quality-governance.R:41`
- `audit_timebase()`
  Source: `R/007-coordinates-time.R:183`
- `audit_trial_coverage()`
  Source: `R/011-quality-governance.R:139`
- `audit_validation_completion()`
  Source: `R/023-validation-orchestration.R:1220`
- `audit_vendor_field_coverage()`
  Source: `R/025-vendor-corpus.R:582`
- `audit_vendor_validation()`
  Source: `R/021-validation-program.R:65`
- `audit_visual_context_dependence()`
  Source: `R/062-context-process-structure-0-8.R:141`
- `autoplot_eyeprocess()`
  Source: `R/030-measurement-intelligence-utils.R:344`
- `baseline_pupil()`
  Source: `R/009-preprocessing.R:171`
- `bayesian_process_diagnostic_flags()`
  Source: `R/068-bayesian-3pl-process-diagnostics-0-8.R:142`
- `bayesian_process_diagnostics_dashboard()`
  Source: `R/068-bayesian-3pl-process-diagnostics-0-8.R:25`
- `benchmark_expected_outputs()`
  Source: `R/029-benchmark-reproducibility.R:87`
- `benchmark_eye_storage()`
  Source: `R/028-api-storage-adapters.R:483`
- `benchmark_eyeprocess()`
  Source: `R/021-validation-program.R:1172`
- `bind_process_windows()`
  Source: `R/060-process-window-representations-0-8.R:248`
- `biometric_imputation_sensitivity()`
  Source: `R/067-advanced-sensitivity-diagnostics-0-8.R:155`
- `bootstrap_representative_scanpath()`
  Source: `R/041-representative-scanpaths.R:126`
- `build_aoi_visits()`
  Source: `R/008-trials-aoi.R:361`
- `build_compatibility_matrix()`
  Source: `R/025-vendor-corpus.R:637`
- `build_evidence_graph()`
  Source: `R/046-evidence-provenance-graph.R:22`
- `build_gazepoint_media_trials()`
  Source: `R/019-gazepoint-downstream-workflow.R:173`
- `build_item_responses()`
  Source: `R/008-trials-aoi.R:190`
- `build_stimulus_intervals()`
  Source: `R/008-trials-aoi.R:79`
- `build_trials()`
  Source: `R/008-trials-aoi.R:1`
- `calibration_transfer_audit()`
  Source: `R/052-irt-validation-0-7.R:930`
- `canonical_table_names()`
  Source: `R/001-schema.R:148`
- `check_feature_level()`
  Source: `R/011-quality-governance.R:205`
- `check_local_dependence()`
  Source: `R/013-models.R:325`
- `check_process_leakage()`
  Source: `R/011-quality-governance.R:188`
- `classify_item_missingness()`
  Source: `R/050-process-irt-models-0-7.R:372`
- `collect_eye_storage()`
  Source: `R/020-interoperability-storage.R:217`
- `collect_validation_evidence()`
  Source: `R/063-operational-validation-0-8.R:118`
- `collect_validation_jobs()`
  Source: `R/023-validation-orchestration.R:907`
- `combine_eye_datasets()`
  Source: `R/003-mapping-adapters.R:184`
- `compact_eye_dataset()`
  Source: `R/002-class.R:348`
- `compare_aoi_compositions()`
  Source: `R/032-compositional-aoi.R:207`
- `compare_aoi_definitions()`
  Source: `R/011-quality-governance.R:301`
- `compare_aoi_trajectories()`
  Source: `R/060-process-window-representations-0-8.R:418`
- `compare_bayesian_process_models()`
  Source: `R/067-advanced-sensitivity-diagnostics-0-8.R:245`
- `compare_decision_provenance()`
  Source: `R/046-evidence-provenance-graph.R:87`
- `compare_deployment_batches()`
  Source: `R/059-deployment-drift-0-8.R:207`
- `compare_diffusion_accuracy_rt()`
  Source: `R/027-strategy-diffusion-engines.R:1062`
- `compare_dynamic_transition_models()`
  Source: `R/024-dynamic-irtree-engine.R:817`
- `compare_engine_adapters()`
  Source: `R/028-api-storage-adapters.R:633`
- `compare_episode_structure()`
  Source: `R/042-process-episodes.R:107`
- `compare_eye_datasets()`
  Source: `R/017-format-validation.R:459`
- `compare_functional_scalar_models()`
  Source: `R/026-functional-pupil-engine.R:890`
- `compare_irt_models()`
  Source: `R/049-multimodal-irt-registry.R:363`
- `compare_latent_distribution_models()`
  Source: `R/055-requested-api-completion-0-7.R:378`
- `compare_model_engines()`
  Source: `R/021-validation-program.R:734`
- `compare_parametric_nonparametric_irf()`
  Source: `R/051-advanced-process-irt-0-7.R:655`
- `compare_preprocessing()`
  Source: `R/011-quality-governance.R:283`
- `compare_presentation_fairness()`
  Source: `R/058-process-preflight-governance-0-8.R:513`
- `compare_process_criterion_models()`
  Source: `R/062-context-process-structure-0-8.R:447`
- `compare_process_profile_solutions()`
  Source: `R/062-context-process-structure-0-8.R:370`
- `compare_pupil_kernels()`
  Source: `R/061-pupil-advanced-representations-0-8.R:308`
- `compare_raw_adjusted_pupil()`
  Source: `R/061-pupil-advanced-representations-0-8.R:466`
- `compare_scanpath_distributions()`
  Source: `R/041-representative-scanpaths.R:100`
- `compare_signal_filters()`
  Source: `R/061-pupil-advanced-representations-0-8.R:542`
- `compare_strategy_heterogeneity()`
  Source: `R/027-strategy-diffusion-engines.R:615`
- `compare_uncertainty_budgets()`
  Source: `R/033-process-uncertainty.R:203`
- `compare_validation_engines()`
  Source: `R/052-irt-validation-0-7.R:828`
- `compare_vendor_semantics()`
  Source: `R/025-vendor-corpus.R:431`
- `compare_visual_context_irt()`
  Source: `R/062-context-process-structure-0-8.R:121`
- `compatibility_evidence_matrix()`
  Source: `R/048-semantic-validation-0-7.R:746`
- `context_factor_effects()`
  Source: `R/062-context-process-structure-0-8.R:131`
- `convert_coordinates()`
  Source: `R/007-coordinates-time.R:67`
- `convert_xy()`
  Source: `R/007-coordinates-time.R:31`
- `coordinate_fidelity_audit()`
  Source: `R/048-semantic-validation-0-7.R:358`
- `coordinate_space()`
  Source: `R/007-coordinates-time.R:14`
- `create_public_benchmark()`
  Source: `R/021-validation-program.R:1266`
- `create_validation_bundle()`
  Source: `R/017-format-validation.R:1410`
- `cross_device_process_equating_audit()`
  Source: `R/054-additional-process-measurement-0-7.R:344`
- `cross_recurrence()`
  Source: `R/039-recurrence-analysis.R:67`
- `cross_version_adapter_regression()`
  Source: `R/055-requested-api-completion-0-7.R:700`
- `crossed_grouped_cv()`
  Source: `R/021-validation-program.R:1012`
- `crossed_grouped_folds()`
  Source: `R/021-validation-program.R:965`
- `crossmodal_recurrence_model()`
  Source: `R/047-measurement-intelligence-adapters.R:27`
- `decode_dynamic_states()`
  Source: `R/024-dynamic-irtree-engine.R:624`
- `decompose_dif_evidence()`
  Source: `R/044-process-dif-fairness.R:109`
- `decompose_pupil_phase_amplitude()`
  Source: `R/037-pupil-registration.R:74`
- `derive_all_features()`
  Source: `R/010-features.R:326`
- `derive_aoi_composition()`
  Source: `R/032-compositional-aoi.R:50`
- `derive_biometric_features()`
  Source: `R/010-features.R:293`
- `derive_gaze_features()`
  Source: `R/010-features.R:181`
- `derive_gazepoint_workflow_features()`
  Source: `R/019-gazepoint-downstream-workflow.R:558`
- `derive_pupil_features()`
  Source: `R/010-features.R:234`
- `derive_rt_features()`
  Source: `R/010-features.R:278`
- `design_process_dstudy()`
  Source: `R/035-process-reliability.R:76`
- `detect_blinks()`
  Source: `R/009-preprocessing.R:235`
- `detect_calibration_drift()`
  Source: `R/034-calibration-recalibration.R:37`
- `detect_corrupt_partitions()`
  Source: `R/028-api-storage-adapters.R:433`
- `detect_eye_format()`
  Source: `R/003-mapping-adapters.R:127`
- `detect_fixations_idt()`
  Source: `R/009-preprocessing.R:317`
- `detect_fixations_ivt()`
  Source: `R/009-preprocessing.R:269`
- `detect_irt_changepoints()`
  Source: `R/050-process-irt-models-0-7.R:629`
- `detect_process_changepoint()`
  Source: `R/055-requested-api-completion-0-7.R:179`
- `detect_process_changepoints()`
  Source: `R/042-process-episodes.R:14`
- `detect_saccades()`
  Source: `R/009-preprocessing.R:364`
- `device_facet_effects()`
  Source: `R/055-requested-api-completion-0-7.R:154`
- `diagnose_gaze_point_process()`
  Source: `R/040-fixation-point-process.R:175`
- `diffusion_identification_study()`
  Source: `R/027-strategy-diffusion-engines.R:1162`
- `diffusion_parameter_diagnostics()`
  Source: `R/027-strategy-diffusion-engines.R:952`
- `diffusion_posterior_predictive()`
  Source: `R/027-strategy-diffusion-engines.R:985`
- `discover_validation_cases()`
  Source: `R/017-format-validation.R:1027`
- `distractor_process_map()`
  Source: `R/050-process-irt-models-0-7.R:325`
- `drift_by_device()`
  Source: `R/059-deployment-drift-0-8.R:255`
- `drift_by_site()`
  Source: `R/059-deployment-drift-0-8.R:262`
- `drift_by_stimulus_version()`
  Source: `R/059-deployment-drift-0-8.R:276`
- `drift_by_vendor()`
  Source: `R/059-deployment-drift-0-8.R:269`
- `dynamic_irtree_recovery()`
  Source: `R/024-dynamic-irtree-engine.R:939`
- `dynamic_irtree_spec()`
  Source: `R/024-dynamic-irtree-engine.R:60`
- `dynamic_posterior_predictive_check()`
  Source: `R/024-dynamic-irtree-engine.R:684`
- `dynamic_transition_design()`
  Source: `R/024-dynamic-irtree-engine.R:322`
- `empty_eye_table()`
  Source: `R/001-schema.R:102`
- `encode_response_combinations()`
  Source: `R/057-emerging-process-irt-0-7.R:13`
- `engine_adapter_status()`
  Source: `R/028-api-storage-adapters.R:537`
- `equate_irt_scales()`
  Source: `R/051-advanced-process-irt-0-7.R:393`
- `estimate_clock_transform()`
  Source: `R/007-coordinates-time.R:211`
- `estimate_device_specific_error()`
  Source: `R/036-device-linking.R:137`
- `estimate_ez_diffusion()`
  Source: `R/016-advanced-experimental.R:152`
- `estimate_process_uncertainty()`
  Source: `R/033-process-uncertainty.R:54`
- `estimate_sampling_rate()`
  Source: `R/007-coordinates-time.R:139`
- `estimate_visual_exposure_probability()`
  Source: `R/050-process-irt-models-0-7.R:397`
- `event_roundtrip_audit()`
  Source: `R/055-requested-api-completion-0-7.R:628`
- `event_semantics_audit()`
  Source: `R/048-semantic-validation-0-7.R:481`
- `expected_process_information()`
  Source: `R/051-advanced-process-irt-0-7.R:803`
- `explain_latent_interaction()`
  Source: `R/055-requested-api-completion-0-7.R:246`
- `export_canonical()`
  Source: `R/015-export-report-bridges.R:118`
- `export_eye_bids()`
  Source: `R/020-interoperability-storage.R:267`
- `export_validation_bundle()`
  Source: `R/063-operational-validation-0-8.R:256`
- `external_model_engines()`
  Source: `R/028-api-storage-adapters.R:526`
- `external_validate_irt()`
  Source: `R/052-irt-validation-0-7.R:722`
- `extract_diffusion_parameters()`
  Source: `R/027-strategy-diffusion-engines.R:933`
- `extract_functional_pupil_parameters()`
  Source: `R/026-functional-pupil-engine.R:712`
- `extract_parameter_truth()`
  Source: `R/055-requested-api-completion-0-7.R:475`
- `extract_process_windows()`
  Source: `R/060-process-window-representations-0-8.R:131`
- `eye_format_profiles()`
  Source: `R/017-format-validation.R:57`
- `eye_mapping()`
  Source: `R/003-mapping-adapters.R:1`
- `eye_plot_spec()`
  Source: `R/030-measurement-intelligence-utils.R:240`
- `eye_schema()`
  Source: `R/001-schema.R:1`
- `eye_storage_spec()`
  Source: `R/020-interoperability-storage.R:93`
- `eye_stream_fidelity_audit()`
  Source: `R/048-semantic-validation-0-7.R:447`
- `eyeprocess_api_version()`
  Source: `R/028-api-storage-adapters.R:18`
- `eyeprocess_benchmark_study()`
  Source: `R/029-benchmark-reproducibility.R:22`
- `eyeprocess_deprecation()`
  Source: `R/028-api-storage-adapters.R:184`
- `facet_effects()`
  Source: `R/050-process-irt-models-0-7.R:541`
- `feature_dictionary()`
  Source: `R/010-features.R:355`
- `feature_spec()`
  Source: `R/010-features.R:1`
- `features_wide()`
  Source: `R/010-features.R:340`
- `field_fidelity_report()`
  Source: `R/048-semantic-validation-0-7.R:169`
- `filter_eye_signal()`
  Source: `R/061-pupil-advanced-representations-0-8.R:488`
- `filter_gaze()`
  Source: `R/009-preprocessing.R:52`
- `filter_pupil()`
  Source: `R/009-preprocessing.R:153`
- `filter_pupil_signal()`
  Source: `R/061-pupil-advanced-representations-0-8.R:523`
- `fingerprint_eye_dataset()`
  Source: `R/017-format-validation.R:431`
- `fingerprint_validation_case()`
  Source: `R/025-vendor-corpus.R:118`
- `fit_accuracy_rt()`
  Source: `R/013-models.R:167`
- `fit_aoi_compositional_model()`
  Source: `R/032-compositional-aoi.R:159`
- `fit_aoi_growth_curve()`
  Source: `R/060-process-window-representations-0-8.R:384`
- `fit_brms_adapter()`
  Source: `R/028-api-storage-adapters.R:650`
- `fit_censored_normal_process_irt()`
  Source: `R/054-additional-process-measurement-0-7.R:54`
- `fit_changepoint_multimodal_irt()`
  Source: `R/050-process-irt-models-0-7.R:681`
- `fit_changepoint_rt_irt()`
  Source: `R/050-process-irt-models-0-7.R:669`
- `fit_cognitive_diagnosis_process()`
  Source: `R/051-advanced-process-irt-0-7.R:199`
- `fit_continuous_time_irt()`
  Source: `R/051-advanced-process-irt-0-7.R:713`
- `fit_crossclassified_process_irt()`
  Source: `R/051-advanced-process-irt-0-7.R:291`
- `fit_crossclassified_process_irt_mhrm()`
  Source: `R/065-frontier-gates-0-8.R:166`
- `fit_device_linking()`
  Source: `R/036-device-linking.R:25`
- `fit_dif()`
  Source: `R/013-models.R:205`
- `fit_diffirt_adapter()`
  Source: `R/020-interoperability-storage.R:672`
- `fit_diffirt_engine_adapter()`
  Source: `R/028-api-storage-adapters.R:683`
- `fit_dynamic_aoi_model()`
  Source: `R/013-models.R:366`
- `fit_dynamic_gpirt()`
  Source: `R/051-advanced-process-irt-0-7.R:700`
- `fit_dynamic_irtree()`
  Source: `R/024-dynamic-irtree-engine.R:972`
- `fit_dynamic_irtree_stan()`
  Source: `R/024-dynamic-irtree-engine.R:583`
- `fit_event_time_irt()`
  Source: `R/055-requested-api-completion-0-7.R:426`
- `fit_explanatory_irt()`
  Source: `R/013-models.R:122`
- `fit_external_engine()`
  Source: `R/028-api-storage-adapters.R:578`
- `fit_eyetrackingr_adapter()`
  Source: `R/028-api-storage-adapters.R:685`
- `fit_fixation_point_process()`
  Source: `R/040-fixation-point-process.R:14`
- `fit_flow_mirt()`
  Source: `R/051-advanced-process-irt-0-7.R:748`
- `fit_functional_pupil_stan()`
  Source: `R/026-functional-pupil-engine.R:557`
- `fit_gaze_anchored_3pl_audit()`
  Source: `R/068-bayesian-3pl-process-diagnostics-0-8.R:172`
- `fit_gaze_diffusion_irt()`
  Source: `R/027-strategy-diffusion-engines.R:877`
- `fit_gaze_diffusion_stan()`
  Source: `R/027-strategy-diffusion-engines.R:825`
- `fit_gaze_informed_irt()`
  Source: `R/016-advanced-experimental.R:45`
- `fit_gaze_informed_missingness_irt()`
  Source: `R/055-requested-api-completion-0-7.R:62`
- `fit_gaze_weighted_choice()`
  Source: `R/016-advanced-experimental.R:197`
- `fit_gdina_adapter()`
  Source: `R/028-api-storage-adapters.R:668`
- `fit_gpirt()`
  Source: `R/051-advanced-process-irt-0-7.R:625`
- `fit_irt()`
  Source: `R/013-models.R:98`
- `fit_irt_model()`
  Source: `R/049-multimodal-irt-registry.R:314`
- `fit_item_parameter_seed_model()`
  Source: `R/062-context-process-structure-0-8.R:464`
- `fit_joint_functional_pupil_irt()`
  Source: `R/026-functional-pupil-engine.R:612`
- `fit_joint_gaze_rt_irt()`
  Source: `R/050-process-irt-models-0-7.R:53`
- `fit_joint_graded_rt_process_irt()`
  Source: `R/050-process-irt-models-0-7.R:164`
- `fit_joint_process_model()`
  Source: `R/013-models.R:332`
- `fit_joint_signal_missingness()`
  Source: `R/038-informative-missingness.R:44`
- `fit_kde_latent_distribution_irt()`
  Source: `R/065-frontier-gates-0-8.R:52`
- `fit_latent_class_process_irt()`
  Source: `R/051-advanced-process-irt-0-7.R:252`
- `fit_latent_space_irt()`
  Source: `R/051-advanced-process-irt-0-7.R:325`
- `fit_lnirt_adapter()`
  Source: `R/028-api-storage-adapters.R:652`
- `fit_manyfacet_process_irt()`
  Source: `R/050-process-irt-models-0-7.R:501`
- `fit_marked_gaze_process()`
  Source: `R/040-fixation-point-process.R:116`
- `fit_mirt_adapter()`
  Source: `R/028-api-storage-adapters.R:646`
- `fit_mixture_irt_process_classes()`
  Source: `R/067-advanced-sensitivity-diagnostics-0-8.R:20`
- `fit_multiblock_process_map()`
  Source: `R/062-context-process-structure-0-8.R:196`
- `fit_multimodal_irt()`
  Source: `R/016-advanced-experimental.R:55`
- `fit_multimodal_trait_irt()`
  Source: `R/054-additional-process-measurement-0-7.R:275`
- `fit_multinomial_transition()`
  Source: `R/024-dynamic-irtree-engine.R:400`
- `fit_multiple_response_process_irt()`
  Source: `R/057-emerging-process-irt-0-7.R:98`
- `fit_nominal_gaze_irt()`
  Source: `R/050-process-irt-models-0-7.R:244`
- `fit_nonignorable_missing_irt()`
  Source: `R/065-frontier-gates-0-8.R:92`
- `fit_offline_recalibration()`
  Source: `R/034-calibration-recalibration.R:96`
- `fit_omission_survival_irt()`
  Source: `R/050-process-irt-models-0-7.R:425`
- `fit_openmx_adapter()`
  Source: `R/028-api-storage-adapters.R:681`
- `fit_openmx_process_model()`
  Source: `R/020-interoperability-storage.R:688`
- `fit_persistence_gaze_diffusion_irt()`
  Source: `R/065-frontier-gates-0-8.R:72`
- `fit_phase_amplitude_irt()`
  Source: `R/037-pupil-registration.R:113`
- `fit_process_dif()`
  Source: `R/044-process-dif-fairness.R:14`
- `fit_process_gstudy()`
  Source: `R/035-process-reliability.R:18`
- `fit_process_hmm_irt()`
  Source: `R/051-advanced-process-irt-0-7.R:64`
- `fit_process_irt()`
  Source: `R/016-advanced-experimental.R:29`
- `fit_process_missingness_model()`
  Source: `R/047-measurement-intelligence-adapters.R:11`
- `fit_process_norms()`
  Source: `R/045-process-norms.R:12`
- `fit_process_observation_model()`
  Source: `R/038-informative-missingness.R:12`
- `fit_process_profile_mixture()`
  Source: `R/062-context-process-structure-0-8.R:294`
- `fit_process_rasch_tree()`
  Source: `R/067-advanced-sensitivity-diagnostics-0-8.R:216`
- `fit_pupil_confound_model()`
  Source: `R/061-pupil-advanced-representations-0-8.R:329`
- `fit_pupil_event_deconvolution()`
  Source: `R/061-pupil-advanced-representations-0-8.R:237`
- `fit_pupil_informed_irt()`
  Source: `R/016-advanced-experimental.R:50`
- `fit_pupillometryr_adapter()`
  Source: `R/028-api-storage-adapters.R:687`
- `fit_response_process_embedding_irt()`
  Source: `R/051-advanced-process-irt-0-7.R:592`
- `fit_revisit_process_cdm()`
  Source: `R/057-emerging-process-irt-0-7.R:154`
- `fit_seqhmm_adapter()`
  Source: `R/028-api-storage-adapters.R:656`
- `fit_shared_process_factor()`
  Source: `R/013-models.R:235`
- `fit_speed_accuracy_engagement_irt()`
  Source: `R/050-process-irt-models-0-7.R:137`
- `fit_strategy_mixture()`
  Source: `R/016-advanced-experimental.R:107`
- `fit_strategy_mixture_em()`
  Source: `R/027-strategy-diffusion-engines.R:262`
- `fit_strategy_mixture_stan()`
  Source: `R/027-strategy-diffusion-engines.R:348`
- `fit_tam_adapter()`
  Source: `R/028-api-storage-adapters.R:648`
- `fit_theory_strategy_irt()`
  Source: `R/027-strategy-diffusion-engines.R:379`
- `fit_traminer_adapter()`
  Source: `R/028-api-storage-adapters.R:654`
- `fit_validation_replicate()`
  Source: `R/055-requested-api-completion-0-7.R:508`
- `fit_variational_irt()`
  Source: `R/051-advanced-process-irt-0-7.R:761`
- `fit_visual_context_irt()`
  Source: `R/062-context-process-structure-0-8.R:80`
- `flag_gaze_outliers()`
  Source: `R/009-preprocessing.R:71`
- `format_compatibility_matrix()`
  Source: `R/017-format-validation.R:131`
- `format_validation_spec()`
  Source: `R/017-format-validation.R:3`
- `functional_pupil_basis()`
  Source: `R/026-functional-pupil-engine.R:463`
- `functional_pupil_diagnostics()`
  Source: `R/026-functional-pupil-engine.R:734`
- `functional_pupil_features()`
  Source: `R/016-advanced-experimental.R:73`
- `functional_pupil_irt_spec()`
  Source: `R/026-functional-pupil-engine.R:126`
- `gaze_anchored_3pl_alignment()`
  Source: `R/068-bayesian-3pl-process-diagnostics-0-8.R:254`
- `gaze_diffusion_spec()`
  Source: `R/027-strategy-diffusion-engines.R:706`
- `gaze_entropy()`
  Source: `R/010-features.R:147`
- `gaze_recurrence()`
  Source: `R/039-recurrence-analysis.R:45`
- `gaze_velocity()`
  Source: `R/009-preprocessing.R:103`
- `gazepoint_analysis_tables()`
  Source: `R/019-gazepoint-downstream-workflow.R:622`
- `gazepoint_irt_tables()`
  Source: `R/019-gazepoint-downstream-workflow.R:803`
- `gazepoint_workflow_spec()`
  Source: `R/019-gazepoint-downstream-workflow.R:28`
- `generalizability_process_study()`
  Source: `R/054-additional-process-measurement-0-7.R:303`
- `get_eye_table()`
  Source: `R/002-class.R:274`
- `get_irt_model()`
  Source: `R/049-multimodal-irt-registry.R:299`
- `gp_align_media_ids()`
  Source: `R/005-import-gazepoint.R:493`
- `gp_audit_file_pairs()`
  Source: `R/018-gazepoint-real-exports.R:753`
- `gp_check_biometrics_sync()`
  Source: `R/005-import-gazepoint.R:501`
- `gp_check_fixation_ids()`
  Source: `R/005-import-gazepoint.R:498`
- `gp_check_media_timing()`
  Source: `R/005-import-gazepoint.R:499`
- `gp_check_pupil_channels()`
  Source: `R/005-import-gazepoint.R:500`
- `gp_check_sampling_rate()`
  Source: `R/005-import-gazepoint.R:496`
- `gp_check_validity_fields()`
  Source: `R/005-import-gazepoint.R:497`
- `gp_identify_export_type()`
  Source: `R/018-gazepoint-real-exports.R:216`
- `gp_list_export_fields()`
  Source: `R/005-import-gazepoint.R:78`
- `gp_match_biometrics()`
  Source: `R/005-import-gazepoint.R:378`
- `gp_match_recordings()`
  Source: `R/005-import-gazepoint.R:377`
- `gp_pair_exports()`
  Source: `R/018-gazepoint-real-exports.R:734`
- `gp_parse_markers()`
  Source: `R/005-import-gazepoint.R:494`
- `gp_parse_media_events()`
  Source: `R/005-import-gazepoint.R:466`
- `gp_parse_user_events()`
  Source: `R/005-import-gazepoint.R:433`
- `gp_profile_export()`
  Source: `R/005-import-gazepoint.R:57`
- `gp_reconstruct_stimuli()`
  Source: `R/005-import-gazepoint.R:492`
- `gp_reconstruct_trials()`
  Source: `R/005-import-gazepoint.R:491`
- `gp_validate_export()`
  Source: `R/005-import-gazepoint.R:86`
- `grade_model_evidence()`
  Source: `R/052-irt-validation-0-7.R:975`
- `grouped_cv()`
  Source: `R/021-validation-program.R:908`
- `grouped_folds()`
  Source: `R/021-validation-program.R:881`
- `import_benchmark_study()`
  Source: `R/029-benchmark-reproducibility.R:99`
- `import_canonical()`
  Source: `R/015-export-report-bridges.R:119`
- `import_eye_bids()`
  Source: `R/020-interoperability-storage.R:406`
- `incremental_process_validity()`
  Source: `R/062-context-process-structure-0-8.R:437`
- `infer_eye_mapping()`
  Source: `R/003-mapping-adapters.R:66`
- `init_validation_corpus()`
  Source: `R/017-format-validation.R:982`
- `init_vendor_corpus()`
  Source: `R/025-vendor-corpus.R:56`
- `inspect_eye_source()`
  Source: `R/017-format-validation.R:187`
- `interpolate_pupil()`
  Source: `R/009-preprocessing.R:122`
- `interpretive_warnings()`
  Source: `R/011-quality-governance.R:225`
- `irt_compositional_channel()`
  Source: `R/049-multimodal-irt-registry.R:83`
- `irt_continuous_channel()`
  Source: `R/054-additional-process-measurement-0-7.R:13`
- `irt_count_channel()`
  Source: `R/049-multimodal-irt-registry.R:46`
- `irt_functional_channel()`
  Source: `R/049-multimodal-irt-registry.R:109`
- `irt_model_spec()`
  Source: `R/049-multimodal-irt-registry.R:131`
- `irt_nominal_channel()`
  Source: `R/049-multimodal-irt-registry.R:71`
- `irt_response_channel()`
  Source: `R/049-multimodal-irt-registry.R:22`
- `irt_rt_channel()`
  Source: `R/049-multimodal-irt-registry.R:34`
- `irt_sequence_channel()`
  Source: `R/049-multimodal-irt-registry.R:96`
- `irt_survival_channel()`
  Source: `R/049-multimodal-irt-registry.R:59`
- `irt_validation_spec()`
  Source: `R/052-irt-validation-0-7.R:66`
- `is_eye_dataset()`
  Source: `R/002-class.R:60`
- `is_eyelink_export()`
  Source: `R/006-import-other-vendors.R:364`
- `is_gazepoint_export()`
  Source: `R/018-gazepoint-real-exports.R:195`
- `is_pupil_labs_export()`
  Source: `R/006-import-other-vendors.R:117`
- `is_smi_export()`
  Source: `R/006-import-other-vendors.R:566`
- `is_tobii_export()`
  Source: `R/006-import-other-vendors.R:3`
- `item_objective_spec()`
  Source: `R/043-item-bank-optimization.R:12`
- `item_parameters()`
  Source: `R/013-models.R:269`
- `item_pareto_front()`
  Source: `R/043-item-bank-optimization.R:50`
- `label_process_episodes()`
  Source: `R/042-process-episodes.R:76`
- `latent_distribution_stress_test()`
  Source: `R/055-requested-api-completion-0-7.R:408`
- `latent_trait_trajectory()`
  Source: `R/051-advanced-process-irt-0-7.R:727`
- `leave_device_out_validation()`
  Source: `R/052-irt-validation-0-7.R:747`
- `leave_item_out_validation()`
  Source: `R/052-irt-validation-0-7.R:783`
- `leave_session_out_validation()`
  Source: `R/052-irt-validation-0-7.R:759`
- `leave_site_out_validation()`
  Source: `R/052-irt-validation-0-7.R:771`
- `list_irt_models()`
  Source: `R/049-multimodal-irt-registry.R:278`
- `map_latent_classes_to_process_profiles()`
  Source: `R/067-advanced-sensitivity-diagnostics-0-8.R:53`
- `migrate_eye_storage_schema()`
  Source: `R/028-api-storage-adapters.R:460`
- `model_data()`
  Source: `R/013-models.R:49`
- `model_fit_statistics()`
  Source: `R/013-models.R:304`
- `model_missing_process()`
  Source: `R/016-advanced-experimental.R:209`
- `model_promotion_spec()`
  Source: `R/023-validation-orchestration.R:1420`
- `model_validation_spec()`
  Source: `R/021-validation-program.R:162`
- `model_validation_summary()`
  Source: `R/021-validation-program.R:331`
- `monitor_dif_drift()`
  Source: `R/044-process-dif-fairness.R:59`
- `multiblock_contributions()`
  Source: `R/062-context-process-structure-0-8.R:257`
- `multiblock_person_coordinates()`
  Source: `R/062-context-process-structure-0-8.R:265`
- `multiblock_variable_coordinates()`
  Source: `R/062-context-process-structure-0-8.R:273`
- `negative_control_process_test()`
  Source: `R/052-irt-validation-0-7.R:894`
- `new_aoi()`
  Source: `R/008-trials-aoi.R:214`
- `new_coordinate_space()`
  Source: `R/001-schema.R:150`
- `new_eye_dataset()`
  Source: `R/002-class.R:1`
- `normalize_timebase()`
  Source: `R/007-coordinates-time.R:152`
- `object_schema()`
  Source: `R/028-api-storage-adapters.R:31`
- `open_eye_storage()`
  Source: `R/020-interoperability-storage.R:190`
- `open_partitioned_eye_storage()`
  Source: `R/028-api-storage-adapters.R:340`
- `optimize_item_bank()`
  Source: `R/043-item-bank-optimization.R:103`
- `option_process_information()`
  Source: `R/050-process-irt-models-0-7.R:306`
- `package_reproducibility_manifest()`
  Source: `R/029-benchmark-reproducibility.R:252`
- `parameter_recovery()`
  Source: `R/014-simulation.R:211`
- `partition_eye_storage()`
  Source: `R/028-api-storage-adapters.R:208`
- `person_scores()`
  Source: `R/013-models.R:287`
- `plot_aoi_balance_biplot()`
  Source: `R/032-compositional-aoi.R:329`
- `plot_aoi_boundary_risk()`
  Source: `R/031-probabilistic-aoi.R:382`
- `plot_aoi_composition_trajectory()`
  Source: `R/032-compositional-aoi.R:335`
- `plot_aoi_dwell()`
  Source: `R/012-plots.R:93`
- `plot_aoi_metric_uncertainty()`
  Source: `R/031-probabilistic-aoi.R:393`
- `plot_aoi_probability_map()`
  Source: `R/031-probabilistic-aoi.R:379`
- `plot_aoi_ternary()`
  Source: `R/032-compositional-aoi.R:327`
- `plot_aoi_transition_matrix()`
  Source: `R/064-next-generation-plots-0-8.R:590`
- `plot_aoi_transition_rank()`
  Source: `R/064-next-generation-plots-0-8.R:610`
- `plot_aoi_variation_matrix()`
  Source: `R/032-compositional-aoi.R:331`
- `plot_bank_information_coverage()`
  Source: `R/043-item-bank-optimization.R:204`
- `plot_biometrics()`
  Source: `R/012-plots.R:139`
- `plot_calibration_error_ellipses()`
  Source: `R/034-calibration-recalibration.R:241`
- `plot_calibration_vector_field()`
  Source: `R/034-calibration-recalibration.R:239`
- `plot_changepoint_ribbons()`
  Source: `R/042-process-episodes.R:158`
- `plot_clock_alignment()`
  Source: `R/012-plots.R:228`
- `plot_complete_case_sensitivity()`
  Source: `R/038-informative-missingness.R:154`
- `plot_compositional_group_difference()`
  Source: `R/032-compositional-aoi.R:333`
- `plot_coordinate_spaces()`
  Source: `R/012-plots.R:219`
- `plot_covariate_effect_surface()`
  Source: `R/040-fixation-point-process.R:220`
- `plot_cross_vendor_metric_matrix()`
  Source: `R/036-device-linking.R:187`
- `plot_crossmodal_recurrence()`
  Source: `R/039-recurrence-analysis.R:172`
- `plot_decision_stability()`
  Source: `R/043-item-bank-optimization.R:206`
- `plot_dependability_surface()`
  Source: `R/035-process-reliability.R:200`
- `plot_device_agreement()`
  Source: `R/036-device-linking.R:179`
- `plot_device_bias_by_magnitude()`
  Source: `R/036-device-linking.R:181`
- `plot_device_equivalence_intervals()`
  Source: `R/036-device-linking.R:185`
- `plot_device_transfer_curve()`
  Source: `R/036-device-linking.R:183`
- `plot_diagnostics()`
  Source: `R/030-measurement-intelligence-utils.R:290`
- `plot_diagonal_recurrence_profile()`
  Source: `R/039-recurrence-analysis.R:170`
- `plot_dif_drift_heatmap()`
  Source: `R/044-process-dif-fairness.R:177`
- `plot_distractor_information()`
  Source: `R/055-requested-api-completion-0-7.R:15`
- `plot_drift_over_time()`
  Source: `R/034-calibration-recalibration.R:243`
- `plot_episode_duration_distribution()`
  Source: `R/042-process-episodes.R:164`
- `plot_episode_transition_graph()`
  Source: `R/042-process-episodes.R:162`
- `plot_episode_waterfall()`
  Source: `R/042-process-episodes.R:160`
- `plot_evidence()`
  Source: `R/030-measurement-intelligence-utils.R:315`
- `plot_evidence_graph()`
  Source: `R/046-evidence-provenance-graph.R:164`
- `plot_eye_overview()`
  Source: `R/012-plots.R:10`
- `plot_eye_trace()`
  Source: `R/012-plots.R:29`
- `plot_fairness_transport_matrix()`
  Source: `R/044-process-dif-fairness.R:179`
- `plot_feature_correlation()`
  Source: `R/012-plots.R:206`
- `plot_feature_distribution()`
  Source: `R/012-plots.R:194`
- `plot_fixation_intensity()`
  Source: `R/040-fixation-point-process.R:214`
- `plot_fixations()`
  Source: `R/012-plots.R:42`
- `plot_fuzzy_transition_matrix()`
  Source: `R/031-probabilistic-aoi.R:390`
- `plot_gaze_heatmap()`
  Source: `R/012-plots.R:73`
- `plot_gazepoint_workflow()`
  Source: `R/019-gazepoint-downstream-workflow.R:950`
- `plot_group_icc_process_overlay()`
  Source: `R/044-process-dif-fairness.R:173`
- `plot_group_scanpath_transport()`
  Source: `R/041-representative-scanpaths.R:182`
- `plot_interval_coverage()`
  Source: `R/023-validation-orchestration.R:1325`
- `plot_irf_uncertainty()`
  Source: `R/055-requested-api-completion-0-7.R:282`
- `plot_item_decision_path()`
  Source: `R/046-evidence-provenance-graph.R:166`
- `plot_item_difficulty()`
  Source: `R/012-plots.R:240`
- `plot_item_group_process_curves()`
  Source: `R/044-process-dif-fairness.R:181`
- `plot_item_normative_deviation()`
  Source: `R/045-process-norms.R:137`
- `plot_item_pareto()`
  Source: `R/043-item-bank-optimization.R:200`
- `plot_item_phase_delay()`
  Source: `R/037-pupil-registration.R:213`
- `plot_item_sampling_reliability()`
  Source: `R/035-process-reliability.R:206`
- `plot_metric_dependency_graph()`
  Source: `R/046-evidence-provenance-graph.R:168`
- `plot_missingness()`
  Source: `R/012-plots.R:170`
- `plot_missingness_by_aoi()`
  Source: `R/038-informative-missingness.R:150`
- `plot_missingness_by_time()`
  Source: `R/038-informative-missingness.R:148`
- `plot_mnar_tipping_point()`
  Source: `R/038-informative-missingness.R:152`
- `plot_model_decision_impact()`
  Source: `R/046-evidence-provenance-graph.R:170`
- `plot_model_diagnostics()`
  Source: `R/012-plots.R:249`
- `plot_normative_fan()`
  Source: `R/045-process-norms.R:133`
- `plot_objective_tradeoffs()`
  Source: `R/043-item-bank-optimization.R:202`
- `plot_observation_probability()`
  Source: `R/038-informative-missingness.R:146`
- `plot_observed_expected_fixations()`
  Source: `R/040-fixation-point-process.R:222`
- `plot_parameter_recovery()`
  Source: `R/023-validation-orchestration.R:1297`
- `plot_person_item_space()`
  Source: `R/055-requested-api-completion-0-7.R:210`
- `plot_person_normative_profile()`
  Source: `R/045-process-norms.R:135`
- `plot_phase_amplitude_scores()`
  Source: `R/037-pupil-registration.R:211`
- `plot_probabilistic_scanpath()`
  Source: `R/031-probabilistic-aoi.R:387`
- `plot_process_centiles()`
  Source: `R/045-process-norms.R:131`
- `plot_process_changepoint()`
  Source: `R/055-requested-api-completion-0-7.R:187`
- `plot_process_channel_ablation_delta()`
  Source: `R/064-next-generation-plots-0-8.R:653`
- `plot_process_dif_forest()`
  Source: `R/044-process-dif-fairness.R:175`
- `plot_process_episodes()`
  Source: `R/042-process-episodes.R:156`
- `plot_process_feature_stability()`
  Source: `R/064-next-generation-plots-0-8.R:633`
- `plot_process_window_sensitivity()`
  Source: `R/064-next-generation-plots-0-8.R:821`
- `plot_pupil_activity_sensitivity()`
  Source: `R/064-next-generation-plots-0-8.R:813`
- `plot_pupil_activity_windows()`
  Source: `R/064-next-generation-plots-0-8.R:797`
- `plot_pupil_band_power()`
  Source: `R/064-next-generation-plots-0-8.R:783`
- `plot_pupil_components()`
  Source: `R/064-next-generation-plots-0-8.R:563`
- `plot_pupil_preprocessing_audit()`
  Source: `R/064-next-generation-plots-0-8.R:540`
- `plot_pupil_registration()`
  Source: `R/037-pupil-registration.R:207`
- `plot_pupil_spectrum()`
  Source: `R/064-next-generation-plots-0-8.R:764`
- `plot_pupil_timeseries()`
  Source: `R/012-plots.R:114`
- `plot_recalibration_before_after()`
  Source: `R/034-calibration-recalibration.R:245`
- `plot_recurrence_matrix()`
  Source: `R/039-recurrence-analysis.R:166`
- `plot_recurrence_network()`
  Source: `R/039-recurrence-analysis.R:174`
- `plot_registered_pupil_effects()`
  Source: `R/037-pupil-registration.R:215`
- `plot_reliability_by_metric()`
  Source: `R/035-process-reliability.R:202`
- `plot_representative_scanpath()`
  Source: `R/041-representative-scanpaths.R:178`
- `plot_sampling_rate()`
  Source: `R/012-plots.R:162`
- `plot_sbc_rank()`
  Source: `R/023-validation-orchestration.R:1352`
- `plot_scanpath()`
  Source: `R/012-plots.R:57`
- `plot_scanpath_atlas()`
  Source: `R/041-representative-scanpaths.R:176`
- `plot_scanpath_dispersion()`
  Source: `R/041-representative-scanpaths.R:180`
- `plot_scanpath_similarity_matrix()`
  Source: `R/041-representative-scanpaths.R:184`
- `plot_screen_coverage()`
  Source: `R/034-calibration-recalibration.R:247`
- `plot_selected_bank_profile()`
  Source: `R/043-item-bank-optimization.R:208`
- `plot_sensitivity()`
  Source: `R/030-measurement-intelligence-utils.R:329`
- `plot_session_stability()`
  Source: `R/035-process-reliability.R:204`
- `plot_signal_quality()`
  Source: `R/012-plots.R:154`
- `plot_spatial_residuals()`
  Source: `R/040-fixation-point-process.R:216`
- `plot_temporal_excitation_kernel()`
  Source: `R/040-fixation-point-process.R:218`
- `plot_transition_matrix()`
  Source: `R/012-plots.R:103`
- `plot_trial_timeline()`
  Source: `R/012-plots.R:181`
- `plot_uncertainty_by_item()`
  Source: `R/033-process-uncertainty.R:272`
- `plot_uncertainty_by_stage()`
  Source: `R/033-process-uncertainty.R:274`
- `plot_uncertainty_tornado()`
  Source: `R/033-process-uncertainty.R:270`
- `plot_uncertainty_waterfall()`
  Source: `R/033-process-uncertainty.R:268`
- `plot_validation_failures()`
  Source: `R/023-validation-orchestration.R:1376`
- `plot_validation_runtime()`
  Source: `R/023-validation-orchestration.R:1395`
- `plot_variance_components()`
  Source: `R/035-process-reliability.R:198`
- `plot_warping_functions()`
  Source: `R/037-pupil-registration.R:209`
- `plot_windowed_recurrence()`
  Source: `R/039-recurrence-analysis.R:168`
- `posterior_predictive_discrepancies()`
  Source: `R/052-irt-validation-0-7.R:537`
- `posterior_sbc_contract()`
  Source: `R/052-irt-validation-0-7.R:437`
- `power_process_simulation()`
  Source: `R/014-simulation.R:260`
- `preaction_process_features()`
  Source: `R/066-process-decision-features-0-8.R:14`
- `predict_aoi_trajectory()`
  Source: `R/060-process-window-representations-0-8.R:406`
- `predict_fixation_intensity()`
  Source: `R/040-fixation-point-process.R:145`
- `predict_item_parameter_priors()`
  Source: `R/062-context-process-structure-0-8.R:505`
- `predict_process_centiles()`
  Source: `R/045-process-norms.R:46`
- `predict_theta_at_time()`
  Source: `R/051-advanced-process-irt-0-7.R:738`
- `preflight_decisions()`
  Source: `R/058-process-preflight-governance-0-8.R:285`
- `preflight_exclusion_manifest()`
  Source: `R/058-process-preflight-governance-0-8.R:311`
- `preflight_failures()`
  Source: `R/058-process-preflight-governance-0-8.R:293`
- `preflight_passed()`
  Source: `R/058-process-preflight-governance-0-8.R:301`
- `prepare_dynamic_irtree_data()`
  Source: `R/024-dynamic-irtree-engine.R:194`
- `prepare_functional_pupil_data()`
  Source: `R/026-functional-pupil-engine.R:383`
- `prepare_gaze_diffusion_data()`
  Source: `R/027-strategy-diffusion-engines.R:767`
- `prepare_strategy_mixture_data()`
  Source: `R/027-strategy-diffusion-engines.R:188`
- `prepare_structured_unstructured_process_features()`
  Source: `R/065-frontier-gates-0-8.R:117`
- `preprocess_eye()`
  Source: `R/009-preprocessing.R:397`
- `preprocess_spec()`
  Source: `R/009-preprocessing.R:1`
- `preprocessing_multiverse()`
  Source: `R/021-validation-program.R:1135`
- `process_anomaly_distance()`
  Source: `R/058-process-preflight-governance-0-8.R:427`
- `process_channel_ablation()`
  Source: `R/054-additional-process-measurement-0-7.R:228`
- `process_criterion_associations()`
  Source: `R/062-context-process-structure-0-8.R:429`
- `process_dependent_discrimination_audit()`
  Source: `R/054-additional-process-measurement-0-7.R:149`
- `process_dif_nuisance_surrogate()`
  Source: `R/051-advanced-process-irt-0-7.R:483`
- `process_drift_alerts()`
  Source: `R/059-deployment-drift-0-8.R:194`
- `process_drift_spec()`
  Source: `R/059-deployment-drift-0-8.R:16`
- `process_feature_blocks()`
  Source: `R/062-context-process-structure-0-8.R:161`
- `process_feature_family_registry()`
  Source: `R/066-process-decision-features-0-8.R:110`
- `process_feature_stability()`
  Source: `R/066-process-decision-features-0-8.R:153`
- `process_irt_diagnostics()`
  Source: `R/016-advanced-experimental.R:60`
- `process_irt_spec()`
  Source: `R/016-advanced-experimental.R:1`
- `process_item_information()`
  Source: `R/051-advanced-process-irt-0-7.R:779`
- `process_ngram_features()`
  Source: `R/051-advanced-process-irt-0-7.R:538`
- `process_pattern_mixture()`
  Source: `R/038-informative-missingness.R:77`
- `process_person_fit()`
  Source: `R/051-advanced-process-irt-0-7.R:444`
- `process_preflight_spec()`
  Source: `R/058-process-preflight-governance-0-8.R:110`
- `process_profile_probabilities()`
  Source: `R/062-context-process-structure-0-8.R:351`
- `process_profile_summary()`
  Source: `R/062-context-process-structure-0-8.R:359`
- `process_residual_map()`
  Source: `R/051-advanced-process-irt-0-7.R:345`
- `process_sequence_embedding()`
  Source: `R/051-advanced-process-irt-0-7.R:569`
- `process_state_occupancy()`
  Source: `R/051-advanced-process-irt-0-7.R:168`
- `process_state_transition_summary()`
  Source: `R/051-advanced-process-irt-0-7.R:176`
- `process_uncertainty_spec()`
  Source: `R/033-process-uncertainty.R:12`
- `process_variance_components()`
  Source: `R/035-process-reliability.R:61`
- `process_window_spec()`
  Source: `R/060-process-window-representations-0-8.R:13`
- `promote_irt_model()`
  Source: `R/049-multimodal-irt-registry.R:393`
- `promote_vendor_support()`
  Source: `R/025-vendor-corpus.R:613`
- `propagate_aoi_uncertainty()`
  Source: `R/031-probabilistic-aoi.R:251`
- `propagate_process_uncertainty()`
  Source: `R/033-process-uncertainty.R:127`
- `provenance_manifest()`
  Source: `R/002-class.R:334`
- `prune_validation_checkpoints()`
  Source: `R/023-validation-orchestration.R:1615`
- `public_validation_corpus()`
  Source: `R/048-semantic-validation-0-7.R:687`
- `pupil_activity_index()`
  Source: `R/061-pupil-advanced-representations-0-8.R:92`
- `pupil_band_power()`
  Source: `R/061-pupil-advanced-representations-0-8.R:20`
- `pupil_confound_effects()`
  Source: `R/061-pupil-advanced-representations-0-8.R:411`
- `pupil_deconvolve()`
  Source: `R/009-preprocessing.R:211`
- `pupil_event_effects()`
  Source: `R/061-pupil-advanced-representations-0-8.R:298`
- `pupil_event_regressor()`
  Source: `R/061-pupil-advanced-representations-0-8.R:211`
- `pupil_frequency_features()`
  Source: `R/061-pupil-advanced-representations-0-8.R:120`
- `pupil_labs_format()`
  Source: `R/006-import-other-vendors.R:133`
- `pupil_preprocessing_grid()`
  Source: `R/026-functional-pupil-engine.R:792`
- `pupil_preprocessing_sensitivity()`
  Source: `R/026-functional-pupil-engine.R:829`
- `pupil_response_kernel()`
  Source: `R/061-pupil-advanced-representations-0-8.R:194`
- `pupil_unit_fidelity_audit()`
  Source: `R/048-semantic-validation-0-7.R:407`
- `pupil_velocity_activity()`
  Source: `R/061-pupil-advanced-representations-0-8.R:41`
- `quantify_process_leakage()`
  Source: `R/021-validation-program.R:1083`
- `query_eye_storage()`
  Source: `R/028-api-storage-adapters.R:375`
- `raven_reproduction_spec()`
  Source: `R/021-validation-program.R:794`
- `read_benchmark_table()`
  Source: `R/029-benchmark-reproducibility.R:72`
- `read_eye_dataset()`
  Source: `R/015-export-report-bridges.R:73`
- `read_eye_export()`
  Source: `R/003-mapping-adapters.R:152`
- `read_eye_folder()`
  Source: `R/003-mapping-adapters.R:172`
- `read_eye_generic()`
  Source: `R/004-import-generic.R:126`
- `read_eyelink_asc()`
  Source: `R/006-import-other-vendors.R:380`
- `read_eyelink_edf()`
  Source: `R/006-import-other-vendors.R:546`
- `read_eyelink_report()`
  Source: `R/006-import-other-vendors.R:540`
- `read_gazepoint()`
  Source: `R/018-gazepoint-real-exports.R:265`
- `read_gazepoint_aoi_statistics()`
  Source: `R/018-gazepoint-real-exports.R:538`
- `read_gazepoint_biometrics()`
  Source: `R/018-gazepoint-real-exports.R:771`
- `read_gazepoint_combined()`
  Source: `R/018-gazepoint-real-exports.R:848`
- `read_gazepoint_events()`
  Source: `R/005-import-gazepoint.R:321`
- `read_gazepoint_fixations()`
  Source: `R/018-gazepoint-real-exports.R:325`
- `read_gazepoint_folder()`
  Source: `R/018-gazepoint-real-exports.R:658`
- `read_gazepoint_gaze()`
  Source: `R/005-import-gazepoint.R:189`
- `read_gazepoint_summary()`
  Source: `R/018-gazepoint-real-exports.R:432`
- `read_pupil_core()`
  Source: `R/006-import-other-vendors.R:270`
- `read_pupil_neon()`
  Source: `R/006-import-other-vendors.R:148`
- `read_pupillabs()`
  Source: `R/006-import-other-vendors.R:140`
- `read_smi()`
  Source: `R/006-import-other-vendors.R:579`
- `read_smi_aoi_export()`
  Source: `R/006-import-other-vendors.R:629`
- `read_smi_event_export()`
  Source: `R/006-import-other-vendors.R:628`
- `read_smi_raw_export()`
  Source: `R/006-import-other-vendors.R:627`
- `read_tobii()`
  Source: `R/006-import-other-vendors.R:24`
- `read_validation_job_manifest()`
  Source: `R/023-validation-orchestration.R:307`
- `read_validation_manifest()`
  Source: `R/017-format-validation.R:969`
- `read_vendor_registry()`
  Source: `R/025-vendor-corpus.R:86`
- `recalibrate_after_changepoint()`
  Source: `R/050-process-irt-models-0-7.R:696`
- `recommended_validation_replications()`
  Source: `R/052-irt-validation-0-7.R:353`
- `recurrence_features()`
  Source: `R/039-recurrence-analysis.R:119`
- `redact_validation_case()`
  Source: `R/025-vendor-corpus.R:321`
- `register_aois()`
  Source: `R/008-trials-aoi.R:258`
- `register_coordinate_space()`
  Source: `R/007-coordinates-time.R:1`
- `register_eye_adapter()`
  Source: `R/003-mapping-adapters.R:95`
- `register_irt_model()`
  Source: `R/049-multimodal-irt-registry.R:159`
- `register_pupil_curves()`
  Source: `R/037-pupil-registration.R:14`
- `register_validation_case()`
  Source: `R/025-vendor-corpus.R:207`
- `register_vendor_semantics()`
  Source: `R/025-vendor-corpus.R:398`
- `remap_recording_ids()`
  Source: `R/003-mapping-adapters.R:227`
- `report_eye_dataset()`
  Source: `R/015-export-report-bridges.R:134`
- `report_processirt()`
  Source: `R/015-export-report-bridges.R:188`
- `reporting_guideline_audit()`
  Source: `R/021-validation-program.R:1198`
- `representative_scanpath()`
  Source: `R/041-representative-scanpaths.R:32`
- `response_matrix()`
  Source: `R/013-models.R:1`
- `response_time_matrix()`
  Source: `R/013-models.R:24`
- `resume_validation_jobs()`
  Source: `R/023-validation-orchestration.R:843`
- `rolling_apply()`
  Source: `R/009-preprocessing.R:39`
- `roundtrip_eye_bids()`
  Source: `R/055-requested-api-completion-0-7.R:678`
- `roundtrip_eye_dataset()`
  Source: `R/017-format-validation.R:575`
- `run_benchmark_reproduction()`
  Source: `R/029-benchmark-reproducibility.R:167`
- `run_eyeprocess_validation_program()`
  Source: `R/021-validation-program.R:1345`
- `run_gazepoint_workflow()`
  Source: `R/019-gazepoint-downstream-workflow.R:1327`
- `run_model_validation()`
  Source: `R/021-validation-program.R:215`
- `run_posterior_sbc()`
  Source: `R/052-irt-validation-0-7.R:453`
- `run_raven_reproduction()`
  Source: `R/021-validation-program.R:824`
- `run_sbc()`
  Source: `R/052-irt-validation-0-7.R:380`
- `run_validation_jobs()`
  Source: `R/023-validation-orchestration.R:708`
- `sbc_summary()`
  Source: `R/021-validation-program.R:672`
- `scanpath_dispersion()`
  Source: `R/041-representative-scanpaths.R:80`
- `scanpath_sequence()`
  Source: `R/010-features.R:94`
- `schema_coverage()`
  Source: `R/017-format-validation.R:275`
- `schema_coverage_summary()`
  Source: `R/017-format-validation.R:321`
- `schema_table()`
  Source: `R/001-schema.R:94`
- `score_partial_response_pattern()`
  Source: `R/063-operational-validation-0-8.R:11`
- `score_process_deviation()`
  Source: `R/045-process-norms.R:69`
- `score_response_stream()`
  Source: `R/063-operational-validation-0-8.R:29`
- `segment_process_episodes()`
  Source: `R/042-process-episodes.R:50`
- `select_next_item_process()`
  Source: `R/051-advanced-process-irt-0-7.R:817`
- `semantic_fidelity_spec()`
  Source: `R/048-semantic-validation-0-7.R:98`
- `semantic_loss_map()`
  Source: `R/048-semantic-validation-0-7.R:660`
- `semantic_roundtrip_audit()`
  Source: `R/048-semantic-validation-0-7.R:620`
- `sensitivity_missing_process()`
  Source: `R/016-advanced-experimental.R:225`
- `sensitivity_mnar_process()`
  Source: `R/038-informative-missingness.R:105`
- `sensitivity_process()`
  Source: `R/011-quality-governance.R:316`
- `session_facet_effects()`
  Source: `R/055-requested-api-completion-0-7.R:162`
- `set_eye_table()`
  Source: `R/002-class.R:281`
- `simulate_advanced_process_data()`
  Source: `R/022-advanced-models-v2.R:481`
- `simulate_dynamic_irtree_data()`
  Source: `R/024-dynamic-irtree-engine.R:871`
- `simulate_eye_dataset()`
  Source: `R/014-simulation.R:1`
- `simulate_from_model()`
  Source: `R/055-requested-api-completion-0-7.R:461`
- `simulate_gaze_diffusion_data()`
  Source: `R/027-strategy-diffusion-engines.R:1103`
- `simulate_irt_model()`
  Source: `R/049-multimodal-irt-registry.R:332`
- `simulate_presentation_variants()`
  Source: `R/058-process-preflight-governance-0-8.R:491`
- `simulate_process_cat()`
  Source: `R/051-advanced-process-irt-0-7.R:847`
- `simulate_process_irt()`
  Source: `R/014-simulation.R:178`
- `simulate_strategy_mixture_data()`
  Source: `R/027-strategy-diffusion-engines.R:662`
- `simulation_based_calibration()`
  Source: `R/021-validation-program.R:610`
- `source_preservation_audit()`
  Source: `R/017-format-validation.R:349`
- `split_validation_plan()`
  Source: `R/023-validation-orchestration.R:323`
- `standardize_eye_table()`
  Source: `R/001-schema.R:110`
- `storage_transaction_manifest()`
  Source: `R/028-api-storage-adapters.R:446`
- `store_quality()`
  Source: `R/011-quality-governance.R:10`
- `strategy_aoi_sensitivity()`
  Source: `R/027-strategy-diffusion-engines.R:541`
- `strategy_classification_uncertainty()`
  Source: `R/027-strategy-diffusion-engines.R:476`
- `strategy_label_switching_diagnostics()`
  Source: `R/027-strategy-diffusion-engines.R:499`
- `strategy_posterior_probabilities()`
  Source: `R/027-strategy-diffusion-engines.R:433`
- `streaming_score_history()`
  Source: `R/063-operational-validation-0-8.R:98`
- `stress_test_latent_distribution()`
  Source: `R/052-irt-validation-0-7.R:615`
- `stress_test_local_dependence()`
  Source: `R/052-irt-validation-0-7.R:630`
- `stress_test_missingness()`
  Source: `R/052-irt-validation-0-7.R:657`
- `stress_test_misspecification()`
  Source: `R/052-irt-validation-0-7.R:577`
- `stress_test_preprocessing()`
  Source: `R/052-irt-validation-0-7.R:673`
- `stress_test_speededness()`
  Source: `R/052-irt-validation-0-7.R:643`
- `structural_transition_mask()`
  Source: `R/024-dynamic-irtree-engine.R:278`
- `summarise_aoi_membership()`
  Source: `R/031-probabilistic-aoi.R:204`
- `summarize_fixations()`
  Source: `R/010-features.R:51`
- `summarize_parameter_recovery()`
  Source: `R/052-irt-validation-0-7.R:132`
- `summarize_process_windows()`
  Source: `R/060-process-window-representations-0-8.R:229`
- `supported_eye_formats()`
  Source: `R/003-mapping-adapters.R:114`
- `synchronize_eye_biometrics()`
  Source: `R/007-coordinates-time.R:257`
- `theory_strategy_spec()`
  Source: `R/027-strategy-diffusion-engines.R:66`
- `timestamp_fidelity_audit()`
  Source: `R/048-semantic-validation-0-7.R:290`
- `trace_item_decision()`
  Source: `R/046-evidence-provenance-graph.R:67`
- `transform_aoi_composition()`
  Source: `R/032-compositional-aoi.R:117`
- `transition_entropy()`
  Source: `R/010-features.R:170`
- `transition_matrix()`
  Source: `R/010-features.R:123`
- `transition_residual_diagnostics()`
  Source: `R/024-dynamic-irtree-engine.R:746`
- `trial_table()`
  Source: `R/010-features.R:46`
- `uncertainty_budget()`
  Source: `R/033-process-uncertainty.R:190`
- `unregister_eye_adapter()`
  Source: `R/003-mapping-adapters.R:108`
- `update_person_score()`
  Source: `R/063-operational-validation-0-8.R:82`
- `upgrade_eye_dataset()`
  Source: `R/028-api-storage-adapters.R:139`
- `upgrade_eyeprocess_model()`
  Source: `R/028-api-storage-adapters.R:162`
- `validate_benchmark_study()`
  Source: `R/029-benchmark-reproducibility.R:118`
- `validate_bids_eye_semantics()`
  Source: `R/048-semantic-validation-0-7.R:560`
- `validate_engine_adapter()`
  Source: `R/028-api-storage-adapters.R:617`
- `validate_eye_corpus()`
  Source: `R/017-format-validation.R:1053`
- `validate_eye_dataset()`
  Source: `R/002-class.R:124`
- `validate_eye_mapping()`
  Source: `R/003-mapping-adapters.R:53`
- `validate_eye_source()`
  Source: `R/017-format-validation.R:623`
- `validate_eye_storage_metadata()`
  Source: `R/028-api-storage-adapters.R:409`
- `validate_eye_table()`
  Source: `R/001-schema.R:121`
- `validate_eyelink_export()`
  Source: `R/017-format-validation.R:1506`
- `validate_gazepoint_workflow()`
  Source: `R/019-gazepoint-downstream-workflow.R:1475`
- `validate_generic_export()`
  Source: `R/017-format-validation.R:1532`
- `validate_hed_event_semantics()`
  Source: `R/048-semantic-validation-0-7.R:523`
- `validate_irt_model()`
  Source: `R/049-multimodal-irt-registry.R:347`
- `validate_latent_space_process_similarity()`
  Source: `R/051-advanced-process-irt-0-7.R:364`
- `validate_model_object()`
  Source: `R/028-api-storage-adapters.R:92`
- `validate_process_windows()`
  Source: `R/060-process-window-representations-0-8.R:261`
- `validate_pupillabs_export()`
  Source: `R/017-format-validation.R:1495`
- `validate_smi_export()`
  Source: `R/017-format-validation.R:1522`
- `validate_strategy_manipulation()`
  Source: `R/027-strategy-diffusion-engines.R:576`
- `validate_tobii_export()`
  Source: `R/017-format-validation.R:1481`
- `validate_vendor_semantics()`
  Source: `R/055-requested-api-completion-0-7.R:568`
- `validate_vendor_timestamp_semantics()`
  Source: `R/048-semantic-validation-0-7.R:801`
- `validation_bundle_manifest()`
  Source: `R/063-operational-validation-0-8.R:145`
- `validation_calibration_summary()`
  Source: `R/023-validation-orchestration.R:1100`
- `validation_evidence_levels()`
  Source: `R/048-semantic-validation-0-7.R:63`
- `validation_failure_summary()`
  Source: `R/023-validation-orchestration.R:1050`
- `validation_failure_taxonomy()`
  Source: `R/052-irt-validation-0-7.R:260`
- `validation_job_plan()`
  Source: `R/023-validation-orchestration.R:127`
- `validation_manifest()`
  Source: `R/017-format-validation.R:868`
- `validation_mcse()`
  Source: `R/052-irt-validation-0-7.R:323`
- `validation_recovery_summary()`
  Source: `R/023-validation-orchestration.R:996`
- `validation_report()`
  Source: `R/063-operational-validation-0-8.R:181`
- `validation_runtime_summary()`
  Source: `R/023-validation-orchestration.R:1072`
- `validation_sbc_summary()`
  Source: `R/023-validation-orchestration.R:1141`
- `validation_seed()`
  Source: `R/023-validation-orchestration.R:77`
- `validation_thresholds()`
  Source: `R/023-validation-orchestration.R:1186`
- `vendor_schema_contract()`
  Source: `R/055-requested-api-completion-0-7.R:548`
- `vendor_validation_spec()`
  Source: `R/021-validation-program.R:31`
- `verify_reproducibility_manifest()`
  Source: `R/029-benchmark-reproducibility.R:267`
- `visual_context_registry()`
  Source: `R/062-context-process-structure-0-8.R:13`
- `windowed_recurrence()`
  Source: `R/039-recurrence-analysis.R:92`
- `write_advanced_model_evidence_report()`
  Source: `R/021-validation-program.R:578`
- `write_benchmark_data_dictionary()`
  Source: `R/029-benchmark-reproducibility.R:233`
- `write_eye_dataset()`
  Source: `R/015-export-report-bridges.R:4`
- `write_eye_storage()`
  Source: `R/020-interoperability-storage.R:126`
- `write_format_validation_report()`
  Source: `R/017-format-validation.R:1321`
- `write_gazepoint_workflow_report()`
  Source: `R/019-gazepoint-downstream-workflow.R:1140`
- `write_model_promotion_report()`
  Source: `R/023-validation-orchestration.R:1594`
- `write_partitioned_eye_storage()`
  Source: `R/028-api-storage-adapters.R:266`
- `write_provenance()`
  Source: `R/015-export-report-bridges.R:121`
- `write_reporting_guideline_report()`
  Source: `R/021-validation-program.R:1243`
- `write_software_paper_reproduction()`
  Source: `R/029-benchmark-reproducibility.R:282`
- `write_software_paper_scaffold()`
  Source: `R/021-validation-program.R:1302`
- `write_validation_job_manifest()`
  Source: `R/023-validation-orchestration.R:281`
- `write_validation_manifest()`
  Source: `R/017-format-validation.R:963`
- `write_validation_release_report()`
  Source: `R/023-validation-orchestration.R:1550`
- `write_validation_report()`
  Source: `R/063-operational-validation-0-8.R:237`
- `write_vendor_case_report()`
  Source: `R/025-vendor-corpus.R:691`
- `write_vendor_registry()`
  Source: `R/025-vendor-corpus.R:102`
- `write_vendor_validation_report()`
  Source: `R/021-validation-program.R:134`

## Registered S3 methods

- `as_eye_biometrics.data.frame()`
  Registration: `S3method(as_eye_biometrics, data.frame)`
  Source: `R/015-export-report-bridges.R:256`
- `as_eye_biometrics.eye_dataset()`
  Registration: `S3method(as_eye_biometrics, eye_dataset)`
  Source: `R/015-export-report-bridges.R:251`
- `as_eye_dataset.data.frame()`
  Registration: `S3method(as_eye_dataset, data.frame)`
  Source: `R/002-class.R:66`
- `as_eye_dataset.default()`
  Registration: `S3method(as_eye_dataset, default)`
  Source: `R/002-class.R:70`
- `as_eye_dataset.eye_dataset()`
  Registration: `S3method(as_eye_dataset, eye_dataset)`
  Source: `R/002-class.R:64`
- `as_eye_dataset.gp3_analysis()`
  Registration: `S3method(as_eye_dataset, gp3_analysis)`
  Source: `R/015-export-report-bridges.R:220`
- `as_eye_dataset.gp3_data()`
  Registration: `S3method(as_eye_dataset, gp3_data)`
  Source: `R/015-export-report-bridges.R:221`
- `as_eye_dataset.gp3_recording()`
  Registration: `S3method(as_eye_dataset, gp3_recording)`
  Source: `R/015-export-report-bridges.R:213`
- `as_eye_dataset.gpbiometrics_data()`
  Registration: `S3method(as_eye_dataset, gpbiometrics_data)`
  Source: `R/015-export-report-bridges.R:223`
- `plot.eye_corpus_validation()`
  Registration: `S3method(plot, eye_corpus_validation)`
  Source: `R/017-format-validation.R:1137`
- `plot.eye_dataset()`
  Registration: `S3method(plot, eye_dataset)`
  Source: `R/012-plots.R:8`
- `plot.eye_format_validation()`
  Registration: `S3method(plot, eye_format_validation)`
  Source: `R/017-format-validation.R:835`
- `plot.eye_parameter_recovery()`
  Registration: `S3method(plot, eye_parameter_recovery)`
  Source: `R/014-simulation.R:252`
- `predict.eyeprocess_model()`
  Registration: `S3method(predict, eyeprocess_model)`
  Source: `R/013-models.R:317`
- `print.eye_aoi()`
  Registration: `S3method(print, eye_aoi)`
  Source: `R/008-trials-aoi.R:253`
- `print.eye_clock_transform()`
  Registration: `S3method(print, eye_clock_transform)`
  Source: `R/007-coordinates-time.R:232`
- `print.eye_corpus_validation()`
  Registration: `S3method(print, eye_corpus_validation)`
  Source: `R/017-format-validation.R:1129`
- `print.eye_dataset()`
  Registration: `S3method(print, eye_dataset)`
  Source: `R/002-class.R:74`
- `print.eye_dynamic_aoi()`
  Registration: `S3method(print, eye_dynamic_aoi)`
  Source: `R/013-models.R:378`
- `print.eye_feature_spec()`
  Registration: `S3method(print, eye_feature_spec)`
  Source: `R/010-features.R:20`
- `print.eye_format_detection()`
  Registration: `S3method(print, eye_format_detection)`
  Source: `R/003-mapping-adapters.R:146`
- `print.eye_format_validation()`
  Registration: `S3method(print, eye_format_validation)`
  Source: `R/017-format-validation.R:809`
- `print.eye_format_validation_spec()`
  Registration: `S3method(print, eye_format_validation_spec)`
  Source: `R/017-format-validation.R:44`
- `print.eye_gazepoint_workflow()`
  Registration: `S3method(print, eye_gazepoint_workflow)`
  Source: `R/019-gazepoint-downstream-workflow.R:1441`
- `print.eye_gazepoint_workflow_spec()`
  Registration: `S3method(print, eye_gazepoint_workflow_spec)`
  Source: `R/019-gazepoint-downstream-workflow.R:87`
- `print.eye_mapping()`
  Registration: `S3method(print, eye_mapping)`
  Source: `R/003-mapping-adapters.R:43`
- `print.eye_missing_sensitivity()`
  Registration: `S3method(print, eye_missing_sensitivity)`
  Source: `R/016-advanced-experimental.R:240`
- `print.eye_preprocess_spec()`
  Registration: `S3method(print, eye_preprocess_spec)`
  Source: `R/009-preprocessing.R:30`
- `print.eye_readiness()`
  Registration: `S3method(print, eye_readiness)`
  Source: `R/011-quality-governance.R:276`
- `print.eye_roundtrip_validation()`
  Registration: `S3method(print, eye_roundtrip_validation)`
  Source: `R/017-format-validation.R:600`
- `print.eye_sensitivity()`
  Registration: `S3method(print, eye_sensitivity)`
  Source: `R/011-quality-governance.R:322`
- `print.eye_strategy_mixture()`
  Registration: `S3method(print, eye_strategy_mixture)`
  Source: `R/016-advanced-experimental.R:146`
- `print.eye_two_stage_rt()`
  Registration: `S3method(print, eye_two_stage_rt)`
  Source: `R/013-models.R:199`
- `print.eye_validation()`
  Registration: `S3method(print, eye_validation)`
  Source: `R/002-class.R:264`
- `print.eyeprocess_model()`
  Registration: `S3method(print, eyeprocess_model)`
  Source: `R/013-models.R:71`
- `print.gazepoint_summary()`
  Registration: `S3method(print, gazepoint_summary)`
  Source: `R/018-gazepoint-real-exports.R:454`
- `print.process_irt_spec()`
  Registration: `S3method(print, process_irt_spec)`
  Source: `R/016-advanced-experimental.R:20`
- `print.summary.eye_dataset()`
  Registration: `S3method(print, summary.eye_dataset)`
  Source: `R/002-class.R:113`
- `print.summary.eyeprocess_model()`
  Registration: `S3method(print, summary.eyeprocess_model)`
  Source: `R/013-models.R:89`
- `summary.eye_dataset()`
  Registration: `S3method(summary, eye_dataset)`
  Source: `R/002-class.R:93`
- `summary.eye_format_validation()`
  Registration: `S3method(summary, eye_format_validation)`
  Source: `R/017-format-validation.R:819`
- `summary.eye_gazepoint_workflow()`
  Registration: `S3method(summary, eye_gazepoint_workflow)`
  Source: `R/019-gazepoint-downstream-workflow.R:1454`
- `summary.eye_parameter_recovery()`
  Registration: `S3method(summary, eye_parameter_recovery)`
  Source: `R/014-simulation.R:246`
- `summary.eyeprocess_model()`
  Registration: `S3method(summary, eyeprocess_model)`
  Source: `R/013-models.R:80`
- `print.eye_storage()`
  Registration: `S3method(print, eye_storage)`
  Source: `R/020-interoperability-storage.R:244`
- `print.eye_vendor_validation()`
  Registration: `S3method(print, eye_vendor_validation)`
  Source: `R/021-validation-program.R:113`
- `plot.eye_vendor_validation()`
  Registration: `S3method(plot, eye_vendor_validation)`
  Source: `R/021-validation-program.R:120`
- `print.eye_model_validation()`
  Registration: `S3method(print, eye_model_validation)`
  Source: `R/021-validation-program.R:363`
- `plot.eye_model_validation()`
  Registration: `S3method(plot, eye_model_validation)`
  Source: `R/021-validation-program.R:370`
- `print.eye_advanced_evidence_audit()`
  Registration: `S3method(print, eye_advanced_evidence_audit)`
  Source: `R/021-validation-program.R:557`
- `plot.eye_advanced_evidence_audit()`
  Registration: `S3method(plot, eye_advanced_evidence_audit)`
  Source: `R/021-validation-program.R:564`
- `print.eye_sbc()`
  Registration: `S3method(print, eye_sbc)`
  Source: `R/021-validation-program.R:708`
- `plot.eye_sbc()`
  Registration: `S3method(plot, eye_sbc)`
  Source: `R/021-validation-program.R:715`
- `print.eye_engine_comparison()`
  Registration: `S3method(print, eye_engine_comparison)`
  Source: `R/021-validation-program.R:765`
- `plot.eye_engine_comparison()`
  Registration: `S3method(plot, eye_engine_comparison)`
  Source: `R/021-validation-program.R:773`
- `print.eye_empirical_reproduction()`
  Registration: `S3method(print, eye_empirical_reproduction)`
  Source: `R/021-validation-program.R:850`
- `plot.eye_empirical_reproduction()`
  Registration: `S3method(plot, eye_empirical_reproduction)`
  Source: `R/021-validation-program.R:858`
- `print.eye_grouped_cv()`
  Registration: `S3method(print, eye_grouped_cv)`
  Source: `R/021-validation-program.R:936`
- `plot.eye_grouped_cv()`
  Registration: `S3method(plot, eye_grouped_cv)`
  Source: `R/021-validation-program.R:945`
- `print.eye_crossed_grouped_cv()`
  Registration: `S3method(print, eye_crossed_grouped_cv)`
  Source: `R/021-validation-program.R:1057`
- `plot.eye_crossed_grouped_cv()`
  Registration: `S3method(plot, eye_crossed_grouped_cv)`
  Source: `R/021-validation-program.R:1066`
- `print.eye_multiverse()`
  Registration: `S3method(print, eye_multiverse)`
  Source: `R/021-validation-program.R:1151`
- `plot.eye_multiverse()`
  Registration: `S3method(plot, eye_multiverse)`
  Source: `R/021-validation-program.R:1159`
- `print.eye_reporting_audit()`
  Registration: `S3method(print, eye_reporting_audit)`
  Source: `R/021-validation-program.R:1224`
- `plot.eye_reporting_audit()`
  Registration: `S3method(plot, eye_reporting_audit)`
  Source: `R/021-validation-program.R:1231`
- `plot.eye_benchmark()`
  Registration: `S3method(plot, eye_benchmark)`
  Source: `R/021-validation-program.R:1187`
- `print.eye_validation_program()`
  Registration: `S3method(print, eye_validation_program)`
  Source: `R/021-validation-program.R:1549`
- `print.eye_dynamic_irtree()`
  Registration: `S3method(print, eye_dynamic_irtree)`
  Source: `R/024-dynamic-irtree-engine.R:1027`
- `plot.eye_dynamic_irtree()`
  Registration: `S3method(plot, eye_dynamic_irtree)`
  Source: `R/024-dynamic-irtree-engine.R:1037`
- `print.eye_functional_pupil_irt()`
  Registration: `S3method(print, eye_functional_pupil_irt)`
  Source: `R/026-functional-pupil-engine.R:658`
- `plot.eye_functional_pupil_irt()`
  Registration: `S3method(plot, eye_functional_pupil_irt)`
  Source: `R/026-functional-pupil-engine.R:674`
- `print.eye_theory_strategy_irt()`
  Registration: `S3method(print, eye_theory_strategy_irt)`
  Source: `R/027-strategy-diffusion-engines.R:419`
- `plot.eye_theory_strategy_irt()`
  Registration: `S3method(plot, eye_theory_strategy_irt)`
  Source: `R/022-advanced-models-v2.R:284`
- `print.eye_gaze_diffusion_irt()`
  Registration: `S3method(print, eye_gaze_diffusion_irt)`
  Source: `R/027-strategy-diffusion-engines.R:918`
- `plot.eye_gaze_diffusion_irt()`
  Registration: `S3method(plot, eye_gaze_diffusion_irt)`
  Source: `R/022-advanced-models-v2.R:375`
- `plot.eye_dynamic_ppc()`
  Registration: `S3method(plot, eye_dynamic_ppc)`
  Source: `R/024-dynamic-irtree-engine.R:730`
- `plot.eye_functional_pupil_diagnostics()`
  Registration: `S3method(plot, eye_functional_pupil_diagnostics)`
  Source: `R/026-functional-pupil-engine.R:778`
- `plot.eye_functional_pupil_sensitivity()`
  Registration: `S3method(plot, eye_functional_pupil_sensitivity)`
  Source: `R/026-functional-pupil-engine.R:873`
- `plot.eye_model_promotion_audit()`
  Registration: `S3method(plot, eye_model_promotion_audit)`
  Source: `R/023-validation-orchestration.R:1522`
- `plot.eye_roundtrip_loss_audit()`
  Registration: `S3method(plot, eye_roundtrip_loss_audit)`
  Source: `R/025-vendor-corpus.R:568`
- `plot.eye_storage_benchmark()`
  Registration: `S3method(plot, eye_storage_benchmark)`
  Source: `R/028-api-storage-adapters.R:506`
- `plot.eye_strategy_aoi_sensitivity()`
  Registration: `S3method(plot, eye_strategy_aoi_sensitivity)`
  Source: `R/027-strategy-diffusion-engines.R:557`
- `plot.eye_transition_diagnostics()`
  Registration: `S3method(plot, eye_transition_diagnostics)`
  Source: `R/024-dynamic-irtree-engine.R:805`
- `plot.eye_vendor_compatibility_matrix()`
  Registration: `S3method(plot, eye_vendor_compatibility_matrix)`
  Source: `R/025-vendor-corpus.R:676`
- `predict.eye_multinomial_transition()`
  Registration: `S3method(predict, eye_multinomial_transition)`
  Source: `R/024-dynamic-irtree-engine.R:465`
- `print.eye_benchmark_reproduction()`
  Registration: `S3method(print, eye_benchmark_reproduction)`
  Source: `R/029-benchmark-reproducibility.R:220`
- `print.eye_benchmark_study()`
  Registration: `S3method(print, eye_benchmark_study)`
  Source: `R/029-benchmark-reproducibility.R:33`
- `print.eye_benchmark_validation()`
  Registration: `S3method(print, eye_benchmark_validation)`
  Source: `R/029-benchmark-reproducibility.R:154`
- `print.eye_diffusion_diagnostics()`
  Registration: `S3method(print, eye_diffusion_diagnostics)`
  Source: `R/027-strategy-diffusion-engines.R:971`
- `print.eye_diffusion_identification_study()`
  Registration: `S3method(print, eye_diffusion_identification_study)`
  Source: `R/027-strategy-diffusion-engines.R:1199`
- `print.eye_dynamic_ppc()`
  Registration: `S3method(print, eye_dynamic_ppc)`
  Source: `R/024-dynamic-irtree-engine.R:722`
- `print.eye_dynamic_recovery()`
  Registration: `S3method(print, eye_dynamic_recovery)`
  Source: `R/024-dynamic-irtree-engine.R:954`
- `print.eye_engine_adapter_result()`
  Registration: `S3method(print, eye_engine_adapter_result)`
  Source: `R/028-api-storage-adapters.R:560`
- `print.eye_functional_pupil_data()`
  Registration: `S3method(print, eye_functional_pupil_data)`
  Source: `R/026-functional-pupil-engine.R:447`
- `print.eye_functional_pupil_diagnostics()`
  Registration: `S3method(print, eye_functional_pupil_diagnostics)`
  Source: `R/026-functional-pupil-engine.R:771`
- `print.eye_functional_pupil_sensitivity()`
  Registration: `S3method(print, eye_functional_pupil_sensitivity)`
  Source: `R/026-functional-pupil-engine.R:865`
- `print.eye_functional_scalar_comparison()`
  Registration: `S3method(print, eye_functional_scalar_comparison)`
  Source: `R/026-functional-pupil-engine.R:932`
- `print.eye_gaze_diffusion_data()`
  Registration: `S3method(print, eye_gaze_diffusion_data)`
  Source: `R/027-strategy-diffusion-engines.R:807`
- `print.eye_gaze_diffusion_spec()`
  Registration: `S3method(print, eye_gaze_diffusion_spec)`
  Source: `R/027-strategy-diffusion-engines.R:740`
- `print.eye_model_contract_validation()`
  Registration: `S3method(print, eye_model_contract_validation)`
  Source: `R/028-api-storage-adapters.R:123`
- `print.eye_model_promotion_audit()`
  Registration: `S3method(print, eye_model_promotion_audit)`
  Source: `R/023-validation-orchestration.R:1515`
- `print.eye_multinomial_transition()`
  Registration: `S3method(print, eye_multinomial_transition)`
  Source: `R/024-dynamic-irtree-engine.R:450`
- `print.eye_partition_spec()`
  Registration: `S3method(print, eye_partition_spec)`
  Source: `R/028-api-storage-adapters.R:219`
- `print.eye_partitioned_storage()`
  Registration: `S3method(print, eye_partitioned_storage)`
  Source: `R/028-api-storage-adapters.R:357`
- `print.eye_redaction_result()`
  Registration: `S3method(print, eye_redaction_result)`
  Source: `R/025-vendor-corpus.R:379`
- `print.eye_roundtrip_loss_audit()`
  Registration: `S3method(print, eye_roundtrip_loss_audit)`
  Source: `R/025-vendor-corpus.R:561`
- `print.eye_strategy_data()`
  Registration: `S3method(print, eye_strategy_data)`
  Source: `R/027-strategy-diffusion-engines.R:226`
- `print.eye_strategy_manipulation_validation()`
  Registration: `S3method(print, eye_strategy_manipulation_validation)`
  Source: `R/027-strategy-diffusion-engines.R:603`
- `print.eye_theory_strategy_spec()`
  Registration: `S3method(print, eye_theory_strategy_spec)`
  Source: `R/027-strategy-diffusion-engines.R:149`
- `print.eye_transition_design()`
  Registration: `S3method(print, eye_transition_design)`
  Source: `R/024-dynamic-irtree-engine.R:365`
- `print.eye_transition_diagnostics()`
  Registration: `S3method(print, eye_transition_diagnostics)`
  Source: `R/024-dynamic-irtree-engine.R:798`
- `print.eye_validation_case_fingerprint()`
  Registration: `S3method(print, eye_validation_case_fingerprint)`
  Source: `R/025-vendor-corpus.R:146`
- `print.eye_validation_collection()`
  Registration: `S3method(print, eye_validation_collection)`
  Source: `R/023-validation-orchestration.R:968`
- `print.eye_validation_completion_audit()`
  Registration: `S3method(print, eye_validation_completion_audit)`
  Source: `R/023-validation-orchestration.R:1277`
- `print.eye_validation_job_plan()`
  Registration: `S3method(print, eye_validation_job_plan)`
  Source: `R/023-validation-orchestration.R:191`
- `print.eye_validation_run()`
  Registration: `S3method(print, eye_validation_run)`
  Source: `R/023-validation-orchestration.R:862`
- `print.eye_vendor_case()`
  Registration: `S3method(print, eye_vendor_case)`
  Source: `R/025-vendor-corpus.R:284`
- `print.eye_vendor_compatibility_matrix()`
  Registration: `S3method(print, eye_vendor_compatibility_matrix)`
  Source: `R/025-vendor-corpus.R:669`
- `print.eye_vendor_semantic_comparison()`
  Registration: `S3method(print, eye_vendor_semantic_comparison)`
  Source: `R/025-vendor-corpus.R:465`
- `summary.eye_validation_job_plan()`
  Registration: `S3method(summary, eye_validation_job_plan)`
  Source: `R/023-validation-orchestration.R:204`
- `plot.eye_aoi_composition()`
  Registration: `S3method(plot, eye_aoi_composition)`
  Source: `R/032-compositional-aoi.R:265`
- `plot.eye_aoi_composition_comparison()`
  Registration: `S3method(plot, eye_aoi_composition_comparison)`
  Source: `R/032-compositional-aoi.R:298`
- `plot.eye_aoi_composition_model()`
  Registration: `S3method(plot, eye_aoi_composition_model)`
  Source: `R/032-compositional-aoi.R:313`
- `plot.eye_aoi_separation_audit()`
  Registration: `S3method(plot, eye_aoi_separation_audit)`
  Source: `R/031-probabilistic-aoi.R:341`
- `plot.eye_aoi_uncertainty()`
  Registration: `S3method(plot, eye_aoi_uncertainty)`
  Source: `R/031-probabilistic-aoi.R:353`
- `plot.eye_bank_decision_stability()`
  Registration: `S3method(plot, eye_bank_decision_stability)`
  Source: `R/043-item-bank-optimization.R:194`
- `plot.eye_calibration_drift()`
  Registration: `S3method(plot, eye_calibration_drift)`
  Source: `R/034-calibration-recalibration.R:207`
- `plot.eye_cross_recurrence()`
  Registration: `S3method(plot, eye_cross_recurrence)`
  Source: `R/039-recurrence-analysis.R:155`
- `plot.eye_crossmodal_recurrence_model()`
  Registration: `S3method(plot, eye_crossmodal_recurrence_model)`
  Source: `R/047-measurement-intelligence-adapters.R:65`
- `plot.eye_decision_trace()`
  Registration: `S3method(plot, eye_decision_trace)`
  Source: `R/046-evidence-provenance-graph.R:156`
- `plot.eye_device_equivalence()`
  Registration: `S3method(plot, eye_device_equivalence)`
  Source: `R/036-device-linking.R:167`
- `plot.eye_device_linking()`
  Registration: `S3method(plot, eye_device_linking)`
  Source: `R/036-device-linking.R:145`
- `plot.eye_dif_drift()`
  Registration: `S3method(plot, eye_dif_drift)`
  Source: `R/044-process-dif-fairness.R:151`
- `plot.eye_episode_comparison()`
  Registration: `S3method(plot, eye_episode_comparison)`
  Source: `R/042-process-episodes.R:149`
- `plot.eye_evidence_graph()`
  Registration: `S3method(plot, eye_evidence_graph)`
  Source: `R/046-evidence-provenance-graph.R:150`
- `plot.eye_fairness_transportability()`
  Registration: `S3method(plot, eye_fairness_transportability)`
  Source: `R/044-process-dif-fairness.R:168`
- `plot.eye_fixation_point_process()`
  Registration: `S3method(plot, eye_fixation_point_process)`
  Source: `R/040-fixation-point-process.R:191`
- `plot.eye_gaze_point_process_diagnostics()`
  Registration: `S3method(plot, eye_gaze_point_process_diagnostics)`
  Source: `R/040-fixation-point-process.R:209`
- `plot.eye_item_bank_optimization()`
  Registration: `S3method(plot, eye_item_bank_optimization)`
  Source: `R/043-item-bank-optimization.R:179`
- `plot.eye_item_pareto()`
  Registration: `S3method(plot, eye_item_pareto)`
  Source: `R/043-item-bank-optimization.R:169`
- `plot.eye_mnar_sensitivity()`
  Registration: `S3method(plot, eye_mnar_sensitivity)`
  Source: `R/038-informative-missingness.R:133`
- `plot.eye_mnar_tipping_point()`
  Registration: `S3method(plot, eye_mnar_tipping_point)`
  Source: `R/038-informative-missingness.R:140`
- `plot.eye_norm_transportability()`
  Registration: `S3method(plot, eye_norm_transportability)`
  Source: `R/045-process-norms.R:125`
- `plot.eye_phase_amplitude_irt()`
  Registration: `S3method(plot, eye_phase_amplitude_irt)`
  Source: `R/037-pupil-registration.R:195`
- `plot.eye_probabilistic_aoi()`
  Registration: `S3method(plot, eye_probabilistic_aoi)`
  Source: `R/031-probabilistic-aoi.R:308`
- `plot.eye_process_changepoints()`
  Registration: `S3method(plot, eye_process_changepoints)`
  Source: `R/042-process-episodes.R:122`
- `plot.eye_process_dif()`
  Registration: `S3method(plot, eye_process_dif)`
  Source: `R/044-process-dif-fairness.R:142`
- `plot.eye_process_dstudy()`
  Registration: `S3method(plot, eye_process_dstudy)`
  Source: `R/035-process-reliability.R:179`
- `plot.eye_process_episodes()`
  Registration: `S3method(plot, eye_process_episodes)`
  Source: `R/042-process-episodes.R:130`
- `plot.eye_process_gstudy()`
  Registration: `S3method(plot, eye_process_gstudy)`
  Source: `R/035-process-reliability.R:172`
- `plot.eye_process_norms()`
  Registration: `S3method(plot, eye_process_norms)`
  Source: `R/045-process-norms.R:104`
- `plot.eye_process_observation_model()`
  Registration: `S3method(plot, eye_process_observation_model)`
  Source: `R/038-informative-missingness.R:120`
- `plot.eye_process_reliability_audit()`
  Registration: `S3method(plot, eye_process_reliability_audit)`
  Source: `R/035-process-reliability.R:191`
- `plot.eye_process_uncertainty()`
  Registration: `S3method(plot, eye_process_uncertainty)`
  Source: `R/033-process-uncertainty.R:223`
- `plot.eye_process_uncertainty_propagation()`
  Registration: `S3method(plot, eye_process_uncertainty_propagation)`
  Source: `R/033-process-uncertainty.R:247`
- `plot.eye_provenance_comparison()`
  Registration: `S3method(plot, eye_provenance_comparison)`
  Source: `R/046-evidence-provenance-graph.R:160`
- `plot.eye_pupil_phase_amplitude()`
  Registration: `S3method(plot, eye_pupil_phase_amplitude)`
  Source: `R/037-pupil-registration.R:185`
- `plot.eye_pupil_registration()`
  Registration: `S3method(plot, eye_pupil_registration)`
  Source: `R/037-pupil-registration.R:168`
- `plot.eye_recalibration_audit()`
  Registration: `S3method(plot, eye_recalibration_audit)`
  Source: `R/034-calibration-recalibration.R:227`
- `plot.eye_recurrence()`
  Registration: `S3method(plot, eye_recurrence)`
  Source: `R/039-recurrence-analysis.R:139`
- `plot.eye_scanpath_bootstrap()`
  Registration: `S3method(plot, eye_scanpath_bootstrap)`
  Source: `R/041-representative-scanpaths.R:171`
- `plot.eye_scanpath_comparison()`
  Registration: `S3method(plot, eye_scanpath_comparison)`
  Source: `R/041-representative-scanpaths.R:159`
- `plot.eye_scanpath_representative()`
  Registration: `S3method(plot, eye_scanpath_representative)`
  Source: `R/041-representative-scanpaths.R:140`
- `plot.eye_uncertainty_budget_comparison()`
  Registration: `S3method(plot, eye_uncertainty_budget_comparison)`
  Source: `R/033-process-uncertainty.R:255`
- `plot.eye_windowed_recurrence()`
  Registration: `S3method(plot, eye_windowed_recurrence)`
  Source: `R/039-recurrence-analysis.R:160`
- `plot_diagnostics.default()`
  Registration: `S3method(plot_diagnostics, default)`
  Source: `R/030-measurement-intelligence-utils.R:304`
- `plot_diagnostics.eye_mi_result()`
  Registration: `S3method(plot_diagnostics, eye_mi_result)`
  Source: `R/030-measurement-intelligence-utils.R:307`
- `plot_evidence.default()`
  Registration: `S3method(plot_evidence, default)`
  Source: `R/030-measurement-intelligence-utils.R:318`
- `plot_evidence.eye_mi_result()`
  Registration: `S3method(plot_evidence, eye_mi_result)`
  Source: `R/030-measurement-intelligence-utils.R:321`
- `plot_sensitivity.default()`
  Registration: `S3method(plot_sensitivity, default)`
  Source: `R/030-measurement-intelligence-utils.R:332`
- `plot_sensitivity.eye_mi_result()`
  Registration: `S3method(plot_sensitivity, eye_mi_result)`
  Source: `R/030-measurement-intelligence-utils.R:335`
- `print.eye_mi_result()`
  Registration: `S3method(print, eye_mi_result)`
  Source: `R/030-measurement-intelligence-utils.R:276`
- `print.eye_plot_spec()`
  Registration: `S3method(print, eye_plot_spec)`
  Source: `R/030-measurement-intelligence-utils.R:262`
- `print.eye_process_uncertainty_spec()`
  Registration: `S3method(print, eye_process_uncertainty_spec)`
  Source: `R/033-process-uncertainty.R:39`
- `plot.eye_adapter_regression_audit()`
  Registration: `S3method(plot, eye_adapter_regression_audit)`
  Source: `R/056-completion-plots-0-7.R:133`
- `plot.eye_bids_roundtrip()`
  Registration: `S3method(plot, eye_bids_roundtrip)`
  Source: `R/056-completion-plots-0-7.R:124`
- `plot.eye_compatibility_evidence_matrix()`
  Registration: `S3method(plot, eye_compatibility_evidence_matrix)`
  Source: `R/048-semantic-validation-0-7.R:858`
- `plot.eye_event_roundtrip_audit()`
  Registration: `S3method(plot, eye_event_roundtrip_audit)`
  Source: `R/056-completion-plots-0-7.R:108`
- `plot.eye_event_time_irt()`
  Registration: `S3method(plot, eye_event_time_irt)`
  Source: `R/056-completion-plots-0-7.R:75`
- `plot.eye_gaze_informed_missingness_irt()`
  Registration: `S3method(plot, eye_gaze_informed_missingness_irt)`
  Source: `R/056-completion-plots-0-7.R:17`
- `plot.eye_gpirt()`
  Registration: `S3method(plot, eye_gpirt)`
  Source: `R/053-process-irt-plots-0-7.R:306`
- `plot.eye_incremental_information_audit()`
  Registration: `S3method(plot, eye_incremental_information_audit)`
  Source: `R/053-process-irt-plots-0-7.R:436`
- `plot.eye_irt_changepoints()`
  Registration: `S3method(plot, eye_irt_changepoints)`
  Source: `R/053-process-irt-plots-0-7.R:179`
- `plot.eye_irt_equating()`
  Registration: `S3method(plot, eye_irt_equating)`
  Source: `R/053-process-irt-plots-0-7.R:291`
- `plot.eye_irt_ppc()`
  Registration: `S3method(plot, eye_irt_ppc)`
  Source: `R/053-process-irt-plots-0-7.R:423`
- `plot.eye_irt_recovery_summary()`
  Registration: `S3method(plot, eye_irt_recovery_summary)`
  Source: `R/053-process-irt-plots-0-7.R:380`
- `plot.eye_irt_sbc()`
  Registration: `S3method(plot, eye_irt_sbc)`
  Source: `R/053-process-irt-plots-0-7.R:396`
- `plot.eye_joint_gaze_rt_irt()`
  Registration: `S3method(plot, eye_joint_gaze_rt_irt)`
  Source: `R/053-process-irt-plots-0-7.R:28`
- `plot.eye_joint_graded_rt_process_irt()`
  Registration: `S3method(plot, eye_joint_graded_rt_process_irt)`
  Source: `R/053-process-irt-plots-0-7.R:53`
- `plot.eye_latent_distribution_comparison()`
  Registration: `S3method(plot, eye_latent_distribution_comparison)`
  Source: `R/056-completion-plots-0-7.R:60`
- `plot.eye_latent_space_irt()`
  Registration: `S3method(plot, eye_latent_space_irt)`
  Source: `R/053-process-irt-plots-0-7.R:245`
- `plot.eye_manyfacet_process_irt()`
  Registration: `S3method(plot, eye_manyfacet_process_irt)`
  Source: `R/053-process-irt-plots-0-7.R:131`
- `plot.eye_nominal_gaze_irt()`
  Registration: `S3method(plot, eye_nominal_gaze_irt)`
  Source: `R/053-process-irt-plots-0-7.R:73`
- `plot.eye_omission_survival_irt()`
  Registration: `S3method(plot, eye_omission_survival_irt)`
  Source: `R/053-process-irt-plots-0-7.R:104`
- `plot.eye_process_cat_simulation()`
  Registration: `S3method(plot, eye_process_cat_simulation)`
  Source: `R/053-process-irt-plots-0-7.R:360`
- `plot.eye_process_channel_ablation()`
  Registration: `S3method(plot, eye_process_channel_ablation)`
  Source: `R/054-additional-process-measurement-0-7.R:251`
- `plot.eye_process_dependent_discrimination()`
  Registration: `S3method(plot, eye_process_dependent_discrimination)`
  Source: `R/054-additional-process-measurement-0-7.R:197`
- `plot.eye_process_facet_effects()`
  Registration: `S3method(plot, eye_process_facet_effects)`
  Source: `R/056-completion-plots-0-7.R:41`
- `plot.eye_process_g_study()`
  Registration: `S3method(plot, eye_process_g_study)`
  Source: `R/054-additional-process-measurement-0-7.R:326`
- `plot.eye_process_hmm_irt()`
  Registration: `S3method(plot, eye_process_hmm_irt)`
  Source: `R/053-process-irt-plots-0-7.R:207`
- `plot.eye_process_local_dependence_audit()`
  Registration: `S3method(plot, eye_process_local_dependence_audit)`
  Source: `R/057-emerging-process-irt-0-7.R:181`
- `plot.eye_process_negative_control()`
  Registration: `S3method(plot, eye_process_negative_control)`
  Source: `R/053-process-irt-plots-0-7.R:449`
- `plot.eye_process_person_fit()`
  Registration: `S3method(plot, eye_process_person_fit)`
  Source: `R/053-process-irt-plots-0-7.R:269`
- `plot.eye_sbc_audit()`
  Registration: `S3method(plot, eye_sbc_audit)`
  Source: `R/053-process-irt-plots-0-7.R:409`
- `plot.eye_semantic_roundtrip()`
  Registration: `S3method(plot, eye_semantic_roundtrip)`
  Source: `R/048-semantic-validation-0-7.R:842`
- `plot.eye_vendor_semantic_validation()`
  Registration: `S3method(plot, eye_vendor_semantic_validation)`
  Source: `R/056-completion-plots-0-7.R:92`
- `predict.eye_censored_normal_process_irt()`
  Registration: `S3method(predict, eye_censored_normal_process_irt)`
  Source: `R/054-additional-process-measurement-0-7.R:112`
- `print.eye_irt_evidence_grade()`
  Registration: `S3method(print, eye_irt_evidence_grade)`
  Source: `R/052-irt-validation-0-7.R:1054`
- `print.eye_irt_model_spec()`
  Registration: `S3method(print, eye_irt_model_spec)`
  Source: `R/049-multimodal-irt-registry.R:417`
- `print.eye_irt_validation_spec()`
  Registration: `S3method(print, eye_irt_validation_spec)`
  Source: `R/052-irt-validation-0-7.R:1040`
- `print.eye_process_negative_control()`
  Registration: `S3method(print, eye_process_negative_control)`
  Source: `R/052-irt-validation-0-7.R:1066`
- `print.eye_vendor_schema_contract()`
  Registration: `S3method(print, eye_vendor_schema_contract)`
  Source: `R/056-completion-plots-0-7.R:150`
- `plot.eye_aoi_growth_curve()`
  Registration: `S3method(plot, eye_aoi_growth_curve)`
  Source: `R/064-next-generation-plots-0-8.R:297`
- `plot.eye_aoi_trajectory()`
  Registration: `S3method(plot, eye_aoi_trajectory)`
  Source: `R/064-next-generation-plots-0-8.R:273`
- `plot.eye_bayesian_process_dashboard()`
  Registration: `S3method(plot, eye_bayesian_process_dashboard)`
  Source: `R/068-bayesian-3pl-process-diagnostics-0-8.R:301`
- `plot.eye_biometric_imputation_sensitivity()`
  Registration: `S3method(plot, eye_biometric_imputation_sensitivity)`
  Source: `R/067-advanced-sensitivity-diagnostics-0-8.R:319`
- `plot.eye_biometric_preflight()`
  Registration: `S3method(plot, eye_biometric_preflight)`
  Source: `R/064-next-generation-plots-0-8.R:14`
- `plot.eye_candidate_item_bank_audit()`
  Registration: `S3method(plot, eye_candidate_item_bank_audit)`
  Source: `R/064-next-generation-plots-0-8.R:473`
- `plot.eye_decision_process_proxy()`
  Registration: `S3method(plot, eye_decision_process_proxy)`
  Source: `R/066-process-decision-features-0-8.R:197`
- `plot.eye_gated_process_model()`
  Registration: `S3method(plot, eye_gated_process_model)`
  Source: `R/065-frontier-gates-0-8.R:35`
- `plot.eye_gaze_anchored_3pl_audit()`
  Registration: `S3method(plot, eye_gaze_anchored_3pl_audit)`
  Source: `R/068-bayesian-3pl-process-diagnostics-0-8.R:336`
- `plot.eye_item_parameter_seed()`
  Registration: `S3method(plot, eye_item_parameter_seed)`
  Source: `R/064-next-generation-plots-0-8.R:453`
- `plot.eye_item_reduction_sensitivity()`
  Registration: `S3method(plot, eye_item_reduction_sensitivity)`
  Source: `R/067-advanced-sensitivity-diagnostics-0-8.R:306`
- `plot.eye_latent_process_alignment()`
  Registration: `S3method(plot, eye_latent_process_alignment)`
  Source: `R/067-advanced-sensitivity-diagnostics-0-8.R:276`
- `plot.eye_mixture_irt_process()`
  Registration: `S3method(plot, eye_mixture_irt_process)`
  Source: `R/067-advanced-sensitivity-diagnostics-0-8.R:263`
- `plot.eye_multiblock_process_map()`
  Registration: `S3method(plot, eye_multiblock_process_map)`
  Source: `R/064-next-generation-plots-0-8.R:310`
- `plot.eye_nonparametric_rasch_audit()`
  Registration: `S3method(plot, eye_nonparametric_rasch_audit)`
  Source: `R/067-advanced-sensitivity-diagnostics-0-8.R:294`
- `plot.eye_preaction_process_features()`
  Registration: `S3method(plot, eye_preaction_process_features)`
  Source: `R/066-process-decision-features-0-8.R:183`
- `plot.eye_presentation_accessibility()`
  Registration: `S3method(plot, eye_presentation_accessibility)`
  Source: `R/064-next-generation-plots-0-8.R:53`
- `plot.eye_presentation_fairness_comparison()`
  Registration: `S3method(plot, eye_presentation_fairness_comparison)`
  Source: `R/064-next-generation-plots-0-8.R:745`
- `plot.eye_process_anomaly_audit()`
  Registration: `S3method(plot, eye_process_anomaly_audit)`
  Source: `R/064-next-generation-plots-0-8.R:38`
- `plot.eye_process_drift_audit()`
  Registration: `S3method(plot, eye_process_drift_audit)`
  Source: `R/064-next-generation-plots-0-8.R:70`
- `plot.eye_process_external_validity()`
  Registration: `S3method(plot, eye_process_external_validity)`
  Source: `R/064-next-generation-plots-0-8.R:402`
- `plot.eye_process_feature_blocks()`
  Registration: `S3method(plot, eye_process_feature_blocks)`
  Source: `R/064-next-generation-plots-0-8.R:735`
- `plot.eye_process_profile_mixture()`
  Registration: `S3method(plot, eye_process_profile_mixture)`
  Source: `R/064-next-generation-plots-0-8.R:345`
- `plot.eye_process_rasch_tree()`
  Registration: `S3method(plot, eye_process_rasch_tree)`
  Source: `R/067-advanced-sensitivity-diagnostics-0-8.R:330`
- `plot.eye_process_window_sensitivity()`
  Registration: `S3method(plot, eye_process_window_sensitivity)`
  Source: `R/064-next-generation-plots-0-8.R:131`
- `plot.eye_process_windows()`
  Registration: `S3method(plot, eye_process_windows)`
  Source: `R/064-next-generation-plots-0-8.R:688`
- `plot.eye_pupil_confound_model()`
  Registration: `S3method(plot, eye_pupil_confound_model)`
  Source: `R/064-next-generation-plots-0-8.R:239`
- `plot.eye_pupil_deconvolution()`
  Registration: `S3method(plot, eye_pupil_deconvolution)`
  Source: `R/064-next-generation-plots-0-8.R:193`
- `plot.eye_pupil_fatigue_drift()`
  Registration: `S3method(plot, eye_pupil_fatigue_drift)`
  Source: `R/064-next-generation-plots-0-8.R:720`
- `plot.eye_pupil_frequency_features()`
  Registration: `S3method(plot, eye_pupil_frequency_features)`
  Source: `R/064-next-generation-plots-0-8.R:155`
- `plot.eye_pupil_frequency_stability()`
  Registration: `S3method(plot, eye_pupil_frequency_stability)`
  Source: `R/064-next-generation-plots-0-8.R:178`
- `plot.eye_signal_filter_audit()`
  Registration: `S3method(plot, eye_signal_filter_audit)`
  Source: `R/064-next-generation-plots-0-8.R:439`
- `plot.eye_streaming_score()`
  Registration: `S3method(plot, eye_streaming_score)`
  Source: `R/064-next-generation-plots-0-8.R:382`
- `plot.eye_validation_bundle()`
  Registration: `S3method(plot, eye_validation_bundle)`
  Source: `R/064-next-generation-plots-0-8.R:523`
- `plot.eye_visual_context_irt()`
  Registration: `S3method(plot, eye_visual_context_irt)`
  Source: `R/064-next-generation-plots-0-8.R:487`
- `print.eye_gated_process_model()`
  Registration: `S3method(print, eye_gated_process_model)`
  Source: `R/065-frontier-gates-0-8.R:23`
- `print.eye_process_drift_spec()`
  Registration: `S3method(print, eye_process_drift_spec)`
  Source: `R/059-deployment-drift-0-8.R:47`
- `print.eye_process_preflight_spec()`
  Registration: `S3method(print, eye_process_preflight_spec)`
  Source: `R/058-process-preflight-governance-0-8.R:160`
- `print.eye_process_window_spec()`
  Registration: `S3method(print, eye_process_window_spec)`
  Source: `R/060-process-window-representations-0-8.R:36`
- `print.eye_validation_bundle()`
  Registration: `S3method(print, eye_validation_bundle)`
  Source: `R/063-operational-validation-0-8.R:295`
- `print.eye_visual_context_registry()`
  Registration: `S3method(print, eye_visual_context_registry)`
  Source: `R/062-context-process-structure-0-8.R:51`
- `summary.eye_validation_bundle()`
  Registration: `S3method(summary, eye_validation_bundle)`
  Source: `R/063-operational-validation-0-8.R:306`
