-- Deploy events:statechart/aws/s3_photo_upload_flow-1.0 to pg

-- FILE AUTOMATICALLY GENERATED. MANUAL CHANGES MIGHT BE OVERWRITTEN

BEGIN;
do $$
declare
chart bigint;
begin
insert into fsm.statechart (name, version) values ('aws.s3_photo_upload_flow', to_semver('1.0')) returning id into chart;
insert into fsm.state (statechart_id, id, name, parent_id, is_initial, is_final, on_entry, on_exit) values
(chart, 'upload_url_ready', 'UPLOAD_URL_READY', null, true, false, array[('aws', 'materialize_s3_upload_status'),('aws', 'enqueue_s3_file_upload_status_check_job')]::fsm_callback_name[], array[]::fsm_callback_name[]),
(chart, 'upload_completed', 'UPLOAD_COMPLETED', null, false, true, array[('aws', 'materialize_s3_upload_status'),('aws', 'promote_uploaded_photo')]::fsm_callback_name[], array[]::fsm_callback_name[]),
(chart, 'error', 'ERROR', null, false, true, array[('aws', 'materialize_s3_upload_status')]::fsm_callback_name[], array[]::fsm_callback_name[]);
insert into fsm.transition (statechart_id, event, source_state, target_state) values
(chart, 'check_upload_progress', 'upload_url_ready', 'upload_url_ready'),
(chart, 'upload_verified', 'upload_url_ready', 'upload_completed'),
(chart, 'error', 'upload_url_ready', 'error');
end
$$;
COMMIT;
