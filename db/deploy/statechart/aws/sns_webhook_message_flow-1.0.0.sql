-- Deploy events:statechart/aws/sns_webhook_message_flow-1.0.0 to pg

-- FILE AUTOMATICALLY GENERATED. MANUAL CHANGES MIGHT BE OVERWRITTEN

BEGIN;
do $$
declare
chart bigint;
begin
insert into fsm.statechart (name, version) values ('aws.sns_webhook_message_flow', fsm.to_semver('1.0.0')) returning id into chart;
insert into fsm.state (statechart_id, id, name, parent_id, is_initial, is_final, on_entry, on_exit) values
(chart, 'queued', 'QUEUED', null, true, false, array[('aws', 'materialize_sns_webhook_message_status'),('aws', 'enqueue_process_sns_webhook_job')]::fsm_callback_name[], array[]::fsm_callback_name[]),
(chart, 'processing_failed', 'PROCESSING_FAILED', null, false, false, array[('aws', 'materialize_sns_webhook_message_status')]::fsm_callback_name[], array[]::fsm_callback_name[]),
(chart, 'processed', 'PROCESSED', null, false, true, array[('aws', 'materialize_sns_webhook_message_status')]::fsm_callback_name[], array[]::fsm_callback_name[]);
insert into fsm.transition (statechart_id, event, source_state, target_state) values
(chart, 'sns_webhook.processed', 'queued', 'processed'),
(chart, 'sns_webhook.processing_failed', 'queued', 'processing_failed'),
(chart, 'sns_webhook.processed', 'processing_failed', 'processed');
end
$$;
COMMIT;
