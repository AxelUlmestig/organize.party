-- Deploy events:statechart/email/email_flow-1.0 to pg

-- FILE AUTOMATICALLY GENERATED. MANUAL CHANGES MIGHT BE OVERWRITTEN

BEGIN;
do $$
declare
chart bigint;
begin
insert into fsm.statechart (name, version) values ('email.email_flow', to_semver('1.0')) returning id into chart;
insert into fsm.state (statechart_id, id, name, parent_id, is_initial, is_final, on_entry, on_exit) values
(chart, 'checking_if_blocked', 'CHECKING_IF_BLOCKED', null, true, false, array[('email', 'check_if_email_address_is_blocked')]::fsm_callback_name[], array[]::fsm_callback_name[]),
(chart, 'email_address_blocked', 'EMAIL_ADDRESS_BLOCKED', null, false, true, array[('email', 'materialize_email_status')]::fsm_callback_name[], array[]::fsm_callback_name[]),
(chart, 'queued', 'QUEUED', null, false, false, array[('email', 'materialize_email_status'),('email', 'enqueue_send_email_job')]::fsm_callback_name[], array[]::fsm_callback_name[]),
(chart, 'failed_to_send', 'FAILED_TO_SEND', null, false, false, array[('email', 'materialize_email_status')]::fsm_callback_name[], array[]::fsm_callback_name[]),
(chart, 'sent', 'SENT', null, false, false, array[('email', 'materialize_email_status'),('email', 'update_email_sent_at')]::fsm_callback_name[], array[]::fsm_callback_name[]),
(chart, 'received', 'RECEIVED', null, false, false, array[('email', 'materialize_email_status')]::fsm_callback_name[], array[]::fsm_callback_name[]),
(chart, 'bounced', 'BOUNCED', null, false, true, array[('email', 'materialize_email_status'),('email', 'block_bounced_email_address')]::fsm_callback_name[], array[]::fsm_callback_name[]),
(chart, 'marked_as_spam', 'MARKED_AS_SPAM', null, false, true, array[('email', 'materialize_email_status'),('email', 'block_email_address_marked_as_spam')]::fsm_callback_name[], array[]::fsm_callback_name[]);
insert into fsm.transition (statechart_id, event, source_state, target_state) values
(chart, 'email.address_not_blocked', 'checking_if_blocked', 'queued'),
(chart, 'email.address_blocked', 'checking_if_blocked', 'email_address_blocked'),
(chart, 'email.sent', 'queued', 'sent'),
(chart, 'email.failed_to_send', 'queued', 'failed_to_send'),
(chart, 'email.retry', 'failed_to_send', 'checking_if_blocked'),
(chart, 'email.received', 'sent', 'received'),
(chart, 'email.bounced', 'sent', 'bounced'),
(chart, 'email.marked_as_spam', 'sent', 'marked_as_spam'),
(chart, 'email.marked_as_spam', 'received', 'marked_as_spam');
end
$$;
COMMIT;
