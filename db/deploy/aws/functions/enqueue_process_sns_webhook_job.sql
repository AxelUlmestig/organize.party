-- Deploy events:aws/functions/enqueue_process_sns_webhook_job to pg

BEGIN;

  create or replace function aws.enqueue_process_sns_webhook_job(event_payload fsm_event_payload)
    returns void as
    $$
      insert into job_queue.queued_worker_jobs(
        definition
      )
      select jsonb_build_object(
        'type', 'ProcessAwsSnsWebhookMessage',
        'payload', jsonb_build_object(
          'awsSnsWebhookMessageId', id
        )
      )
      from aws.sns_webhook_messages
      where
        sns_webhook_messages.state_machine_id = (event_payload).machine_id
    $$
    language sql;

COMMIT;
