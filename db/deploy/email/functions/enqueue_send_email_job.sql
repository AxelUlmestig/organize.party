-- Deploy events:email/functions/enqueue_send_email_job to pg

BEGIN;

  create or replace function email.enqueue_send_email_job(event_payload fsm_event_payload)
    returns void as
    $$
      insert into job_queue.queued_worker_jobs(
        definition
      )
      select jsonb_build_object(
        'type', 'SendEmail',
        'payload', jsonb_build_object(
          'emailId', id
        )
      )
      from email.emails
      where
        emails.state_machine_id = (event_payload).machine_id
    $$
    language sql;

COMMIT;
