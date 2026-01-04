-- Deploy events:email/triggers/emails/queue_email_job to pg

BEGIN;

  create or replace function email.queue_email_job()
  returns trigger as $$
  begin
    insert into job_queue.queued_worker_jobs(
      definition
    )
    values (
      jsonb_build_object(
        'type', 'SendEmail',
        'payload', jsonb_build_object(
          'emailId', new.id
        )
      )
    );

    return new;
  end;
  $$ language plpgsql;
  
  create trigger trigger_notify_worker
  after insert on email.emails
  for each row
  execute function email.queue_email_job();


COMMIT;
