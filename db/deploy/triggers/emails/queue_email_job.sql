-- Deploy events:triggers/emails/queue_email_job to pg

BEGIN;

  create or replace function queue_email_job()
  returns trigger as $$
  begin
    insert into queued_worker_jobs(
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
  after insert on emails
  for each row
  execute function queue_email_job();


COMMIT;
