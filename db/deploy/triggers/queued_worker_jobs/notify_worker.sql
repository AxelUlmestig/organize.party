-- Deploy events:triggers/queued_worker_jobs/notify_worker to pg

BEGIN;

  create or replace function notify_worker()
  returns trigger as $$
  begin
    perform pg_notify('new_worker_job', new.id::text);
    return new;
  end;
  $$ language plpgsql;
  
  create trigger trigger_notify_worker
  after insert on queued_worker_jobs
  for each row
  execute function notify_worker();

COMMIT;
