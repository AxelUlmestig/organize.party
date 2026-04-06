-- Deploy events:job_queue/triggers/queued_worker_jobs/notify_worker to pg

BEGIN;

  create or replace function job_queue.notify_worker()
  returns trigger as $$
  begin
    perform pg_notify(
      'new_worker_job',
      jsonb_agg(
        jsonb_build_object(
          'microsecondsUntilRunAt',
          (extract(epoch from inserted.run_at - now()) * 1000000)::int
        )
      )::text
    )
    from inserted;

    return null;
  end;
  $$ language plpgsql;

  create or replace trigger trigger_notify_worker
  after insert on job_queue.queued_worker_jobs
  referencing new table as inserted
  for each statement
  execute function job_queue.notify_worker();

COMMIT;
