-- Revert events:triggers/queued_worker_jobs/notify_worker from pg

BEGIN;

  drop trigger trigger_notify_worker on job_queue.queued_worker_jobs;
  drop function job_queue.notify_worker;

COMMIT;
