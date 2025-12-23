-- Revert events:triggers/queued_worker_jobs/notify_worker from pg

BEGIN;

  drop trigger if exists trigger_notify_worker on queued_worker_jobs;

  drop function if exists notify_worker();

COMMIT;
