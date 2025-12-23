-- Revert events:queued_worker_jobs from pg

BEGIN;

  drop table if exists queued_worker_jobs;

COMMIT;
