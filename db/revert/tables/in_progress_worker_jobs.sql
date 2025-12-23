-- Revert events:tables/in_progress_worker_jobs from pg

BEGIN;

  drop table if exists in_progress_worker_jobs;

COMMIT;
