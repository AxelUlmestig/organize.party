-- Revert events:tables/completed_worker_jobs from pg

BEGIN;

  drop table if exists completed_worker_jobs;

COMMIT;
