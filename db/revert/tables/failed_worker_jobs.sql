-- Revert events:tables/failed_worker_jobs from pg

BEGIN;

  drop table if exists failed_worker_jobs;

COMMIT;
