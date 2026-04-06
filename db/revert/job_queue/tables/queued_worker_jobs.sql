-- Revert events:job_queue/tables/queued_worker_jobs from pg

BEGIN;

  drop table job_queue.queued_worker_jobs;

COMMIT;
