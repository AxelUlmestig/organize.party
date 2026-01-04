-- Revert events:schemas/job_queue from pg

BEGIN;

  drop schema if exists job_queue;

COMMIT;
