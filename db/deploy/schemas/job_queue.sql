-- Deploy events:schemas/job_queue to pg

BEGIN;

  create schema if not exists job_queue;

COMMIT;
