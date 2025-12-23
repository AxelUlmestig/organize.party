-- Deploy events:queued_worker_jobs to pg

BEGIN;

  create table if not exists queued_worker_jobs(
    id uuid default uuidv7(),
    run_at timestamptz not null default now(),
    failed_attempts int not null default 0,
    definition jsonb not null,

    primary key (id)
  );

  create index if not exists idx_worker_jobs_run_at
    on queued_worker_jobs(run_at);

COMMIT;
