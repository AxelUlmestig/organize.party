-- Deploy events:aws/tables/sns_webhook_messages to pg

BEGIN;

  create table if not exists aws.sns_webhook_messages (
    id uuid not null default uuidv7(),
    received_at timestamptz not null default now(),
    state_machine_id bigint not null,
    materialized_status text not null default 'queued',
    contents jsonb not null,

    primary key (id)
  );

  create index if not exists idx_aws_sns_webhook_messages_state_machine_id
    on aws.sns_webhook_messages (state_machine_id);

COMMIT;
