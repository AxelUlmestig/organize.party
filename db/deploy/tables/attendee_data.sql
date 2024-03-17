-- Deploy events:tables/attendee_data to pg

BEGIN;

  create table if not exists attendee_data (
    attendee_id bigint not null,
    name text not null,
    status attendee_status,
    plus_one bool not null default false,
    get_notified_on_comments bool not null default false,
    rsvp_at timestamp with time zone not null default now(),
    superseded_at timestamp with time zone,

    foreign key (attendee_id)
      references attendees(id)
      on delete cascade
  );

  create unique index if not exists unique_attendee_data_attendee_idx
    on attendee_data (attendee_id)
    where superseded_at is null;

COMMIT;
