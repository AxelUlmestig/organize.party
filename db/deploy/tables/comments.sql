-- Deploy events:tables/comments to pg

BEGIN;

  create table if not exists comments (
    event_id uuid not null references events(id),
    attendee_id bigint not null,
    created_at timestamptz not null default now(),
    deleted_at timestamptz,
    comment text not null,
    force_notification_on_comment bool not null default false,

    check (deleted_at is null <> comment is null),
    check ((deleted_at is null) or (comment = 'Comment deleted by user')),

    foreign key (event_id, attendee_id)
      references attendees(event_id, id)
      on delete cascade
  );

  create index if not exists idx_comments_event_id
    on comments (event_id);

  -- 👇 Alterations below 👇

  alter table comments
    add column if not exists attendee_id bigint,
    add column if not exists deleted_at timestamptz;

  update comments set
    attendee_id =
      (
        select attendee_id
        from add_attendee_data(
          event_id_ => commenters.event_id,
          email_ => commenters.email,
          name_ => coalesce(commenters.name, attendee_data.name)
        )
      )
  from commenters
  left join attendees
    on attendees.event_id = commenters.event_id
    and attendees.email = commenters.email
  left join attendee_data
    on attendee_data.attendee_id = attendees.id
  where
    comments.email = commenters.email
    and comments.event_id = commenters.event_id;

  alter table comments
    drop column if exists email,
    alter column attendee_id set not null,
    add constraint no_comment_when_deleted check ((deleted_at is null) or (comment = 'Comment deleted by user')),
    add foreign key (attendee_id) references attendees(id) on delete cascade;

  drop trigger if exists sync_commenters_attendees_names on comments;
  drop function if exists trig_sync_commenters_attendees_names;

COMMIT;
