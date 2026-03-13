-- Deploy events:functions/add_attendee_data to pg

BEGIN;

  create or replace function add_attendee_data(
    event_id_ uuid,
    email_ text,
    name_ text,
    plus_one_ bool default null,
    get_notified_on_comments_ bool default null,
    status_ attendee_status default null
  )
  returns table (
    event_id uuid,
    attendee_id bigint,
    email email,
    name text,
    plus_one bool,
    get_notified_on_comments bool,
    status attendee_status,
    rsvp_at timestamptz
  ) as
  $$
    declare
      attendee_id_ bigint;
      values_unchanged_ bool := false;
      previous_plus_one_ bool;
      previous_get_notified_on_comments_ bool;
      previous_status_ attendee_status;
    begin
      -- get attendee_id
      insert into attendees (event_id, email)
      values (event_id_, email_)
      on conflict ((attendees.event_id), (attendees.email)) do nothing
      returning id into attendee_id_;

      if attendee_id_ is null then
        select id into attendee_id_
        from attendees
        where
          attendees.event_id = event_id_
          and attendees.email = email_;
      end if;

      -- early return if this won't change any values
      select exists (
        select 1
        from attendee_data
        where
          superseded_at is null
          and attendee_data.attendee_id = attendee_id_
          and attendee_data.name = name_
          -- 'is not distinct from' => null = null
          and (
            attendee_data.plus_one is not distinct from plus_one_
            or plus_one_ is null
          )
          and (
            attendee_data.get_notified_on_comments is not distinct from get_notified_on_comments_
            or get_notified_on_comments_ is null
          )
          and (
            attendee_data.status is not distinct from status_
            or status_ is null
          )
      ) into values_unchanged_;

      -- not sure why early return in the if statement doesn't work with
      -- 'return query'. But doing an else clause solves the problem
      if values_unchanged_ then
        return query
          select
            attendees.event_id,
            attendees.id as attendee_id,
            attendees.email,
            attendee_data.name text,
            attendee_data.plus_one bool,
            attendee_data.get_notified_on_comments bool,
            attendee_data.status attendee_status,
            attendee_data.rsvp_at
          from attendees
          join attendee_data
            on attendee_data.attendee_id = attendees.id
            and attendee_data.superseded_at is null
          where
            attendees.id = attendee_id_;
      else
        -- insert new attendee_data
        update attendee_data
        set superseded_at = now()
        where
          attendee_data.attendee_id = attendee_id_
          and superseded_at is null
        returning
          attendee_data.status,
          attendee_data.plus_one,
          attendee_data.get_notified_on_comments
        into
          previous_status_,
          previous_plus_one_,
          previous_get_notified_on_comments_;

        insert into attendee_data (
          attendee_id,
          name,
          status,
          plus_one,
          get_notified_on_comments
        )
        select
          attendee_id_,
          name_,
          coalesce(status_, previous_status_),
          coalesce(plus_one_, previous_plus_one_, false),
          coalesce(get_notified_on_comments_, previous_get_notified_on_comments_, false);

        return query
          select
            attendees.event_id,
            attendees.id as attendee_id,
            attendees.email,
            attendee_data.name text,
            attendee_data.plus_one bool,
            attendee_data.get_notified_on_comments bool,
            attendee_data.status attendee_status,
            attendee_data.rsvp_at
          from attendees
          join attendee_data
            on attendee_data.attendee_id = attendees.id
            and attendee_data.superseded_at is null
          where
            attendees.id = attendee_id_;
      end if;
    end;
  $$ language plpgsql rows 1;

COMMIT;
