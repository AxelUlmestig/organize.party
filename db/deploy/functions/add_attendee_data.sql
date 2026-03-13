-- Deploy events:functions/add_attendee_data to pg

BEGIN;

  -- needed to change return type, delete in next rework
  drop function if exists add_attendee_data;

  do $$
  begin
    create type full_attendee_data as (
      id bigint,
      event_id uuid,
      email text,
      unsubscribe_id uuid,
      name text,
      status attendee_status,
      plus_one bool,
      rsvp_at timestamptz
    );
  exception
    when duplicate_object then null;
  end $$;

  create or replace function add_attendee_data(
    host_url_ text,
    event_id_ uuid,
    email_ text,
    name_ text,
    plus_one_ bool default null,
    get_notified_on_comments_ bool default null,
    status_ attendee_status default null
  )
  returns full_attendee_data
  -- returns attendee_data
  /*
  table (
    event_id uuid,
    attendee_id bigint,
    email email,
    name text,
    plus_one bool,
    get_notified_on_comments bool,
    status attendee_status,
    rsvp_at timestamptz,
    unsubscribe_id uuid
  ) as
  */
  as
  $$
    declare
      attendee_id_ bigint;
      -- values_unchanged_ bool := false;
      output_ full_attendee_data;
      previous_plus_one_ bool;
      previous_get_notified_on_comments_ bool;
      previous_status_ attendee_status;
      ics_email_sent_ bool;
      event_url_ text;
    begin
      -- get attendee_id
      insert into attendees (event_id, email)
        values (event_id_, email_)
      on conflict ((attendees.event_id), (attendees.email))
        do nothing
      returning id into attendee_id_;

      if attendee_id_ is null then
        select id into attendee_id_
        from attendees
        where
          attendees.event_id = event_id_
          and attendees.email = email_;
      end if;

      -- early return if this won't change any values
      select
        attendees.id,
        attendees.event_id,
        attendees.email,
        attendees.unsubscribe_id,
        attendee_data.name,
        attendee_data.status,
        attendee_data.plus_one,
        attendee_data.rsvp_at
      into output_
      from attendees
      join attendee_data
        on attendee_data.attendee_id = attendees.id
      where
        attendees.id = attendee_id_
        and attendee_data.superseded_at is null
        and attendee_data.name = name_
        and attendee_data.plus_one = coalesce(plus_one_, plus_one)
        and attendee_data.get_notified_on_comments = coalesce(get_notified_on_comments_, get_notified_on_comments)
        and attendee_data.status = coalesce(status_, status);

      if output_.event_id is not null then
        return output_;
      end if;

      /*
        return query
          select
            attendees.event_id,
            attendees.id as attendee_id,
            attendees.email,
            attendee_data.name text,
            attendee_data.plus_one bool,
            attendee_data.get_notified_on_comments bool,
            attendee_data.status attendee_status,
            attendee_data.rsvp_at,
            attendees.unsubscribe_id
          from attendees
          join attendee_data
            on attendee_data.attendee_id = attendees.id
            and attendee_data.superseded_at is null
          where
            attendees.id = attendee_id_;
            */

      -- mark the old attendee_data as superseded (if it exists)
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

      -- insert new attendee_data
      with inserted as (
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
          coalesce(get_notified_on_comments_, previous_get_notified_on_comments_, false)
        returning *
      )

      select
        attendees.id,
        attendees.event_id,
        attendees.email,
        attendees.unsubscribe_id,
        inserted.name,
        inserted.status,
        inserted.plus_one,
        inserted.rsvp_at
      into output_
      from inserted
      join attendees
        on attendees.id = inserted.attendee_id;

      -- send calendar invite if it hasn't been sent already
      select ics_email_sent
      into ics_email_sent_
      from attendees
      where id = attendee_id_;

      if not ics_email_sent_ and output_.status in ('coming', 'maybe_coming') then

        event_url_ := host_url_ || '/e/' || event_id_;

        with
          inserted_email as (
            insert into email.emails (
              recipient_email,
              recipient_name,
              subject,
              body
            )
            select
              email_,
              name_,
              event_data.title,
              event_data.description ||
              '
<br>
<br>
<a href="' || event_url_ || '">' || event_url_ || '</a>
<br>
<br>
<div style="font-size: x-small">
  If you never want to receive an email from this event again, <a href="' || host_url_ || '/unsubscribe/' || attendees.unsubscribe_id || '">click here to unsubscribe</a>. Warning, this can not be undone
</div>
'
            from attendees
            join event_data
              on event_data.id = attendees.event_id
              and event_data.superseded_at is null
            where
              attendees.id = attendee_id_
            returning *
          )

        insert into email.email_attachments (
          email_id,
          content_type,
          file_name,
          file_contents
        )
        select
          inserted_email.id,
          'text/calendar',
          'invitation.ics',
          convert_to(
            email.create_icalendar_string(
              event_data,
              events.created_at,
              host_url_,
              email_
            ),
            'UTF8'
          )
        from inserted_email
        join events on
          events.id = event_id_
        join event_data
          on event_data.id = events.id
          and event_data.superseded_at is null;

      end if;

      return output_;
      /*
        return query
          select
            attendees.event_id,
            attendees.id as attendee_id,
            attendees.email,
            attendee_data.name text,
            attendee_data.plus_one bool,
            attendee_data.get_notified_on_comments bool,
            attendee_data.status attendee_status,
            attendee_data.rsvp_at,
            attendees.unsubscribe_id
          from attendees
          join attendee_data
            on attendee_data.attendee_id = attendees.id
            and attendee_data.superseded_at is null
          where
            attendees.id = attendee_id_;
      end if;
      */
    end;
  $$ language plpgsql;

COMMIT;
