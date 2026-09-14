-- Deploy events:functions/edit_event to pg

BEGIN;

  create or replace function edit_event(
    host_url_ text,
    event_id_ uuid,
    title_ text,
    description_ text,
    start_time_ timestamptz,
    end_time_ timestamptz,
    location_ text,
    google_maps_link_ text,
    password_ text,
    photo_id_ uuid
  )
  returns void
  as
  $$
    declare
      event_url_ text not null := host_url_ || '/e/' || event_id_;
      correct_password_ bool;
      no_changes_ bool := false;
    begin
      -- verify that the password is correct
      select password_hash = digest(password_ || password_salt, 'sha256')::text
      into correct_password_
      from events
      where id = event_id_;

      if correct_password_ is null then
        raise exception 'event not found' using errcode = 'P0404', hint = '404';
      elsif not correct_password_ then
        raise exception 'invalid password' using errcode = 'P0403', hint = '403';
      end if;

      -- check if there are any actual changes
      select exists (
        select 1
        from event_data
        where
          id = event_id_
          and superseded_at is null
          -- compare old data
          and title is not distinct from title_
          and description is not distinct from description_
          and time_start is not distinct from start_time_
          and time_end is not distinct from end_time_
          and location is not distinct from location_
          and location_google_maps_link is not distinct from google_maps_link_
          and photo_id is not distinct from photo_id_
      ) into no_changes_;

      if no_changes_ then
        return;
      end if;

      -- update event info
      update event_data
      set superseded_at = now()
      where
        id = event_id_
        and superseded_at is null;

      insert into event_data (
        id,
        title,
        description,
        time_start,
        time_end,
        location,
        location_google_maps_link,
        photo_id
      )
      values (
        event_id_,
        title_,
        description_,
        start_time_,
        end_time_,
        location_,
        google_maps_link_,
        photo_id_
      );

      -- send email notifying attendees of update
      with
        inserted as (
          insert into email.emails (
            recipient_email,
            recipient_name,
            subject,
            body
          )
          select
            attendees.email,
            attendee_data.name,
            event_data.title,
            event_data.description ||
'
<br>
<br>
<a href="' || event_url_ || '">' || event_url_ || '</a>
<br>
<br>
<div style="font-size: x-small">
  If you never want to receive an email from this event again, <a href="#{emailHostUrl}/unsubscribe/#{unsubscribeId}">click here to unsubscribe</a>. Warning, this can not be undone
</div>'

          from event_data
          join attendees
            on attendees.event_id = event_data.id
          join attendee_data
            on attendee_data.attendee_id = attendees.id
            and attendee_data.superseded_at is null
          where
            event_data.id = event_id_
            and event_data.superseded_at is null
            and attendee_data.status in ('coming', 'maybe_coming')
            and attendees.email is not null -- it becomes null if the user has requested to be forgotten
          returning *
        )

      insert into email.email_attachments (
        email_id,
        content_type,
        file_name,
        file_contents
      )
      select
        inserted.id,
        'text/calendar',
        'invitation.ics',
        convert_to(
          email.create_icalendar_string(
            event_data,
            events.created_at,
            host_url_,
            inserted.recipient_email
          ),
          'UTF8'
        )
      from inserted
      join events on
        events.id = event_id_
      join event_data
        on event_data.id = events.id
        and event_data.superseded_at is null;

    end;
  $$ language plpgsql;

COMMIT;
