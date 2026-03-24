-- Deploy events:views/latest_attendee_data to pg

BEGIN;

  create or replace view latest_attendee_data as
    select
      attendees.id,
      attendees.event_id,
      attendees.email,
      attendees.gravatar_url,
      attendees.unsubscribe_id,
      attendees.unsubscribed_at,
      attendees.deleted_at,
      attendees.ics_email_sent,
      attendee_data.name,
      attendee_data.status,
      attendee_data.plus_one,
      attendee_data.rsvp_at,
      attendee_data.get_notified_on_comments
    from attendees
    join attendee_data
      on attendee_data.attendee_id = attendees.id
      and attendee_data.superseded_at is null;

COMMIT;
