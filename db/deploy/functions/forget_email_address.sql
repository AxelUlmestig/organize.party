-- Deploy events:functions/forget_email_address to pg

BEGIN;

  create or replace function forget_email_address(forgetme_request_id_ uuid)
  returns timestamptz
  as
  $$
    declare
      deleted_at_ timestamptz;
      email_address_ text;
      attendee_ids_ bigint[];
    begin
      -- check if already forgotten
      select
        deleted_at,
        email
      into
        deleted_at_,
        email_address_
      from forgetme_requests
      where
        id = forgetme_request_id_;

      if deleted_at_ is not null then
        return deleted_at_;
      end if;

      -- delete attendee data
      with
        deleted_attendees as (
          update attendees set
            email = null,
            deleted_at = now()
          where email = email_address_
          returning id as attendee_id
        )

      select array_agg(attendee_id)
      into attendee_ids_
      from deleted_attendees;

      update attendee_data set
        name = 'deleted user'
      where
        attendee_id = any(attendee_ids_);

      update comments set
        comment = 'Comment deleted by user',
        deleted_at = now()
      where
        attendee_id = any(attendee_ids_);

      -- delete emails
      delete from email.emails
      cascade
      where recipient_email = email_address_;

      -- update forgetme_request
      update forgetme_requests set
        deleted_at = now(),
        email = null
      where
        id = forgetme_request_id_
      returning deleted_at
      into deleted_at_;

      return deleted_at_;
    end;
  $$ language plpgsql strict;

COMMIT;
