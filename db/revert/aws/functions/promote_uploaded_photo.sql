-- Revert events:aws/functions/promote_uploaded_photo from pg

BEGIN;

  drop function if exists aws.promote_uploaded_photo;

COMMIT;
