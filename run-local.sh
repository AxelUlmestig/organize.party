#!/bin/sh

DB_HOST=localhost DB_PORT=5433 SMTP_SERVER=localhost SMTP_PORT=1111 SMTP_LOGIN=user SMTP_PASSWORD=password cabal run
