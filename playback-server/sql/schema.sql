BEGIN;

DROP TABLE IF EXISTS recording;
DROP TABLE IF EXISTS token;
DROP TABLE IF EXISTS account;

CREATE TABLE account (
  id SERIAL PRIMARY KEY,
  username TEXT UNIQUE NOT NULL,
  password_hash TEXT NOT NULL
);

CREATE TABLE token (
  id SERIAL PRIMARY KEY,
  account_id INTEGER UNIQUE NOT NULL REFERENCES account ( id ),
  token TEXT UNIQUE NOT NULL,
  expires_at TIMESTAMPTZ NOT NULL DEFAULT now() + interval '30 days'
);

CREATE TABLE recording (
  id SERIAL PRIMARY KEY,
  account_id INTEGER NOT NULL REFERENCES account ( id ),
  createdat TIMESTAMPTZ NOT NULL DEFAULT now(),
  recordedat TIMESTAMPTZ NOT NULL,
  recordinglength FLOAT8 NOT NULL,
  recording BYTEA NOT NULL,
  latitude FLOAT8,
  longitude FLOAT8
);

COMMIT;
