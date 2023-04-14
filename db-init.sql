CREATE TABLE public.seen (
    id SERIAL,
    ts timestamptz DEFAULT CURRENT_TIMESTAMP,
    link VARCHAR(300) NOT NULL
);

CREATE TABLE STATE (
  FF_SESSIONS int DEFAULT 0,
  FF_ERRORS   int DEFAULT 0,
  FF_IMAGES   int DEFAULT 0,
  FF_POSTS    int DEFAULT 0,
  WP_SESSIONS int DEFAULT 0,
  WP_ERRORS   int DEFAULT 0,
  WEB_CALLS   int DEFAULT 0
);
INSERT INTO STATE VALUES(0);

CREATE TABLE LOG (
  ts timestamptz DEFAULT CURRENT_TIMESTAMP,
  severity CHAR(5) NOT NULL,
  text TEXT
);
