-- cms.sql

CREATE TABLE log_type (
       name CHARACTER VARYING(32) PRIMARY KEY
);
INSERT INTO log_type (name) VALUES ('exception'), ('IO exception'),
                                   ('SQL error'), ('404'), ('parse error'),
                                   ('config');

CREATE TABLE log (
       time TIMESTAMP (0) WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
       type CHARACTER VARYING(32) REFERENCES log_type (name) ON UPDATE CASCADE,
       message TEXT
);
CREATE INDEX log_time_index ON log (time);
COMMENT ON TABLE log IS 'We''d like to keep track of errors and other events we might be interested in. This is not meant to be a permanent log. To create a primary key for this table, we''d need a way of identifying the host and thread that the message came from. For now, we''ll ignore that';

CREATE TABLE article (
       name TEXT PRIMARY KEY
);

CREATE TABLE article_tag (
       name TEXT NOT NULL REFERENCES article (name),
       tag TEXT NOT NULL,
       PRIMARY KEY (name, tag)
);

CREATE TABLE format (
       name TEXT PRIMARY KEY
);
INSERT INTO format (name) VALUES ('html');

CREATE TABLE article_version (
       name TEXT NOT NULL REFERENCES article (name),
       format TEXT NOT NULL REFERENCES format (name),
       version INTEGER NOT NULL DEFAULT 1,
       title TEXT NOT NULL,
       time TIMESTAMP (0) WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
       PRIMARY KEY (name, format, version)
);

CREATE TABLE comment (
       comment_no SERIAL PRIMARY KEY,
       email TEXT,
       name TEXT,
       url TEXT,
       time TIMESTAMP (0) WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
       body TEXT NOT NULL,
       parent_no INTEGER REFERENCES comment (comment_no)
);

CREATE TABLE article_comments (
       name TEXT NOT NULL REFERENCES article (name) PRIMARY KEY,
       root_comment INTEGER REFERENCES comment (comment_no) NOT NULL
);

-- Because of the way that connectby() works, we must always have a root comment
-- for each article. We won't display it, but it needs to be valid.
CREATE RULE "add article" AS
  ON INSERT TO article
  DO ALSO (
    INSERT INTO comment (author, comment) VALUES ('root', '');
    INSERT INTO article_comments (name, root_comment)
                          VALUES (NEW.name, (SELECT currval('comment_comment_no_seq')));
  );
