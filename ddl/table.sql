DROP TABLE IF EXISTS task;
DROP SEQUENCE IF EXISTS task_id_seq;
CREATE SEQUENCE task_id_seq;
CREATE TABLE task (
  id    INTEGER  DEFAULT nextval('task_id_seq') PRIMARY KEY,
  name  VARCHAR(255) NOT NULL,
  is_done BOOLEAN NOT NULL
);

INSERT INTO task(name, is_done) VALUES ('Prove theorem', false);
