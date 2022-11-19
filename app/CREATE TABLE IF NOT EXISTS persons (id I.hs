CREATE TABLE IF NOT EXISTS persons (id INTEGER PRIMARY KEY, first_name TEXT, last_name TEXT, age INTEGER, group_id INTEGER NOT NULL, FOREIGN KEY (id) REFERENCES groups (id))

CREATE TABLE IF NOT EXISTS groups (id INTEGER PRIMARY KEY, name TEXT NOT NULL UNIQUE)

insert into groups (id, name) values (1, "abc")
