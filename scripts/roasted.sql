CREATE DATABASE roasted
    WITH
    OWNER = postgres
    ENCODING = 'UTF8'
    CONNECTION LIMIT = -1;


CREATE TABLE coffee(
    name VARCHAR(45) NOT NULL,
    description VARCHAR(500)
);
