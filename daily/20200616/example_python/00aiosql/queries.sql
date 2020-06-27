-- name: get-all-users
-- Get all user records
SELECT * FROM users;


-- name: get-user-by-username^
-- record_class: User
-- Get user with the given username field.
SELECT user_id,
       username,
       firstname,
       lastname
  FROM users
 WHERE username = :username;

-- name: get-all-greetings
-- Get all the greetings in the database
SELECT greeting_id, greeting FROM greetings;


-- name: create-greeting<!
INSERT INTO greetings (
 greeting
)
 VALUES (
 :text
);
