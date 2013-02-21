# --- !Ups

CREATE TABLE "NativeUser" (
    "userId" BIGINT(19) NOT NULL,
    "registrationDate" TIMESTAMP NOT NULL,
    "email" VARCHAR(255),
    "lastLogin" TIMESTAMP NOT NULL,
    "loginCount" BIGINT(10)
);

ALTER TABLE "NativeUser" ADD FOREIGN KEY ("userId") REFERENCES "Users"("user_id") ON DELETE CASCADE;

