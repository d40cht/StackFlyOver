# --- !Ups

DELETE FROM "Location";

ALTER TABLE "Location" ADD COLUMN "quality" REAL NOT NULL;
ALTER TABLE "Location" ADD COLUMN "neighborhood" VARCHAR(255) NOT NULL;
ALTER TABLE "Location" ADD COLUMN "city" VARCHAR(255) NOT NULL;
ALTER TABLE "Location" ADD COLUMN "county" VARCHAR(255) NOT NULL;
ALTER TABLE "Location" ADD COLUMN "state" VARCHAR(255) NOT NULL;
ALTER TABLE "Location" ADD COLUMN "country" VARCHAR(255) NOT NULL;

