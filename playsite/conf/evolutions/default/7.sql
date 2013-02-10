# --- !Ups

ALTER TABLE "UserRole" ADD "modified" TIMESTAMP;
ALTER TABLE "DataHierarchy" ADD "created" TIMESTAMP;
ALTER TABLE "Users" ADD "email" VARCHAR;
ALTER TABLE "Users" ADD "lastScanned" TIMESTAMP;
ALTER TABLE "Users" ADD "detailFresh" BOOLEAN;

UPDATE "UserRole" SET "modified"='2013-01-01 00:00:00';
UPDATE "DataHierarchy" SET "created"='2013-01-01 00:00:00';
UPDATE "Users" SET "lastScanned"='2013-01-01 00:00:00';
UPDATE "Users" SET "detailFresh"='FALSE';

