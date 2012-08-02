# --- !Ups


CREATE TABLE "LocationName"("id" BIGINT(19) PRIMARY KEY AUTO_INCREMENT, "name" VARCHAR(512));
CREATE TABLE "Location"("name_id" BIGINT(19) PRIMARY KEY, "longitude" REAL, "latitude" REAL, "radius" REAL);
ALTER TABLE "Location" ADD FOREIGN KEY ("name_id") REFERENCES "LocationName"("id") ON DELETE CASCADE;

INSERT INTO "LocationName"("name") SELECT DISTINCT "location" FROM "UserRole";
INSERT INTO "LocationName"("name") SELECT DISTINCT "location" FROM "Users";
INSERT INTO "Location" SELECT "id", "longitude", "latitude", "radius" FROM "Locations" as l1 INNER JOIN "LocationName" as l2 ON l1."name"=l2."name";

DELETE FROM "LocationName" WHERE "name"=''
ALTER TABLE "Users" ADD "location_id" BIGINT(19);
UPDATE "Users" AS u SET "location_id"=(SELECT "id" FROM "LocationName" WHERE "name"=u."location" LIMIT 1);

ALTER TABLE "UserRole" ADD "location_id" BIGINT(19);
UPDATE "UserRole" AS u SET "location_id"=(SELECT "id" FROM "LocationName" WHERE "name"=u."location" LIMIT 1);

ALTER TABLE "Users" DROP COLUMN "location";
ALTER TABLE "UserRole" DROP COLUMN "location";

ALTER TABLE "Users" ADD FOREIGN KEY ("location_id") REFERENCES "LocationName"("id") ON DELETE CASCADE;
ALTER TABLE "UserRole" ADD FOREIGN KEY ("location_id") REFERENCES "LocationName"("id") ON DELETE CASCADE;

DROP TABLE "Locations";
 
# --- !Downs
 


