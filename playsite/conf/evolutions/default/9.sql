# --- !Ups

DELETE FROM "DataHierarchy";
DELETE FROM "InstitutionMap";
DELETE FROM "TagMap";
DELETE FROM "UserMap";

ALTER TABLE "TagMap" ADD FOREIGN KEY ("dh_id") REFERENCES "DataHierarchy"("id") ON DELETE CASCADE;
ALTER TABLE "UserMap" ADD FOREIGN KEY ("dh_id") REFERENCES "DataHierarchy"("id") ON DELETE CASCADE;

DROP INDEX "DataHierarchy_lon";
DROP INDEX "DataHierarchy_lat";
CREATE INDEX "DataHierarchy_search" ON "DataHierarchy"("created", "level", "longitude", "latitude");

