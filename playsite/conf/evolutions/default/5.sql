# --- !Ups

CREATE TABLE "InstitutionMap"("dh_id" BIGINT(19), "institution_id" BIGINT(19));
ALTER TABLE "InstitutionMap" ADD FOREIGN KEY("dh_id") REFERENCES "DataHierarchy"("id") ON DELETE CASCADE;
ALTER TABLE "InstitutionMap" ADD FOREIGN KEY("institution_id") REFERENCES "Institutions"("id") ON DELETE CASCADE;
 
# --- !Downs
 


