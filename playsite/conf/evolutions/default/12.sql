# --- !Ups
 
CREATE TABLE "CompanyWatch" (
    "id" BIGINT(19) PRIMARY KEY AUTO_INCREMENT NOT NULL,
    "user_id" BIGINT(19) NOT NULL,
    "institution_id" BIGINT(19) NOT NULL,
    "location_name_id" BIGINT(19) NOT NULL
);

CREATE INDEX "CompanyWatch_user_id" ON "CompanyWatch"("user_id");
ALTER TABLE "CompanyWatch" ADD FOREIGN KEY ("user_id") REFERENCES "Users"("user_id") ON DELETE CASCADE;
ALTER TABLE "CompanyWatch" ADD FOREIGN KEY ("institution_id") REFERENCES "Institutions"("id") ON DELETE CASCADE;
ALTER TABLE "CompanyWatch" ADD FOREIGN KEY ("location_name_id") REFERENCES "LocationName"("id") ON DELETE CASCADE;
