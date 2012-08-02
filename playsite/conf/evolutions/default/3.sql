# --- !Ups

# Add a load of indices and foreign key constraints

CREATE INDEX "DataHierarchy_lon" ON "DataHierarchy"("longitude");
CREATE INDEX "DataHierarchy_lat" ON "DataHierarchy"("latitude");
CREATE INDEX "RoleSOTags_role_id" ON "RoleSOTags"("role_id");
CREATE INDEX "RoleSectorTags_role_id" ON "RoleSectorTags"("role_id");
CREATE INDEX "TagMap_dh_id" ON "TagMap"("dh_id");
CREATE INDEX "UserRole_user_id" ON "UserRole"("user_id");

ALTER TABLE "UserRole" ADD FOREIGN KEY ("user_id") REFERENCES "Users"("user_id") ON DELETE CASCADE;
ALTER TABLE "UserRole" ADD FOREIGN KEY ("institution_id") REFERENCES "Institutions"("id") ON DELETE CASCADE;
ALTER TABLE "RoleSOTags" ADD FOREIGN KEY ("role_id") REFERENCES "UserRole"("id") ON DELETE CASCADE;
ALTER TABLE "RoleSOTags" ADD FOREIGN KEY ("tag_id") REFERENCES "Tags"("id") ON DELETE CASCADE;
ALTER TABLE "RoleSectorTags" ADD FOREIGN KEY ("role_id") REFERENCES "UserRole"("id") ON DELETE CASCADE;
ALTER TABLE "RoleSectorTags" ADD FOREIGN KEY ("tag_id") REFERENCES "SectorTags"("id") ON DELETE CASCADE;
ALTER TABLE "UserTags" ADD FOREIGN KEY ("tag_id") REFERENCES "Tags"("id") ON DELETE CASCADE;
ALTER TABLE "UserTags" ADD FOREIGN KEY ("user_id") REFERENCES "Users"("user_id") ON DELETE CASCADE;

ALTER TABLE "TagMap" ADD FOREIGN KEY ("dh_id") REFERENCES "DataHierarchy"("id") ON DELETE CASCADE;
ALTER TABLE "TagMap" ADD FOREIGN KEY ("tag_id") REFERENCES "Tags"("id") ON DELETE CASCADE;
ALTER TABLE "UserMap" ADD FOREIGN KEY ("dh_id") REFERENCES "DataHierarchy"("id") ON DELETE CASCADE;
ALTER TABLE "UserMap" ADD FOREIGN KEY ("user_id") REFERENCES "Users"("user_id") ON DELETE CASCADE;
 
# --- !Downs
 
