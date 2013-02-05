# --- !Ups

ALTER TABLE "TagMap" ADD CONSTRAINT "dh_id_tag_id_unique" UNIQUE("dh_id", "tag_id");
ALTER TABLE "UserMap" ADD CONSTRAINT "dh_id_user_id_unique" UNIQUE("dh_id", "user_id");
ALTER TABLE "InstitutionMap" ADD CONSTRAINT "dh_id_institution_id_unique" UNIQUE("dh_id", "institution_id");
