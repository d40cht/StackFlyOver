# --- !Ups


ALTER TABLE "Jobs" ADD "start_time" TIMESTAMP;
ALTER TABLE "Jobs" ADD "end_time" TIMESTAMP;
 
# --- !Downs
 
DROP TABLE Jobs;

