# --- !Ups
 
CREATE TABLE "Jobs" (
    "job_id" varchar(255) NOT NULL,
    "name" varchar(255) NOT NULL,
    "progress" REAL NOT NULL,
    "status" varchar(255) NOT NULL,

    PRIMARY KEY ("job_id")
);
 
# --- !Downs
 
DROP TABLE Jobs;

