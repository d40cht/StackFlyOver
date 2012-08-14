#!/bin/bash
java -cp ~/.ivy2/cache/com.h2database/h2/jars/h2-1.3.167.jar org.h2.tools.Script -url jdbc:h2:tcp://localhost/stack_users -user "" -script stack_users.sql.zip -options compression zip
