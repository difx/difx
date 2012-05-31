\set ON_ERROR_STOP

DROP TABLE IF EXISTS "ExperimentStatus";
CREATE TABLE IF NOT EXISTS "ExperimentStatus" (
  "id" serial primary key,
  "statuscode" integer NOT NULL,
  "experimentstatus" varchar(100) NOT NULL
);

DROP TABLE IF EXISTS "Experiment";
CREATE TABLE IF NOT EXISTS "Experiment" (
  "id" serial primary key,
  "code" varchar(20) NOT NULL UNIQUE,
  "number" integer NOT NULL,
  "statusID" integer NOT NULL DEFAULT 1 REFERENCES "ExperimentStatus" ("id"),
  "dateCreated" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX "ExperimentStatusIDKey" ON "Experiment" ("statusID");

DROP TABLE IF EXISTS "Module";
CREATE TABLE IF NOT EXISTS "Module" (
  "id" serial primary key,
  "experimentID" integer REFERENCES "Experiment" ("id"),
  "vsn" varchar(8) NOT NULL unique,
  "capacity" integer NOT NULL,
  "datarate" integer NOT NULL,
  "numScans" integer,
  "stationCode" varchar(2),
  "received" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  "shipped" timestamp
);

CREATE INDEX "ModuleExperimentIdKey" ON "Module" ("experimentID");

DROP TABLE IF EXISTS "ExperimentAndModule";
CREATE TABLE IF NOT EXISTS "ExperimentAndModule" (
  "experimentID" integer NOT NULL REFERENCES "Experiment" ("id"),
  "moduleID" integer NOT NULL REFERENCES "Module" ("id")
);

CREATE INDEX "ExperimentAndModuleExperimentIdKey" ON "ExperimentAndModule" ("experimentID");
CREATE INDEX "ExperimentAndModuleModuleIdKey" ON "ExperimentAndModule" ("moduleID");

DROP TABLE IF EXISTS "Job";
CREATE TABLE IF NOT EXISTS "Job" (
  "id" serial primary key,
  "passID" integer NOT NULL,
  "jobNumber" integer NOT NULL,
  "queueTime" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  "priority" integer NOT NULL DEFAULT '0',
  "correlationStart" timestamp,
  "correlationEnd" timestamp,
  "jobStart" float NOT NULL,
  "jobDuration" float NOT NULL,
  "inputFile" varchar(255) NOT NULL,
  "outputFile" varchar(255),
  "outputSize" integer,
  "difxVersion" varchar(30) NOT NULL,
  "speedupFactor" float,
  "numAntennas" integer NOT NULL,
  "numForeign" integer NOT NULL,
  "dutyCycle" float,
  "statusID" integer NOT NULL
);

DROP TABLE IF EXISTS "JobStatus";
CREATE TABLE IF NOT EXISTS "JobStatus" (
  "id" serial primary key,
  "status" varchar(100) NOT NULL,
  "active" integer NOT NULL
);


DROP TABLE IF EXISTS "Pass";
CREATE TABLE IF NOT EXISTS "Pass" (
  "id" serial primary key,
  "experimentID" integer NOT NULL,
  "passName" varchar(30) NOT NULL,
  "passTypeID" integer NOT NULL
);


DROP TABLE IF EXISTS "PassType";
CREATE TABLE IF NOT EXISTS "PassType" (
  "id" serial primary key,
  "type" varchar(50) NOT NULL
);

DROP TABLE IF EXISTS "Slot";
CREATE TABLE IF NOT EXISTS "Slot" (
  "id" serial primary key,
  "moduleID" integer unique REFERENCES "Module" ("id"),
  "location" varchar(30) NOT NULL unique,
  "isActive" integer NOT NULL DEFAULT 1
);
