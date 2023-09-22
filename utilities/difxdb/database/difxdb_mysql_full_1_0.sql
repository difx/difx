-- MySQL dump 10.13  Distrib 5.1.47, for koji-linux-gnu (x86_64)
--
-- Host: 127.0.1    Database: difxdb
-- ------------------------------------------------------
-- Server version	5.1.49-3-log

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Current Database: `difxdb`
--

CREATE DATABASE /*!32312 IF NOT EXISTS*/ `difxdb` /*!40100 DEFAULT CHARACTER SET utf8 */;

USE `difxdb`;

--
-- Table structure for table `Experiment`
--

DROP TABLE IF EXISTS `Experiment`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Experiment` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `code` varchar(20) NOT NULL,
  `number` int(4) unsigned zerofill NOT NULL,
  `statusID` bigint(20) unsigned NOT NULL DEFAULT '7',
  `directory` varchar(255) DEFAULT NULL,
  `vexfile` varchar(100) DEFAULT NULL,
  `dateCreated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`),
  UNIQUE KEY `code` (`code`),
  KEY `statusID` (`statusID`),
  CONSTRAINT `Experiment_ibfk_1` FOREIGN KEY (`statusID`) REFERENCES `ExperimentStatus` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB AUTO_INCREMENT=180 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `ExperimentAndModule`
--

DROP TABLE IF EXISTS `ExperimentAndModule`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `ExperimentAndModule` (
  `experimentID` bigint(20) unsigned NOT NULL,
  `moduleID` bigint(20) unsigned NOT NULL,
  KEY `experimentID` (`experimentID`),
  KEY `moduleID` (`moduleID`),
  CONSTRAINT `ExperimentAndModule_ibfk_1` FOREIGN KEY (`experimentID`) REFERENCES `Experiment` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `ExperimentAndModule_ibfk_2` FOREIGN KEY (`moduleID`) REFERENCES `Module` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `ExperimentStatus`
--

DROP TABLE IF EXISTS `ExperimentStatus`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `ExperimentStatus` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `statuscode` int(11) NOT NULL,
  `experimentstatus` varchar(100) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `statuscode` (`statuscode`)
) ENGINE=InnoDB AUTO_INCREMENT=10 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `Job`
--

DROP TABLE IF EXISTS `Job`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Job` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `passID` bigint(20) unsigned NOT NULL,
  `jobNumber` int(11) NOT NULL,
  `queueTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `priority` int(11) NOT NULL DEFAULT '0',
  `correlationStart` timestamp NULL DEFAULT NULL,
  `correlationEnd` timestamp NULL DEFAULT NULL,
  `jobStart` float NOT NULL,
  `jobDuration` float NOT NULL,
  `inputFile` varchar(255) NOT NULL,
  `outputFile` varchar(255) DEFAULT NULL,
  `outputSize` bigint(20) DEFAULT NULL,
  `difxVersion` varchar(30) NOT NULL,
  `speedupFactor` float DEFAULT NULL,
  `numAntennas` int(11) NOT NULL,
  `numForeign` int(11) NOT NULL,
  `dutyCycle` float DEFAULT NULL,
  `statusID` bigint(20) unsigned NOT NULL,
  PRIMARY KEY (`id`),
  KEY `statusID` (`statusID`),
  KEY `passID` (`passID`),
  CONSTRAINT `Job_ibfk_1` FOREIGN KEY (`passID`) REFERENCES `Pass` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `Job_ibfk_2` FOREIGN KEY (`statusID`) REFERENCES `JobStatus` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `JobStatus`
--

DROP TABLE IF EXISTS `JobStatus`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `JobStatus` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `status` varchar(100) NOT NULL,
  `active` tinyint(1) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=7 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `Module`
--

DROP TABLE IF EXISTS `Module`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Module` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `vsn` varchar(8) NOT NULL,
  `capacity` int(11) NOT NULL,
  `datarate` int(11) NOT NULL,
  `numScans` int(11) DEFAULT NULL,
  `stationCode` varchar(2) DEFAULT NULL,
  `comment` text,
  `received` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `shipped` timestamp NULL DEFAULT '0000-00-00 00:00:00',
  PRIMARY KEY (`id`),
  UNIQUE KEY `vsn` (`vsn`)
) ENGINE=InnoDB AUTO_INCREMENT=394 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `Pass`
--

DROP TABLE IF EXISTS `Pass`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Pass` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `experimentID` bigint(20) unsigned NOT NULL,
  `passName` varchar(30) NOT NULL,
  `passTypeID` bigint(20) unsigned NOT NULL,
  PRIMARY KEY (`id`),
  KEY `passTypeID` (`passTypeID`),
  KEY `experimentID` (`experimentID`),
  CONSTRAINT `Pass_ibfk_1` FOREIGN KEY (`experimentID`) REFERENCES `Experiment` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `Pass_ibfk_2` FOREIGN KEY (`passTypeID`) REFERENCES `PassType` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `PassType`
--

DROP TABLE IF EXISTS `PassType`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `PassType` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `type` varchar(50) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=4 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `Slot`
--

DROP TABLE IF EXISTS `Slot`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Slot` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `moduleID` bigint(20) unsigned DEFAULT NULL,
  `location` varchar(30) NOT NULL,
  `isActive` tinyint(1) NOT NULL DEFAULT '1',
  PRIMARY KEY (`id`),
  UNIQUE KEY `location` (`location`),
  UNIQUE KEY `moduleID` (`moduleID`),
  CONSTRAINT `Slot_ibfk_1` FOREIGN KEY (`moduleID`) REFERENCES `Module` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB AUTO_INCREMENT=563 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `VersionHistory`
--

DROP TABLE IF EXISTS `VersionHistory`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `VersionHistory` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `major` int(11) NOT NULL,
  `minor` int(11) NOT NULL,
  `dateCreated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2012-01-30 10:00:10
-- MySQL dump 10.13  Distrib 5.1.47, for koji-linux-gnu (x86_64)
--
-- Host: 127.0.1    Database: difxdb
-- ------------------------------------------------------
-- Server version	5.1.49-3-log

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Dumping data for table `ExperimentStatus`
--

LOCK TABLES `ExperimentStatus` WRITE;
/*!40000 ALTER TABLE `ExperimentStatus` DISABLE KEYS */;
INSERT INTO `ExperimentStatus` (`id`, `statuscode`, `experimentstatus`) VALUES (1,10,'scheduled'),(2,20,'waiting for correlation'),(3,30,'started correlation'),(4,40,'finished correlation'),(5,100,'released'),(6,110,'correlated elsewhere'),(7,0,'unknown'),(8,200,'aborted'),(9,25,'waiting for data');
/*!40000 ALTER TABLE `ExperimentStatus` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Dumping data for table `JobStatus`
--

LOCK TABLES `JobStatus` WRITE;
/*!40000 ALTER TABLE `JobStatus` DISABLE KEYS */;
INSERT INTO `JobStatus` (`id`, `status`, `active`) VALUES (1,'unknown',0),(2,'queued',1),(3,'running',1),(4,'complete',0),(5,'killed',0),(6,'failed',0);
/*!40000 ALTER TABLE `JobStatus` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Dumping data for table `PassType`
--

LOCK TABLES `PassType` WRITE;
/*!40000 ALTER TABLE `PassType` DISABLE KEYS */;
INSERT INTO `PassType` (`id`, `type`) VALUES (1,'production'),(2,'clock'),(3,'test');
/*!40000 ALTER TABLE `PassType` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;


INSERT INTO `difxdb`.`VersionHistory` (
`major` ,
`minor`
)
VALUES (
'1', '0'
);

-- Dump completed on 2012-01-30 10:00:11
