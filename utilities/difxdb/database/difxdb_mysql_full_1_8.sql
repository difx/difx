-- phpMyAdmin SQL Dump
-- version 4.4.15.10
-- https://www.phpmyadmin.net
--
-- Host: localhost
-- Generation Time: May 16, 2018 at 03:56 PM
-- Server version: 5.5.44-MariaDB
-- PHP Version: 5.4.16

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- Database: `difxdb`
--
CREATE DATABASE IF NOT EXISTS `difxdb` DEFAULT CHARACTER SET utf8 COLLATE utf8_general_ci;
USE `difxdb`;

-- --------------------------------------------------------

--
-- Table structure for table `Experiment`
--

CREATE TABLE IF NOT EXISTS `Experiment` (
  `id` bigint(20) unsigned NOT NULL,
  `code` varchar(20) NOT NULL,
  `number` int(4) unsigned zerofill NOT NULL,
  `statusID` bigint(20) unsigned NOT NULL DEFAULT '7',
  `directory` varchar(255) DEFAULT NULL,
  `vexfile` varchar(100) DEFAULT NULL,
  `userID` bigint(20) unsigned DEFAULT NULL,
  `emailnotification` varchar(255) DEFAULT NULL,
  `releasedByUserID` bigint(20) unsigned DEFAULT NULL,
  `dateCreated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `dateObserved` date DEFAULT NULL,
  `dateArchived` datetime DEFAULT NULL,
  `dateReleased` datetime DEFAULT NULL,
  `archivedBy` varchar(30) DEFAULT NULL,
  `releasedBy` varchar(30) DEFAULT NULL,
  `comment` longtext
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

--
-- Triggers `Experiment`
--
DELIMITER $$
CREATE TRIGGER `insert_statushistory` AFTER INSERT ON `Experiment`
 FOR EACH ROW BEGIN
INSERT INTO ExperimentStatusHistory (expID, status, dateCreated) VALUES (NEW.id, (select experimentstatus from ExperimentStatus where NEW.statusID = id), NOW());
END
$$
DELIMITER ;

-- --------------------------------------------------------

--
-- Table structure for table `ExperimentAndModule`
--

CREATE TABLE IF NOT EXISTS `ExperimentAndModule` (
  `experimentID` bigint(20) unsigned NOT NULL,
  `moduleID` bigint(20) unsigned NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `ExperimentAndType`
--

CREATE TABLE IF NOT EXISTS `ExperimentAndType` (
  `experimentID` bigint(20) unsigned NOT NULL,
  `experimentTypeID` bigint(20) unsigned NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `ExperimentStatus`
--

CREATE TABLE IF NOT EXISTS `ExperimentStatus` (
  `id` bigint(20) unsigned NOT NULL,
  `statuscode` int(11) NOT NULL,
  `experimentstatus` varchar(100) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `ExperimentStatusHistory`
--

CREATE TABLE IF NOT EXISTS `ExperimentStatusHistory` (
  `id` bigint(20) unsigned NOT NULL,
  `expID` bigint(20) unsigned NOT NULL,
  `status` varchar(50) NOT NULL,
  `dateCreated` datetime NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `ExperimentType`
--

CREATE TABLE IF NOT EXISTS `ExperimentType` (
  `id` bigint(20) unsigned NOT NULL,
  `type` varchar(100) NOT NULL,
  `active` tinyint(1) NOT NULL DEFAULT '1'
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `ExportFile`
--

CREATE TABLE IF NOT EXISTS `ExportFile` (
  `id` bigint(20) unsigned NOT NULL,
  `experimentID` bigint(20) unsigned NOT NULL,
  `filename` varchar(255) NOT NULL,
  `exportPath` varchar(255) NOT NULL,
  `checksum` char(32) NOT NULL,
  `dateCreated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `FileData`
--

CREATE TABLE IF NOT EXISTS `FileData` (
  `id` bigint(20) unsigned NOT NULL,
  `experimentID` bigint(20) unsigned NOT NULL,
  `location` varchar(255) DEFAULT NULL,
  `size` bigint(11) NOT NULL,
  `numScans` int(11) DEFAULT NULL,
  `stationCode` varchar(2) DEFAULT NULL,
  `comment` text,
  `received` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `Job`
--

CREATE TABLE IF NOT EXISTS `Job` (
  `id` bigint(20) unsigned NOT NULL,
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
  `statusID` bigint(20) unsigned NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `JobStatus`
--

CREATE TABLE IF NOT EXISTS `JobStatus` (
  `id` bigint(20) unsigned NOT NULL,
  `status` varchar(100) NOT NULL,
  `active` tinyint(1) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `Module`
--

CREATE TABLE IF NOT EXISTS `Module` (
  `id` bigint(20) unsigned NOT NULL,
  `vsn` varchar(8) NOT NULL,
  `capacity` int(11) NOT NULL,
  `datarate` int(11) NOT NULL,
  `numScans` int(11) DEFAULT NULL,
  `stationCode` varchar(2) DEFAULT NULL,
  `comment` text,
  `received` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `shipped` timestamp NULL DEFAULT '0000-00-00 00:00:00'
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `Pass`
--

CREATE TABLE IF NOT EXISTS `Pass` (
  `id` bigint(20) unsigned NOT NULL,
  `experimentID` bigint(20) unsigned NOT NULL,
  `passName` varchar(30) NOT NULL,
  `passTypeID` bigint(20) unsigned NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `PassType`
--

CREATE TABLE IF NOT EXISTS `PassType` (
  `id` bigint(20) unsigned NOT NULL,
  `type` varchar(50) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `Slot`
--

CREATE TABLE IF NOT EXISTS `Slot` (
  `id` bigint(20) unsigned NOT NULL,
  `moduleID` bigint(20) unsigned DEFAULT NULL,
  `location` varchar(30) NOT NULL,
  `isActive` tinyint(1) NOT NULL DEFAULT '1'
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `User`
--

CREATE TABLE IF NOT EXISTS `User` (
  `id` bigint(20) unsigned NOT NULL,
  `name` varchar(50) NOT NULL,
  `enabled` tinyint(1) NOT NULL DEFAULT '1'
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `VersionHistory`
--

CREATE TABLE IF NOT EXISTS `VersionHistory` (
  `id` bigint(20) NOT NULL,
  `major` int(11) NOT NULL,
  `minor` int(11) NOT NULL,
  `dateCreated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

--
-- Indexes for dumped tables
--

--
-- Indexes for table `Experiment`
--
ALTER TABLE `Experiment`
  ADD PRIMARY KEY (`id`),
  ADD UNIQUE KEY `code` (`code`),
  ADD KEY `statusID` (`statusID`),
  ADD KEY `userID` (`userID`),
  ADD KEY `releasedByUserID` (`releasedByUserID`);

--
-- Indexes for table `ExperimentAndModule`
--
ALTER TABLE `ExperimentAndModule`
  ADD KEY `experimentID` (`experimentID`),
  ADD KEY `moduleID` (`moduleID`);

--
-- Indexes for table `ExperimentAndType`
--
ALTER TABLE `ExperimentAndType`
  ADD KEY `experimentID` (`experimentID`),
  ADD KEY `experimentTypeID` (`experimentTypeID`);

--
-- Indexes for table `ExperimentStatus`
--
ALTER TABLE `ExperimentStatus`
  ADD PRIMARY KEY (`id`),
  ADD UNIQUE KEY `statuscode` (`statuscode`);

--
-- Indexes for table `ExperimentStatusHistory`
--
ALTER TABLE `ExperimentStatusHistory`
  ADD PRIMARY KEY (`id`),
  ADD KEY `expid` (`expID`);

--
-- Indexes for table `ExperimentType`
--
ALTER TABLE `ExperimentType`
  ADD PRIMARY KEY (`id`),
  ADD UNIQUE KEY `type` (`type`);

--
-- Indexes for table `ExportFile`
--
ALTER TABLE `ExportFile`
  ADD PRIMARY KEY (`id`),
  ADD KEY `experimentID` (`experimentID`);

--
-- Indexes for table `FileData`
--
ALTER TABLE `FileData`
  ADD PRIMARY KEY (`id`),
  ADD KEY `experimentID` (`experimentID`);

--
-- Indexes for table `Job`
--
ALTER TABLE `Job`
  ADD PRIMARY KEY (`id`),
  ADD KEY `statusID` (`statusID`),
  ADD KEY `passID` (`passID`);

--
-- Indexes for table `JobStatus`
--
ALTER TABLE `JobStatus`
  ADD PRIMARY KEY (`id`);

--
-- Indexes for table `Module`
--
ALTER TABLE `Module`
  ADD PRIMARY KEY (`id`),
  ADD UNIQUE KEY `vsn` (`vsn`);

--
-- Indexes for table `Pass`
--
ALTER TABLE `Pass`
  ADD PRIMARY KEY (`id`),
  ADD KEY `passTypeID` (`passTypeID`),
  ADD KEY `experimentID` (`experimentID`);

--
-- Indexes for table `PassType`
--
ALTER TABLE `PassType`
  ADD PRIMARY KEY (`id`);

--
-- Indexes for table `Slot`
--
ALTER TABLE `Slot`
  ADD PRIMARY KEY (`id`),
  ADD UNIQUE KEY `location` (`location`),
  ADD UNIQUE KEY `moduleID` (`moduleID`);

--
-- Indexes for table `User`
--
ALTER TABLE `User`
  ADD PRIMARY KEY (`id`);

--
-- Indexes for table `VersionHistory`
--
ALTER TABLE `VersionHistory`
  ADD PRIMARY KEY (`id`);

--
-- AUTO_INCREMENT for dumped tables
--

--
-- AUTO_INCREMENT for table `Experiment`
--
ALTER TABLE `Experiment`
  MODIFY `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT;
--
-- AUTO_INCREMENT for table `ExperimentStatus`
--
ALTER TABLE `ExperimentStatus`
  MODIFY `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT;
--
-- AUTO_INCREMENT for table `ExperimentStatusHistory`
--
ALTER TABLE `ExperimentStatusHistory`
  MODIFY `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT;
--
-- AUTO_INCREMENT for table `ExperimentType`
--
ALTER TABLE `ExperimentType`
  MODIFY `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT;
--
-- AUTO_INCREMENT for table `ExportFile`
--
ALTER TABLE `ExportFile`
  MODIFY `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT;
--
-- AUTO_INCREMENT for table `FileData`
--
ALTER TABLE `FileData`
  MODIFY `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT;
--
-- AUTO_INCREMENT for table `Job`
--
ALTER TABLE `Job`
  MODIFY `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT;
--
-- AUTO_INCREMENT for table `JobStatus`
--
ALTER TABLE `JobStatus`
  MODIFY `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT;
--
-- AUTO_INCREMENT for table `Module`
--
ALTER TABLE `Module`
  MODIFY `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT;
--
-- AUTO_INCREMENT for table `Pass`
--
ALTER TABLE `Pass`
  MODIFY `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT;
--
-- AUTO_INCREMENT for table `PassType`
--
ALTER TABLE `PassType`
  MODIFY `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT;
--
-- AUTO_INCREMENT for table `Slot`
--
ALTER TABLE `Slot`
  MODIFY `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT;
--
-- AUTO_INCREMENT for table `User`
--
ALTER TABLE `User`
  MODIFY `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT;
--
-- AUTO_INCREMENT for table `VersionHistory`
--
ALTER TABLE `VersionHistory`
  MODIFY `id` bigint(20) NOT NULL AUTO_INCREMENT;
--
-- Constraints for dumped tables
--

--
-- Constraints for table `Experiment`
--
ALTER TABLE `Experiment`
  ADD CONSTRAINT `Experiment_ibfk_1` FOREIGN KEY (`statusID`) REFERENCES `ExperimentStatus` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  ADD CONSTRAINT `Experiment_ibfk_2` FOREIGN KEY (`userID`) REFERENCES `User` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  ADD CONSTRAINT `Experiment_ibfk_3` FOREIGN KEY (`releasedByUserID`) REFERENCES `User` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION;

--
-- Constraints for table `ExperimentAndModule`
--
ALTER TABLE `ExperimentAndModule`
  ADD CONSTRAINT `ExperimentAndModule_ibfk_1` FOREIGN KEY (`experimentID`) REFERENCES `Experiment` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  ADD CONSTRAINT `ExperimentAndModule_ibfk_2` FOREIGN KEY (`moduleID`) REFERENCES `Module` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION;

--
-- Constraints for table `ExperimentAndType`
--
ALTER TABLE `ExperimentAndType`
  ADD CONSTRAINT `ExperimentAndType_ibfk_1` FOREIGN KEY (`experimentID`) REFERENCES `Experiment` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  ADD CONSTRAINT `ExperimentAndType_ibfk_2` FOREIGN KEY (`experimentTypeID`) REFERENCES `ExperimentType` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION;

--
-- Constraints for table `ExperimentStatusHistory`
--
ALTER TABLE `ExperimentStatusHistory`
  ADD CONSTRAINT `ExperimentStatusHistory_ibfk_1` FOREIGN KEY (`expID`) REFERENCES `Experiment` (`id`) ON DELETE CASCADE;

--
-- Constraints for table `ExportFile`
--
ALTER TABLE `ExportFile`
  ADD CONSTRAINT `ExportFile_ibfk_1` FOREIGN KEY (`experimentID`) REFERENCES `Experiment` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION;

--
-- Constraints for table `FileData`
--
ALTER TABLE `FileData`
  ADD CONSTRAINT `FileData_ibfk_1` FOREIGN KEY (`experimentID`) REFERENCES `Experiment` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION;

--
-- Constraints for table `Job`
--
ALTER TABLE `Job`
  ADD CONSTRAINT `Job_ibfk_1` FOREIGN KEY (`passID`) REFERENCES `Pass` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  ADD CONSTRAINT `Job_ibfk_2` FOREIGN KEY (`statusID`) REFERENCES `JobStatus` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION;

--
-- Constraints for table `Pass`
--
ALTER TABLE `Pass`
  ADD CONSTRAINT `Pass_ibfk_1` FOREIGN KEY (`experimentID`) REFERENCES `Experiment` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  ADD CONSTRAINT `Pass_ibfk_2` FOREIGN KEY (`passTypeID`) REFERENCES `PassType` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION;

--
-- Constraints for table `Slot`
--
ALTER TABLE `Slot`
  ADD CONSTRAINT `Slot_ibfk_1` FOREIGN KEY (`moduleID`) REFERENCES `Module` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION;

--
-- Dumping data for table `ExperimentStatus`
--

INSERT INTO `ExperimentStatus` (`id`, `statuscode`, `experimentstatus`) VALUES
(1, 10, 'scheduled'),
(2, 20, 'waiting for correlation'),
(3, 30, 'started correlation'),
(4, 40, 'finished correlation'),
(5, 100, 'released'),
(6, 110, 'correlated elsewhere'),
(7, 0, 'unknown'),
(8, 200, 'aborted'),
(9, 25, 'waiting for data'),
(10, 60, 'data released to PI'),

--
-- Dumping data for table `ExperimentType`
--

INSERT INTO `ExperimentType` (`id`, `type`, `active`) VALUES
(8, 'Geo', 1),
(10, 'Astro', 1),
(11, 'Test', 1);

--
-- Dumping data for table `JobStatus`
--

INSERT INTO `JobStatus` (`id`, `status`, `active`) VALUES
(1, 'unknown', 0),
(2, 'queued', 1),
(3, 'running', 1),
(4, 'complete', 0),
(5, 'killed', 0),
(6, 'failed', 0);

--
-- Dumping data for table `PassType`
--

INSERT INTO `PassType` (`id`, `type`) VALUES
(1, 'production'),
(2, 'clock'),
(3, 'test');

--
-- Dumping data for table `VersionHistory`
--

INSERT INTO `VersionHistory` (`id`, `major`, `minor`, `dateCreated`) VALUES
(1, 1, 8, '2018-05-09 12:07:56');

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
