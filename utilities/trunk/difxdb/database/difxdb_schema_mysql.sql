-- phpMyAdmin SQL Dump
-- version 3.3.7deb6
-- http://www.phpmyadmin.net
--
-- Host: localhost
-- Erstellungszeit: 30. November 2011 um 10:30
-- Server Version: 5.1.49
-- PHP-Version: 5.3.3-7+squeeze3

SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

USE difxdb;
--
-- Datenbank: `difxdb`
--

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `Experiment`
--

DROP TABLE IF EXISTS `Experiment`;
CREATE TABLE IF NOT EXISTS `Experiment` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `code` varchar(20) NOT NULL,
  `number` int(4) unsigned zerofill NOT NULL,
  `statusID` bigint(20) unsigned NOT NULL DEFAULT '1',
  `dateCreated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`),
  UNIQUE KEY `code` (`code`),
  KEY `statusID` (`statusID`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=98 ;

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `ExperimentAndModule`
--

DROP TABLE IF EXISTS `ExperimentAndModule`;
CREATE TABLE IF NOT EXISTS `ExperimentAndModule` (
  `experimentID` bigint(20) unsigned NOT NULL,
  `moduleID` bigint(20) unsigned NOT NULL,
  KEY `experimentID` (`experimentID`),
  KEY `moduleID` (`moduleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `ExperimentStatus`
--

DROP TABLE IF EXISTS `ExperimentStatus`;
CREATE TABLE IF NOT EXISTS `ExperimentStatus` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `statuscode` int(11) NOT NULL,
  `experimentstatus` varchar(100) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=10 ;

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `Job`
--

DROP TABLE IF EXISTS `Job`;
CREATE TABLE IF NOT EXISTS `Job` (
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
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `JobStatus`
--

DROP TABLE IF EXISTS `JobStatus`;
CREATE TABLE IF NOT EXISTS `JobStatus` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `status` varchar(100) NOT NULL,
  `active` tinyint(1) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=7 ;

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `Module`
--

DROP TABLE IF EXISTS `Module`;
CREATE TABLE IF NOT EXISTS `Module` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `experimentID` bigint(20) unsigned DEFAULT NULL,
  `vsn` varchar(8) NOT NULL,
  `capacity` int(11) NOT NULL,
  `datarate` int(11) NOT NULL,
  `numScans` int(11) DEFAULT NULL,
  `stationCode` varchar(2) DEFAULT NULL,
  `received` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `shipped` timestamp NULL DEFAULT '0000-00-00 00:00:00',
  PRIMARY KEY (`id`),
  UNIQUE KEY `vsn` (`vsn`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=302 ;

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `Pass`
--

DROP TABLE IF EXISTS `Pass`;
CREATE TABLE IF NOT EXISTS `Pass` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `experimentID` bigint(20) unsigned NOT NULL,
  `passName` varchar(30) NOT NULL,
  `passTypeID` bigint(20) unsigned NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `PassType`
--

DROP TABLE IF EXISTS `PassType`;
CREATE TABLE IF NOT EXISTS `PassType` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `type` varchar(50) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=4 ;

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `Slot`
--

DROP TABLE IF EXISTS `Slot`;
CREATE TABLE IF NOT EXISTS `Slot` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `moduleID` bigint(20) unsigned DEFAULT NULL,
  `location` varchar(30) NOT NULL,
  `isActive` tinyint(1) NOT NULL DEFAULT '1',
  PRIMARY KEY (`id`),
  UNIQUE KEY `location` (`location`),
  UNIQUE KEY `moduleID` (`moduleID`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=563 ;

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `vDOIQueue`
--

DROP TABLE IF EXISTS `vDOIQueue`;
--
-- CREATE ALGORITHM=UNDEFINED DEFINER=`difxdb_admin`@`127.0.0.1` SQL SECURITY DEFINER VIEW `difxdb`.`vDOIQueue` AS select `E`.`code` AS `code`,`P`.`passName` AS `passName`,`J`.`jobNumber` AS `jobNumber`,`J`.`priority` AS `priority`,`J`.`jobStart` AS `jobStart`,`J`.`jobDuration` AS `jobDuration`,`J`.`inputFile` AS `inputFile`,`J`.`speedupFactor` AS `speedupFactor`,`J`.`numAntennas` AS `numAntennas`,`S`.`status` AS `status` from ((((`difxdb`.`Job` `J` join `difxdb`.`Pass` `P` on((`J`.`passID` = `P`.`id`))) join `difxdb`.`Experiment` `E` on((`P`.`experimentID` = `E`.`id`))) join `difxdb`.`JobStatus` `S` on((`S`.`id` = `J`.`statusID`))) join `difxdb`.`PassType` on((`difxdb`.`PassType`.`id` = `P`.`passTypeID`)));
--

--
-- Constraints der exportierten Tabellen
--

--
-- Constraints der Tabelle `Experiment`
--
ALTER TABLE `Experiment`
  ADD CONSTRAINT `Experiment_ibfk_1` FOREIGN KEY (`statusID`) REFERENCES `ExperimentStatus` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION;

--
-- Constraints der Tabelle `ExperimentAndModule`
--
ALTER TABLE `ExperimentAndModule`
  ADD CONSTRAINT `ExperimentAndModule_ibfk_1` FOREIGN KEY (`experimentID`) REFERENCES `Experiment` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  ADD CONSTRAINT `ExperimentAndModule_ibfk_2` FOREIGN KEY (`moduleID`) REFERENCES `Module` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION;

--
-- Constraints der Tabelle `Slot`
--
ALTER TABLE `Slot`
  ADD CONSTRAINT `Slot_ibfk_1` FOREIGN KEY (`moduleID`) REFERENCES `Module` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION;
