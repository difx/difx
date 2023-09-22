-- phpMyAdmin SQL Dump
-- version 3.3.7deb6
-- http://www.phpmyadmin.net
--
-- Host: localhost
-- Erstellungszeit: 30. November 2011 um 10:31
-- Server Version: 5.1.49
-- PHP-Version: 5.3.3-7+squeeze3



/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
use difxdb;

--
-- Datenbank: `difxdb`
--

--
-- Daten für Tabelle `ExperimentStatus`
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
(9, 25, 'waiting for data');

--
-- Daten für Tabelle `JobStatus`
--

INSERT INTO `JobStatus` (`id`, `status`, `active`) VALUES
(1, 'unknown', 0),
(2, 'queued', 1),
(3, 'running', 1),
(4, 'complete', 0),
(5, 'killed', 0),
(6, 'failed', 0);

--
-- Daten für Tabelle `PassType`
--

INSERT INTO `PassType` (`id`, `type`) VALUES
(1, 'production'),
(2, 'clock'),
(3, 'test');
