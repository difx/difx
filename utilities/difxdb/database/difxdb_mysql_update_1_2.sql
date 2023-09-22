USE difxdb;

CREATE TABLE IF NOT EXISTS `ExperimentAndType` (
  `experimentID` bigint(20) unsigned NOT NULL,
  `experimentTypeID` bigint(20) unsigned NOT NULL,
  KEY `experimentTypeID` (`experimentTypeID`),
  KEY `experimentID` (`experimentID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

CREATE TABLE IF NOT EXISTS `ExperimentType` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `type` varchar(100) NOT NULL,
  `active` tinyint(1) NOT NULL DEFAULT '1',
  PRIMARY KEY (`id`),
  UNIQUE KEY `type` (`type`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=13 ;
--
-- Constraints der Tabelle `ExperimentAndType`
--
ALTER TABLE `ExperimentAndType`
  ADD CONSTRAINT `ExperimentAndType_ibfk_1` FOREIGN KEY (`experimentID`) REFERENCES `Experiment` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  ADD CONSTRAINT `ExperimentAndType_ibfk_2` FOREIGN KEY (`experimentTypeID`) REFERENCES `ExperimentType` (`Id`) ON DELETE NO ACTION ON UPDATE NO ACTION;

INSERT INTO `difxdb`.`VersionHistory` (
`major` ,
`minor`
)
VALUES (
'1', '2'
);

