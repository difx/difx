USE difxdb;

CREATE TABLE IF NOT EXISTS `ExportFile` (
`id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `filename` varchar(255) NOT NULL,
  `exportPath` varchar(255) NOT NULL,
  `checksum` char(32) NOT NULL,
  `dateCreated` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `ExperimentAndExportFile` (
  `experimentID` bigint(20) DEFAULT NULL,
  `exportFileID` bigint(20) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

INSERT INTO `difxdb`.`VersionHistory` (
`major` ,
`minor`
)
VALUES (
'1', '5'
);
