USE difxdb;

CREATE TABLE IF NOT EXISTS `ExportFile` (
`id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `filename` varchar(255) NOT NULL,
  `exportPath` varchar(255) NOT NULL,
  `checksum` char(32) NOT NULL,
  `dateCreated` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;


ALTER TABLE `ExportFile` ADD INDEX(`experimentID`);

ALTER TABLE `ExportFile` ADD FOREIGN KEY (`experimentID`) REFERENCES `difxdb`.`Experiment`(`id`) ON DELETE NO ACTION ON UPDATE NO ACTION;

INSERT INTO `difxdb`.`VersionHistory` (
`major` ,
`minor`
)
VALUES (
'1', '5'
);
